package mycpu

import chisel3._
import chisel3.util._

class LsqEntry extends Bundle {
    val valid      = Bool()
    val is_load    = Bool()
    val is_store   = Bool()
    val is_cacop   = Bool()

    val rob_idx    = UInt(Config.robPtrWidth.W)
    val pc         = UInt(32.W)     
    val pdest      = UInt(Config.prfPtrWidth.W)

    val branch_mask= UInt(4.W)      

    val addr_valid = Bool()
    val paddr      = UInt(32.W)
    val vaddr      = UInt(32.W)
    val size       = UInt(2.W)
    val uncached   = Bool()
    val wdata      = UInt(32.W)
    val wstrb      = UInt(4.W)
    val cacop_op   = UInt(5.W)      

    val req_sent   = Bool()         // ★ Cache 请求已发出
    val executed   = Bool()         // ★ Cache 数据已返回
    val has_exc    = Bool()
    val ecode      = UInt(6.W)
    val committed  = Bool()         

    val lsOp       = UInt(8.W) // ★ 补充：记录 Load 指令的具体操作码

    val ticket     = UInt(8.W)      // ★ 终极凭证
    val wb_sent    = Bool()         // ★ 标记是否已经送入 CDB 写回队列
}

class LSQ extends Module {
    val io = IO(new Bundle {
        val flush      = Input(Bool()) 
        val br_resolve = Input(new BranchResolve()) 
        
        // ----------------------------------------------------
        // 接口 A：与前台 (Dispatch) 的交互
        // ----------------------------------------------------
        val alloc      = new LsqAllocIO()
        val state      = new LsqStatePort() 
        val violation  = new LsqViolationPort()
        val commit_mem = Flipped(new CommitMemPort())

        // ----------------------------------------------------
        // 接口 B：与 AGU 的交互 (算址完成)
        // ----------------------------------------------------
        val agu_in = Flipped(Valid(new Agu2Lsq()))

        // ----------------------------------------------------
        // 接口 D：与 Data Cache 的交互
        // ----------------------------------------------------
        val dcache = new SramIo()
        val dcache_uncached = Output(Bool())
        val cacop_en = Output(Bool())
        val cacop_op = Output(UInt(2.W))
        val cacop_is_icache = Output(Bool())

        // ----------------------------------------------------
        // 接口 E：与 ROB 的交互
        // ----------------------------------------------------
        val lsq_wb = Decoupled(new PipelineData())

       

        val dcache_req_id = Output(UInt(8.W)) // ★ 新增
        val dcache_ret_id = Input(UInt(8.W))  // ★ 新增

        val rob_head   = Input(UInt(Config.robPtrWidth.W))

        // ★★★ 新增：提前唤醒专线 (Early Wakeup) ★★★
        val early_wakeup = Valid(UInt(Config.prfPtrWidth.W))
    })
    val ticket_counter = RegInit(0.U(8.W))

    val entries = RegInit(VecInit(Seq.fill(16)(0.U.asTypeOf(new LsqEntry()))))
    val head = RegInit(0.U(4.W)) 
    val tail = RegInit(0.U(4.W)) 
    val is_full = RegInit(false.B)
    val is_empty = (!is_full && (head === tail))

    //==========================================
    // Flush logic
    //==========================================
    val br_fail = io.br_resolve.valid && io.br_resolve.mispredict
    val br_tag_bit = 1.U(4.W) << io.br_resolve.tag
    def is_killed(mask: UInt): Bool = br_fail && ((mask & br_tag_bit) =/= 0.U)
    val clear_mask = Mux(io.br_resolve.valid && !io.br_resolve.mispredict, ~br_tag_bit, "b1111".U(4.W))











    // ★ 冗余清理：彻底删除 outstanding_idx 和 outstanding_valid 及其相关的阻塞隔离带
    io.state.current_tail := tail
    io.alloc.req.ready    := !is_full 
    io.alloc.idx          := tail

    val alloc_bits = io.alloc.req.bits
    val br_restore_tail = io.state.br_restore

    // ==========================================
    // 1. 分支爆破与尾指针回退 (Mispredict Rollback)
    // ==========================================
    val is_mispredict = io.br_resolve.valid && io.br_resolve.mispredict
    val tag_bit = 1.U(4.W) << io.br_resolve.tag

    for (i <- 0 until 16) {
        when(entries(i).valid && io.br_resolve.valid) {
            val is_dependent = (entries(i).branch_mask & tag_bit) =/= 0.U
            when(io.br_resolve.mispredict) {
                when(is_dependent) { entries(i).valid := false.B }
            } .otherwise {
                when(is_dependent) { entries(i).branch_mask := entries(i).branch_mask & ~tag_bit }
            }
        }
    }

    val real_alloc = io.alloc.req.valid && !is_mispredict
    when(real_alloc && io.alloc.req.ready) {
        entries(tail).valid      := true.B
        entries(tail).is_load    := alloc_bits.req_type === 0.U
        entries(tail).is_store   := alloc_bits.req_type === 1.U
        entries(tail).is_cacop   := alloc_bits.req_type === 2.U
        entries(tail).rob_idx    := alloc_bits.rob
        entries(tail).pc         := alloc_bits.pc
        entries(tail).pdest      := alloc_bits.pdest
        entries(tail).branch_mask:= alloc_bits.mask
        entries(tail).cacop_op   := alloc_bits.cacop
        entries(tail).lsOp       := alloc_bits.lsOp
        entries(tail).addr_valid := false.B
        entries(tail).req_sent   := false.B
        entries(tail).executed   := false.B
        entries(tail).has_exc    := false.B
        entries(tail).committed  := false.B
        entries(tail).ticket     := ticket_counter
        entries(tail).wb_sent    := false.B
        ticket_counter := ticket_counter + 1.U
        
        val next_tail = tail + 1.U
        tail := next_tail
        when(next_tail === head) { is_full := true.B }
    } .elsewhen(is_mispredict) {
        tail := br_restore_tail 
        // ★ 核心修复：只有当 tail 真的发生了回退（杀死了未提交指令）时，才能解除 full 状态！
        // 如果 tail 没变，说明它原本是满的，回档后依然是满的！
        when(tail =/= br_restore_tail) { is_full := false.B }
    }

    // ==========================================
    // 3. 接收 AGU 计算结果 & 违例检测 CAM
    // ==========================================
    val violation_reg = RegInit(false.B)
    val violation_rob = RegInit(0.U(Config.robPtrWidth.W))
    val violation_pc  = RegInit(0.U(32.W))
    // ★ 新增：记录是 LSQ 的哪一个位置惹的祸
    val violation_lsq = RegInit(0.U(4.W))

    // 1. 仅当拍写入信息 (剥离了庞大的 CAM 逻辑，让它极速完成)
    when(io.agu_in.valid && entries(io.agu_in.bits.lsqIdx).valid) {
        val idx = io.agu_in.bits.lsqIdx
        entries(idx).addr_valid := true.B
        entries(idx).paddr      := io.agu_in.bits.paddr
        entries(idx).vaddr      := io.agu_in.bits.vaddr
        entries(idx).size       := io.agu_in.bits.size
        entries(idx).uncached   := io.agu_in.bits.uncached
        entries(idx).wdata      := io.agu_in.bits.wdata
        entries(idx).wstrb      := io.agu_in.bits.wstrb
        entries(idx).has_exc    := io.agu_in.bits.has_exc  // 注意：Bundle里叫has_exc
        entries(idx).ecode      := io.agu_in.bits.ecode
    }

    // 2. ★ 核心时序修复：将触发 CAM 查表的条件提取出来，打一拍 (Retiming)
    val agu_is_store_or_cacop = entries(io.agu_in.bits.lsqIdx).is_store || entries(io.agu_in.bits.lsqIdx).is_cacop
    val check_valid = RegInit(false.B)
    val check_idx   = RegNext(io.agu_in.bits.lsqIdx)
    val check_paddr = RegNext(io.agu_in.bits.paddr)

    // ★ 幽灵防御：遇到流水线冲刷时，必须清空 check_valid，防止下一拍报出“前朝的幽灵违例”
    when(io.flush) {
        check_valid := false.B
    } .otherwise {
        check_valid := io.agu_in.valid && entries(io.agu_in.bits.lsqIdx).valid && agu_is_store_or_cacop
    }

    // 3. 在下一拍 (T+1) 从容地进行 16 项大并发 CAM 查表
    val v_vec = WireDefault(VecInit(Seq.fill(16)(false.B)))
    
    for (i <- 0 until 16) {
        val e = entries(i)
        // 环形指针判断年龄 (T+1 拍比较绝对安全，因为 Store 还没提交，head 绝对不可能越过 check_idx)
        val is_younger = (i.U - head) > (check_idx - head)
        // 使用打过一拍的 check_paddr 进行地址行比对，彻底斩断上游的 34 级逻辑链！
        val addr_conflict = e.addr_valid && (e.paddr(31,2) === check_paddr(31,2))
        
        when(check_valid && e.valid && e.is_load && e.executed && !e.committed && is_younger && addr_conflict) {
            v_vec(i) := true.B
        }
    }
    
    val has_v = v_vec.asUInt.orR
    val v_idx = PriorityEncoder(v_vec)
    
    // 写入 violation_reg
    when(has_v && !violation_reg) {
        violation_reg := true.B
        violation_rob := entries(v_idx).rob_idx
        violation_pc  := entries(v_idx).pc
        violation_lsq := v_idx // ★ 记下肇事者的 LSQ 编号
    }
    
    io.violation.valid := violation_reg
    io.violation.rob   := violation_rob
    io.violation.pc    := violation_pc

    // ★ 核心破咒逻辑：找个空白处（或者直接放在上面这段后面）加上这段代码：
    // 如果肇事者在 LSQ 里被分支预测杀死了（valid 变成了 0），立刻解除全线警报！
    when(violation_reg && !entries(violation_lsq).valid) {
        violation_reg := false.B
    }

    // ==========================================
    // 4. 接收 ROB 提交信号
    // ==========================================
    when(io.commit_mem.valid0) {
        for (i <- 0 until 16) {
            when(entries(i).valid && entries(i).rob_idx === io.commit_mem.idx0) {
                entries(i).committed := true.B
            }
        }
    }
    when(io.commit_mem.valid1) {
        for (i <- 0 until 16) {
            when(entries(i).valid && entries(i).rob_idx === io.commit_mem.idx1) {
                entries(i).committed := true.B
            }
        }
    }
    // =====================================================================
    // 5. 终极流水线切片：非阻塞乱序发射引擎 (Stage 1 & 2)
    // =====================================================================
    
    // ---------------------------------------------------------------------
    // [STAGE 1]：纯组合逻辑依赖分析 (CAM 匹配)
    // ---------------------------------------------------------------------
    val stlf_conflict_vec   = WireDefault(VecInit(Seq.fill(16)(false.B)))
    val stlf_can_fwd_vec    = WireDefault(VecInit(Seq.fill(16)(false.B)))
    val stlf_fwd_data_vec   = WireDefault(VecInit(Seq.fill(16)(0.U(32.W))))
    // ★ 核心修复：把 addr_valid 也送入流水线打拍，与 STLF 结果严格对齐！
    val stlf_addr_valid_vec = WireDefault(VecInit(Seq.fill(16)(false.B)))

    for (i <- 0 until 16) {
        val e = entries(i)
        stlf_addr_valid_vec(i) := e.addr_valid

        val load_conflict = WireDefault(false.B)
        val can_fwd       = WireDefault(false.B)
        val fwd_data      = WireDefault(0.U(32.W))

        val load_offset = e.paddr(1, 0)
        val load_mask = Mux(e.size === 0.U, 1.U(4.W) << load_offset,
                        Mux(e.size === 1.U, 3.U(4.W) << load_offset, "b1111".U(4.W)))
        val load_age = (i.U + 16.U - head)(3,0)

        for (d <- 15 to 1 by -1) {
            val j = (i + 16 - d) % 16
            val older = entries(j)
            val older_write = older.valid && (older.is_store || older.is_cacop)
            val is_really_older = d.U <= load_age
            val same_word = older.addr_valid && (older.paddr(31,2) === e.paddr(31,2))

            when(is_really_older && older_write) {
                when(!older.addr_valid) {
                    load_conflict := true.B; can_fwd := false.B
                } .elsewhen(same_word) {
                    val overlap = (older.wstrb & load_mask) =/= 0.U
                    val full_cover = (older.wstrb & load_mask) === load_mask
                    val is_uncached_hazard = e.uncached || older.uncached

                    when(full_cover) {
                        when(is_uncached_hazard) {
                            load_conflict := true.B; can_fwd := false.B 
                        } .otherwise {
                            can_fwd := true.B; fwd_data := older.wdata; load_conflict := false.B 
                        }
                    } .elsewhen(overlap) {
                        load_conflict := true.B; can_fwd := false.B 
                    }
                }
            }
        }
        stlf_conflict_vec(i) := load_conflict
        stlf_can_fwd_vec(i)  := can_fwd
        stlf_fwd_data_vec(i) := fwd_data
    }

    // ★ 时序核武器：所有查表结果与前提条件，打包进入同一级流水线！
    val pipe_conflict   = RegNext(stlf_conflict_vec)
    val pipe_can_fwd    = RegNext(stlf_can_fwd_vec)
    val pipe_fwd_data   = RegNext(stlf_fwd_data_vec)
    val pipe_addr_valid = RegNext(stlf_addr_valid_vec)

    // ---------------------------------------------------------------------
    // [STAGE 2]：状态过滤与仲裁发射 (极速路线)
    // ---------------------------------------------------------------------
    val normal_in_flight = entries.map(e => e.valid && !e.is_cacop && e.req_sent && !e.executed).reduce(_ || _)
    val cacop_in_flight_state = entries.map(e => e.valid && e.is_cacop && e.req_sent && !e.executed).reduce(_ || _)

    val safe_to_issue_cacop  = !normal_in_flight
    val safe_to_issue_normal = !cacop_in_flight_state

    val actual_can_issue = WireDefault(VecInit(Seq.fill(16)(false.B)))
    val actual_do_stlf   = WireDefault(VecInit(Seq.fill(16)(false.B)))

    for(i <- 0 until 16) {
        val e = entries(i)
        val is_active = e.valid && !e.req_sent && !e.executed && !e.has_exc

        val conflict     = pipe_conflict(i)
        val can_fwd      = pipe_can_fwd(i)
        val p_addr_valid = pipe_addr_valid(i)

        // ★★★ 终极防线：双重地址锁 ★★★
        // 必须同时满足当拍有地址、且上一拍也有地址。
        // 这样既给了 STLF 矩阵一拍的时间出结果，又完美屏蔽了槽位刚被分配时继承的老指令幽灵状态！
        val stable_addr_valid = e.addr_valid && p_addr_valid

        val store_ready = (e.is_store || e.is_cacop) && stable_addr_valid && e.committed
        val load_ready  = e.is_load && stable_addr_valid
        val base_rdy    = store_ready || load_ready


        val is_oldest = (e.rob_idx === io.rob_head)
        // 2. 终极幽灵防线：
        //    - 如果是普通的 Cached 指令，直接安全 (!e.uncached)
        //    - 如果是 Store/CACOP，它们有 committed 护体，直接安全 (e.committed)
        //    - 如果是 Uncached Load，必须等到它是最老指令，才算安全 (is_oldest)
        val uncached_safe = !e.uncached || e.committed || is_oldest
        val e_can_issue_to_cache = is_active && base_rdy && uncached_safe && (!e.is_load || (!conflict && !can_fwd))
        actual_can_issue(i) := e_can_issue_to_cache && Mux(e.is_cacop, safe_to_issue_cacop, safe_to_issue_normal)

        actual_do_stlf(i) := is_active && e.is_load && base_rdy && can_fwd
    }

    // --- 2.1 VIP 前递通道 (直接吃掉流水线数据) ---
    for (i <- 0 until 16) {
        when(actual_do_stlf(i)) {
            entries(i).req_sent := true.B
            entries(i).executed := true.B
            entries(i).wdata    := pipe_fwd_data(i)
        }
    }

    // --- 2.2 独热码环形仲裁器 ---
    // 为了防止截断报警，严格规范位宽
    val req = actual_can_issue.asUInt(15, 0)
    val head_mask = (~((1.U(16.W) << head) - 1.U))(15, 0)
    val masked_req = req & head_mask
    val grant_masked_oh   = PriorityEncoderOH(masked_req)
    val grant_unmasked_oh = PriorityEncoderOH(req)
    val grant_oh = Mux(masked_req.orR, grant_masked_oh, grant_unmasked_oh)

    val do_issue  = req.orR
    val issue_e   = Mux1H(grant_oh, entries)
    val issue_idx = OHToUInt(grant_oh)

    // --- 2.3 斩断发射长路径：直接打入发射寄存器 ---
    val out_valid_reg = RegInit(false.B)
    val out_e_reg     = RegInit(0.U.asTypeOf(new LsqEntry()))
    val out_ready = !out_valid_reg || io.dcache.addr_ok

    when (out_ready) {
        out_valid_reg := do_issue
        when (do_issue) {
            out_e_reg := issue_e
            entries(issue_idx).req_sent := true.B 
        }
    }

    io.dcache.req      := out_valid_reg
    io.dcache_req_id   := out_e_reg.ticket
    io.dcache.wr       := out_e_reg.is_store
    io.dcache.size     := out_e_reg.size
    io.dcache.addr     := out_e_reg.paddr
    io.dcache.wdata    := out_e_reg.wdata
    io.dcache.wstrb    := out_e_reg.wstrb
    io.dcache_uncached := out_e_reg.uncached

    io.cacop_en        := out_valid_reg && out_e_reg.is_cacop
    io.cacop_op        := out_e_reg.cacop_op(4,3)
    io.cacop_is_icache := out_e_reg.is_cacop && (out_e_reg.cacop_op(2,0) === 0.U)
    
    // ---------------- 异步接收外卖 (Ticket 匹配机制) ----------------
    // 打拍缓冲，斩断 DCache 到 LSQ 的长布线延迟
    val dcache_data_ok_reg = RegNext(io.dcache.data_ok, false.B)
    val dcache_rdata_reg   = RegNext(io.dcache.rdata)
    val dcache_ret_id_reg  = RegNext(io.dcache_ret_id)

    val ret_ticket = dcache_ret_id_reg
    val ret_match_vec = WireDefault(VecInit(Seq.fill(16)(false.B)))
    for(i <- 0 until 16) {
        ret_match_vec(i) := entries(i).valid && (entries(i).ticket === ret_ticket)
    }
    
    val ret_valid = dcache_data_ok_reg && ret_match_vec.asUInt.orR
    val ret_idx   = PriorityEncoder(ret_match_vec)

    when(ret_valid) {
        entries(ret_idx).executed := true.B
        when(entries(ret_idx).is_load) {
            entries(ret_idx).wdata := dcache_rdata_reg 
        }
    }

    // ==========================================
    // 6. 乱序写回仲裁 (Writeback Arbiter)
    // ==========================================
    val can_wb_vec = WireDefault(VecInit(Seq.fill(16)(false.B)))
    for(i <- 0 until 16) {
        // ★ 核心大改 1：Load 只要成功执行了，或者发生了异常，都具备了写回资格！
        val ready_to_wb = entries(i).executed || entries(i).has_exc
        can_wb_vec(i) := entries(i).valid && entries(i).is_load && ready_to_wb && !entries(i).wb_sent
    }
    val do_wb = can_wb_vec.asUInt.orR
    val wb_idx = PriorityEncoder(can_wb_vec)
    val wb_e = entries(wb_idx)
    
    val final_offset = wb_e.paddr(1, 0) 
    val byte_data = MuxLookup(final_offset, 0.U(8.W))(Seq(
        0.U -> wb_e.wdata(7, 0),   1.U -> wb_e.wdata(15, 8),
        2.U -> wb_e.wdata(23, 16), 3.U -> wb_e.wdata(31, 24)
    ))
    val half_data = Mux(final_offset(1), wb_e.wdata(31, 16), wb_e.wdata(15, 0))

    val formatted_result = MuxLookup(wb_e.lsOp, wb_e.wdata)(Seq(
        LsOp.LD_B  -> Cat(Fill(24, byte_data(7)), byte_data),
        LsOp.LD_BU -> Cat(0.U(24.W), byte_data),
        LsOp.LD_H  -> Cat(Fill(16, half_data(15)), half_data),
        LsOp.LD_HU -> Cat(0.U(16.W), half_data),
        LsOp.LD_W  -> wb_e.wdata
    ))

    val wb_data = WireDefault(0.U.asTypeOf(new PipelineData()))
    wb_data.rob_idx    := wb_e.rob_idx
    wb_data.pdest      := wb_e.pdest
    // ★ 核心修复：如果是异常，必须把 paddr（里面存着 AGU 传来的坏虚拟地址）作为 ex_result 报给 ROB！
    // 否则 ROB 会把残留的垃圾数据（wdata）当成坏地址写进 CSR badv！
    wb_data.ex_result  := Mux(wb_e.has_exc, wb_e.vaddr, formatted_result)
    wb_data.regWriteEn := true.B
    // ★ 核心大改 2：写回 CDB 时，必须把异常信息原封不动地带上！
    wb_data.hasException := wb_e.has_exc
    wb_data.ecode        := wb_e.ecode
    wb_data.resFromMem   := true.B
    wb_data.lsq_idx      := wb_idx
    wb_data.aux_data     := wb_e.ticket
    wb_data.pc           := wb_e.pc

    // ★ 直接用仲裁结果驱动 CDB！没有握手成功就憋在 LSQ 表项里，随时接受分支冲刷！
    io.lsq_wb.valid := do_wb
    io.lsq_wb.bits  := wb_data
    
    // 当真实写回成功时，才打上 wb_sent 标记
    when(io.lsq_wb.fire) { 
        entries(wb_idx).wb_sent := true.B 
    }

    // ---------------- 队头出队 ----------------
    val head_entry = entries(head)
    
    // 正常指令的退役条件 (必须有效，且完成执行，且被 ROB 提交)
    val head_is_done = head_entry.executed || head_entry.has_exc
    val normal_can_pop = head_entry.valid && head_is_done && head_entry.committed && (!head_entry.is_load || head_entry.wb_sent)
    
    // ★ 终极防死锁修复：如果队头是一具已经被分支预测杀死的尸体（valid == 0），必须无条件直接弹出！
    val ghost_can_pop = !head_entry.valid

    // 只要是非空状态，无论是正常提交还是清理尸体，都允许出队！
    val head_can_pop = !is_empty && (normal_can_pop || ghost_can_pop)
    
    when(head_can_pop) {
        entries(head).valid := false.B // 虽然 ghost 已经是 false，再写一次也没关系
        head := head + 1.U
        is_full := false.B
    }

    // ==========================================
    // 7. 终极异常冲刷 (Flush)
    // ==========================================
    // 统计当前 LSQ 里有多少个“合法且已经提交”的残留指令
    // ==========================================
    // 7. 终极异常冲刷 (Flush)
    // ==========================================
    // 统计当前 LSQ 里有多少个“合法且已经提交”的残留指令
    val commit_cnt = PopCount(entries.map(e => e.valid && e.committed))

    when(io.flush) {
        // ★ 修复 Bug 1 (过河拆桥)：绝对不准杀掉 committed 的 Store！
        for(i <- 0 until 16) { 
            when(!entries(i).committed) {
                entries(i).valid := false.B 
            }
        }
        // 既然把没提交的都杀了，留下来的 committed 指令必定连在一起。
        // 所以直接把尾指针拉到最后一个 committed 指令的后面！
        tail := head + commit_cnt
        
        // ★ 核心修复：如果留下的 committed 指令刚好有 16 个，队列依然是满的！绝不能无脑清零！
        is_full := (commit_cnt === 16.U)
        violation_reg := false.B
    }




    // ==========================================
    // ★ 前递网络：推测唤醒 (Early Wakeup)
    // ==========================================
    // 1. 抓取即将从 DCache 回来的外卖
    val early_ret_ticket = io.dcache_ret_id
    val early_match_vec = WireDefault(VecInit(Seq.fill(16)(false.B)))
    for(i <- 0 until 16) {
        early_match_vec(i) := entries(i).valid && (entries(i).ticket === early_ret_ticket) && entries(i).is_load
    }
    val early_ret_valid = io.dcache.data_ok && early_match_vec.asUInt.orR
    val early_ret_idx   = PriorityEncoder(early_match_vec)

    // 2. 抓取内部 STLF (Store-to-Load Forwarding) 即将成功的数据
    val stlf_wakeup_valid = actual_do_stlf.asUInt.orR
    val stlf_wakeup_idx   = PriorityEncoder(actual_do_stlf)

    // 3. 只要有一路成功，立刻向 IQ 广播该指令的目标物理寄存器号 (pdest)！
    // ★ 核心修复：强行打一拍 (RegNext)，切断 Wakeup-Select 27ns 超长组合逻辑链！
    io.early_wakeup.valid := RegNext(early_ret_valid || stlf_wakeup_valid, false.B)
    io.early_wakeup.bits  := RegNext(Mux(early_ret_valid, entries(early_ret_idx).pdest, entries(stlf_wakeup_idx).pdest), 0.U)

}