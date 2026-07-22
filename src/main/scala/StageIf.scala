package mycpu

import chisel3._
import chisel3.util._

// ====================================================================
// ★ 新增：支持双进单出的自适应指令队列 (Dual-Fetch Buffer)
// ====================================================================
class DualFetchBuffer(depth: Int = 8) extends Module {
    val io = IO(new Bundle {
        val flush = Input(Bool())
        val in0   = Flipped(Decoupled(new PipelineData()))
        val in1   = Flipped(Decoupled(new PipelineData()))
        val out   = new FetchQueueOut() // ★ 替换为双出接口
    })
    
    val buffer = Reg(Vec(depth, new PipelineData()))
    val head   = RegInit(0.U(log2Ceil(depth).W))
    val tail   = RegInit(0.U(log2Ceil(depth).W))
    val count  = RegInit(0.U((log2Ceil(depth) + 1).W))

    // 只要有 2 个及以上的空位，就允许入队
    val in_ready = count <= (depth - 2).U
    io.in0.ready := in_ready
    io.in1.ready := in_ready

    val enq0 = io.in0.valid && io.in0.ready
    val enq1 = io.in1.valid && io.in1.ready
    

    def wrapAdd(ptr: UInt, add: UInt) = {
        val sum = ptr + add
        // 强行截断，满足 Vec 的索引位宽要求，消除 W004 警告
        Mux(sum >= depth.U, sum - depth.U, sum)(log2Ceil(depth)-1, 0)
    }

    io.out.valid0 := count > 0.U
    io.out.inst0  := buffer(head)
    io.out.valid1 := count > 1.U
    io.out.inst1  := buffer(wrapAdd(head, 1.U))

    when(io.flush) {
        head := 0.U; tail := 0.U; count := 0.U
    } .otherwise {
        // ★ 根据后级的消化能力动态弹栈
        head := wrapAdd(head, io.out.pop)
        
        val t0 = tail
        val t1 = wrapAdd(tail, 1.U)
        
        // ★ 优化：在循环外统一进行 One-Hot 译码
        val t0_oh = UIntToOH(t0, depth)
        val t1_oh = UIntToOH(t1, depth)
        
        // 将嵌套的 MUX 拍平为并行独立的 Write Enable
        for (i <- 0 until depth) {
            val we0 = enq0 && t0_oh(i)
            val we1 = enq1 && t1_oh(i)
            
            when(we0) {
                buffer(i) := io.in0.bits
            } .elsewhen(we1) {
                buffer(i) := io.in1.bits
            }
        }
        
        // ★ 第二刀：优化 count 计算，移除 AND 门
        val do_enq_cnt = Mux(enq1, 2.U, Mux(enq0, 1.U, 0.U))
        tail := wrapAdd(tail, do_enq_cnt)
        count := count + do_enq_cnt - io.out.pop
    }
}

// ====================================================================
// 改造后的超标量双发取指前端
// ====================================================================
class StageIF extends Module {
    val io = IO(new Bundle {
        // ★ 核心修改：改为双通道输出
        val out0            = Decoupled(new PipelineData())
        val out1            = Decoupled(new PipelineData())

        val flush           = Input(Bool())
        val flush_target_pc = Input(UInt(32.W))
        val inst_sram       = new SramIo()
        val inst_uncached   = Output(Bool())

        val mmu_config      = Input(new MmuConfig())
        val tlb_s0_vppn     = Output(UInt(19.W))
        val tlb_s0_va_bit12 = Output(Bool())
        val tlb_s0_asid     = Output(UInt(10.W))
        val tlb_s0_found    = Input(Bool())
        val tlb_s0_ppn      = Input(UInt(20.W))
        val tlb_s0_ps       = Input(UInt(6.W))
        val tlb_s0_plv      = Input(UInt(2.W)) 
        val tlb_s0_mat      = Input(UInt(2.W)) 
        val tlb_s0_v        = Input(Bool())    
        // ★ 新增：来自后端的 BTB 训练线
        val bpu_update      = Flipped(Valid(new BpuUpdate()))
    })

    val pc_reg = RegInit(Config.START_PC)
    val va = pc_reg
    
    // ==========================================
    // MMU 翻译逻辑 (保持不变)
    // ==========================================
    io.tlb_s0_vppn     := va(31, 13)
    io.tlb_s0_va_bit12 := va(12)
    io.tlb_s0_asid     := io.mmu_config.asid.asid
    
    val dmw0_hit = (io.mmu_config.crmd.pg === 1.U) && (io.mmu_config.crmd.da === 0.U) && (va(31, 29) === io.mmu_config.dmw0.vseg) &&
               ((io.mmu_config.crmd.plv === 0.U && io.mmu_config.dmw0.plv0 === 1.U) || (io.mmu_config.crmd.plv === 3.U && io.mmu_config.dmw0.plv3 === 1.U))
    val dmw1_hit = (io.mmu_config.crmd.pg === 1.U) && (io.mmu_config.crmd.da === 0.U) && (va(31, 29) === io.mmu_config.dmw1.vseg) &&
                ((io.mmu_config.crmd.plv === 0.U && io.mmu_config.dmw1.plv0 === 1.U) || (io.mmu_config.crmd.plv === 3.U && io.mmu_config.dmw1.plv3 === 1.U))
                
    val dmw_hit = dmw0_hit || dmw1_hit
    val dmw_pa  = Mux(dmw0_hit, Cat(io.mmu_config.dmw0.pseg, va(28, 0)), Cat(io.mmu_config.dmw1.pseg, va(28, 0)))
    val tlb_pa = Mux(io.tlb_s0_ps === 12.U, Cat(io.tlb_s0_ppn, va(11, 0)), Cat(io.tlb_s0_ppn(19, 9), va(20, 0)))
    
    val pa = Mux((io.mmu_config.crmd.da === 1.U) && (io.mmu_config.crmd.pg === 0.U), va, Mux(dmw_hit, dmw_pa, Mux(io.tlb_s0_found && io.tlb_s0_v, tlb_pa, va)))

    val dmw_mat = Mux(dmw0_hit, io.mmu_config.dmw0.mat, io.mmu_config.dmw1.mat)
    val current_mat = Mux((io.mmu_config.crmd.da === 1.U) && (io.mmu_config.crmd.pg === 0.U), io.mmu_config.crmd.datf, Mux(dmw_hit, dmw_mat, io.tlb_s0_mat))                       
    io.inst_uncached := (current_mat === 0.U)

    val is_mapped = (io.mmu_config.crmd.pg === 1.U) && (io.mmu_config.crmd.da === 0.U) && !dmw_hit
    val exc_tlb_refill_if = is_mapped && !io.tlb_s0_found
    val exc_pif = is_mapped && io.tlb_s0_found && !io.tlb_s0_v
    val exc_ppi_if = is_mapped && io.tlb_s0_found && io.tlb_s0_v && (io.mmu_config.crmd.plv === 3.U) && (io.tlb_s0_plv === 0.U)
    val mmu_exc_now = exc_tlb_refill_if || exc_pif || exc_ppi_if
    val ecode_now = Mux(exc_tlb_refill_if, "h3F".U(6.W), Mux(exc_pif, "h03".U(6.W), Mux(exc_ppi_if, "h07".U(6.W), 0.U(6.W))))

    // ==========================================
    // ★ 智能双发核心逻辑
    // ==========================================
    // 跨行检测：如果取指令的起始地址是 0xC，下一个字就在新的 Cache 行里了，必须降级为单发！
    // ★ 智能降级防线：跨行检测与 Uncached 拦截
    // 1. 如果取指令是 0xC，下一个字在新的 Cache 行里，降级！
    // 2. 如果是 Uncached 取指，AXI 每次只能拿回 32 位，高位是空的，绝对必须降级为单发！
    val is_uncached_fetch = io.inst_uncached
    val is_cross_line = (va(3, 2) === 3.U) || is_uncached_fetch
    val pc_step = Mux(is_cross_line, 4.U, 8.U)

    val wait_data_reg = RegInit(false.B)
    val discard_reg   = RegInit(false.B)
    val buf_valid     = RegInit(false.B)
    val inst_buffer   = Reg(UInt(64.W))

    // ==========================================
    // ★ 零气泡 BTB 预测引擎 (512 项)
    // ==========================================
    val btb_valid = RegInit(VecInit(Seq.fill(512)(false.B)))

    // ★ 修复：RAS 深度极小，直接用 RegInit，彻底杜绝幽灵弹栈引发的 X 态爆炸！
    val ras = RegInit(VecInit(Seq.fill(16)(0.U(32.W))))
    val tos = RegInit(0.U(4.W))

    class BtbPayload extends Bundle {
        val tag      = UInt(21.W)
        val target   = UInt(32.W)
        val bpu_type = UInt(2.W)
    }
    val btb_payload = Mem(512, new BtbPayload())

    // ==========================================
    // ★ GShare 方向预测引擎 (1024 项 BHT + 10 位 GHR)
    // ==========================================
    val ghr = RegInit(0.U(10.W))
    // ★ 修复：保留 Mem 以拯救时序，外挂一层 Valid 护盾屏蔽仿真 X 态！
    val bht = Mem(1024, UInt(2.W))
    val bht_valid = RegInit(VecInit(Seq.fill(1024)(false.B))) 

    val allow_req = !wait_data_reg && !buf_valid && !discard_reg
    val req_valid = allow_req
    val addr_handshaked = req_valid && io.inst_sram.addr_ok

    // ------------------------------------------
    // 训练逻辑：ALU 后端发来的 BHT 饱和更新
    // ------------------------------------------
    when(io.bpu_update.valid && io.bpu_update.bits.bpu_type === BpuType.COND) {
        val update_hash = io.bpu_update.bits.pc(11, 2) ^ io.bpu_update.bits.ghr
        
        // ★ 拦截 X 态：如果没被写过，强制认为是 1.U (弱不跳转)
        val raw_old_ctr = bht(update_hash)
        val old_ctr = Mux(bht_valid(update_hash), raw_old_ctr, 1.U(2.W))
        
        val new_ctr = Mux(io.bpu_update.bits.taken,
            Mux(old_ctr === 3.U, 3.U, old_ctr + 1.U), 
            Mux(old_ctr === 0.U, 0.U, old_ctr - 1.U)) 
            
        bht(update_hash) := new_ctr
        bht_valid(update_hash) := true.B 
    }

    // ------------------------------------------
    // 训练逻辑：BTB 非破坏性更新
    // ------------------------------------------
    when(io.bpu_update.valid) {
        val w_idx = io.bpu_update.bits.pc(10, 2)
        when(io.bpu_update.bits.taken) {
            btb_valid(w_idx) := true.B
            val write_data = Wire(new BtbPayload())
            write_data.tag      := io.bpu_update.bits.pc(31, 11)
            write_data.target   := io.bpu_update.bits.target
            write_data.bpu_type := io.bpu_update.bits.bpu_type
            btb_payload(w_idx) := write_data
        }
    }

    // ==========================================
    // 预测逻辑：双通道同拍查表 (0 气泡)
    // ==========================================
    val idx0 = pc_reg(10, 2)
    val idx1 = (pc_reg(10, 2) + 1.U)(8, 0)
    val tag_match = pc_reg(31, 11)

    val payload0 = btb_payload(idx0)
    val payload1 = btb_payload(idx1)
    val hit0 = btb_valid(idx0) && (payload0.tag === tag_match)
    val hit1 = btb_valid(idx1) && (payload1.tag === tag_match) && !is_cross_line

    val hash0 = pc_reg(11, 2) ^ ghr
    val hash1 = (pc_reg(11, 2) + 1.U)(9, 0) ^ ghr
    val raw_bht_out0 = bht(hash0)
    val raw_bht_out1 = bht(hash1)
    
    // ★ 拦截 X 态读取
    val bht_out0 = Mux(bht_valid(hash0), raw_bht_out0, 1.U(2.W))
    val bht_out1 = Mux(bht_valid(hash1), raw_bht_out1, 1.U(2.W))

    val is_cond0 = hit0 && (payload0.bpu_type === BpuType.COND)
    val is_cond1 = hit1 && (payload1.bpu_type === BpuType.COND)
    val is_call0 = hit0 && (payload0.bpu_type === BpuType.CALL)
    val is_ret0  = hit0 && (payload0.bpu_type === BpuType.RET)

    val pred_taken0  = Mux(is_cond0, bht_out0(1), hit0)
    
    val is_call1 = hit1 && (payload1.bpu_type === BpuType.CALL) && !pred_taken0
    val is_ret1  = hit1 && (payload1.bpu_type === BpuType.RET) && !pred_taken0

    val tos_after_0 = Mux(is_call0, tos + 1.U, Mux(is_ret0, tos - 1.U, tos))(3, 0)
    val tos_after_1 = Mux(is_call1, tos_after_0 + 1.U, Mux(is_ret1, tos_after_0 - 1.U, tos_after_0))(3, 0)

    val call_ret_pc0 = pc_reg + 4.U 
    
    // ★ RAS 超前并行计算 (斩断串行依赖)
    val ras_val_tos_minus_1 = ras((tos - 1.U)(3, 0)) 
    val ras_val_tos_minus_2 = ras((tos - 2.U)(3, 0)) 

    val ras_pop_addr0 = ras_val_tos_minus_1
    val ras_pop_addr1 = Mux(is_call0, call_ret_pc0, 
                        Mux(is_ret0,  ras_val_tos_minus_2, 
                                      ras_val_tos_minus_1))

    val pred_target0 = Mux(is_ret0, ras_pop_addr0, payload0.target)
    val pred_type0   = payload0.bpu_type

    val pred_taken1  = Mux(is_cond1, bht_out1(1), hit1) && !pred_taken0
    val pred_target1 = Mux(is_ret1, ras_pop_addr1, payload1.target)
    val pred_type1   = payload1.bpu_type

    val next_pc_base = pc_reg + pc_step
    val btb_target_pc = Mux(pred_taken0, pred_target0, Mux(pred_taken1, pred_target1, next_pc_base))

    // ------------------------------------------
    // ★ 核心：状态的双端推测与死亡回档 (完美抗污染版)
    // ------------------------------------------
    when(io.bpu_update.valid && io.bpu_update.bits.mispredict) {
        val u = io.bpu_update.bits
        
        // 【1. RAS 回档与真理修复】
        // 拿回这条指令执行前的清白栈顶！不管前端怎么瞎推测，现在一切以 ALU 查明的真相为准。
        val base_tos = u.ras_tos
        // 如果 ALU 说是 CALL，就在清白栈顶上+1；如果是 RET，就-1。
        val final_tos = Mux(u.bpu_type === BpuType.CALL, base_tos + 1.U, 
                        Mux(u.bpu_type === BpuType.RET,  base_tos - 1.U, base_tos))(3, 0)
        tos := final_tos
        
        // 如果真相是 CALL，立刻把正确的返回地址 (PC+4) 压入堆栈！
        when(u.bpu_type === BpuType.CALL) {
            ras(base_tos) := u.pc + 4.U
        }

        // 【2. GShare 回档】
        when(u.bpu_type === BpuType.COND) {
            ghr := Cat(u.ghr(8, 0), u.taken)
        } .otherwise {
            ghr := u.ghr
        }
        
    } .elsewhen(addr_handshaked) {
        // 【1. RAS 推测更新】
        tos := tos_after_1
        
        val call_ret_pc1 = pc_reg + 8.U

        when(is_call0 && is_call1) {
            ras(tos) := call_ret_pc0
            ras((tos + 1.U)(3, 0)) := call_ret_pc1
        } .elsewhen(is_call0) {
            ras(tos) := call_ret_pc0
        } .elsewhen(is_call1) {
            ras(tos_after_0) := call_ret_pc1
        }

        // 【2. GShare 推测更新】
        val shift_0 = is_cond0
        val shift_1 = is_cond1 && !pred_taken0 

        when(shift_0 && shift_1) {
            ghr := Cat(ghr(7, 0), pred_taken0, pred_taken1)
        } .elsewhen(shift_0) {
            ghr := Cat(ghr(8, 0), pred_taken0)
        } .elsewhen(shift_1) {
            ghr := Cat(ghr(8, 0), pred_taken1)
        }
    }

    // ★ 隐藏的致命 Bug 修复：同步打拍，防止异常位被下一拍提前篡改
    val pc_buf    = RegInit(0.U(32.W))
    val cross_buf = RegInit(false.B)
    val exc_buf   = RegInit(0.U.asTypeOf(new Bundle{ val exc=Bool(); val ecode=UInt(6.W) }))

    class PredBuf extends Bundle {
        val taken = Bool()
        val target= UInt(32.W)
        val btype = UInt(2.W)
        val ghr   = UInt(10.W) // ★ 加这行
        val ras_tos = UInt(4.W) // ★ 加这行
    }
    val pred_buf = RegInit(0.U.asTypeOf(Vec(2, new PredBuf())))

    val pc_alignment_error = WireDefault(false.B) // 这里占位，实际校验看后面
    val safe_pa = Mux(pc_alignment_error || mmu_exc_now, "h1c000000".U(32.W), pa)

    io.inst_sram.req    := req_valid
    io.inst_sram.wr     := false.B
    io.inst_sram.size   := 2.U
    io.inst_sram.wstrb  := 0.U
    io.inst_sram.addr   := safe_pa   
    io.inst_sram.wdata  := 0.U

    val real_data_ok = io.inst_sram.data_ok && !discard_reg
    val set_discard = (wait_data_reg || addr_handshaked) && io.flush && !io.inst_sram.data_ok

    when(io.flush) {
        pc_reg := io.flush_target_pc
    } .elsewhen(addr_handshaked) {
        pc_reg        := btb_target_pc
        pc_buf        := pc_reg
        cross_buf     := is_cross_line
        exc_buf.exc   := mmu_exc_now
        exc_buf.ecode := ecode_now

        // ★ 把 pred_buf 的更新挪到这里来！变量顺序就绝对安全了
        pred_buf(0).taken  := pred_taken0
        pred_buf(0).target := pred_target0
        pred_buf(0).btype  := pred_type0
        pred_buf(1).taken  := pred_taken1
        pred_buf(1).target := pred_target1
        pred_buf(1).btype  := pred_type1
        pred_buf(0).ghr := ghr // ★ 注意：这里直接取当前的 ghr，此时它还没被推测更新覆盖，最纯净！
        pred_buf(1).ghr := ghr
        // ★ 核心：抓取指令执行前的干净状态！0 号拿自己面前的，1 号拿 0 执行完之后的。
        pred_buf(0).ras_tos := tos 
        pred_buf(1).ras_tos := tos_after_0
    }

    when(io.flush) { wait_data_reg := false.B } 
    .elsewhen(addr_handshaked) { wait_data_reg := true.B } 
    .elsewhen(wait_data_reg && real_data_ok) { wait_data_reg := false.B }

    when(set_discard) { discard_reg := true.B } 
    .elsewhen(discard_reg && io.inst_sram.data_ok) { discard_reg := false.B }

    // 取指缓冲逻辑，只要前端不就绪，就把 64 位数据全部抱住
    val current_cross = Mux(addr_handshaked && !wait_data_reg && !buf_valid, is_cross_line, cross_buf)
    val pipe_ready = io.out0.ready && (current_cross || io.out1.ready)

    when(io.flush) {
        buf_valid := false.B
    } .elsewhen(real_data_ok && !pipe_ready) {
        inst_buffer := io.inst_sram.rdata
        buf_valid   := true.B
    } .elsewhen(pipe_ready) {
        buf_valid   := false.B
    }

    // ==========================================
    // 分发与数据拼装
    // ==========================================
    val final_valid = (real_data_ok || buf_valid) && !io.flush
    val final_rdata = Mux(buf_valid, inst_buffer, io.inst_sram.rdata)

    val current_pc0   = Mux(addr_handshaked && !wait_data_reg && !buf_valid, pc_reg, pc_buf)
    val current_exc   = Mux(addr_handshaked && !wait_data_reg && !buf_valid, mmu_exc_now, exc_buf.exc)
    val current_ecode = Mux(addr_handshaked && !wait_data_reg && !buf_valid, ecode_now, exc_buf.ecode)

    val align_err0 = (current_pc0(1, 0) =/= 0.U)
    val align_err1 = ((current_pc0 + 4.U)(1, 0) =/= 0.U)

    val has_exc0 = align_err0 || current_exc
    val has_exc1 = align_err1 || current_exc

    val safe_inst0 = final_rdata(31, 0)
    val safe_inst1 = final_rdata(63, 32)

    val current_pred0_taken  = Mux(addr_handshaked && !wait_data_reg && !buf_valid, pred_taken0, pred_buf(0).taken)
    val current_pred0_target = Mux(addr_handshaked && !wait_data_reg && !buf_valid, pred_target0, pred_buf(0).target)
    val current_pred0_type   = Mux(addr_handshaked && !wait_data_reg && !buf_valid, pred_type0, pred_buf(0).btype)

    val current_pred1_taken  = Mux(addr_handshaked && !wait_data_reg && !buf_valid, pred_taken1, pred_buf(1).taken)
    val current_pred1_target = Mux(addr_handshaked && !wait_data_reg && !buf_valid, pred_target1, pred_buf(1).target)
    val current_pred1_type   = Mux(addr_handshaked && !wait_data_reg && !buf_valid, pred_type1, pred_buf(1).btype)

    io.out0.valid := final_valid
    // ★ 错路屏蔽：如果槽位 0 被预测跳转，槽位 1 的机器码立刻化为灰烬！
    io.out1.valid := final_valid && !current_cross && !current_pred0_taken

    val current_pred0_ghr = Mux(addr_handshaked && !wait_data_reg && !buf_valid, ghr, pred_buf(0).ghr)
    val current_pred1_ghr = Mux(addr_handshaked && !wait_data_reg && !buf_valid, ghr, pred_buf(1).ghr)
    val current_pred0_tos = Mux(addr_handshaked && !wait_data_reg && !buf_valid, tos, pred_buf(0).ras_tos)
    val current_pred1_tos = Mux(addr_handshaked && !wait_data_reg && !buf_valid, tos_after_0, pred_buf(1).ras_tos)

    val out0_data = WireDefault(0.U.asTypeOf(new PipelineData()))
    out0_data.pc           := current_pc0
    out0_data.inst         := safe_inst0
    out0_data.hasException := has_exc0
    out0_data.ecode        := Mux(align_err0, "h08".U, current_ecode)
    out0_data.pred_taken  := current_pred0_taken
    out0_data.pred_target := current_pred0_target
    out0_data.bpu_type    := current_pred0_type
    out0_data.ghr := current_pred0_ghr
    out0_data.ras_tos := current_pred0_tos

    val out1_data = WireDefault(0.U.asTypeOf(new PipelineData()))
    out1_data.pc           := (current_pc0 + 4.U)(31, 0)
    out1_data.inst         := safe_inst1
    out1_data.hasException := has_exc1
    out1_data.ecode        := Mux(align_err1, "h08".U, current_ecode)
    out1_data.pred_taken  := current_pred1_taken
    out1_data.pred_target := current_pred1_target
    out1_data.bpu_type    := current_pred1_type
    out1_data.ghr := current_pred1_ghr
    out1_data.ras_tos := current_pred1_tos

    io.out0.bits := out0_data
    io.out1.bits := out1_data
}