package mycpu

import chisel3._
import chisel3.util._

// ====================================================================
// ★ 新增：IF1 传给 IF2 的元数据包裹 (记录发请求时的各种状态)
// ====================================================================
class FetchMeta extends Bundle {
    val pc           = UInt(32.W)
    val is_cross     = Bool()
    val has_exc      = Bool()
    val ecode        = UInt(6.W)
    val pred_taken0  = Bool()
    val pred_target0 = UInt(32.W)
    val pred_type0   = UInt(2.W)
    val pred_taken1  = Bool()
    val pred_target1 = UInt(32.W)
    val pred_type1   = UInt(2.W)
    val ghr          = UInt(10.W)
    val ras_tos      = UInt(4.W)
    val ras_tos1     = UInt(4.W)
    val ticket       = UInt(8.W) // ★ 新增：记住自己的取餐码
}


// ====================================================================
// 改造后的超标量双发取指前端
// ====================================================================
class StageIF extends Module {
    val io = IO(new Bundle {
        val out0            = Decoupled(new PipelineData())
        val out1            = Decoupled(new PipelineData())

        val flush           = Input(Bool())
        val flush_target_pc = Input(UInt(32.W))

        val cache_io        = new SramIo()
        val inst_uncached   = Output(Bool())
        // ★ 新增：与顶层对接的 8 位车票通道
        val inst_req_id     = Output(UInt(8.W))
        val inst_ret_id     = Input(UInt(8.W))

        val mmu_config      = Input(new MmuConfig())
        val tlb_port        = new TlbSearchPort()
        // ★ 新增：来自后端的 BTB 训练线
        val bpu_update      = Flipped(Valid(new BpuUpdate()))
    })

    val pc_reg = RegInit(Config.START_PC)
    val va = pc_reg
    
    // ==========================================
    // MMU 翻译逻辑 (保持不变)
    // ==========================================
    io.tlb_port.vppn     := va(31, 13)
    io.tlb_port.va_bit12 := va(12)
    io.tlb_port.asid     := io.mmu_config.asid.asid
    
    val dmw0_hit = (io.mmu_config.crmd.pg === 1.U) && (io.mmu_config.crmd.da === 0.U) && (va(31, 29) === io.mmu_config.dmw0.vseg) &&
               ((io.mmu_config.crmd.plv === 0.U && io.mmu_config.dmw0.plv0 === 1.U) || (io.mmu_config.crmd.plv === 3.U && io.mmu_config.dmw0.plv3 === 1.U))
    val dmw1_hit = (io.mmu_config.crmd.pg === 1.U) && (io.mmu_config.crmd.da === 0.U) && (va(31, 29) === io.mmu_config.dmw1.vseg) &&
                ((io.mmu_config.crmd.plv === 0.U && io.mmu_config.dmw1.plv0 === 1.U) || (io.mmu_config.crmd.plv === 3.U && io.mmu_config.dmw1.plv3 === 1.U))
                
    val dmw_hit = dmw0_hit || dmw1_hit
    val dmw_pa  = Mux(dmw0_hit, Cat(io.mmu_config.dmw0.pseg, va(28, 0)), Cat(io.mmu_config.dmw1.pseg, va(28, 0)))
    val tlb_pa = Mux(io.tlb_port.ps === 12.U, Cat(io.tlb_port.ppn, va(11, 0)), Cat(io.tlb_port.ppn(19, 9), va(20, 0)))
    
    val pa = Mux((io.mmu_config.crmd.da === 1.U) && (io.mmu_config.crmd.pg === 0.U), va, Mux(dmw_hit, dmw_pa, Mux(io.tlb_port.found && io.tlb_port.v, tlb_pa, va)))

    val dmw_mat = Mux(dmw0_hit, io.mmu_config.dmw0.mat, io.mmu_config.dmw1.mat)
    val current_mat = Mux((io.mmu_config.crmd.da === 1.U) && (io.mmu_config.crmd.pg === 0.U), io.mmu_config.crmd.datf, Mux(dmw_hit, dmw_mat, io.tlb_port.mat))                       
    io.inst_uncached := (current_mat === 0.U)

    val is_mapped = (io.mmu_config.crmd.pg === 1.U) && (io.mmu_config.crmd.da === 0.U) && !dmw_hit
    val exc_tlb_refill_if = is_mapped && !io.tlb_port.found
    val exc_pif = is_mapped && io.tlb_port.found && !io.tlb_port.v
    val exc_ppi_if = is_mapped && io.tlb_port.found && io.tlb_port.v && (io.mmu_config.crmd.plv === 3.U) && (io.tlb_port.plv === 0.U)
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
    //// ★ 性能解封：适配 32 字节 Cache 行，仅在真实行尾 (0x1C) 时才触发跨行降级！
    val is_cross_line = (va(4, 2) === 7.U) || is_uncached_fetch
    val pc_step = Mux(is_cross_line, 4.U, 8.U)

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

    // ★ 防毒面具：未命中时强制清零，绝不让 Mem 的 X 态流出！
    val pred_target0 = Mux(hit0, Mux(is_ret0, ras_pop_addr0, payload0.target), 0.U(32.W))
    val pred_type0   = Mux(hit0, payload0.bpu_type, BpuType.UNCOND) // 未命中当做非条件分支，绝不触发 GHR 移位

    val pred_taken1  = Mux(is_cond1, bht_out1(1), hit1) && !pred_taken0
    val pred_target1 = Mux(hit1, Mux(is_ret1, ras_pop_addr1, payload1.target), 0.U(32.W))
    val pred_type1   = Mux(hit1, payload1.bpu_type, BpuType.UNCOND)

    val next_pc_base = pc_reg + pc_step
    val btb_target_pc = Mux(pred_taken0, pred_target0, Mux(pred_taken1, pred_target1, next_pc_base))

    val q_reset = reset.asBool || io.flush
    val meta_queue = withReset(q_reset) { Module(new Queue(new FetchMeta(), 32)) }
    // ★ 新增：256 项的乱序数据接收台与就绪状态表
    val rdata_table = Reg(Vec(256, UInt(64.W)))
    val data_ready_table = RegInit(VecInit(Seq.fill(256)(false.B)))

    // ====================================================================
    // ★ 终极重构：物理雷达与逻辑生死簿双重校验
    // ====================================================================
    val ticket_cnt   = RegInit(0.U(8.W))
    val valid_table  = RegInit(VecInit(Seq.fill(256)(false.B))) // 逻辑生死簿（会被 Flush 烧毁）
    val flying_table = RegInit(VecInit(Seq.fill(256)(false.B))) // 物理雷达（记录总线实际在飞的请求）

    // 发起请求的终极条件：队列有空，且当前轮到的 ticket 号在总线上没有幽灵残留！
    val can_req = !io.flush && meta_queue.io.enq.ready && !flying_table(ticket_cnt)
    
    val if1_fire = can_req && io.cache_io.addr_ok

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
        
    } .elsewhen(if1_fire) {
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
    // ====================================================================
    // ★ 核心重构：8 位 Ticket ID 生死簿
    // ====================================================================
    val req_fire  = if1_fire
    val resp_fire = io.cache_io.data_ok

    // 1. 发请求：颁发 Ticket
    io.inst_req_id := ticket_cnt
    when(req_fire) {
        ticket_cnt := ticket_cnt + 1.U
        valid_table(ticket_cnt)  := true.B  // 逻辑上：我需要这个数据
        flying_table(ticket_cnt) := true.B  // 物理上：它起飞了
        data_ready_table(ticket_cnt) := false.B // 清扫柜子
    }

    // 2. 收响应：核销车票 (不论是真数据还是幽灵，只要回来，物理雷达就解锁)
    when(resp_fire) {
        valid_table(io.inst_ret_id)  := false.B
        flying_table(io.inst_ret_id) := false.B
    }

    // 3. 逻辑大屠杀：Flush 当拍，只烧毁逻辑生死簿，绝不碰物理雷达！
    when(io.flush) {
        for (i <- 0 until 256) {
            valid_table(i) := false.B
        }
    }

    // 4. 纯净过滤器：只有数据回来了，且逻辑生死簿上确认需要它，才存入外卖柜
    val real_data_ok = io.cache_io.data_ok && valid_table(io.inst_ret_id)
    when(real_data_ok) {
        rdata_table(io.inst_ret_id)      := io.cache_io.rdata
        data_ready_table(io.inst_ret_id) := true.B
    }
    
    // ====================================================================
    // ★ 顺序修复：IF2 基于 Ticket 从乱序储物柜里取货拼装
    // ====================================================================
    val meta = meta_queue.io.deq.bits
    val head_ticket = meta.ticket

    // 用队头的 ticket 去读表，完美拿到对应 PC 的数据
    val final_rdata = rdata_table(head_ticket)
    val head_data_ready = data_ready_table(head_ticket)

    val pipe_ready = io.out0.ready && (meta.is_cross || meta.pred_taken0 || io.out1.ready)
    
    // ★ 条件变更：只要 meta 在，且对应的数据已经送进柜子，就能发射！
    val if2_fire   = meta_queue.io.deq.valid && head_data_ready && pipe_ready

    meta_queue.io.deq.ready  := if2_fire

    // 取走数据后，随手关门（清理就绪状态，防止下次 ticket 轮转时串键）
    when(if2_fire) {
        data_ready_table(head_ticket) := false.B
    }

    // ==========================================
    // IF1 阶段：连接 Cache 请求与 PC 更新
    // ==========================================
    // ★ 安全护盾：防止取指未对齐导致 AXI 总线挂死
    val pc_alignment_error = (pc_reg(1, 0) =/= 0.U)
    val safe_pa = Mux(pc_alignment_error || mmu_exc_now, "h1c000000".U(32.W), pa)

    io.cache_io.req   := can_req
    io.cache_io.wr    := false.B
    // ★ 终极修复：双发架构必须一次性拉回 64 位 (8 字节)！
    io.cache_io.size  := 3.U  // 从 2.U 改为 3.U
    io.cache_io.wstrb := 0.U
    io.cache_io.addr  := safe_pa   
    io.cache_io.wdata := 0.U

    // 压入元数据小队列
    meta_queue.io.enq.valid := if1_fire
    meta_queue.io.enq.bits.pc           := pc_reg
    meta_queue.io.enq.bits.is_cross     := is_cross_line
    meta_queue.io.enq.bits.has_exc      := mmu_exc_now
    meta_queue.io.enq.bits.ecode        := ecode_now
    meta_queue.io.enq.bits.pred_taken0  := pred_taken0
    meta_queue.io.enq.bits.pred_target0 := pred_target0
    meta_queue.io.enq.bits.pred_type0   := pred_type0
    meta_queue.io.enq.bits.pred_taken1  := pred_taken1
    meta_queue.io.enq.bits.pred_target1 := pred_target1
    meta_queue.io.enq.bits.pred_type1   := pred_type1
    meta_queue.io.enq.bits.ghr          := ghr
    meta_queue.io.enq.bits.ras_tos      := tos
    meta_queue.io.enq.bits.ras_tos1     := tos_after_0
    meta_queue.io.enq.bits.ticket := ticket_cnt // ★ 新增

    // PC 狂飙：只在 if1_fire 时更新！
    when(io.flush) {
        pc_reg := io.flush_target_pc
    } .elsewhen(if1_fire) {
        pc_reg := btb_target_pc
    }

    // ==========================================
    // IF2 阶段：数据拼装与分发
    // ==========================================
    val align_err0 = (meta.pc(1, 0) =/= 0.U)
    val align_err1 = ((meta.pc + 4.U)(1, 0) =/= 0.U)

    val out0_data = WireDefault(0.U.asTypeOf(new PipelineData()))
    out0_data.pc           := meta.pc
    out0_data.inst         := final_rdata(31, 0) 
    out0_data.hasException := meta.has_exc || align_err0
    out0_data.ecode        := Mux(align_err0, "h08".U, meta.ecode)
    out0_data.pred_taken   := meta.pred_taken0
    out0_data.pred_target  := meta.pred_target0
    out0_data.bpu_type     := meta.pred_type0
    out0_data.ghr          := meta.ghr
    out0_data.ras_tos      := meta.ras_tos

    // ★ 修复：如果 inst0 是条件分支，计算它移位后的新 GHR
    val ghr_after_0 = Mux(meta.pred_type0 === BpuType.COND, 
                          Cat(meta.ghr(8, 0), meta.pred_taken0), 
                          meta.ghr)
    val out1_data = WireDefault(0.U.asTypeOf(new PipelineData()))
    out1_data.pc           := (meta.pc + 4.U)(31, 0)
    out1_data.inst         := final_rdata(63, 32) 
    out1_data.hasException := meta.has_exc || align_err1
    out1_data.ecode        := Mux(align_err1, "h08".U, meta.ecode)
    out1_data.pred_taken   := meta.pred_taken1
    out1_data.pred_target  := meta.pred_target1
    out1_data.bpu_type     := meta.pred_type1
    // ★ 让 inst1 使用接力后的 GHR，杜绝历史污染！
    out1_data.ghr          := ghr_after_0
    out1_data.ras_tos      := meta.ras_tos1

    io.out0.valid := if2_fire
    io.out1.valid := if2_fire && !meta.is_cross && !meta.pred_taken0
    
    io.out0.bits := out0_data
    io.out1.bits := out1_data
}