package mycpu

import chisel3._
import chisel3.util._

class RobEntry extends Bundle {
    val valid    = Bool()
    val pc       = UInt(32.W)
    val rf_we    = Bool()
    val rf_waddr = UInt(5.W)
    val rf_paddr = UInt(Config.prfPtrWidth.W)
    val old_paddr= UInt(Config.prfPtrWidth.W)
    val rf_wdata = UInt(32.W)
    val done     = Bool()
    val branch_mask = UInt(4.W)

    // 异常与特殊档案
    val has_exc    = Bool()
    val ecode      = UInt(6.W)
    val ertn       = Bool()
    val is_refetch = Bool()
    val exc_addr   = UInt(32.W) 
    val is_data_mmu= Bool()
    val is_cacop   = Bool()

    val csr_we    = Bool()
    val csr_num   = UInt(14.W)
    val csr_wmask = UInt(32.W)
    val csr_wdata = UInt(32.W)
    val tlb_we    = Bool()
    val tlb_fill  = Bool()
    val tlbrd_we  = Bool()
}

class ROB extends Module {
    val numEntries = Config.robEntries
    val ptrWidth = Config.robPtrWidth
    
    val io = IO(new Bundle {
        val flush = Input(Bool())
        val head_idx = Output(UInt(ptrWidth.W))
        
        // 1. Alloc 端口 0
        val alloc_valid = Input(Bool())
        val alloc_pc    = Input(UInt(32.W))
        val alloc_we    = Input(Bool())
        val alloc_waddr = Input(UInt(5.W))
        val alloc_paddr = Input(UInt(Config.prfPtrWidth.W))
        val alloc_old_p = Input(UInt(Config.prfPtrWidth.W))
        val alloc_br_mask = Input(UInt(4.W))
        val alloc_idx = Output(UInt(ptrWidth.W))
        val alloc_ready = Output(Bool())

        // 1. Alloc 端口 1 (★ 新增)
        val alloc1_valid = Input(Bool())
        val alloc1_pc    = Input(UInt(32.W))
        val alloc1_we    = Input(Bool())
        val alloc1_waddr = Input(UInt(5.W))
        val alloc1_paddr = Input(UInt(Config.prfPtrWidth.W))
        val alloc1_old_p = Input(UInt(Config.prfPtrWidth.W))
        val alloc1_br_mask = Input(UInt(4.W))
        val alloc1_idx = Output(UInt(ptrWidth.W))
        val alloc1_ready = Output(Bool())

        val br_resolve  = Input(new BranchResolve())

        // 2. 乱序双写回端口 (监听 CDB0 和 CDB1)
        val cdb0 = Flipped(Valid(new PipelineData()))
        val cdb1 = Flipped(Valid(new PipelineData()))

        val lsq_violation_valid = Input(Bool())
        val lsq_violation_rob = Input(UInt(ptrWidth.W))
        val lsq_violation_pc    = Input(UInt(32.W))

        // ★ 修改：LSQ 提交通知改为双通道
        val commit_mem_valid0 = Output(Bool())
        val commit_mem_idx0 = Output(UInt(ptrWidth.W))
        val commit_mem_valid1 = Output(Bool())
        val commit_mem_idx1 = Output(UInt(ptrWidth.W))

        // 3. Commit 探针与 Rename 交互 
        val commit_valid = Output(Bool())
        val commit_pc    = Output(UInt(32.W))
        val commit_we    = Output(Bool())
        val commit_waddr = Output(UInt(5.W))
        val commit_wdata = Output(UInt(32.W))
        val commit_paddr = Output(UInt(Config.prfPtrWidth.W))
        val commit_old_p = Output(UInt(Config.prfPtrWidth.W))

        // ★ 新增：双提交 1 号通道
        val commit1_valid= Output(Bool())
        val commit1_pc   = Output(UInt(32.W))
        val commit1_we   = Output(Bool())
        val commit1_waddr= Output(UInt(5.W))
        val commit1_wdata= Output(UInt(32.W))
        val commit1_paddr= Output(UInt(Config.prfPtrWidth.W))
        val commit1_old_p= Output(UInt(Config.prfPtrWidth.W))
        val commit1_csr_we = Output(Bool())

        // 4. 系统控制信号输出
        val wb_flush      = Output(Bool())
        val wb_target_pc  = Output(UInt(32.W))
        
        val commit_csr_we    = Output(Bool())
        val commit_csr_num   = Output(UInt(14.W))
        val commit_csr_wmask = Output(UInt(32.W))
        val commit_csr_wdata = Output(UInt(32.W))
        val commit_has_exc   = Output(Bool())
        val commit_ecode     = Output(UInt(6.W))
        val commit_pc_out    = Output(UInt(32.W))
        val commit_exc_addr  = Output(UInt(32.W))
        val commit_ertn      = Output(Bool())

        // 5. TLB 与 CSR 交互
        val tlb_we       = Output(Bool())
        val tlb_w_idx    = Output(UInt(4.W))
        val tlb_w_dat    = Output(new TlbEntry())
        val commit_tlbrd_we = Output(Bool())
        
        val csr_tlbrentryOut = Input(UInt(32.W))
        val csr_eentryOut    = Input(UInt(32.W))
        val csr_eraOut       = Input(UInt(32.W))
        val csr_tlbidx_out   = Input(UInt(32.W))
        val csr_tlb_out      = Input(new TlbEntry())
        val has_int          = Input(Bool())
    })

    // 新代码：
    val entries = RegInit(VecInit(Seq.fill(numEntries)(0.U.asTypeOf(new RobEntry()))))
    val head = RegInit(0.U(ptrWidth.W))
    val tail = RegInit(0.U(ptrWidth.W))
    val is_full = RegInit(false.B)
    val is_empty = (!is_full && (head === tail))

    // ==========================================
    // ★ 改造入队 (Dual Allocate)
    // ==========================================
    // 强制截断为低 4 位，完美闭环！
    val tail_next  = (tail + 1.U)(ptrWidth - 1, 0)
    val tail_next2 = (tail + 2.U)(ptrWidth - 1, 0)  
    val is_full_for_1 = is_full
    val is_full_for_2 = is_full || (tail_next === head)

    io.alloc_ready := !is_full_for_1
    io.alloc1_ready := !is_full_for_2
    io.alloc_idx := tail
    io.alloc1_idx := tail_next

    // 过滤掉因为预测失败而被杀掉的死信
    val real_alloc0 = io.alloc_valid && !(io.br_resolve.valid && io.br_resolve.mispredict)
    val real_alloc1 = io.alloc1_valid && !(io.br_resolve.valid && io.br_resolve.mispredict)

    val fire0 = real_alloc0 && !is_full_for_1
    val fire1 = real_alloc1 && !is_full_for_2 && fire0 // 0 不进，1 绝对不能进！

    when(fire0 && fire1) {
        val t0 = tail
        val t1 = tail_next
        
        // 灌入指令 0
        entries(t0).valid     := true.B
        entries(t0).pc        := io.alloc_pc
        entries(t0).rf_we     := io.alloc_we
        entries(t0).rf_waddr  := io.alloc_waddr
        entries(t0).rf_paddr  := io.alloc_paddr
        entries(t0).old_paddr := io.alloc_old_p
        entries(t0).branch_mask := io.alloc_br_mask
        entries(t0).has_exc    := false.B
        entries(t0).ertn       := false.B
        entries(t0).is_refetch := false.B
        entries(t0).is_cacop := false.B
        entries(t0).csr_we   := false.B
        entries(t0).tlb_we   := false.B
        entries(t0).tlb_fill := false.B
        entries(t0).tlbrd_we := false.B
        entries(t0).done     := false.B

        // 灌入指令 1
        entries(t1).valid     := true.B
        entries(t1).pc        := io.alloc1_pc
        entries(t1).rf_we     := io.alloc1_we
        entries(t1).rf_waddr  := io.alloc1_waddr
        entries(t1).rf_paddr  := io.alloc1_paddr
        entries(t1).old_paddr := io.alloc1_old_p
        entries(t1).branch_mask := io.alloc1_br_mask
        entries(t1).has_exc    := false.B
        entries(t1).ertn       := false.B
        entries(t1).is_refetch := false.B
        entries(t1).is_cacop := false.B
        entries(t1).csr_we   := false.B
        entries(t1).tlb_we   := false.B
        entries(t1).tlb_fill := false.B
        entries(t1).tlbrd_we := false.B
        entries(t1).done     := false.B

        tail := tail_next2
        when(tail_next2 === head) { is_full := true.B }

    } .elsewhen(fire0) {
        val t0 = tail
        entries(t0).valid     := true.B
        entries(t0).pc        := io.alloc_pc
        entries(t0).rf_we     := io.alloc_we
        entries(t0).rf_waddr  := io.alloc_waddr
        entries(t0).rf_paddr  := io.alloc_paddr
        entries(t0).old_paddr := io.alloc_old_p
        entries(t0).branch_mask := io.alloc_br_mask
        entries(t0).has_exc    := false.B
        entries(t0).ertn       := false.B
        entries(t0).is_refetch := false.B
        entries(t0).csr_we   := false.B
        entries(t0).tlb_we   := false.B
        entries(t0).tlb_fill := false.B
        entries(t0).tlbrd_we := false.B
        entries(t0).done     := false.B

        tail := tail_next
        when(tail_next === head) { is_full := true.B }
    }

    // ==========================================
    // 监听分支结算与双路写回 (Writeback)
    // ==========================================
    when(io.br_resolve.valid) {
        val tag_bit = 1.U(4.W) << io.br_resolve.tag
        for (i <- 0 until numEntries) {
            when(entries(i).valid) {
                val is_dependent = (entries(i).branch_mask & tag_bit) =/= 0.U
                when(io.br_resolve.mispredict) {
                    when(is_dependent) { entries(i).valid := false.B } // 自杀
                } .otherwise {
                    when(is_dependent) { entries(i).branch_mask := entries(i).branch_mask & ~tag_bit }
                }
            }
        }
    }

    

    for (i <- 0 until numEntries) {
        val match0 = io.cdb0.valid && entries(i).valid && (i.U === io.cdb0.bits.rob_idx)
        val match1 = io.cdb1.valid && entries(i).valid && (i.U === io.cdb1.bits.rob_idx)
        
        when(match0) {
            entries(i).done         := true.B
            entries(i).rf_wdata     := io.cdb0.bits.ex_result
            entries(i).rf_we        := io.cdb0.bits.regWriteEn && !io.cdb0.bits.hasException
            entries(i).has_exc      := io.cdb0.bits.hasException
            entries(i).ecode        := io.cdb0.bits.ecode
            entries(i).ertn         := io.cdb0.bits.inst_ertn
            entries(i).is_refetch   := io.cdb0.bits.is_refetch
            entries(i).exc_addr     := io.cdb0.bits.ex_result 
            entries(i).is_data_mmu  := io.cdb0.bits.resFromMem || io.cdb0.bits.memWe || io.cdb0.bits.is_cacop
            entries(i).csr_we       := io.cdb0.bits.csrWe
            entries(i).csr_num      := io.cdb0.bits.csrNum
            entries(i).csr_wmask    := io.cdb0.bits.aux_data
            entries(i).csr_wdata    := Mux(io.cdb0.bits.isCsr, io.cdb0.bits.src2_value, io.cdb0.bits.ex_result)
            entries(i).tlb_we       := io.cdb0.bits.tlbOp === TlbOp.WR
            entries(i).tlb_fill     := io.cdb0.bits.tlbOp === TlbOp.FILL
            entries(i).tlbrd_we     := io.cdb0.bits.tlbOp === TlbOp.RD
            entries(i).is_cacop     := io.cdb0.bits.is_cacop // 对于 match0
        }
        when(match1) {
            entries(i).done         := true.B
            entries(i).rf_wdata     := io.cdb1.bits.ex_result
            entries(i).rf_we        := io.cdb1.bits.regWriteEn && !io.cdb1.bits.hasException
            entries(i).has_exc      := io.cdb1.bits.hasException
            entries(i).ecode        := io.cdb1.bits.ecode
            entries(i).ertn         := io.cdb1.bits.inst_ertn
            entries(i).is_refetch   := io.cdb1.bits.is_refetch
            entries(i).exc_addr     := io.cdb1.bits.ex_result
            entries(i).is_data_mmu  := io.cdb1.bits.resFromMem || io.cdb1.bits.memWe || io.cdb1.bits.is_cacop
            entries(i).csr_we       := io.cdb1.bits.csrWe
            entries(i).csr_num      := io.cdb1.bits.csrNum
            entries(i).csr_wmask    := io.cdb1.bits.aux_data
            entries(i).csr_wdata    := Mux(io.cdb1.bits.isCsr, io.cdb1.bits.src2_value, io.cdb1.bits.ex_result)
            entries(i).tlb_we       := io.cdb1.bits.tlbOp === TlbOp.WR
            entries(i).tlb_fill     := io.cdb1.bits.tlbOp === TlbOp.FILL
            entries(i).tlbrd_we     := io.cdb1.bits.tlbOp === TlbOp.RD
            entries(i).is_cacop     := io.cdb1.bits.is_cacop // 对于 match1
        }
    }
    when(io.lsq_violation_valid) {
        val v_idx = io.lsq_violation_rob
        entries(v_idx).has_exc  := true.B
        entries(v_idx).ecode    := "h3E".U
        entries(v_idx).exc_addr := io.lsq_violation_pc 
    }
    // ==========================================
    // ★ 队头按序双提交 (Dual Commit) 与极限闪避
    // ==========================================
    val h0 = head
    val h1 = (head + 1.U)(ptrWidth - 1, 0)

    val h0_is_wb0 = io.cdb0.valid && (io.cdb0.bits.rob_idx === h0)
    val h0_is_wb1 = io.cdb1.valid && (io.cdb1.bits.rob_idx === h0)
    val h0_is_wbing = h0_is_wb0 || h0_is_wb1

    val h1_is_wb0 = io.cdb0.valid && (io.cdb0.bits.rob_idx === h1)
    val h1_is_wb1 = io.cdb1.valid && (io.cdb1.bits.rob_idx === h1)
    val h1_is_wbing = h1_is_wb0 || h1_is_wb1

    val e0 = entries(h0)
    val e1 = entries(h1)

    // ★ 核心时序拯救：斩断 ALU -> ROB 组合逻辑链！
    // 队头 e0 和 e1 是全系统最老指令，当拍的分支绝不可能反杀它们。
    val e0_real_valid = e0.valid
    val e1_real_valid = e1.valid

    // --- 0 号通道解析 (斩断 CDB 旁路，强制打拍提交) ---
    val h0_raw_exc   = e0.has_exc
    val h0_raw_ecode = e0.ecode
    val h0_raw_ertn  = e0.ertn
    val h0_raw_cacop = e0.is_cacop
    val h0_raw_we    = e0.rf_we
    
    val h0_raw_csr_we   = e0.csr_we
    val h0_raw_tlb_we   = e0.tlb_we
    val h0_raw_tlb_fill = e0.tlb_fill
    val h0_raw_tlbrd_we = e0.tlbrd_we
    val h0_raw_refetch  = e0.is_refetch
    
    val is_ghost0   = !e0_real_valid
    // ★ 核心改动：删除了 || h0_is_wbing，指令必须在 ROB 里沉淀一拍才能提交！
    val can_commit0 = e0_real_valid && e0.done && !io.flush

    val h0_is_replay = h0_raw_exc && (h0_raw_ecode === "h3E".U)
    val take_int     = io.has_int && can_commit0 && !is_ghost0 && !h0_raw_exc && !h0_raw_ertn
    val h0_real_exc  = (can_commit0 && h0_raw_exc && !h0_is_replay) || take_int

    val h0_serialize = e0_real_valid && (h0_raw_exc || h0_raw_csr_we || h0_raw_tlb_we || h0_raw_tlb_fill || h0_raw_tlbrd_we || h0_raw_ertn || h0_raw_refetch || h0_raw_cacop || take_int)

    // --- 1 号通道解析 (同理，全部简化) ---
    val h1_raw_exc   = e1.has_exc
    val h1_raw_cacop = e1.is_cacop
    val h1_raw_we    = e1.rf_we

    val h1_raw_csr_we   = e1.csr_we
    val h1_raw_tlb_we   = e1.tlb_we
    val h1_raw_tlb_fill = e1.tlb_fill
    val h1_raw_tlbrd_we = e1.tlbrd_we
    val h1_raw_refetch  = e1.is_refetch
    val h1_raw_ertn     = e1.ertn

    val is_ghost1   = !e1_real_valid
    val h1_serialize = e1_real_valid && (h1_raw_exc || h1_raw_csr_we || h1_raw_tlb_we || h1_raw_tlb_fill || h1_raw_tlbrd_we || h1_raw_ertn || h1_raw_refetch || h1_raw_cacop)
    
    val can_commit1 = e1_real_valid && e1.done && !h0_serialize && !h1_serialize && can_commit0 && !io.flush

    // --- 端口 0 输出 ---
    io.commit_valid := !is_empty && can_commit0
    io.commit_pc    := e0.pc
    io.commit_waddr := e0.rf_waddr
    io.commit_wdata := e0.rf_wdata  // 彻底删除 CDB 旁路 Mux
    io.commit_paddr := e0.rf_paddr
    io.commit_old_p := e0.old_paddr
    io.commit_we    := Mux(h0_real_exc || h0_is_replay, false.B, can_commit0 && h0_raw_we)
    io.commit_csr_we:= Mux(take_int || h0_is_replay, false.B, can_commit0 && h0_raw_csr_we)
    
    // --- 端口 1 输出 ---
    io.commit1_valid:= !is_empty && can_commit1
    io.commit1_pc   := e1.pc
    io.commit1_waddr:= e1.rf_waddr
    io.commit1_wdata:= e1.rf_wdata  // 彻底删除 CDB 旁路 Mux
    io.commit1_paddr:= e1.rf_paddr
    io.commit1_old_p:= e1.old_paddr
    io.commit1_we   := can_commit1 && h1_raw_we
    io.commit1_csr_we:= false.B 

    // --- 发给 LSQ 的双通道确认 ---
    io.commit_mem_valid0 := can_commit0 && !is_ghost0 && !h0_real_exc && !h0_is_replay
    io.commit_mem_idx0   := h0
    io.commit_mem_valid1 := can_commit1 && !is_ghost1
    io.commit_mem_idx1   := h1

    // --- 全局系统控制 ---
    io.commit_has_exc   := h0_real_exc
    io.commit_ecode     := Mux(take_int, 0.U(6.W), Mux(h0_real_exc, h0_raw_ecode, 0.U))
    io.commit_ertn      := Mux(take_int, false.B, can_commit0 && h0_raw_ertn)
    io.commit_pc_out    := e0.pc
    
    val raw_exc_addr = e0.exc_addr
    val raw_data_mmu = e0.is_data_mmu
    val is_fetch_exc_real = h0_real_exc && (io.commit_ecode === ExcCode.ADEF || io.commit_ecode === ExcCode.PIF || (io.commit_ecode === ExcCode.TLBR && !raw_data_mmu))
    io.commit_exc_addr := Mux(take_int, 0.U(32.W), Mux(is_fetch_exc_real, e0.pc, raw_exc_addr))

    io.commit_csr_num   := e0.csr_num
    io.commit_csr_wmask := e0.csr_wmask
    io.commit_csr_wdata := e0.csr_wdata

    val real_tlb_fill = Mux(take_int || h0_is_replay, false.B, can_commit0 && h0_raw_tlb_fill)
    val real_tlb_we   = Mux(take_int || h0_is_replay, false.B, can_commit0 && h0_raw_tlb_we)
    io.commit_tlbrd_we:= Mux(take_int || h0_is_replay, false.B, can_commit0 && h0_raw_tlbrd_we)
    
    val rand_idx = RegInit(0.U(4.W))
    rand_idx := rand_idx + 1.U
    io.tlb_we    := real_tlb_we || real_tlb_fill
    io.tlb_w_idx := Mux(real_tlb_fill, rand_idx, io.csr_tlbidx_out(3,0))
    io.tlb_w_dat := io.csr_tlb_out

    val real_refetch = Mux(take_int || h0_is_replay, false.B, can_commit0 && h0_raw_refetch)
    val do_replay = can_commit0 && h0_is_replay
    
    val is_csr_write = can_commit0 && io.commit_csr_we
    val is_tlb_write = can_commit0 && (real_tlb_we || real_tlb_fill || io.commit_tlbrd_we)
    val sync_flush   = is_csr_write || is_tlb_write
    
    // 1. 提取出当拍内部使用的组合逻辑信号
    val rob_flush_comb = h0_real_exc || io.commit_ertn || real_refetch || do_replay || sync_flush

    // 2. 提取目标 PC 的组合逻辑
    val rob_target_pc_comb = Mux(h0_real_exc, Mux(io.commit_ecode === "h3F".U, io.csr_tlbrentryOut, io.csr_eentryOut),
                           Mux(io.commit_ertn, io.csr_eraOut,
                           Mux(do_replay, raw_exc_addr, e0.pc + 4.U)))

    // 3. ★ 斩断世纪大路径！将发往全芯片的冲刷信号打一拍！
    io.wb_flush     := RegNext(rob_flush_comb, false.B)
    io.wb_target_pc := RegNext(rob_target_pc_comb, 0.U(32.W))

    // --- 头指针推演 ---
    // ★ 直接使用寄存器的 valid 状态，绝不去组合逻辑预测“当拍即将被杀的指令”。
    val valid_mask = Wire(Vec(numEntries, Bool()))
    for (i <- 0 until numEntries) {
        val idx = (head + i.U)(ptrWidth - 1, 0)
        valid_mask(i) := entries(idx).valid
    }
    val valid_bits = valid_mask.asUInt
    val first_valid_idx = PriorityEncoder(valid_bits)
    val no_valid_left = !valid_bits.orR

    when(!is_empty) {
        when(no_valid_left) {
            head := tail
            is_full := false.B
        } .elsewhen(is_ghost0) {
            head := (head + first_valid_idx)(ptrWidth - 1, 0)
            is_full := false.B
        } .elsewhen(can_commit0 && can_commit1) {
            entries(h0).valid := false.B
            entries(h1).valid := false.B
            head := (head + 2.U)(ptrWidth - 1, 0)
            is_full := false.B
        } .elsewhen(can_commit0) {
            entries(h0).valid := false.B
            head := (head + 1.U)(ptrWidth - 1, 0)
            is_full := false.B
        }
    }
    io.head_idx := head

    when(io.flush) {
        head := 0.U; tail := 0.U; is_full := false.B
        for (i <- 0 until numEntries) { entries(i).valid := false.B }
    }
}