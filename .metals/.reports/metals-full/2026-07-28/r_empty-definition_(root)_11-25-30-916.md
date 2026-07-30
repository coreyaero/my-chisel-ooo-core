error id: file://<WORKSPACE>/src/main/scala/Exec.scala:mycpu/MaskedQueue#io.
file://<WORKSPACE>/src/main/scala/Exec.scala
empty definition using pc, found symbol in pc: 
found definition using semanticdb; symbol mycpu/MaskedQueue#io.
empty definition using fallback
non-local guesses:

offset: 4933
uri: file://<WORKSPACE>/src/main/scala/Exec.scala
text:
```scala
package mycpu

import chisel3._
import chisel3.util._

class ExecutionEngine extends Module {
    val io = IO(new Bundle {
        //IQ
        val in_alu0     = Flipped(Decoupled(new PipelineData()))
        val in_alu1     = Flipped(Decoupled(new PipelineData()))
        val in_mdu      = Flipped(Decoupled(new PipelineData()))
        val in_agu      = Flipped(Decoupled(new PipelineData()))

        //CDB to PRF, IQ, ROB
        val cdb0        = Valid(new PipelineData())
        val cdb1        = Valid(new PipelineData())

        //Flush and Branch Prediction
        val flush       = Input(Bool())

        val br_resolve  = Output(new BranchResolve())
        val branch_req = Output(Bool())
        val branch_pc  = Output(UInt(32.W))

        

        // ---------------- 4. 访存与 TLB 透传接口 (来自 AGU) ----------------
        val lsq_req_id = Output(UInt(8.W))
        val lsq_ret_id = Input(UInt(8.W))


        val data_sram     = new SramIo()
        val data_uncached = Output(Bool())
        val mmu_config    = Input(new MmuConfig())
        
        //AGU <-> TLB
        val tlb_port        = new TlbSearchPort()
        val invtlb_valid    = Output(Bool())
        val invtlb_op       = Output(UInt(5.W))

        // ---------------- 5. LSQ 透传接口 ----------------
        val lsq_current_tail = Output(UInt(4.W)) 
        val lsq_br_restore   = Input(UInt(4.W))  
        val lsq_alloc_valid  = Input(Bool())
        val lsq_alloc_type   = Input(UInt(2.W))
        val lsq_alloc_rob    = Input(UInt(Config.robPtrWidth.W))
        val lsq_alloc_pc     = Input(UInt(32.W))
        val lsq_alloc_pdest  = Input(UInt(6.W))
        val lsq_alloc_mask   = Input(UInt(4.W))
        val lsq_alloc_cacop  = Input(UInt(5.W))
        val lsq_alloc_ready  = Output(Bool())
        val lsq_alloc_idx    = Output(UInt(4.W))
        val lsq_violation_valid = Output(Bool())
        val lsq_violation_rob   = Output(UInt(Config.robPtrWidth.W))
        val lsq_violation_pc    = Output(UInt(32.W))
        val commit_mem_valid0 = Input(Bool())
        val commit_mem_idx0   = Input(UInt(Config.robPtrWidth.W))
        val commit_mem_valid1 = Input(Bool())
        val commit_mem_idx1   = Input(UInt(Config.robPtrWidth.W))


        val csr_raddr = Output(UInt(14.W))
        val csr_rdata = Input(UInt(32.W))

        val cacop_en = Output(Bool())
        val cacop_op = Output(UInt(2.W))
        val cacop_is_icache = Output(Bool())

        val lsq_alloc_lsOp   = Input(UInt(8.W))

        val bpu_update = Valid(new BpuUpdate())

        val timer_in   = Input(UInt(64.W))

        val debug_cdb0_pc = Output(UInt(32.W))
        val debug_cdb1_pc = Output(UInt(32.W))
    })

    val alu0    = Module(new AluUnit())
    val alu1    = Module(new AluSimpleUnit())
    val mdu     = Module(new MduUnit())
    val agu     = Module(new AguUnit())
    val lsq     = Module(new LSQ())
    val arbiter = Module(new CdbArbiter())
    
    alu0.io.in <> io.in_alu0
    alu1.io.in <> io.in_alu1
    mdu.io.in  <> io.in_mdu
    agu.io.in  <> io.in_agu

    alu0.io.flush := io.flush
    alu1.io.flush := io.flush
    mdu.io.flush  := io.flush
    agu.io.flush  := io.flush

    // ==========================================
    // 广播网接驳 (ALU0 的分支结果通报全军)
    // ==========================================
    val global_br_resolve = alu0.io.br_resolve
    
    alu0.io.br_resolve_in := global_br_resolve
    alu1.io.br_resolve_in := global_br_resolve
    mdu.io.br_resolve_in  := global_br_resolve
    agu.io.br_resolve_in  := global_br_resolve
    
    io.br_resolve := global_br_resolve
    io.branch_req := alu0.io.branch_req
    io.branch_pc  := alu0.io.branch_pc
    io.bpu_update := alu0.io.bpu_update  // ★ 接这根线出来

    alu0.io.timer_in := io.timer_in

    // ==========================================
    // 访存与 TLB 专线 (AGU 独占)
    // ==========================================
    agu.io.mmu_config := io.mmu_config

    io.tlb_port <> agu.io.tlb_port
    io.invtlb_valid := agu.io.invtlb_valid
    io.invtlb_op    := agu.io.invtlb_op


    val agu_cdb_q = Module(new MaskedQueue())
    agu_cdb_q.io.flush := io.flush
    agu_cdb_q.io.br_resolve_in := global_br_resolve

    val is_load = agu.io.out.bits.resFromMem
    val agu_needs_cdb = !is_load
    
    agu_cdb_q.io.enq.valid := agu.io.out.valid && agu_needs_cdb
    agu_cdb_q.io.enq.bits  := agu.io.out.bits

    agu.io.out.ready := Mux(agu_needs_cdb, agu_cdb_q.io.enq.ready, true.B)
    //==========================================
    // CDB
    //==========================================
    val agu_cdb_q = Module(new MaskedQueue())
    agu_cdb_q.io.flush := io.flush
    agu_cdb_q.io.br_resolve_in := global_br_resolve

    val is_load = agu.io.out.bits.resFromMem
    val agu_needs_cdb = !is_load
    
    agu_cdb_q.io.enq.valid := agu.io.out.valid && agu_needs_cdb
    agu_cdb_q.io.enq.bits  := agu.io.out.bits

    agu.io.out.ready := Mux(agu_needs_cdb, agu_cdb_q.i@@o.enq.ready, true.B)

    arbiter.io.reqs(0) <> mdu.io.out
    arbiter.io.reqs(1) <> lsq.io.lsq_wb
    arbiter.io.reqs(2) <> agu_cdb_q.io.deq
    arbiter.io.reqs(3) <> alu0.io.out
    arbiter.io.reqs(4) <> alu1.io.out

    
    

    // 输出最终的 CDB！
    io.cdb0 := arbiter.io.cdb0
    io.cdb1 := arbiter.io.cdb1

    io.csr_raddr := alu0.io.csr_raddr
    alu0.io.csr_rdata := io.csr_rdata
    

    // ==========================================
    // 终极连线：AGU -> LSQ -> Top
    // ==========================================
    //这几部分同理，你不能改改lsq他们的接口，让他可以一键连过来吗？
    lsq.io.flush      := io.flush
    lsq.io.br_resolve := global_br_resolve

    io.lsq_req_id := lsq.io.dcache_req_id
    lsq.io.dcache_ret_id := io.lsq_ret_id

    lsq.io.alloc_valid := io.lsq_alloc_valid
    lsq.io.alloc_type  := io.lsq_alloc_type
    lsq.io.alloc_rob   := io.lsq_alloc_rob
    lsq.io.alloc_pc    := io.lsq_alloc_pc
    lsq.io.alloc_pdest := io.lsq_alloc_pdest
    lsq.io.alloc_mask  := io.lsq_alloc_mask
    lsq.io.alloc_cacop := io.lsq_alloc_cacop
    io.lsq_alloc_ready := lsq.io.alloc_ready
    io.lsq_alloc_idx   := lsq.io.alloc_idx
    io.lsq_current_tail:= lsq.io.current_lsq_tail
    lsq.io.br_restore_tail := io.lsq_br_restore

    lsq.io.agu_in.valid := agu.io.to_lsq.valid && agu.io.out.ready
    lsq.io.agu_in.bits  := agu.io.to_lsq.bits

    // Cache 透传完全交给 LSQ 控制 (AGU 成功隐退)
    io.data_sram       <> lsq.io.dcache
    io.data_uncached   := lsq.io.dcache_uncached
    io.cacop_en        := lsq.io.cacop_en
    io.cacop_op        := lsq.io.cacop_op
    io.cacop_is_icache := lsq.io.cacop_is_icache

    io.lsq_violation_valid := lsq.io.lsq_violation_valid
    io.lsq_violation_rob   := lsq.io.lsq_violation_rob
    io.lsq_violation_pc    := lsq.io.lsq_violation_pc

    lsq.io.commit_mem_valid0 := io.commit_mem_valid0
    lsq.io.commit_mem_idx0   := io.commit_mem_idx0
    lsq.io.commit_mem_valid1 := io.commit_mem_valid1
    lsq.io.commit_mem_idx1   := io.commit_mem_idx1

    lsq.io.alloc_lsOp := io.lsq_alloc_lsOp


    ///////////
    io.debug_cdb0_pc := arbiter.io.cdb0.bits.pc
    io.debug_cdb1_pc := arbiter.io.cdb1.bits.pc
}

class CdbArbiter extends Module {
    val io = IO(new Bundle {
        //0: MDU, 1: LSQ, 2: AGU, 3: ALU0, 4: ALU1
        val reqs = Vec(5, Flipped(Decoupled(new PipelineData())))
        
        val cdb0 = Valid(new PipelineData())
        val cdb1 = Valid(new PipelineData())
    })
    //==========================================
    // Module Selection
    //==========================================
    val req_valids = Cat(io.reqs(4).valid, io.reqs(3).valid, io.reqs(2).valid, io.reqs(1).valid, io.reqs(0).valid)
    val has_any_req = req_valids.orR
    val sel0 = PriorityEncoder(req_valids)
    val grant0 = Mux(has_any_req, UIntToOH(sel0), 0.U(5.W))

    val req_valids1 = req_valids & ~grant0 
    val has_any_req1 = req_valids1.orR
    val sel1 = PriorityEncoder(req_valids1)
    val grant1 = Mux(has_any_req1, UIntToOH(sel1), 0.U(5.W))

    for (i <- 0 until 5) { io.reqs(i).ready := grant0(i) || grant1(i)}

    io.cdb0.valid := has_any_req
    io.cdb0.bits  := Mux1H(grant0, io.reqs.map(_.bits))

    io.cdb1.valid := has_any_req1
    io.cdb1.bits  := Mux1H(grant1, io.reqs.map(_.bits))
}
class MaskedQueue extends Module {
    val io = IO(new Bundle {
        val enq = Flipped(Decoupled(new PipelineData()))
        val deq = Decoupled(new PipelineData())
        
        val flush = Input(Bool())
        val br_resolve_in = Input(new BranchResolve())
    })

    val valid_0 = RegInit(false.B)
    val data_0  = RegInit(0.U.asTypeOf(new PipelineData()))
    
    val valid_1 = RegInit(false.B)
    val data_1  = RegInit(0.U.asTypeOf(new PipelineData()))

    //==========================================
    // 击杀与净化逻辑
    //==========================================
    val br_fail = io.br_resolve_in.valid && io.br_resolve_in.mispredict
    val br_tag_bit = 1.U(4.W) << io.br_resolve_in.tag
    def is_killed(mask: UInt): Bool = br_fail && ((mask & br_tag_bit) =/= 0.U)
    val clear_mask = Mux(io.br_resolve_in.valid && !io.br_resolve_in.mispredict, ~br_tag_bit, "b1111".U(4.W))

    // 检查当前槽位里的数据，如果遭遇击杀，直接判死刑 (keep = false)
    val keep_0 = valid_0 && !is_killed(data_0.branch_mask)
    val keep_1 = valid_1 && !is_killed(data_1.branch_mask)
    val enq_kept = io.enq.valid && !is_killed(io.enq.bits.branch_mask)

    val do_deq = io.deq.ready && keep_0

    // ==========================================
    // 队列握手信号
    // ==========================================
    // 能不能进水？取决于本周期结束后，还剩下多少个数据。
    val slots_used = Cat(0.U(1.W), keep_0 && !do_deq) + Cat(0.U(1.W), keep_1)
    io.enq.ready := (slots_used < 2.U)

    // 出水永远看 0 号槽 (坍缩逻辑保证了数据总是向 0 号挤)
    io.deq.valid := keep_0
    val out_data = WireDefault(data_0)
    out_data.branch_mask := data_0.branch_mask & clear_mask
    io.deq.bits  := out_data

    // 准备好要写入的洗白数据
    val clean_enq_data = WireDefault(io.enq.bits)
    clean_enq_data.branch_mask := io.enq.bits.branch_mask & clear_mask
    val clean_data_1 = WireDefault(data_1)
    clean_data_1.branch_mask := data_1.branch_mask & clear_mask

    // ==========================================
    // 核心：瞬间坍缩状态机
    // ==========================================
    when(io.flush) {
        valid_0 := false.B
        valid_1 := false.B
    } .otherwise {
        // 提取这三个数据源的存活状态
        val item_0_v = keep_0 && !do_deq
        val item_1_v = keep_1
        val item_2_v = io.enq.ready && enq_kept
        
        // 拼成一个 3bit 的存活向量 (新来的, 槽1的, 槽0的)
        val v_bits = Cat(item_2_v, item_1_v, item_0_v)
        
        // ★ 像俄罗斯方块一样，把存活的数据紧凑地往 0 号槽位掉落 (坍缩)
        when(v_bits === "b000".U) {
            valid_0 := false.B; valid_1 := false.B
        } .elsewhen(v_bits === "b001".U) {
            valid_0 := true.B;  data_0 := out_data
            valid_1 := false.B
        } .elsewhen(v_bits === "b010".U) {
            valid_0 := true.B;  data_0 := clean_data_1
            valid_1 := false.B
        } .elsewhen(v_bits === "b100".U) {
            valid_0 := true.B;  data_0 := clean_enq_data
            valid_1 := false.B
        } .elsewhen(v_bits === "b011".U) {
            valid_0 := true.B;  data_0 := out_data
            valid_1 := true.B;  data_1 := clean_data_1
        } .elsewhen(v_bits === "b101".U) {
            valid_0 := true.B;  data_0 := out_data
            valid_1 := true.B;  data_1 := clean_enq_data
        } .elsewhen(v_bits === "b110".U) {
            valid_0 := true.B;  data_0 := clean_data_1
            valid_1 := true.B;  data_1 := clean_enq_data
        } .otherwise { 
            // b111 是绝对不可能发生的，因为 slots_used < 2 时才会拉高 enq_ready
            valid_0 := false.B; valid_1 := false.B
        }
    }
}

class AguEx2Data extends Bundle {
    val data            = new PipelineData()
    val va              = UInt(32.W)
    val src2            = UInt(32.W)
    val is_tlbsrch      = Bool()
    val dmw_hit         = Bool()
    val dmw_pa          = UInt(32.W)
    val dmw_mat         = UInt(2.W)
    
    val tlb_found       = Bool()
    val tlb_index       = UInt(4.W)
    val tlb_ppn         = UInt(20.W)
    val tlb_ps          = UInt(6.W)
    val tlb_plv         = UInt(2.W)
    val tlb_mat         = UInt(2.W)
    val tlb_d           = Bool()
    val tlb_v           = Bool()
    
    val is_direct_mode  = Bool()
    val is_paged_mode   = Bool()
    val crmd_plv        = UInt(2.W)
    val crmd_datm       = UInt(2.W)
}
class Agu2Lsq extends Bundle {
    val lsqIdx   = UInt(4.W)
    val paddr    = UInt(32.W)
    val size     = UInt(2.W)
    val uncached = Bool()
    val wdata    = UInt(32.W)
    val wstrb    = UInt(4.W)
    val has_exc  = Bool()
    val ecode    = UInt(6.W)
}


class AguUnit extends Module {
    val io = IO(new Bundle {
        val in              = Flipped(Decoupled(new PipelineData()))
        val out             = Decoupled(new PipelineData())
        val to_lsq          = Valid(new Agu2Lsq())

        val flush           = Input(Bool())
        val br_resolve_in   = Input(new BranchResolve())

        val mmu_config      = Input(new MmuConfig())
        val tlb_port        = new TlbSearchPort()

        val invtlb_valid    = Output(Bool())
        val invtlb_op       = Output(UInt(5.W))
    })

    //==========================================
    // Flush logic
    //==========================================
    val br_fail = io.br_resolve_in.valid && io.br_resolve_in.mispredict
    val br_tag_bit = 1.U(4.W) << io.br_resolve_in.tag
    def is_killed(mask: UInt): Bool = br_fail && ((mask & br_tag_bit) =/= 0.U)
    val clear_mask = Mux(io.br_resolve_in.valid && !io.br_resolve_in.mispredict, ~br_tag_bit, "b1111".U(4.W))

    //==========================================
    // EX1 & EX2 Pipeline
    //==========================================
    val ex1_valid = RegInit(false.B)
    val ex1_data  = RegInit(0.U.asTypeOf(new PipelineData()))

    val ex2_valid = RegInit(false.B)
    val ex2_data = RegInit(0.U.asTypeOf(new AguEx2Data()))

    val ex1_active = ex1_valid && !is_killed(ex1_data.branch_mask)
    val ex2_active = ex2_valid && !is_killed(ex2_data.data.branch_mask)

    //If killed, ex1 & 2 is ready to accept new too.
    val ex2_ready = !ex2_active || io.out.ready
    val ex1_ready = !ex1_active || ex2_ready

    io.in.ready     := ex1_ready
    io.out.valid    := ex2_active

    val next_ex2 = Wire(new AguEx2Data())

    when(io.flush) {
        ex1_valid := false.B
        ex2_valid := false.B
    } .otherwise {
        when(ex1_ready) {
            ex1_data := io.in.bits
            ex1_valid := io.in.valid && !is_killed(io.in.bits.branch_mask)
            ex1_data.branch_mask := io.in.bits.branch_mask & clear_mask
        } .otherwise {
            ex1_data.branch_mask := ex1_data.branch_mask & clear_mask
        }
        when(ex2_ready) {
            ex2_data  := next_ex2
            ex2_valid := ex1_active
            //branch cleared in next_ex2
        } .otherwise {
            ex2_data.data.branch_mask := ex2_data.data.branch_mask & clear_mask
        }
    }

    //==========================================
    // EX1 Execution
    //==========================================
    val ex1_src1 = ex1_data.src1_value
    val ex1_src2 = ex1_data.src2_value
    val ex1_va   = ex1_src1 + Mux(ex1_data.src2IsImm, ex1_data.imm, ex1_src2)

    val is_tlbsrch = ex1_data.tlbOp === TlbOp.SRCH
    val is_invtlb  = ex1_data.tlbOp === TlbOp.INV

    io.tlb_port.vppn     := Mux(is_invtlb, ex1_src2(31, 13), Mux(is_tlbsrch, io.mmu_config.tlbehi.vppn, ex1_va(31, 13)))
    io.tlb_port.va_bit12 := ex1_va(12)
    io.tlb_port.asid     := Mux(is_invtlb, ex1_src1(9, 0), io.mmu_config.asid.asid)
    
    io.invtlb_valid := is_invtlb && ex1_active && !ex1_data.hasException
    io.invtlb_op    := ex1_data.invtlb_op

    val pg = io.mmu_config.crmd.pg === 1.U
    val da = io.mmu_config.crmd.da === 1.U
    val is_paged_mode  = pg && !da
    val is_direct_mode = !pg && da
    
    val dmw0_hit = is_paged_mode && (ex1_va(31, 29) === io.mmu_config.dmw0.vseg) &&
                  ((io.mmu_config.crmd.plv === 0.U && io.mmu_config.dmw0.plv0 === 1.U) || (io.mmu_config.crmd.plv === 3.U && io.mmu_config.dmw0.plv3 === 1.U))
    val dmw1_hit = is_paged_mode && (ex1_va(31, 29) === io.mmu_config.dmw1.vseg) &&
                  ((io.mmu_config.crmd.plv === 0.U && io.mmu_config.dmw1.plv0 === 1.U) || (io.mmu_config.crmd.plv === 3.U && io.mmu_config.dmw1.plv3 === 1.U))

    next_ex2.data               := ex1_data
    next_ex2.data.branch_mask   := ex1_data.branch_mask & clear_mask
    next_ex2.va                 := ex1_va
    next_ex2.src2               := ex1_src2
    next_ex2.is_tlbsrch         := is_tlbsrch
    next_ex2.dmw_hit            := dmw0_hit || dmw1_hit
    next_ex2.dmw_pa             := Mux(dmw0_hit, Cat(io.mmu_config.dmw0.pseg, ex1_va(28, 0)), Cat(io.mmu_config.dmw1.pseg, ex1_va(28, 0)))
    next_ex2.dmw_mat            := Mux(dmw0_hit, io.mmu_config.dmw0.mat, io.mmu_config.dmw1.mat)
    next_ex2.tlb_found          := io.tlb_port.found
    next_ex2.tlb_index          := io.tlb_port.index
    next_ex2.tlb_ppn            := io.tlb_port.ppn
    next_ex2.tlb_ps             := io.tlb_port.ps
    next_ex2.tlb_plv            := io.tlb_port.plv
    next_ex2.tlb_mat            := io.tlb_port.mat
    next_ex2.tlb_d              := io.tlb_port.d
    next_ex2.tlb_v              := io.tlb_port.v
    next_ex2.is_paged_mode      := is_paged_mode
    next_ex2.is_direct_mode     := is_direct_mode
    next_ex2.crmd_plv           := io.mmu_config.crmd.plv
    next_ex2.crmd_datm          := io.mmu_config.crmd.datm

    //==========================================
    // EX2 Execution
    //==========================================
    val ex2_d   = ex2_data.data
    val ex2_va  = ex2_data.va

    val tlbsrch_res = Cat(!ex2_data.tlb_found, 0.U(27.W), ex2_data.tlb_index)
    val tlbsrch_mask = Mux(ex2_data.tlb_found, "h8000000F".U(32.W), "h80000000".U(32.W))

    val tlb_pa = Mux(ex2_data.tlb_ps === 12.U, Cat(ex2_data.tlb_ppn, ex2_va(11, 0)), Cat(ex2_data.tlb_ppn(19, 9), ex2_va(20, 0)))
    val pa = Mux(ex2_data.is_direct_mode, ex2_va, Mux(ex2_data.dmw_hit, ex2_data.dmw_pa, tlb_pa))

    val current_mat = Mux(ex2_data.is_direct_mode, ex2_data.crmd_datm, Mux(ex2_data.dmw_hit, ex2_data.dmw_mat, ex2_data.tlb_mat))
    val uncached = (current_mat === 0.U)

    val isWord = ex2_d.lsOp === LsOp.LD_W || ex2_d.lsOp === LsOp.ST_W
    val isHalf = ex2_d.lsOp === LsOp.LD_H || ex2_d.lsOp === LsOp.LD_HU || ex2_d.lsOp === LsOp.ST_H

    val is_load  = ex2_d.resFromMem
    val is_store = ex2_d.memWe
    //cacop
    val is_hit_inv = ex2_d.is_cacop && (ex2_d.cacop_op(4, 3) === 2.U)

    val ale = (is_load || is_store) && ((isWord && ex2_va(1, 0) =/= 0.U) || (isHalf && ex2_va(0) === 1.U))
    val is_mapped = ex2_data.is_paged_mode && !ex2_data.dmw_hit
    val need_tlb  = is_mapped && (is_load || is_store || is_hit_inv)

    val tlb_f = ex2_data.tlb_found
    val tlb_v = ex2_data.tlb_v
    val tlb_d = ex2_data.tlb_d
    val priv_fault = (ex2_data.crmd_plv === 3.U) && (ex2_data.tlb_plv === 0.U)

    val tlb_ecode = Mux(!tlb_f,                                                 "h3F".U(6.W), 0.U) |
                    Mux(tlb_f && tlb_v && priv_fault,                           "h07".U(6.W), 0.U) |
                    Mux(tlb_f && !tlb_v && (is_load || is_hit_inv),             "h01".U(6.W), 0.U) |
                    Mux(tlb_f && !tlb_v && is_store,                            "h02".U(6.W), 0.U) |
                    Mux(tlb_f && tlb_v && !priv_fault && !tlb_d && is_store,    "h04".U(6.W), 0.U)

    val agu_ecode = Mux(ale, "h09".U(6.W), Mux(need_tlb, tlb_ecode, 0.U))
    val has_exc = ex2_d.hasException || (ex2_active && agu_ecode =/= 0.U)

    //==========================================
    // Output
    //==========================================
    val wdata_b = Fill(4, ex2_data.src2(7, 0))
    val wdata_h = Fill(2, ex2_data.src2(15, 0))
    val stMaskB = "b0001".U(4.W) << ex2_va(1, 0)
    val stMaskH = Mux(ex2_va(1), "b1100".U(4.W), "b0011".U(4.W))
    val stMaskW = "b1111".U(4.W)
    val wstrb   = Mux(isWord, stMaskW, Mux(isHalf, stMaskH, stMaskB))

    val is_lsq_inst = (is_load || is_store || ex2_d.is_cacop) && ex2_active
    io.to_lsq.valid         := is_lsq_inst && !io.flush

    io.to_lsq.bits.lsqIdx   := ex2_d.lsq_idx
    io.to_lsq.bits.paddr    := Cat(Mux(ex2_d.is_cacop && (ex2_d.cacop_op(4, 3) =/= 2.U), ex2_va(31,12), pa(31,12)), ex2_va(11, 0))
    io.to_lsq.bits.size     := Mux(isWord, 2.U, Mux(isHalf, 1.U, 0.U))
    io.to_lsq.bits.uncached := uncached
    io.to_lsq.bits.wdata    := Mux(isWord, ex2_data.src2, Mux(isHalf, wdata_h, wdata_b))
    io.to_lsq.bits.wstrb    := Mux(ex2_d.memWe && !has_exc, wstrb, 0.U(4.W))
    io.to_lsq.bits.has_exc  := has_exc
    io.to_lsq.bits.ecode    := agu_ecode

    val out_data = WireDefault(ex2_d)
    out_data.ex_result      := Mux(ex2_data.is_tlbsrch, tlbsrch_res, ex2_va)
    out_data.aux_data       := Mux(ex2_data.is_tlbsrch, tlbsrch_mask, 0.U)
    out_data.hasException   := has_exc
    out_data.ecode          := Mux(ex2_d.hasException, ex2_d.ecode, agu_ecode)
    out_data.is_cacop       := ex2_d.is_cacop
    out_data.cacop_op       := ex2_d.cacop_op

    io.out.bits  := out_data
}
class MduUnit extends Module {
    val io = IO(new Bundle {
        val in  = Flipped(Decoupled(new PipelineData()))
        val out = Decoupled(new PipelineData())

        val flush = Input(Bool())
        val br_resolve_in = Input(new BranchResolve()) 
    })

    //==========================================
    // Flush logic
    //==========================================
    val br_fail = io.br_resolve_in.valid && io.br_resolve_in.mispredict
    val br_tag_bit = 1.U(4.W) << io.br_resolve_in.tag
    def is_killed(mask: UInt): Bool = br_fail && ((mask & br_tag_bit) =/= 0.U)
    val clear_mask = Mux(io.br_resolve_in.valid && !io.br_resolve_in.mispredict, ~br_tag_bit, "b1111".U(4.W))

    //==========================================
    // Pipeline
    //==========================================
    val valid_reg = RegInit(false.B)
    val data_reg  = RegInit(0.U.asTypeOf(new PipelineData()))
    val active    = valid_reg && !is_killed(data_reg.branch_mask)

    val src1_val = data_reg.src1_value
    val src2_val = data_reg.src2_value

    //==========================================
    // MDU
    //==========================================
    val mdu_busy     = RegInit(false.B)
    val mdu_finished = RegInit(false.B)

    val is_div = data_reg.mduOp === MduOp.DIV_W || data_reg.mduOp === MduOp.MOD_W || 
                 data_reg.mduOp === MduOp.DIV_WU || data_reg.mduOp === MduOp.MOD_WU
    val is_mul = data_reg.mduOp === MduOp.MUL_W || data_reg.mduOp === MduOp.MULH_W || 
                 data_reg.mduOp === MduOp.MULH_WU
    val is_mdu_inst = data_reg.resFromMulDiv
    val is_signed = data_reg.mduOp === MduOp.MULH_W || data_reg.mduOp === MduOp.DIV_W || data_reg.mduOp === MduOp.MOD_W

    val valid_mdu_req = active && is_mdu_inst && !data_reg.hasException
    val start_pulse = valid_mdu_req && !mdu_busy && !mdu_finished && !io.flush
    
    val mul = Module(new Multiplier())
    mul.io.src1     := src1_val
    mul.io.src2     := src2_val
    mul.io.isSigned := is_signed
    val mul_done = RegNext(start_pulse && is_mul, false.B)
    
    val div = Module(new Divider())
    div.io.enable   := start_pulse && is_div
    div.io.aresetn  := !(io.flush || is_killed(data_reg.branch_mask))
    div.io.a        := Mux(is_signed && src1_val(31), (~src1_val + 1.U), src1_val)
    div.io.b        := Mux(is_signed && src2_val(31), (~src2_val + 1.U), src2_val)
    val div_done   = div.io.done 

    val math_done = !valid_mdu_req || mdu_finished || (mdu_busy && Mux(is_div, div_done, mul_done))
    io.out.valid := active && math_done
    val in_ready = !active || (math_done && io.out.ready)
    io.in.ready  := in_ready

    val q_sign = src1_val(31) ^ src2_val(31)
    val r_sign = src1_val(31)
    val final_q = Mux(is_signed && q_sign, (~div.io.q + 1.U), div.io.q)
    val final_r = Mux(is_signed && r_sign, (~div.io.r + 1.U), div.io.r)

    val mdu_res = MuxLookup(data_reg.mduOp, 0.U(32.W))(Seq(
        MduOp.MUL_W   -> mul.io.result64(31, 0),
        MduOp.MULH_W  -> mul.io.result64(63, 32),
        MduOp.MULH_WU -> mul.io.result64(63, 32),
        MduOp.DIV_W   -> final_q,
        MduOp.MOD_W   -> final_r,
        MduOp.DIV_WU  -> div.io.q,
        MduOp.MOD_WU  -> div.io.r
    ))

    val out_data = WireDefault(data_reg)
    out_data.ex_result := mdu_res
    io.out.bits := out_data

    //==========================================
    // MDU FSM
    //==========================================
    when(io.flush) {
        valid_reg    := false.B
        mdu_busy     := false.B
        mdu_finished := false.B
    } .otherwise {
        when(in_ready) {
            valid_reg := io.in.valid && !is_killed(io.in.bits.branch_mask)
            data_reg  := io.in.bits
            data_reg.branch_mask := io.in.bits.branch_mask & clear_mask

            mdu_busy     := false.B
            mdu_finished := false.B
        } .otherwise {
            data_reg.branch_mask := data_reg.branch_mask & clear_mask
            when(is_killed(data_reg.branch_mask)) {
                valid_reg    := false.B
                mdu_busy     := false.B
                mdu_finished := false.B
            } .elsewhen(start_pulse) {
                mdu_busy := true.B
            } .elsewhen(mdu_busy && (div_done || mul_done)) {
                mdu_busy     := false.B
                mdu_finished := true.B
            }
        }
    }
}
class AluSimpleUnit extends Module {
    val io = IO(new Bundle {
        val in  = Flipped(Decoupled(new PipelineData()))
        val out = Decoupled(new PipelineData())

        val flush = Input(Bool())
        val br_resolve_in = Input(new BranchResolve()) 
    })

    //==========================================
    // Flush logic
    //==========================================
    val br_fail = io.br_resolve_in.valid && io.br_resolve_in.mispredict
    val br_tag_bit = 1.U(4.W) << io.br_resolve_in.tag
    def is_killed(mask: UInt): Bool = br_fail && ((mask & br_tag_bit) =/= 0.U)
    val clear_mask = Mux(io.br_resolve_in.valid && !io.br_resolve_in.mispredict, ~br_tag_bit, "b1111".U(4.W))

    //==========================================
    // Pipeline
    //==========================================
    val valid_reg = RegInit(false.B)
    val data_reg  = RegInit(0.U.asTypeOf(new PipelineData()))
    val active    = valid_reg && !is_killed(data_reg.branch_mask)

    val in_ready = !active || io.out.ready
    io.in.ready  := in_ready
    io.out.valid := active

    //==========================================
    // FSM
    //==========================================
    when(io.flush) {
        valid_reg := false.B
    } .otherwise {
        when(in_ready) {
            valid_reg := io.in.valid && !is_killed(io.in.bits.branch_mask)
            data_reg  := io.in.bits
            data_reg.branch_mask := io.in.bits.branch_mask & clear_mask
        } .otherwise {
            data_reg.branch_mask := data_reg.branch_mask & clear_mask
            when(is_killed(data_reg.branch_mask)) {
                valid_reg := false.B
            }
        }
    }

    //==========================================
    // ALU
    //==========================================
    val alu_src1 = Mux(data_reg.src1IsPC, data_reg.pc, data_reg.src1_value)
    val alu_src2 = Mux(data_reg.src2IsImm, data_reg.imm, Mux(data_reg.src2IsFour, 4.U, data_reg.src2_value))

    val alu = Module(new ALU())
    alu.io.aluOp := data_reg.aluOp
    alu.io.src1  := alu_src1
    alu.io.src2  := alu_src2

    val out_data = WireDefault(data_reg) 
    out_data.ex_result := alu.io.res

    io.out.bits := out_data
}
class AluUnit extends Module {
    val io = IO(new Bundle {
        val in  = Flipped(Decoupled(new PipelineData()))
        val out = Decoupled(new PipelineData())

        val branch_req = Output(Bool())
        val branch_pc  = Output(UInt(32.W))
        val br_resolve = Output(new BranchResolve())

        val flush = Input(Bool())
        val timer_in = Input(UInt(64.W))
        val br_resolve_in = Input(new BranchResolve()) 

        val csr_raddr = Output(UInt(14.W))
        val csr_rdata = Input(UInt(32.W))

        val bpu_update = Valid(new BpuUpdate())
    })

    //==========================================
    // Flush logic
    //==========================================
    val br_fail = io.br_resolve_in.valid && io.br_resolve_in.mispredict
    val br_tag_bit = 1.U(4.W) << io.br_resolve_in.tag
    def is_killed(mask: UInt): Bool = br_fail && ((mask & br_tag_bit) =/= 0.U)
    val clear_mask = Mux(io.br_resolve_in.valid && !io.br_resolve_in.mispredict, ~br_tag_bit, "b1111".U(4.W))

    //==========================================
    // Pipeline
    //==========================================
    val valid_reg = RegInit(false.B)
    val data_reg  = RegInit(0.U.asTypeOf(new PipelineData()))
    val br_broadcasted = RegInit(false.B)
    val active = valid_reg

    val in_ready = !active || io.out.ready
    io.in.ready  := in_ready
    io.out.valid := active

    //==========================================
    // FSM
    //==========================================
    when(io.flush) {
        valid_reg      := false.B
        br_broadcasted := false.B
    } .otherwise {
        when(in_ready) {
            valid_reg := io.in.valid && !is_killed(io.in.bits.branch_mask)
            data_reg  := io.in.bits
            data_reg.branch_mask := io.in.bits.branch_mask & clear_mask
            br_broadcasted := false.B
        } .otherwise {
            data_reg.branch_mask := data_reg.branch_mask & clear_mask
            when(io.br_resolve.valid) {
                br_broadcasted := true.B
            }
        }
    }

    val src1_val = data_reg.src1_value
    val src2_val = data_reg.src2_value

    //==========================================
    // Branch
    //==========================================
    val eq  = (src1_val === src2_val)
    val lt  = (src1_val.asSInt < src2_val.asSInt)
    val ltu = (src1_val < src2_val)

    val branch_actual_taken = MuxLookup(data_reg.brType, false.B)(Seq(
        BrType.BEQ  -> eq,      BrType.BNE  -> !eq,     BrType.BLT  -> lt,
        BrType.BGE  -> !lt,     BrType.BLTU -> ltu,     BrType.BGEU -> !ltu,
        BrType.JIRL -> true.B,  BrType.B   -> true.B,   BrType.BL  -> true.B
    ))

    val branch_base_pc = Mux(data_reg.brType === BrType.JIRL, src1_val, data_reg.pc)
    val calc_target_pc = branch_base_pc + data_reg.imm

    val dir_wrong = (branch_actual_taken =/= data_reg.pred_taken)
    val addr_wrong = branch_actual_taken && (calc_target_pc =/= data_reg.pred_target)
    val mispredict = dir_wrong || addr_wrong

    val correct_next_pc = Mux(branch_actual_taken, calc_target_pc, data_reg.pc + 4.U)

    val do_br_resolve = active && data_reg.is_branch && !data_reg.hasException && !br_broadcasted
    io.br_resolve.valid      := do_br_resolve
    io.br_resolve.mispredict := mispredict
    io.br_resolve.tag        := data_reg.branch_tag

    //==========================================
    // To BPU
    //==========================================
    io.branch_req := do_br_resolve && mispredict
    io.branch_pc  := correct_next_pc

    val op = data_reg.inst(31, 26)
    val rd = data_reg.inst(4, 0)
    val rj = data_reg.inst(9, 5)
    val is_call   = (op === "b010101".U) || ((op === "b010011".U) && (rd === 1.U))
    val is_ret    = (op === "b010011".U) && (rj === 1.U) && (rd === 0.U)
    val is_uncond = (op === "b010100".U) || is_call || is_ret || (op === "b010011".U)
    val btype     = Mux(is_ret, BpuType.RET, Mux(is_call, BpuType.CALL, Mux(is_uncond, BpuType.UNCOND, BpuType.COND)))

    io.bpu_update.valid := do_br_resolve
    io.bpu_update.bits.pc         := data_reg.pc
    io.bpu_update.bits.taken      := branch_actual_taken
    io.bpu_update.bits.target     := calc_target_pc
    io.bpu_update.bits.bpu_type   := btype
    io.bpu_update.bits.ghr        := data_reg.ghr
    io.bpu_update.bits.ras_tos    := data_reg.ras_tos
    io.bpu_update.bits.mispredict := mispredict

    //==========================================
    // ALU
    //==========================================
    val alu_src1 = Mux(data_reg.src1IsPC, data_reg.pc, src1_val)
    val alu_src2 = Mux(data_reg.src2IsImm, data_reg.imm, Mux(data_reg.src2IsFour, 4.U, src2_val))

    val alu = Module(new ALU())
    alu.io.aluOp := data_reg.aluOp
    alu.io.src1  := alu_src1
    alu.io.src2  := alu_src2

    val csr_mask = Mux(data_reg.src1_addr === 0.U, 0.U(32.W),
                   Mux(data_reg.src1_addr === 1.U, "hFFFFFFFF".U(32.W), src1_val))
    io.csr_raddr := data_reg.csrNum

    val final_ex_result = Mux(data_reg.rdtimel, io.timer_in(31, 0),
                          Mux(data_reg.rdtimeh, io.timer_in(63, 32),
                          Mux(data_reg.isCsr, io.csr_rdata, alu.io.res)))

    val out_data = WireDefault(data_reg) 
    out_data.ex_result := final_ex_result
    out_data.aux_data  := Mux(data_reg.isCsr, csr_mask, src2_val)

    io.out.bits  := out_data
}


class div_gen_0 extends ExtModule {
    val io = FlatIO(new Bundle {
        val aclk                    = Input(Clock())
        val aresetn                 = Input(Bool())
        val s_axis_divisor_tvalid   = Input(Bool())
        val s_axis_divisor_tdata    = Input(UInt(32.W))
        val s_axis_dividend_tvalid  = Input(Bool())
        val s_axis_dividend_tdata   = Input(UInt(32.W))
        val m_axis_dout_tvalid      = Output(Bool())
        val m_axis_dout_tdata       = Output(UInt(64.W))
    })

    setInline("div_gen_0.v",
        """module div_gen_0(
  input         aclk,
  input         aresetn,
  input         s_axis_divisor_tvalid,
  input  [31:0] s_axis_divisor_tdata,
  input         s_axis_dividend_tvalid,
  input  [31:0] s_axis_dividend_tdata,
  output        m_axis_dout_tvalid,
  output [63:0] m_axis_dout_tdata
);
  
  reg [33:0] valid_pipe;
  reg [63:0] data_pipe [0:33];
  
  integer i;
  
  initial begin
      valid_pipe = 34'b0;
      for (i = 0; i < 34; i = i + 1) data_pipe[i] = 64'b0;
  end
  
  always @(posedge aclk) begin
      if (!aresetn) begin
          valid_pipe <= 34'b0;
      end else begin
          valid_pipe[0] <= s_axis_divisor_tvalid & s_axis_dividend_tvalid;
          
          if (s_axis_divisor_tvalid & s_axis_dividend_tvalid) begin
              if (s_axis_divisor_tdata == 32'b0) begin
                  data_pipe[0] <= {32'hffffffff, s_axis_dividend_tdata};
              end else begin
                  data_pipe[0] <= { (s_axis_dividend_tdata / s_axis_divisor_tdata), 
                                    (s_axis_dividend_tdata % s_axis_divisor_tdata) };
              end
          end
          
          for (i = 1; i < 34; i = i + 1) begin
              valid_pipe[i] <= valid_pipe[i-1];
              data_pipe[i]  <= data_pipe[i-1];
          end
      end
  end
  
  assign m_axis_dout_tvalid = valid_pipe[33];
  assign m_axis_dout_tdata  = data_pipe[33];
  
endmodule
""".stripMargin)
}
class Multiplier extends Module {
    val io = IO(new Bundle {
        val src1     = Input(UInt(32.W))
        val src2     = Input(UInt(32.W))
        val isSigned = Input(Bool())
        val result64 = Output(UInt(64.W))
    })
    val signedRes   = RegNext(io.src1.asSInt * io.src2.asSInt).asUInt
    val unsignedRes = RegNext(io.src1 * io.src2)
    io.result64    := Mux(io.isSigned, signedRes, unsignedRes)
}
class Divider extends Module {
    val io = IO(new Bundle {
        val enable = Input(Bool())
        val aresetn= Input(Bool())
        val a      = Input(UInt(32.W))
        val b      = Input(UInt(32.W))
        val q      = Output(UInt(32.W))
        val r      = Output(UInt(32.W))
        val done   = Output(Bool())
    })

    val div_ip = Module(new div_gen_0())
    div_ip.io.aclk                   := clock
    div_ip.io.aresetn                := io.aresetn
    div_ip.io.s_axis_dividend_tvalid := io.enable
    div_ip.io.s_axis_dividend_tdata  := io.a
    div_ip.io.s_axis_divisor_tvalid  := io.enable
    div_ip.io.s_axis_divisor_tdata   := io.b
    
    io.done := div_ip.io.m_axis_dout_tvalid
    io.q    := div_ip.io.m_axis_dout_tdata(63, 32)
    io.r    := div_ip.io.m_axis_dout_tdata(31, 0)
}
class ALU extends Module{
    val io = IO(new Bundle{
        val aluOp   = Input(UInt(13.W))
        val src1    = Input(UInt(32.W))
        val src2    = Input(UInt(32.W))
        val res     = Output(UInt(32.W))
    })

    val Seq(opAdd, opSub, opSlt, opSltu, opAnd, opNor, opOr, opXor, opSll, opSrl, opSra, opLui, opCpucfg) = io.aluOp.asBools

    val adderCin = opSub | opSlt | opSltu
    val adderB = Mux(adderCin, ~io.src2, io.src2)
    val adderRes = io.src1 +& adderB + adderCin

    val addsubRes = adderRes(31, 0)
    val adderCout = adderRes(32)

    val sltBit0 = (io.src1(31) & ~io.src2(31)) | ((io.src1(31) === io.src2(31)) & addsubRes(31))
    val sltRes = Cat(0.U(31.W), sltBit0)
    val sltuRes = Cat(0.U(31.W), ~adderCout)

    val shamt = io.src2(4, 0)
    val sllRes = io.src1 << shamt
    val srlRes = io.src1 >> shamt
    val sraRes = (io.src1.asSInt >> shamt).asUInt

    // =========================================================================
    // ★ CPUCFG 满血配置表 (龙芯 LA32R 规范)
    // =========================================================================
    val cpucfgRes = MuxLookup(io.src1, 0.U(32.W))(Seq(
        // --- 0x00 ~ 0x05: 基础架构与扩展能力 ---
        "h00".U -> "h0014C010".U, // PRID: 处理器标识 (LA32R 特征码)
        "h01".U -> "h00000001".U, // ISA: 标明为 LA32 架构
        "h02".U -> "h00000000".U, // Extensions: 纯整数基础指令集 (无浮点、向量扩展)
        
        // --- 0x03: MMU 配置 (极其重要) ---
        // [14:12] DMW数量-1 = 1 (你有 dmw0 和 dmw1)
        // [11:0]  TLB表项-1 = 15 (你的 tlb_table 是 16 项)
        "h03".U -> "h0000100F".U, 
        
        "h04".U -> "h00000000".U, // OS Features: 无硬件页表漫游等高级特性
        "h05".U -> "h00000000".U, // OS Features

        // --- 0x10 ~ 0x14: 缓存拓扑结构 ---
        "h10".U -> "h00000005".U, // L1 Cache 存在性: ICache(bit 0) = 1, DCache(bit 2) = 1
        
        // L1 ICache: Offset=5 (32B), Index=8 (256 Sets), Ways=3 (4 Ways) -> 0x05080003
        "h11".U -> "h05080003".U, 
        
        // L1 DCache: Offset=5 (32B), Index=8 (256 Sets), Ways=3 (4 Ways) -> 0x05080003
        "h12".U -> "h05080003".U, 
        
        // L2 / L3 Cache 0
        "h13".U -> "h00000000".U, 
        "h14".U -> "h00000000".U  
    ))

    io.res := Mux1H(Seq(
        opAdd       -> addsubRes,
        opSub       -> addsubRes,
        opSlt       -> sltRes,
        opSltu      -> sltuRes,
        opAnd       -> (io.src1 & io.src2),
        opNor       -> ~(io.src1 | io.src2),
        opOr        -> (io.src1 | io.src2),
        opXor       -> (io.src1 ^ io.src2),
        opSll       -> sllRes,
        opSrl       -> srlRes,
        opSra       -> sraRes,
        opLui       -> io.src2,
        opCpucfg    -> cpucfgRes
    ))
}

```


#### Short summary: 

empty definition using pc, found symbol in pc: 