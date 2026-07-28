error id: file://<WORKSPACE>/src/main/scala/AguUnit.scala:mycpu/AguUnit#src2_val.
file://<WORKSPACE>/src/main/scala/AguUnit.scala
empty definition using pc, found symbol in pc: 
empty definition using semanticdb
empty definition using fallback
non-local guesses:
	 -chisel3/src2_val.
	 -chisel3/src2_val#
	 -chisel3/src2_val().
	 -chisel3/util/src2_val.
	 -chisel3/util/src2_val#
	 -chisel3/util/src2_val().
	 -src2_val.
	 -src2_val#
	 -src2_val().
	 -scala/Predef.src2_val.
	 -scala/Predef.src2_val#
	 -scala/Predef.src2_val().
offset: 4679
uri: file://<WORKSPACE>/src/main/scala/AguUnit.scala
text:
```scala
package mycpu

import chisel3._
import chisel3.util._

class AguEx2Data extends Bundle {
    val data       = new PipelineData()
    val va         = UInt(32.W)
    val src2       = UInt(32.W)
    val is_tlbsrch = Bool()
    val dmw_hit    = Bool()
    val dmw_pa     = UInt(32.W)
    val dmw_mat    = UInt(2.W)
    
    val tlb_found  = Bool()
    val tlb_index  = UInt(4.W)
    val tlb_ppn    = UInt(20.W)
    val tlb_ps     = UInt(6.W)
    val tlb_plv    = UInt(2.W)
    val tlb_mat    = UInt(2.W)
    val tlb_d      = Bool()
    val tlb_v      = Bool()
    
    val crmd_pg    = UInt(1.W)
    val crmd_da    = UInt(1.W)
    val crmd_plv   = UInt(2.W)
    val crmd_datm  = UInt(2.W)
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
    val alu_src1 = Mux(ex1_data.src1IsPC, ex1_data.pc, ex1_src1)
    val alu_src2 = Mux(ex1_data.src2IsImm, ex1_data.imm, Mux(ex1_data.src2IsFour, 4.U, ex1_src2))
    val ex1_va   = alu_src1 + alu_src2 

    val is_tlbsrch = ex1_data.tlbOp === TlbOp.SRCH
    val is_invtlb  = ex1_data.tlbOp === TlbOp.INV

    io.tlb_port.vppn     := Mux(is_invtlb, ex1_src2(31, 13), Mux(is_tlbsrch, io.mmu_config.tlbehi.vppn, ex1_va(31, 13)))
    io.tlb_port.va_bit12 := ex1_va(12)
    io.tlb_port.asid     := Mux(is_invtlb, ex1_src1(9, 0), io.mmu_config.asid.asid)
    
    io.invtlb_valid := is_invtlb && ex1_active && !ex1_data.hasException
    io.invtlb_op    := ex1_data.invtlb_op

    val pg = io.mmu_config.crmd.pg === 1.U
    val da = io.mmu_config.crmd.da === 0.U
    val dmw0_hit = pg && da && (ex1_va(31, 29) === io.mmu_config.dmw0.vseg) &&
                  ((io.mmu_config.crmd.plv === 0.U && io.mmu_config.dmw0.plv0 === 1.U) || (io.mmu_config.crmd.plv === 3.U && io.mmu_config.dmw0.plv3 === 1.U))
    val dmw1_hit = pg && da && (ex1_va(31, 29) === io.mmu_config.dmw1.vseg) &&
                  ((io.mmu_config.crmd.plv === 0.U && io.mmu_config.dmw1.plv0 === 1.U) || (io.mmu_config.crmd.plv === 3.U && io.mmu_config.dmw1.plv3 === 1.U))

    next_ex2.data               := ex1_data
    next_ex2.data.branch_mask   := ex1_data.branch_mask & clear_mask
    next_ex2.va                 := ex1_va
    next_ex2.src2               := src@@2_val
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
    next_ex2.crmd_pg            := io.mmu_config.crmd.pg
    next_ex2.crmd_da            := io.mmu_config.crmd.da
    next_ex2.crmd_plv           := io.mmu_config.crmd.plv
    next_ex2.crmd_datm          := io.mmu_config.crmd.datm

    //==========================================
    // EX2 Execution
    //==========================================
    val ex2_d = ex2_data.data
    val va    = ex2_data.va

    val tlbsrch_res = Cat(!ex2_data.tlb_found, 0.U(27.W), ex2_data.tlb_index)
    val tlbsrch_mask = Mux(ex2_data.tlb_found, "h8000000F".U(32.W), "h80000000".U(32.W))

    val tlb_pa = Mux(ex2_data.tlb_ps === 12.U, Cat(ex2_data.tlb_ppn, va(11, 0)), Cat(ex2_data.tlb_ppn(19, 9), va(20, 0)))
    val is_da = (ex2_data.crmd_da === 1.U) && (ex2_data.crmd_pg === 0.U)
    val pa = Mux(is_da, va, Mux(ex2_data.dmw_hit, ex2_data.dmw_pa, tlb_pa))

    val current_mat = Mux(is_da, ex2_data.crmd_datm, Mux(ex2_data.dmw_hit, ex2_data.dmw_mat, ex2_data.tlb_mat))
    val uncached = (current_mat === 0.U)

    val isWord = ex2_d.lsOp === LsOp.LD_W || ex2_d.lsOp === LsOp.ST_W
    val isHalf = ex2_d.lsOp === LsOp.LD_H || ex2_d.lsOp === LsOp.LD_HU || ex2_d.lsOp === LsOp.ST_H

    val is_load  = ex2_d.resFromMem
    val is_store = ex2_d.memWe
    //cacop
    val is_hit_inv = ex2_d.is_cacop && (ex2_d.cacop_op(4, 3) === 2.U)
    val is_ls_inst = is_load || is_store || is_hit_inv

    val ale = (is_load || is_store) && ((isWord && va(1, 0) =/= 0.U) || (isHalf && va(0) === 1.U))
    val is_mapped = (ex2_data.crmd_pg === 1.U) && (ex2_data.crmd_da === 0.U) && !ex2_data.dmw_hit
    val need_tlb  = is_mapped && is_ls_inst

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
    val stMaskB = "b0001".U(4.W) << va(1, 0)
    val stMaskH = Mux(va(1), "b1100".U(4.W), "b0011".U(4.W))
    val stMaskW = "b1111".U(4.W)
    val wstrb   = Mux(isWord, stMaskW, Mux(isHalf, stMaskH, stMaskB))

    val is_lsq_inst = (is_load || is_store || ex2_d.is_cacop) && ex2_active
    io.to_lsq.valid         := is_lsq_inst && !io.flush

    io.to_lsq.bits.lsqIdx   := ex2_d.lsq_idx
    io.to_lsq.bits.paddr    := Cat(Mux(ex2_d.is_cacop && (ex2_d.cacop_op(4, 3) =/= 2.U), va(31,12), pa(31,12)), va(11, 0))
    io.to_lsq.bits.size     := Mux(isWord, 2.U, Mux(isHalf, 1.U, 0.U))
    io.to_lsq.bits.uncached := uncached
    io.to_lsq.bits.wdata    := Mux(isWord, ex2_data.src2, Mux(isHalf, wdata_h, wdata_b))
    io.to_lsq.bits.wstrb    := Mux(ex2_d.memWe && !has_exc, wstrb, 0.U(4.W))
    io.to_lsq.bits.has_exc  := has_exc
    io.to_lsq.bits.ecode    := agu_ecode

    val out_data = WireDefault(ex2_d)
    out_data.ex_result      := Mux(ex2_data.is_tlbsrch, tlbsrch_res, va)
    out_data.aux_data       := Mux(ex2_data.is_tlbsrch, tlbsrch_mask, 0.U)
    out_data.hasException   := has_exc
    out_data.ecode          := Mux(ex2_d.hasException, ex2_d.ecode, agu_ecode)
    out_data.is_cacop       := ex2_d.is_cacop
    out_data.cacop_op       := ex2_d.cacop_op

    
    io.out.bits  := out_data
}
```


#### Short summary: 

empty definition using pc, found symbol in pc: 