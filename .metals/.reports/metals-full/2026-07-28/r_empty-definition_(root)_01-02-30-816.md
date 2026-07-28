error id: file://<WORKSPACE>/src/main/scala/AguUnit.scala:mycpu/AguUnit#ex2_real_valid.
file://<WORKSPACE>/src/main/scala/AguUnit.scala
empty definition using pc, found symbol in pc: 
empty definition using semanticdb
empty definition using fallback
non-local guesses:
	 -chisel3/ex2_real_valid.
	 -chisel3/ex2_real_valid#
	 -chisel3/ex2_real_valid().
	 -chisel3/util/ex2_real_valid.
	 -chisel3/util/ex2_real_valid#
	 -chisel3/util/ex2_real_valid().
	 -ex2_real_valid.
	 -ex2_real_valid#
	 -ex2_real_valid().
	 -scala/Predef.ex2_real_valid.
	 -scala/Predef.ex2_real_valid#
	 -scala/Predef.ex2_real_valid().
offset: 9794
uri: file://<WORKSPACE>/src/main/scala/AguUnit.scala
text:
```scala
package mycpu

import chisel3._
import chisel3.util._

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
        val in          = Flipped(Decoupled(new PipelineData()))
        val out         = Decoupled(new PipelineData())
        val to_lsq      = Valid(new Agu2Lsq())

        val flush       = Input(Bool())
        val br_resolve_in = Input(new BranchResolve())

        val mmu_config      = Input(new MmuConfig())
        val tlb_port      = new TlbSearchPort()

        val invtlb_valid = Output(Bool())
        val invtlb_op    = Output(UInt(5.W))
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
    io.in.ready := ex1_ready

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
            ex2_valid := ex1_active
            ex2_data  := next_ex2
        } .otherwise {
            ex2_data.data.branch_mask := ex2_data.data.branch_mask & clear_mask
        }
    }
    //==========================================
    // EX1 Execution
    //==========================================
    val src1_val = ex1_data.src1_value
    val src2_val = ex1_data.src2_value
    val alu_src1 = Mux(ex1_data.src1IsPC, ex1_data.pc, src1_val)
    val alu_src2 = Mux(ex1_data.src2IsImm, ex1_data.imm, Mux(ex1_data.src2IsFour, 4.U, src2_val))
    val ex1_va   = alu_src1 + alu_src2 

    val is_tlbsrch = ex1_data.tlbOp === TlbOp.SRCH
    val is_invtlb  = ex1_data.tlbOp === TlbOp.INV

    io.tlb_port.vppn     := Mux(is_invtlb, src2_val(31, 13), Mux(is_tlbsrch, io.mmu_config.tlbehi.vppn, ex1_va(31, 13)))
    io.tlb_port.va_bit12 := ex1_va(12)
    io.tlb_port.asid     := Mux(is_invtlb, src1_val(9, 0), io.mmu_config.asid.asid)
    
    io.invtlb_valid := is_invtlb && ex1_active && !ex1_data.hasException
    io.invtlb_op    := ex1_data.invtlb_op

    val pg = io.mmu_config.crmd.pg === 1.U
    val da = io.mmu_config.crmd.da === 0.U
    val dmw0_hit = pg && da && (ex1_va(31, 29) === io.mmu_config.dmw0.vseg) &&
                  ((io.mmu_config.crmd.plv === 0.U && io.mmu_config.dmw0.plv0 === 1.U) || (io.mmu_config.crmd.plv === 3.U && io.mmu_config.dmw0.plv3 === 1.U))
    val dmw1_hit = pg && da && (ex1_va(31, 29) === io.mmu_config.dmw1.vseg) &&
                  ((io.mmu_config.crmd.plv === 0.U && io.mmu_config.dmw1.plv0 === 1.U) || (io.mmu_config.crmd.plv === 3.U && io.mmu_config.dmw1.plv3 === 1.U))

    val next_ex2 = Wire(new AguEx2Data())
    next_ex2.data               := ex1_data
    next_ex2.data.branch_mask   := ex1_data.branch_mask & clear_mask
    next_ex2.va                 := ex1_va
    next_ex2.src2               := src2_val
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
         
    val cacop_is_hit_inval = ex2_d.is_cacop && (ex2_d.cacop_op(4, 3) === 2.U)
    val cacop_is_index     = ex2_d.is_cacop && (ex2_d.cacop_op(4, 3) =/= 2.U)

    val current_mat = Mux((ex2_data.crmd_da === 1.U) && (ex2_data.crmd_pg === 0.U), ex2_data.crmd_datm, 
                      Mux(ex2_data.dmw_hit, ex2_data.dmw_mat, ex2_data.tlb_mat))
    val uncached = (current_mat === 0.U)

    val isWord = ex2_d.lsOp === LsOp.LD_W || ex2_d.lsOp === LsOp.ST_W
    val isHalf = ex2_d.lsOp === LsOp.LD_H || ex2_d.lsOp === LsOp.LD_HU || ex2_d.lsOp === LsOp.ST_H
    
    val ale = (ex2_d.resFromMem || ex2_d.memWe) && ex2_active && 
              ((isWord && (va(1, 0) =/= 0.U)) || (isHalf && va(0) === 1.U))

    val early_is_load  = ex2_d.resFromMem && ex2_active
    val early_is_store = ex2_d.memWe && ex2_active
    val early_hit_inv  = cacop_is_hit_inval && ex2_active
    val early_is_ls    = early_is_load || early_is_store || early_hit_inv
    
    val is_mapped = (ex2_data.crmd_pg === 1.U) && (ex2_data.crmd_da === 0.U) && !ex2_data.dmw_hit
    val early_is_mapped = is_mapped && early_is_ls

    val tlb_f = ex2_data.tlb_found
    val tlb_v = ex2_data.tlb_v
    val tlb_d = ex2_data.tlb_d
    val priv_fault = (ex2_data.crmd_plv === 3.U) && (ex2_data.tlb_plv === 0.U)

    val raw_tlb_code = Mux(!tlb_f,                                              "h3F".U(6.W), 0.U) |
                       Mux(tlb_f && tlb_v && priv_fault,                        "h07".U(6.W), 0.U) |
                       Mux(tlb_f && !tlb_v && (early_is_load || early_hit_inv), "h01".U(6.W), 0.U) |
                       Mux(tlb_f && !tlb_v && early_is_store,                   "h02".U(6.W), 0.U) |
                       Mux(tlb_f && tlb_v && !priv_fault && !tlb_d && early_is_store, "h04".U(6.W), 0.U)

    val mmu_code = Mux(early_is_mapped, raw_tlb_code, 0.U)
    val final_ecode = Mux(ale, "h09".U(6.W), mmu_code)
    val final_has_exc = ex2_d.hasException || ale || (early_is_mapped && raw_tlb_code =/= 0.U)

    // 访存信号输出
    
    io.data_sram.req   := is_mem && !io.flush
    io.data_sram.wr    := ex2_data.memWe
    io.data_sram.size  := Mux(isWord, 2.U, Mux(isHalf, 1.U, 0.U))
    
    val stMaskB = "b0001".U(4.W) << ex2_va(1, 0)
    val stMaskH = Mux(ex2_va(1), "b1100".U(4.W), "b0011".U(4.W))
    val stMaskW = "b1111".U(4.W)
    val base_wstrb = Mux(isWord, stMaskW, Mux(isHalf, stMaskH, stMaskB))
    io.data_sram.wstrb := Mux(ex2_data.memWe && ex2_real_valid && !io.flush, base_wstrb, 0.U(4.W))
    
    val final_pa_high = Mux(cacop_is_index, ex2_va(31,12), pa(31,12))
    io.data_sram.addr := Cat(final_pa_high, ex2_va(11, 0))

    // 写数据拼装 (利用 EX2 寄存器中的 src2_fwd)
    val wdata_b = Fill(4, ex2_src2(7, 0))
    val wdata_h = Fill(2, ex2_src2(15, 0))
    io.data_sram.wdata := Mux(isWord, ex2_src2, Mux(isHalf, wdata_h, wdata_b))

    // 组装发往后端的 out_data
    val out_data = WireDefault(ex2_data)
    out_data.ex_result := Mux(ex2_is_tlbsrch, tlbsrch_res, ex2_va)
    out_data.aux_data  := Mux(ex2_is_tlbsrch, tlbsrch_mask, 0.U)
    out_data.hasException := final_has_exc
    out_data.ecode := Mux(ex2_data.hasException, ex2_data.ecode, final_ecode)
    out_data.is_cacop  := Mux(ex2_real_valid, ex2_data.is_cacop, false.B)
    out_data.cacop_op  := Mux(ex2_real_valid, ex2_data.cacop_op, 0.U(5.W))

    // 透传 CACOP 指令给 LSQ 和 Cache
    val is_doing_cacop = @@ex2_real_valid && ex2_data.is_cacop && !final_has_exc
    io.cacop_en := is_doing_cacop
    io.cacop_op := ex2_data.cacop_op(4, 3) 
    io.cacop_is_icache := is_doing_cacop && (ex2_data.cacop_op(2, 0) === 0.U)

    // 输出级握手
    io.out.valid := ex2_real_valid
    io.out.bits  := out_data





    val is_mem = (ex2_data.resFromMem || ex2_data.memWe || ex2_data.is_cacop) && ex2_real_valid
    io.to_lsq.valid := is_mem && !io.flush
}
```


#### Short summary: 

empty definition using pc, found symbol in pc: 