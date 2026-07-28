error id: file://<WORKSPACE>/src/main/scala/AguUnit.scala:
file://<WORKSPACE>/src/main/scala/AguUnit.scala
empty definition using pc, found symbol in pc: 
empty definition using semanticdb
empty definition using fallback
non-local guesses:
	 -chisel3/is_killed.
	 -chisel3/is_killed#
	 -chisel3/is_killed().
	 -chisel3/util/is_killed.
	 -chisel3/util/is_killed#
	 -chisel3/util/is_killed().
	 -is_killed.
	 -is_killed#
	 -is_killed().
	 -scala/Predef.is_killed.
	 -scala/Predef.is_killed#
	 -scala/Predef.is_killed().
offset: 2401
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

class AguEx2State extends Bundle {
    val data       = new PipelineData()
    val va         = UInt(32.W)
    val src2       = UInt(32.W)
    val is_tlbsrch = Bool()
    val dmw_hit    = Bool()
    val dmw_pa     = UInt(32.W)
    val dmw_mat    = UInt(2.W)
    
    // TLB 快照
    val tlb_found  = Bool()
    val tlb_index  = UInt(4.W)
    val tlb_ppn    = UInt(20.W)
    val tlb_ps     = UInt(6.W)
    val tlb_plv    = UInt(2.W)
    val tlb_mat    = UInt(2.W)
    val tlb_d      = Bool()
    val tlb_v      = Bool()
    
    // MMU 配置快照
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


    // ---------------------------------------------------------------------
    // 2. 极其清爽的流水线寄存器 (总共就 4 个变量)
    // ---------------------------------------------------------------------
    val ex1_valid = RegInit(false.B)
    val ex1_data  = RegInit(0.U.asTypeOf(new PipelineData()))

    val ex2_valid = RegInit(false.B)
    val ex2_state = RegInit(0.U.asTypeOf(new AguEx2State()))

    // 真实存活状态：我的寄存器是有效的，且我当拍没被击杀
    val ex1_real_valid = ex1_valid && !is_k@@illed(ex1_data.branch_mask)
    val ex2_real_valid = ex2_valid && !is_killed(ex2_state.data.branch_mask)

    // 反向传压：允许进水的条件
    val ex2_allow_in = !ex2_valid || !ex2_real_valid || io.out.ready
    val ex1_allow_in = !ex1_valid || !ex1_real_valid || ex2_allow_in
    
    io.in.ready := ex1_allow_in

    // =====================================================================
    // ★ 核心重构：AGU 内部两级流水线状态机
    // =====================================================================
    // --- EX1 级寄存器 (接收 IQ 输入) ---
    val ex1_valid = RegInit(false.B)
    val ex1_data  = RegInit(0.U.asTypeOf(new PipelineData()))

    // --- EX2 级寄存器 (级间暂存) ---
    val ex2_valid = RegInit(false.B)
    val ex2_data  = RegInit(0.U.asTypeOf(new PipelineData()))
    val ex2_va    = RegInit(0.U(32.W))
    val ex2_src2  = RegInit(0.U(32.W)) // 暂存源操作数2，供 EX2 阶段生成 wdata 使用
    
    // 提前在 EX1 算好的 DMW 结果
    val ex2_is_tlbsrch = RegInit(false.B)
    val ex2_dmw_hit    = RegInit(false.B)
    val ex2_dmw_pa     = RegInit(0.U(32.W))
    val ex2_dmw_mat    = RegInit(0.U(2.W))

    // ★ 关键点：将 EX1 阶段查出的 TLB 结果和当时的 MMU 配置打一拍，防止被后续指令污染！
    val ex2_tlb_found = RegInit(false.B)
    val ex2_tlb_index = RegInit(0.U(4.W))
    val ex2_tlb_ppn   = RegInit(0.U(20.W))
    val ex2_tlb_ps    = RegInit(0.U(6.W))
    val ex2_tlb_plv   = RegInit(0.U(2.W))
    val ex2_tlb_mat   = RegInit(0.U(2.W))
    val ex2_tlb_d     = RegInit(false.B)
    val ex2_tlb_v     = RegInit(false.B)

    val ex2_crmd_pg   = RegInit(0.U(1.W))
    val ex2_crmd_da   = RegInit(0.U(1.W))
    val ex2_crmd_plv  = RegInit(0.U(2.W))
    val ex2_crmd_datm = RegInit(0.U(2.W))

    // ==========================================
    // 1. 全局流水线握手与分支冲刷 (Handshake & Flush)
    // ==========================================
    // 错路判定：动态检查两级流水线中的指令是否被击毙
    val ex1_killed = ex1_valid && io.br_resolve_in.valid && io.br_resolve_in.mispredict && ((ex1_data.branch_mask & (1.U(4.W) << io.br_resolve_in.tag)) =/= 0.U)
    val ex1_real_valid = ex1_valid && !ex1_killed

    val ex2_killed = ex2_valid && io.br_resolve_in.valid && io.br_resolve_in.mispredict && ((ex2_data.branch_mask & (1.U(4.W) << io.br_resolve_in.tag)) =/= 0.U)
    val ex2_real_valid = ex2_valid && !ex2_killed

    // 握手逻辑：反向传压
    val ex2_ready_go = true.B // EX2 内部无阻塞，算完当拍走
    val ex2_allow_in = !ex2_valid || ex2_killed || (ex2_ready_go && io.out.ready)
    
    val ex1_ready_go = true.B
    val ex1_allow_in = !ex1_valid || ex1_killed || (ex1_ready_go && ex2_allow_in)

    val incoming_is_killed = io.in.valid && io.br_resolve_in.valid && io.br_resolve_in.mispredict && ((io.in.bits.branch_mask & (1.U(4.W) << io.br_resolve_in.tag)) =/= 0.U)
    val accepted_valid = io.in.valid && !incoming_is_killed

    io.in.ready := ex1_allow_in

    // 状态机流转
    when(io.flush) {
        ex1_valid := false.B
        ex2_valid := false.B
    } .otherwise {
        // EX1 流转
        when(ex1_allow_in) {
            ex1_valid := accepted_valid
        } .elsewhen(ex1_killed) {
            ex1_valid := false.B
        }
        
        // EX2 流转
        when(ex2_allow_in) {
            ex2_valid := ex1_real_valid
        } .elsewhen(ex2_killed) {
            ex2_valid := false.B
        }
    }

    when(io.in.valid && ex1_allow_in) { ex1_data := io.in.bits }

    // 面具净化：分支猜对时，洗掉对应的 Tag
    when(io.br_resolve_in.valid && !io.br_resolve_in.mispredict) {
        val clear_mask = ~(1.U(4.W) << io.br_resolve_in.tag)
        ex1_data.branch_mask := Mux(io.in.valid && ex1_allow_in, io.in.bits.branch_mask, ex1_data.branch_mask) & clear_mask
        ex2_data.branch_mask := Mux(ex1_real_valid && ex2_allow_in, ex1_data.branch_mask, ex2_data.branch_mask) & clear_mask
    }

    // =====================================================================
    // [STAGE 1: EX1] 算址与跨模块 TLB 查表
    // =====================================================================
    val src1_fwd = ex1_data.src1_value
    val src2_fwd = ex1_data.src2_value
    val alu_src1 = Mux(ex1_data.src1IsPC, ex1_data.pc, src1_fwd)
    val alu_src2 = Mux(ex1_data.src2IsImm, ex1_data.imm, Mux(ex1_data.src2IsFour, 4.U, src2_fwd))
    
    // ★ 关键路径起点：算址
    val va = alu_src1 + alu_src2 

    val is_tlbsrch = ex1_data.tlbOp === TlbOp.SRCH
    val is_invtlb  = ex1_data.tlbOp === TlbOp.INV

    // 驱动 TLB 进行组合逻辑查表
    io.tlb_s1_vppn := Mux(is_invtlb,  src2_fwd(31, 13), Mux(is_tlbsrch, io.mmu_config.tlbehi.vppn, va(31, 13)))
    io.tlb_s1_va_bit12 := va(12)
    io.tlb_s1_asid := Mux(is_invtlb,  src1_fwd(9, 0), io.mmu_config.asid.asid)
    
    // invtlb 操作只在 EX1 触发，防止引发组合逻辑冲突
    io.invtlb_valid := is_invtlb && ex1_real_valid && !ex1_data.hasException
    io.invtlb_op    := ex1_data.invtlb_op

    // 提前判断 DMW
    val dmw0_hit = (io.mmu_config.crmd.pg === 1.U) && (io.mmu_config.crmd.da === 0.U) && (va(31, 29) === io.mmu_config.dmw0.vseg) &&
               ((io.mmu_config.crmd.plv === 0.U && io.mmu_config.dmw0.plv0 === 1.U) || (io.mmu_config.crmd.plv === 3.U && io.mmu_config.dmw0.plv3 === 1.U))
    val dmw1_hit = (io.mmu_config.crmd.pg === 1.U) && (io.mmu_config.crmd.da === 0.U) && (va(31, 29) === io.mmu_config.dmw1.vseg) &&
                ((io.mmu_config.crmd.plv === 0.U && io.mmu_config.dmw1.plv0 === 1.U) || (io.mmu_config.crmd.plv === 3.U && io.mmu_config.dmw1.plv3 === 1.U))
    val dmw_hit = dmw0_hit || dmw1_hit
    val dmw_pa  = Mux(dmw0_hit, Cat(io.mmu_config.dmw0.pseg, va(28, 0)), Cat(io.mmu_config.dmw1.pseg, va(28, 0)))
    val dmw_mat = Mux(dmw0_hit, io.mmu_config.dmw0.mat, io.mmu_config.dmw1.mat)

    // 把 EX1 的计算结果和外部 TLB 查表结果，全部锁进 EX2 寄存器
    when(ex1_real_valid && ex2_allow_in) {
        ex2_data := ex1_data
        ex2_va   := va
        ex2_src2 := src2_fwd
        ex2_is_tlbsrch := is_tlbsrch
        ex2_dmw_hit    := dmw_hit
        ex2_dmw_pa     := dmw_pa
        ex2_dmw_mat    := dmw_mat

        // ★ 切断世纪大路径！将 TLB 返回结果打拍
        ex2_tlb_found := io.tlb_s1_found
        ex2_tlb_index := io.tlb_s1_index
        ex2_tlb_ppn   := io.tlb_s1_ppn
        ex2_tlb_ps    := io.tlb_s1_ps
        ex2_tlb_plv   := io.tlb_s1_plv
        ex2_tlb_mat   := io.tlb_s1_mat
        ex2_tlb_d     := io.tlb_s1_d
        ex2_tlb_v     := io.tlb_s1_v

        ex2_crmd_pg   := io.mmu_config.crmd.pg
        ex2_crmd_da   := io.mmu_config.crmd.da
        ex2_crmd_plv  := io.mmu_config.crmd.plv
        ex2_crmd_datm := io.mmu_config.crmd.datm
    }

    // =====================================================================
    // [STAGE 2: EX2] 权限判定、异常定案与接口输出
    // =====================================================================
    val tlbsrch_res = Cat(!ex2_tlb_found, 0.U(27.W), ex2_tlb_index)
    val tlbsrch_mask = Mux(ex2_tlb_found, "h8000000F".U(32.W), "h80000000".U(32.W))

    val tlb_pa = Mux(ex2_tlb_ps === 12.U, Cat(ex2_tlb_ppn, ex2_va(11, 0)), Cat(ex2_tlb_ppn(19, 9), ex2_va(20, 0)))
    val pa = Mux((ex2_crmd_da === 1.U) && (ex2_crmd_pg === 0.U), ex2_va,
         Mux(ex2_dmw_hit, ex2_dmw_pa, Mux(ex2_tlb_found && ex2_tlb_v, tlb_pa, ex2_va)))
         
    val cacop_is_hit_inval = ex2_data.is_cacop && (ex2_data.cacop_op(4, 3) === 2.U)
    val cacop_is_index     = ex2_data.is_cacop && (ex2_data.cacop_op(4, 3) =/= 2.U)

    val current_mat = Mux((ex2_crmd_da === 1.U) && (ex2_crmd_pg === 0.U), ex2_crmd_datm, Mux(ex2_dmw_hit, ex2_dmw_mat, ex2_tlb_mat))
    io.data_uncached := (current_mat === 0.U)

    val isWord = ex2_data.lsOp === LsOp.LD_W || ex2_data.lsOp === LsOp.ST_W
    val isHalf = ex2_data.lsOp === LsOp.LD_H || ex2_data.lsOp === LsOp.LD_HU || ex2_data.lsOp === LsOp.ST_H
    
    val ale = (ex2_data.resFromMem || ex2_data.memWe) && ex2_real_valid && 
              ((isWord && (ex2_va(1, 0) =/= 0.U)) || (isHalf && ex2_va(0) === 1.U))

    val early_is_load  = ex2_data.resFromMem && ex2_real_valid
    val early_is_store = ex2_data.memWe && ex2_real_valid
    val early_hit_inv  = cacop_is_hit_inval && ex2_real_valid
    val early_is_ls    = early_is_load || early_is_store || early_hit_inv
    
    val is_mapped = (ex2_crmd_pg === 1.U) && (ex2_crmd_da === 0.U) && !ex2_dmw_hit
    val early_is_mapped = is_mapped && early_is_ls

    val tlb_f = ex2_tlb_found
    val tlb_v = ex2_tlb_v
    val tlb_d = ex2_tlb_d
    val priv_fault = (ex2_crmd_plv === 3.U) && (ex2_tlb_plv === 0.U)

    // ★ 关键路径终点：由于前面有了寄存器隔离，这串庞大的 Ecode LUT 树获得了长达整整一拍的时间进行从容布线！
    val raw_tlb_code = Mux(!tlb_f,                                              "h3F".U(6.W), 0.U) |
                       Mux(tlb_f && tlb_v && priv_fault,                        "h07".U(6.W), 0.U) |
                       Mux(tlb_f && !tlb_v && (early_is_load || early_hit_inv), "h01".U(6.W), 0.U) |
                       Mux(tlb_f && !tlb_v && early_is_store,                   "h02".U(6.W), 0.U) |
                       Mux(tlb_f && tlb_v && !priv_fault && !tlb_d && early_is_store, "h04".U(6.W), 0.U)

    val mmu_code = Mux(early_is_mapped, raw_tlb_code, 0.U)
    val final_ecode = Mux(ale, "h09".U(6.W), mmu_code)
    val final_has_exc = ex2_data.hasException || ale || (early_is_mapped && raw_tlb_code =/= 0.U)

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
    val is_doing_cacop = ex2_real_valid && ex2_data.is_cacop && !final_has_exc
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