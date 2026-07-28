error id: 55688F87AAB60BED9F12F000ED73C673
file://<WORKSPACE>/src/main/scala/AguUnit.scala
### java.lang.AssertionError: assertion failed: file://<WORKSPACE>/src/main/scala/AguUnit.scala: 13177 >= 13106

occurred in the presentation compiler.



action parameters:
offset: 13177
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
    // ---------------------------------------------------------------------
    // 3. EX1 组合逻辑 (提前打包下一拍的 EX2 状态)
    // ---------------------------------------------------------------------
    val src1_val = ex1_data.src1_value
    val src2_val = ex1_data.src2_value
    val alu_src1 = Mux(ex1_data.src1IsPC, ex1_data.pc, src1_val)
    val alu_src2 = Mux(ex1_data.src2IsImm, ex1_data.imm, Mux(ex1_data.src2IsFour, 4.U, src2_val))
    val ex1_va   = alu_src1 + alu_src2 

    val is_tlbsrch = ex1_data.tlbOp === TlbOp.SRCH
    val is_invtlb  = ex1_data.tlbOp === TlbOp.INV

    // 连向外部 TLB
    io.tlb_port.vppn     := Mux(is_invtlb, src2_val(31, 13), Mux(is_tlbsrch, io.mmu_config.tlbehi.vppn, ex1_va(31, 13)))
    io.tlb_port.va_bit12 := ex1_va(12)
    io.tlb_port.asid     := Mux(is_invtlb, src1_fwd(9, 0), io.mmu_config.asid.asid)
    
    io.invtlb_valid := is_invtlb && ex1_active && !ex1_data.hasException
    io.invtlb_op    := ex1_data.invtlb_op

    // DMW 计算
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
    next_ex2.src2               := src2_fwd
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
}@@
```


presentation compiler configuration:
Scala version: 2.13.18
Classpath:
<WORKSPACE>/.bloop/root/bloop-bsp-clients-classes/classes-Metals-hwlVcpKzSXqmVzMUSf4wNw== [exists ], <HOME>/Library/Caches/Coursier/v1/https/repo1.maven.org/maven2/com/sourcegraph/semanticdb-javac/0.12.3/semanticdb-javac-0.12.3.jar [exists ], <HOME>/Library/Caches/Coursier/v1/https/repo1.maven.org/maven2/org/scala-lang/scala-library/2.13.18/scala-library-2.13.18.jar [exists ], <HOME>/Library/Caches/Coursier/v1/https/repo1.maven.org/maven2/org/chipsalliance/chisel_2.13/7.7.0/chisel_2.13-7.7.0.jar [exists ], <HOME>/Library/Caches/Coursier/v1/https/repo1.maven.org/maven2/com/github/scopt/scopt_2.13/4.1.0/scopt_2.13-4.1.0.jar [exists ], <HOME>/Library/Caches/Coursier/v1/https/repo1.maven.org/maven2/org/apache/commons/commons-text/1.15.0/commons-text-1.15.0.jar [exists ], <HOME>/Library/Caches/Coursier/v1/https/repo1.maven.org/maven2/com/lihaoyi/os-lib_2.13/0.10.7/os-lib_2.13-0.10.7.jar [exists ], <HOME>/Library/Caches/Coursier/v1/https/repo1.maven.org/maven2/io/github/json4s/json4s-native_2.13/4.1.0/json4s-native_2.13-4.1.0.jar [exists ], <HOME>/Library/Caches/Coursier/v1/https/repo1.maven.org/maven2/io/github/alexarchambault/data-class_2.13/0.2.7/data-class_2.13-0.2.7.jar [exists ], <HOME>/Library/Caches/Coursier/v1/https/repo1.maven.org/maven2/org/scala-lang/scala-reflect/2.13.18/scala-reflect-2.13.18.jar [exists ], <HOME>/Library/Caches/Coursier/v1/https/repo1.maven.org/maven2/com/lihaoyi/upickle_2.13/3.3.1/upickle_2.13-3.3.1.jar [exists ], <HOME>/Library/Caches/Coursier/v1/https/repo1.maven.org/maven2/org/chipsalliance/firtool-resolver_2.13/2.0.1/firtool-resolver_2.13-2.0.1.jar [exists ], <HOME>/Library/Caches/Coursier/v1/https/repo1.maven.org/maven2/org/apache/commons/commons-lang3/3.20.0/commons-lang3-3.20.0.jar [exists ], <HOME>/Library/Caches/Coursier/v1/https/repo1.maven.org/maven2/com/lihaoyi/geny_2.13/1.1.1/geny_2.13-1.1.1.jar [exists ], <HOME>/Library/Caches/Coursier/v1/https/repo1.maven.org/maven2/io/github/json4s/json4s-core_2.13/4.1.0/json4s-core_2.13-4.1.0.jar [exists ], <HOME>/Library/Caches/Coursier/v1/https/repo1.maven.org/maven2/io/github/json4s/json4s-native-core_2.13/4.1.0/json4s-native-core_2.13-4.1.0.jar [exists ], <HOME>/Library/Caches/Coursier/v1/https/repo1.maven.org/maven2/com/lihaoyi/ujson_2.13/3.3.1/ujson_2.13-3.3.1.jar [exists ], <HOME>/Library/Caches/Coursier/v1/https/repo1.maven.org/maven2/com/lihaoyi/upack_2.13/3.3.1/upack_2.13-3.3.1.jar [exists ], <HOME>/Library/Caches/Coursier/v1/https/repo1.maven.org/maven2/com/lihaoyi/upickle-implicits_2.13/3.3.1/upickle-implicits_2.13-3.3.1.jar [exists ], <HOME>/Library/Caches/Coursier/v1/https/repo1.maven.org/maven2/org/scala-lang/modules/scala-xml_2.13/2.2.0/scala-xml_2.13-2.2.0.jar [exists ], <HOME>/Library/Caches/Coursier/v1/https/repo1.maven.org/maven2/org/scala-lang/modules/scala-collection-compat_2.13/2.11.0/scala-collection-compat_2.13-2.11.0.jar [exists ], <HOME>/Library/Caches/Coursier/v1/https/repo1.maven.org/maven2/io/github/json4s/json4s-ast_2.13/4.1.0/json4s-ast_2.13-4.1.0.jar [exists ], <HOME>/Library/Caches/Coursier/v1/https/repo1.maven.org/maven2/io/github/json4s/json4s-scalap_2.13/4.1.0/json4s-scalap_2.13-4.1.0.jar [exists ], <HOME>/Library/Caches/Coursier/v1/https/repo1.maven.org/maven2/com/lihaoyi/upickle-core_2.13/3.3.1/upickle-core_2.13-3.3.1.jar [exists ]
Options:
-language:reflectiveCalls -deprecation -feature -Xcheckinit -Ymacro-annotations -Yrangepos -Xplugin-require:semanticdb




#### Error stacktrace:

```
scala.reflect.internal.util.SourceFile.position(SourceFile.scala:34)
	scala.tools.nsc.CompilationUnits$CompilationUnit.position(CompilationUnits.scala:136)
	scala.meta.internal.pc.AutoImportsProvider.autoImports(AutoImportsProvider.scala:28)
	scala.meta.internal.pc.ScalaPresentationCompiler.$anonfun$autoImports$1(ScalaPresentationCompiler.scala:399)
	scala.meta.internal.pc.CompilerAccess.retryWithCleanCompiler(CompilerAccess.scala:182)
	scala.meta.internal.pc.CompilerAccess.$anonfun$withSharedCompiler$1(CompilerAccess.scala:155)
	scala.Option.map(Option.scala:242)
	scala.meta.internal.pc.CompilerAccess.withSharedCompiler(CompilerAccess.scala:154)
	scala.meta.internal.pc.CompilerAccess.$anonfun$withInterruptableCompiler$1(CompilerAccess.scala:92)
	scala.meta.internal.pc.CompilerAccess.$anonfun$onCompilerJobQueue$1(CompilerAccess.scala:209)
	scala.meta.internal.pc.CompilerJobQueue$Job.run(CompilerJobQueue.scala:152)
	java.base/java.util.concurrent.ThreadPoolExecutor.runWorker(ThreadPoolExecutor.java:1136)
	java.base/java.util.concurrent.ThreadPoolExecutor$Worker.run(ThreadPoolExecutor.java:635)
	java.base/java.lang.Thread.run(Thread.java:840)
```
#### Short summary: 

java.lang.AssertionError: assertion failed: file://<WORKSPACE>/src/main/scala/AguUnit.scala: 13177 >= 13106