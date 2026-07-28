error id: 55688F87AAB60BED9F12F000ED73C673
file://<WORKSPACE>/src/main/scala/AluUnit.scala
### java.lang.AssertionError: assertion failed: file://<WORKSPACE>/src/main/scala/AluUnit.scala: 8677 >= 8667

occurred in the presentation compiler.



action parameters:
offset: 8677
uri: file://<WORKSPACE>/src/main/scala/AluUnit.scala
text:
```scala
package mycpu

import chisel3._
import chisel3.util._

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

        // 上一步刚加的 CSR 接口
        val csr_raddr = Output(UInt(14.W))
        val csr_rdata = Input(UInt(32.W))

        val bpu_update = Valid(new BpuUpdate())
    })

    val valid_reg = RegInit(false.B)
    val data_reg  = RegInit(0.U.asTypeOf(new PipelineData()))

    // ==========================================
    // ★ 剪断 1：ALU0 的自我免疫！
    // 只有 ALU0 会发广播，所以 ALU0 屋里的指令绝不可能被同拍击毙。
    // ==========================================
    val real_valid = valid_reg 

    // 门外判定：新来的指令是不是刚好处在错路上？
    val incoming_is_killed = io.in.valid && io.br_resolve_in.valid && io.br_resolve_in.mispredict && ((io.in.bits.branch_mask & (1.U(4.W) << io.br_resolve_in.tag)) =/= 0.U)
    val accepted_valid = io.in.valid && !incoming_is_killed

    val ready_go = true.B
    val allow_in = !valid_reg || (ready_go && io.out.ready)
    
    io.in.ready := allow_in

    when(io.flush) {
        valid_reg := false.B
    } .elsewhen(allow_in) {
        valid_reg := accepted_valid
    }

    when(io.in.valid && allow_in) { data_reg := io.in.bits }

    // 面具净化
    when(io.br_resolve_in.valid && !io.br_resolve_in.mispredict) {
        val clear_mask = ~(1.U(4.W) << io.br_resolve_in.tag)
        data_reg.branch_mask := Mux(io.in.valid && io.in.ready, io.in.bits.branch_mask, data_reg.branch_mask) & clear_mask
    }

    // ==========================================
    // ★ 剪断 2：彻底消灭旧时代的前递 Mux 环路！
    // ==========================================
    val src1_fwd = data_reg.src1_value
    val src2_fwd = data_reg.src2_value

    // ------------------------------------------
    // 分支验尸与单脉冲发射
    // ------------------------------------------
    val eq  = (src1_fwd === src2_fwd)
    val lt  = (src1_fwd.asSInt < src2_fwd.asSInt)
    val ltu = (src1_fwd < src2_fwd)

    val actual_taken = MuxLookup(data_reg.brType, false.B)(Seq(
        BrType.BEQ  -> eq,      BrType.BNE  -> !eq,     BrType.BLT  -> lt,
        BrType.BGE  -> !lt,     BrType.BLTU -> ltu,     BrType.BGEU -> !ltu,
        BrType.JIRL -> true.B,  BrType.B   -> true.B,   BrType.BL  -> true.B
    ))
    val br_base = Mux(data_reg.brType === BrType.JIRL, src1_fwd, data_reg.pc)
    val actual_target = br_base + data_reg.imm

    // ★ 真正的误预测判定：错过了跳、不该跳却跳了、跳错地址了，统统算失败！
    val mispredict = (actual_taken =/= data_reg.pred_taken) || (actual_taken && (actual_target =/= data_reg.pred_target))

    // ★ 恢复地址计算：如果猜错了，我该怎么收拾烂摊子？
    // 猜跳了但没跳 -> 回到 PC+4。真跳了 -> 去 actual_target
    val recovery_target = Mux(actual_taken, actual_target, data_reg.pc + 4.U)

    val br_broadcasted = RegInit(false.B)
    val do_br_resolve = real_valid && data_reg.is_branch && !data_reg.hasException && !br_broadcasted

    when(io.flush) {
        br_broadcasted := false.B
    } .elsewhen(allow_in) { 
        br_broadcasted := false.B
    } .elsewhen(do_br_resolve) {
        br_broadcasted := true.B
    }

    io.br_resolve.valid      := do_br_resolve
    io.br_resolve.mispredict := mispredict    // 告诉全军：被骗了！退档！
    io.br_resolve.tag        := data_reg.branch_tag

    io.branch_req := do_br_resolve && mispredict  // 只有猜错了，才真的冲刷流水线
    io.branch_pc  := recovery_target              // 送去顶层的恢复地址

    // ==========================================
    // ★ 提炼 BpuType 并上报给 BTB 进行反向训练
    // ==========================================
    val op = data_reg.inst(31, 26)
    val rd = data_reg.inst(4, 0)
    val rj = data_reg.inst(9, 5)
    val is_call   = (op === "b010101".U) || ((op === "b010011".U) && (rd === 1.U))
    val is_ret    = (op === "b010011".U) && (rj === 1.U) && (rd === 0.U)
    val is_uncond = (op === "b010100".U) || is_call || is_ret || (op === "b010011".U)
    val btype     = Mux(is_ret, BpuType.RET, Mux(is_call, BpuType.CALL, Mux(is_uncond, BpuType.UNCOND, BpuType.COND)))

    io.bpu_update.valid := do_br_resolve
    io.bpu_update.bits.pc := data_reg.pc
    io.bpu_update.bits.taken := actual_taken
    io.bpu_update.bits.target := actual_target
    io.bpu_update.bits.bpu_type := btype

    // ★ 新增：GShare 的核心训练数据
    io.bpu_update.bits.ghr        := data_reg.ghr
    io.bpu_update.bits.ras_tos    := data_reg.ras_tos // ★ 完璧归赵！
    io.bpu_update.bits.mispredict := mispredict

    // ------------------------------------------
    // ALU 核心计算
    // ------------------------------------------
    val alu_src1 = Mux(data_reg.src1IsPC, data_reg.pc, src1_fwd)
    val alu_src2 = Mux(data_reg.src2IsImm, data_reg.imm, Mux(data_reg.src2IsFour, 4.U, src2_fwd))

    val alu = Module(new ALU())
    alu.io.aluOp := data_reg.aluOp
    alu.io.src1  := alu_src1
    alu.io.src2  := alu_src2
    val alu_res  = alu.io.res

    // ------------------------------------------
    // CSR 与数据打包
    // ------------------------------------------
    val csr_mask = Mux(data_reg.src1_addr === 0.U, 0.U(32.W),
                   Mux(data_reg.src1_addr === 1.U, "hFFFFFFFF".U(32.W),
                   src1_fwd))
    
    io.csr_raddr := data_reg.csrNum
    val final_ex_result = Mux(data_reg.rdtimel, io.timer_in(31, 0),
                          Mux(data_reg.rdtimeh, io.timer_in(63, 32),
                          Mux(data_reg.isCsr, io.csr_rdata, alu_res)))

    val aux_data = Mux(data_reg.isCsr, csr_mask, src2_fwd)

    val out_data = WireDefault(data_reg) 
    out_data.ex_result := final_ex_result
    out_data.aux_data  := aux_data

    io.out.valid := real_valid && ready_go
    io.out.bits  := out_data
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
                valid_reg := false.B // 被击毙，下拍化为气泡
            }
        }
    }

    when(io.flush) {
        valid_reg := false.B
    } .elsewhen(allow_in) {
        valid_reg := accepted_valid
    } .elsewhen(current_is_killed) {
        valid_reg := false.B
    }
    when(io.in.valid && allow_in) { data_reg := io.in.bits }

    when(io.br_resolve_in.valid && !io.br_resolve_in.mispredict) {
        val clear_mask = ~(1.U(4.W) << io.br_resolve_in.tag)
        data_reg.branch_mask := Mux(io.in.valid && io.in.ready, io.in.bits.branch_mask, data_reg.branch_mask) & clear_mask
    }

    val src1_val = data_reg.src1_value
    val src2_val = data_reg.src2_value
    val alu_src1 = Mux(data_reg.src1IsPC, data_reg.pc, src1_val)
    val alu_src2 = Mux(data_reg.src2IsImm, data_reg.imm, Mux(data_reg.src2IsFour, 4.U, src2_val))

    // 核心 ALU 计算 (无分支逻辑)
    val alu = Module(new ALU())
    alu.io.aluOp := data_reg.aluOp
    alu.io.src1  := alu_src1
    alu.io.src2  := alu_src2
    val alu_res  = alu.io.res

    val out_data = WireDefault(data_reg) 
    out_data.ex_result := alu_res // Simple ALU 只有纯算术结果

    io.out.valid := real_valid && ready_go
    io.out.bits  := out_data
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

java.lang.AssertionError: assertion failed: file://<WORKSPACE>/src/main/scala/AluUnit.scala: 8677 >= 8667