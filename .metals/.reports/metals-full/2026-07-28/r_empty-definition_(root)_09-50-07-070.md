error id: file://<WORKSPACE>/src/main/scala/AluUnit.scala:
file://<WORKSPACE>/src/main/scala/AluUnit.scala
empty definition using pc, found symbol in pc: 
empty definition using semanticdb
empty definition using fallback
non-local guesses:
	 -chisel3/real_valid.
	 -chisel3/real_valid#
	 -chisel3/real_valid().
	 -chisel3/util/real_valid.
	 -chisel3/util/real_valid#
	 -chisel3/util/real_valid().
	 -real_valid.
	 -real_valid#
	 -real_valid().
	 -scala/Predef.real_valid.
	 -scala/Predef.real_valid#
	 -scala/Predef.real_valid().
offset: 3268
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

    //==========================================
    // ALU
    //==========================================
    val src1_val = data_reg.src1_value
    val src2_val = data_reg.src2_value

    val eq  = (src1_val === src2_val)
    val lt  = (src1_val.asSInt < src2_val.asSInt)
    val ltu = (src1_val < src2_val)

    val branch_actual_taken = MuxLookup(data_reg.brType, false.B)(Seq(
        BrType.BEQ  -> eq,      BrType.BNE  -> !eq,     BrType.BLT  -> lt,
        BrType.BGE  -> !lt,     BrType.BLTU -> ltu,     BrType.BGEU -> !ltu,
        BrType.JIRL -> true.B,  BrType.B   -> true.B,   BrType.BL  -> true.B
    ))

    val br_base_pc = Mux(data_reg.brType === BrType.JIRL, src1_val, data_reg.pc)

    val actual_target = br_base + data_reg.imm

    // ★ 真正的误预测判定：错过了跳、不该跳却跳了、跳错地址了，统统算失败！
    val mispredict = (actual_taken =/= data_reg.pred_taken) || (actual_taken && (actual_target =/= data_reg.pred_target))

    // ★ 恢复地址计算：如果猜错了，我该怎么收拾烂摊子？
    // 猜跳了但没跳 -> 回到 PC+4。真跳了 -> 去 actual_target
    val recovery_target = Mux(actual_taken, actual_target, data_reg.pc + 4.U)

    val br_broadcasted = RegInit(false.B)
    val do_br_resolve = real_val@@id && data_reg.is_branch && !data_reg.hasException && !br_broadcasted

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


```


#### Short summary: 

empty definition using pc, found symbol in pc: 