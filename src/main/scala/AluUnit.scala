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

    val valid_reg = RegInit(false.B)
    val data_reg  = RegInit(0.U.asTypeOf(new PipelineData()))

    // 面具净化与爆破
    val current_is_killed = valid_reg && io.br_resolve_in.valid && io.br_resolve_in.mispredict && ((data_reg.branch_mask & (1.U(4.W) << io.br_resolve_in.tag)) =/= 0.U)
    val real_valid = valid_reg && !current_is_killed

    val ready_go = true.B
    val allow_in = !valid_reg || current_is_killed || (ready_go && io.out.ready)
    io.in.ready := allow_in

    val incoming_is_killed = io.in.valid && io.br_resolve_in.valid && io.br_resolve_in.mispredict && ((io.in.bits.branch_mask & (1.U(4.W) << io.br_resolve_in.tag)) =/= 0.U)
    val accepted_valid = io.in.valid && !incoming_is_killed

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

    io.out.valid := valid_reg && ready_go
    io.out.bits  := out_data
}