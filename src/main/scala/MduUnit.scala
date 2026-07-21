package mycpu

import chisel3._
import chisel3.util._

class MduUnit extends Module {
    val io = IO(new Bundle {
        val in  = Flipped(Decoupled(new PipelineData()))
        val out = Decoupled(new PipelineData())

        val flush = Input(Bool())
        val br_resolve_in = Input(new BranchResolve()) // 监听全网分支广播
    })

    val valid_reg = RegInit(false.B)
    val data_reg  = RegInit(0.U.asTypeOf(new PipelineData()))

    // ==========================================
    // 面具判定与存活确认
    // ==========================================
    val current_is_killed = valid_reg && io.br_resolve_in.valid && io.br_resolve_in.mispredict && ((data_reg.branch_mask & (1.U(4.W) << io.br_resolve_in.tag)) =/= 0.U)
    val real_valid = valid_reg && !current_is_killed

    

    // ==========================================
    // MDU 状态机 (包含强行打断逻辑)
    // ==========================================
    val is_div = (data_reg.mduOp === MduOp.DIV_W || data_reg.mduOp === MduOp.MOD_W || 
                  data_reg.mduOp === MduOp.DIV_WU || data_reg.mduOp === MduOp.MOD_WU) && !data_reg.hasException
    val is_mul = (data_reg.mduOp === MduOp.MUL_W || data_reg.mduOp === MduOp.MULH_W || 
                  data_reg.mduOp === MduOp.MULH_WU) && !data_reg.hasException
    val is_mdu = data_reg.resFromMulDiv && !data_reg.hasException

    val mdu_busy = RegInit(false.B)
    val mdu_finished = RegInit(false.B)
    val div_done = WireDefault(false.B)
    
    // ★ 注意这里的 mul_done：如果在算乘法时被 flush/kill，直接取消
    //存疑
    val mul_done = RegNext(valid_reg && is_mul && !mdu_busy && !io.flush && !current_is_killed, false.B) 

    val ready_go = !is_mdu || mdu_finished || (mdu_busy && (div_done || mul_done))
    val allow_in = !valid_reg || (ready_go && io.out.ready)

    io.in.ready := allow_in

    // 1. 接收状态机：解耦清理与接收逻辑
    when(io.flush) {
        valid_reg := false.B
    } .elsewhen(allow_in) {
        // 只要能进，优先接收新指令，哪怕前一条刚被杀
        valid_reg := io.in.valid
    } .elsewhen(current_is_killed) {
        valid_reg := false.B
    }
    when(io.in.valid && allow_in) { data_reg := io.in.bits }
    when(io.br_resolve_in.valid && !io.br_resolve_in.mispredict) {
        val clear_mask = ~(1.U(4.W) << io.br_resolve_in.tag)
        data_reg.branch_mask := Mux(io.in.valid && io.in.ready, io.in.bits.branch_mask, data_reg.branch_mask) & clear_mask
    }

    // 状态机流转
    // 2. 运算状态机清理：在这里接管 MDU 内部状态的重置
    when(io.flush || current_is_killed) {
        mdu_busy := false.B
        mdu_finished := false.B
    } .elsewhen(valid_reg && is_mdu && !mdu_busy && !mdu_finished) { 
        mdu_busy := true.B
    } .elsewhen(mdu_busy && (div_done || mul_done)) {
        mdu_busy := false.B
        mdu_finished := !io.out.ready 
    } .elsewhen(valid_reg && ready_go && io.out.ready) {
        mdu_finished := false.B 
    }

    val src1_val = data_reg.src1_value
    val src2_val = data_reg.src2_value
    val mdu_src1_reg = Reg(UInt(32.W))
    val mdu_src2_reg = Reg(UInt(32.W))

    when(valid_reg && !mdu_busy && !mdu_finished) {
        mdu_src1_reg := src1_val
        mdu_src2_reg := src2_val
    }

    val real_mdu_src1 = Mux(mdu_busy || mdu_finished, mdu_src1_reg, src1_val)
    val real_mdu_src2 = Mux(mdu_busy || mdu_finished, mdu_src2_reg, src2_val)
    val is_signed_mdu = data_reg.mduOp === MduOp.MULH_W || data_reg.mduOp === MduOp.DIV_W || data_reg.mduOp === MduOp.MOD_W

    // 例化运算器
    val mul = Module(new Multiplier())
    mul.io.src1     := real_mdu_src1
    mul.io.src2     := real_mdu_src2
    mul.io.isSigned := is_signed_mdu
    
    val div = Module(new Divider())
    val div_src1_abs = Mux(is_signed_mdu && src1_val(31), (~src1_val + 1.U), real_mdu_src1)
    val div_src2_abs = Mux(is_signed_mdu && src2_val(31), (~src2_val + 1.U), real_mdu_src2)
    
    // ★ 给除法器的使能信号增加保护
    div.io.enable := valid_reg && is_div && !mdu_busy && !mdu_finished && !io.flush && !current_is_killed
    div.io.a      := div_src1_abs
    div.io.b      := div_src2_abs
    div_done      := div.io.done 
    
    val q_sign = real_mdu_src1(31) ^ real_mdu_src2(31)
    val r_sign = real_mdu_src1(31)
    val final_q = Mux(is_signed_mdu && q_sign, (~div.io.q + 1.U), div.io.q)
    val final_r = Mux(is_signed_mdu && r_sign, (~div.io.r + 1.U), div.io.r)

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

    io.out.valid := valid_reg && ready_go
    io.out.bits  := out_data
}