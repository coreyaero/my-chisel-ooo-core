error id: file://<WORKSPACE>/src/main/scala/MduUnit.scala:
file://<WORKSPACE>/src/main/scala/MduUnit.scala
empty definition using pc, found symbol in pc: 
empty definition using semanticdb
empty definition using fallback
non-local guesses:
	 -ready.
	 -ready#
	 -ready().
	 -scala/Predef.ready.
	 -scala/Predef.ready#
	 -scala/Predef.ready().
offset: 6402
uri: file://<WORKSPACE>/src/main/scala/MduUnit.scala
text:
```scala
package mycpu

import chisel3._
import chisel3.util._

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

    val valid_mdu_req = active && is_mdu_inst && !data_reg.hasException
    val start_pulse = valid_mdu_req && !mdu_busy && !mdu_finished && !io.flush



    // ==========================================
    // 4. 运算部件例化 (完全干掉冗余寄存器，直接用原数据！)
    // ==========================================
    val src1_val = data_reg.src1_value
    val src2_val = data_reg.src2_value
    val is_signed_mdu = data_reg.mduOp === MduOp.MULH_W || data_reg.mduOp === MduOp.DIV_W || data_reg.mduOp === MduOp.MOD_W

    val mul = Module(new Multiplier())
    mul.io.src1     := src1_val
    mul.io.src2     := src2_val
    mul.io.isSigned := is_signed_mdu
    
    val div = Module(new Divider())
    div.io.enable := start_pulse && is_div  // 严丝合缝的单周期触发脉冲
    div.io.a      := Mux(is_signed_mdu && src1_val(31), (~src1_val + 1.U), src1_val)
    div.io.b      := Mux(is_signed_mdu && src2_val(31), (~src2_val + 1.U), src2_val)
    val div_done   = div.io.done 
    
    // 乘法固定一拍出结果
    val mul_done = RegNext(start_pulse && is_mul, false.B)

    // ==========================================
    // 5. 极简握手逻辑
    // ==========================================
    val mdu_ready_go = !valid_mdu_req || mdu_finished || (mdu_busy && Mux(is_div, div_done, mul_done))
    
    val out_valid = active && mdu_ready_go
    val in_ready  = !active || (out_valid && io.out.ready)

    io.in.ready  := in_ready
    io.out.valid := out_valid

    // ==========================================
    // 6. 流水线流转 (真正的无死角、零废话)
    // ==========================================
    when(io.flush) {
        valid_reg    := false.B
        mdu_busy     := false.B
        mdu_finished := false.B
    } .otherwise {
        when(in_ready) {
            // 当能够进水时，无脑吞数据！
            valid_reg := io.in.valid && !is_killed(io.in.bits.branch_mask)
            data_reg  := io.in.bits
            data_reg.branch_mask := io.in.bits.branch_mask & clear_mask
            
            // ★ 新指令进来，或者气泡进来，状态机瞬间清零，绝不拖泥带水
            mdu_busy     := false.B
            mdu_finished := false.B
        } .otherwise {
            // 被阻塞时，老老实实洗面具
            data_reg.branch_mask := data_reg.branch_mask & clear_mask
            
            // ★ 状态机流转只在阻塞时发生！逻辑严密极了
            when(is_killed(data_reg.branch_mask)) {
                valid_reg    := false.B
                mdu_busy     := false.B
                mdu_finished := false.B
            } .elsewhen(start_pulse) {
                mdu_busy := true.B
            } .elsewhen(mdu_busy && (div_done || mul_done)) {
                mdu_busy     := false.B
                mdu_finished := true.B // 卡在这里，直到 in_ready 触发清理
            }
        }
    }

    // ==========================================
    // 7. 结果拼装
    // ==========================================
    val q_sign = src1_val(31) ^ src2_val(31)
    val r_sign = src1_val(31)
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
    io.out.bits := out_data
}

    


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
    } .elsewhen(valid_reg && ready_go && io.out.r@@eady) {
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

    io.out.valid := real_valid && ready_go
    io.out.bits  := out_data
}
```


#### Short summary: 

empty definition using pc, found symbol in pc: 