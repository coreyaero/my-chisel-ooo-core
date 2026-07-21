package mycpu

import chisel3._
import chisel3.util._

class InstReq extends Bundle {
    val addr = UInt(32.W)
}

class InstMemIO extends Bundle {
    val req  = Decoupled(new InstReq())
    val resp = Flipped(Decoupled(UInt(32.W)))
}

class SramIo extends Bundle {
    val req     = Output(Bool())
    val wr      = Output(Bool())
    val size    = Output(UInt(2.W))
    val wstrb   = Output(UInt(4.W))
    val addr    = Output(UInt(32.W))
    val wdata   = Output(UInt(32.W))

    val addr_ok = Input(Bool())
    val data_ok = Input(Bool())
    val rdata   = Input(UInt(64.W))
}

class ForwardingData extends Bundle {
    val valid        = Bool()
    val regWriteEn   = Bool()
    val regWriteAddr = UInt(5.W)
    val result       = UInt(32.W)
    val resFromMem   = Bool()
    val isCsr        = Bool()
}


class PipelineData extends Bundle{
    //IF Generated
    val pc              = UInt(32.W)
    val inst            = UInt(32.W)

    //ID Generated
    //Used in EX
    val aluOp           = UInt(12.W)
    val mduOp           = UInt(7.W)
    val brType          = UInt(9.W)
    val imm             = UInt(32.W)
    val src1IsPC        = Bool()
    val src2IsImm       = Bool()
    val src2IsFour      = Bool()
    val src1_addr       = UInt(5.W)
    val src2_addr       = UInt(5.W)
    val src1_value      = UInt(32.W) //rj
    val src2_value      = UInt(32.W) //rkd
    val resFromMulDiv   = Bool()
    //Used in MEM
    val memWe           = Bool()
    val lsOp            = UInt(8.W)
    //Used in WB
    val resFromMem      = Bool()
    val regWriteEn      = Bool()
    val destReg         = UInt(5.W)
    
    //EX Generated
    val ex_result       = UInt(32.W)
    val aux_data        = UInt(32.W) //csr or memwdata

    //Exception
    val hasException    = Bool()
    val ecode           = UInt(6.W)
    val esubcode        = UInt(9.W)
    val isCsr           = Bool()
    val csrWe           = Bool()
    val csrNum          = UInt(14.W)
    val inst_ertn       = Bool()
    //These two were just used in EX
    val rdtimel         = Bool()
    val rdtimeh         = Bool()

    //TLB
    val tlbOp           = UInt(5.W)
    val invtlb_op       = UInt(5.W)
    val is_refetch      = Bool()
    
    //Cache
    val is_cacop   = Bool()
    val cacop_op   = UInt(5.W)

    // ★ 新增：记录该指令在 ROB 中的坑位号
    val rob_idx         = UInt(Config.robPtrWidth.W)

    // ★ 乱序架构新增：译码读写需求
    val src1_read       = Bool()
    val src2_read       = Bool()
    
    // ★ 乱序架构新增：物理寄存器追踪
    val pdest           = UInt(Config.prfPtrWidth.W)
    val old_pdest       = UInt(Config.prfPtrWidth.W)
    val psrc1           = UInt(Config.prfPtrWidth.W)
    val psrc2           = UInt(Config.prfPtrWidth.W)

    // ★ 分支快照新增：标识这条指令是否是分支指令，以及它分配到的快照 Tag
    val is_branch       = Bool() 
    val branch_tag      = UInt(2.W) // 2 位宽，刚好支持 0~3 号共 4 个快照
    val branch_mask     = UInt(4.W)   // 致命烙印：我这条指令依赖了哪些老分支？

    // ★ 乱序访存新增：记录指令在 LSQ 中的坑位号
    val lsq_idx = UInt(4.W)

    val pred_taken      = Bool()
    val pred_target     = UInt(32.W)
    val bpu_type        = UInt(2.W)

    val ghr             = UInt(10.W)
    val ras_tos         = UInt(4.W) // ★ 新增：携带栈顶快照
}

class BranchResolve extends Bundle {
    val valid       = Bool()      // 是否有分支指令在当拍出结果
    val mispredict  = Bool()      // 是否预测失败（需要回档）
    val tag         = UInt(2.W)   // 这条分支指令当初分配的快照编号
}


class MmuConfig extends Bundle {
    val crmd    = new CrmdReg()
    val asid    = new AsidReg()
    val dmw0    = new DmwReg()
    val dmw1    = new DmwReg()
    val tlbehi  = new TlbehiReg()
}


class FetchQueueOut extends Bundle {
    val valid0 = Output(Bool())
    val inst0  = Output(new PipelineData())
    val valid1 = Output(Bool())
    val inst1  = Output(new PipelineData())
    val pop    = Input(UInt(2.W)) // 告诉队列，后级成功吃掉了 0/1/2 条指令
}

object BpuType {
    val COND   = 0.U(2.W) // 条件分支 (beq, bne)
    val UNCOND = 1.U(2.W) // 无条件跳转 (b)
    val CALL   = 2.U(2.W) // 函数调用 (bl, jirl rd=1)
    val RET    = 3.U(2.W) // 函数返回 (jirl rj=1, rd=0)
}

class BtbEntry extends Bundle {
    val valid    = Bool()
    val tag      = UInt(21.W) // 32位PC - 9位索引 - 2位对齐 = 21位
    val target   = UInt(32.W)
    val bpu_type = UInt(2.W)
}

class BpuUpdate extends Bundle {
    val pc       = UInt(32.W)
    val target   = UInt(32.W)
    val taken    = Bool()
    val bpu_type = UInt(2.W)

    // ★ GShare 训练新增：历史快照与验尸报告
    val ghr        = UInt(10.W) 
    val mispredict = Bool()
    val ras_tos  = UInt(4.W) // ★ 新增：指令执行前的纯净栈顶快照
}

// ====================================================================
// ★ 双发射保序流水线缓冲 (Superscalar Dispatch Skid Buffer)
// 彻底解决独立队列导致的插队、丢指令问题，并提供绝对的时序物理隔离！
// ====================================================================
class DispatchBuffer extends Module {
    val io = IO(new Bundle {
        val flush = Input(Bool())
        val in0   = Flipped(Decoupled(new PipelineData()))
        val in1   = Flipped(Decoupled(new PipelineData()))
        val out0  = Decoupled(new PipelineData())
        val out1  = Decoupled(new PipelineData())
    })
    
    val valid0 = RegInit(false.B)
    val bits0  = Reg(new PipelineData())
    val valid1 = RegInit(false.B)
    val bits1  = Reg(new PipelineData())
    
    // 弹栈逻辑：必须保证 0 走了，1 才能走
    val pop0 = valid0 && io.out0.ready
    val pop1 = valid1 && io.out1.ready && pop0
    val pop_cnt = Mux(pop0 && pop1, 2.U, Mux(pop0, 1.U, 0.U))
    
    // 计算弹栈后留下的状态 (如果只弹出 0，那么 1 会平移到 0 的位置)
    val remain0_v = Mux(pop_cnt === 2.U, false.B, Mux(pop_cnt === 1.U, valid1, valid0))
    val remain0_b = Mux(pop_cnt >= 1.U, bits1, bits0)
    val remain1_v = Mux(pop_cnt >= 1.U, false.B, valid1)
    val remain1_b = bits1
    
    // 计算剩余可用空间
    val space = Mux(!remain0_v, 2.U, Mux(!remain1_v, 1.U, 0.U))
    io.in0.ready := space >= 1.U
    io.in1.ready := space === 2.U
    
    val enq0 = io.in0.valid && io.in0.ready
    val enq1 = io.in1.valid && io.in1.ready && enq0
    
    when(io.flush) {
        valid0 := false.B
        valid1 := false.B
    } .otherwise {
        when(space === 2.U) {
            valid0 := enq0
            bits0  := io.in0.bits
            valid1 := enq1
            bits1  := io.in1.bits
        } .elsewhen(space === 1.U) {
            valid0 := true.B
            bits0  := remain0_b
            valid1 := enq0 // 当只有 1 个空位时，新进来的必然放在槽位 1
            bits1  := io.in0.bits
        } .otherwise {
            valid0 := remain0_v
            bits0  := remain0_b
            valid1 := remain1_v
            bits1  := remain1_b
        }
    }
    
    io.out0.valid := valid0
    io.out0.bits  := bits0
    io.out1.valid := valid1
    io.out1.bits  := bits1
}

// ====================================================================
// ★ 智能乱序发射缓冲 (Issue Buffer)
// 修复 If-Else 黑洞优先级，绝不吞噬同一拍到来的新指令！
// ====================================================================
class IssueBuffer extends Module {
    val io = IO(new Bundle {
        val flush = Input(Bool())
        val br_resolve = Input(new BranchResolve())
        val enq = Flipped(Decoupled(new PipelineData()))
        val deq = Decoupled(new PipelineData())
    })
    val valid_reg = RegInit(false.B)
    val data_reg  = RegInit(0.U.asTypeOf(new PipelineData()))

    val current_is_killed = valid_reg && io.br_resolve.valid && io.br_resolve.mispredict && ((data_reg.branch_mask & (1.U(4.W) << io.br_resolve.tag)) =/= 0.U)
    val incoming_is_killed = io.enq.valid && io.br_resolve.valid && io.br_resolve.mispredict && ((io.enq.bits.branch_mask & (1.U(4.W) << io.br_resolve.tag)) =/= 0.U)

    val allow_in = !valid_reg || current_is_killed || io.deq.ready
    io.enq.ready := allow_in

    // ★ 核心修复：分离 flush、allow_in 和 kill 的优先级！
    // 必须优先保证新指令能住进来，只有没新指令进来的情况下，老指令才被清空为 false。
    when(io.flush) {
        valid_reg := false.B
    } .elsewhen(allow_in) {
        valid_reg := io.enq.valid && !incoming_is_killed
    } .elsewhen(current_is_killed) {
        valid_reg := false.B
    }

    when(io.enq.valid && allow_in) {
        data_reg := io.enq.bits
    }

    when(io.br_resolve.valid && !io.br_resolve.mispredict) {
        val clear_mask = ~(1.U(4.W) << io.br_resolve.tag)
        data_reg.branch_mask := Mux(io.enq.valid && allow_in, io.enq.bits.branch_mask, data_reg.branch_mask) & clear_mask
    }

    io.deq.valid := valid_reg && !current_is_killed
    
    // ★ 修复局部赋值问题，使用 WireDefault 保证信号干净透传
    val deq_bits_out = WireDefault(data_reg)
    when(io.br_resolve.valid && !io.br_resolve.mispredict) {
        val clear_mask = ~(1.U(4.W) << io.br_resolve.tag)
        deq_bits_out.branch_mask := data_reg.branch_mask & clear_mask
    }
    io.deq.bits := deq_bits_out
}