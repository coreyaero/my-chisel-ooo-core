package mycpu

import chisel3._
import chisel3.util._

object Config{
    val START_PC = 0x1c000000.U(32.W)
    val robEntries  = 32 
    val robPtrWidth = log2Ceil(robEntries)
    val iqEntries  = 16
    val iqPtrWidth = log2Ceil(iqEntries)
    val prfEntries  = 64
    val prfPtrWidth = log2Ceil(prfEntries)
}

class OneHotGenerator(w:Int){
    private var bit = 0;
    def nxt:UInt = {
        val res = (1 << bit).U(w.W)
        bit += 1
        res
    }
    val NOP : UInt = 0.U(w.W)
}

object AluOp{
    private val oh = new OneHotGenerator(13) // ★ 修改：将 12 改为 13
    val NOP = oh.NOP
    val ADD, SUB, SLT, SLTU, AND, NOR, OR, XOR, SLL, SRL, SRA, LUI, CPUCFG = oh.nxt // ★ 追加 CPUCFG
}

object LsOp{
    private val oh = new OneHotGenerator(8)
    val NOP = oh.NOP
    val LD_B, LD_H, LD_W, LD_BU, LD_HU, ST_B, ST_H, ST_W = oh.nxt
}

object MduOp{
    private val oh = new OneHotGenerator(7)
    val NOP = oh.NOP
    val MUL_W, MULH_W, MULH_WU, DIV_W, MOD_W, DIV_WU, MOD_WU = oh.nxt
}

object BrType{
    private val oh = new OneHotGenerator(9)
    val NOP = oh.NOP
    val BEQ, BNE, BLT, BGE, BLTU, BGEU, JIRL, B, BL = oh.nxt
}

object TlbOp{
    private val oh = new OneHotGenerator(5)
    val NOP = oh.NOP
    val SRCH, RD, WR, FILL, INV = oh.nxt
}

class AxiIO extends Bundle {
    val arid    = Output(UInt(4.W))
    val araddr  = Output(UInt(32.W))
    //读突发传输的长度（拍数）。书中建议固定为 0。
    //原因是目前的 CPU 没有实现 Cache，每次取指令或读数据都只需要读 1 个字（即 1 拍就能传完）。
    val arlen   = Output(UInt(8.W))  
    val arsize  = Output(UInt(3.W))
    //突发传输的类型。
    //书中建议固定为 0b01 (INCR 递增模式)。
    val arburst = Output(UInt(2.W))
    //原子锁控制信号。固定为 0。
    val arlock  = Output(UInt(2.W))
    //内存属性（是否可缓存、可缓冲）。固定为 0。
    val arcache = Output(UInt(4.W))
    //保护属性（如区分特权级/用户级、安全/非安全）。固定为 0。
    val arprot  = Output(UInt(3.W))
    //读地址请求有效
    val arvalid = Output(Bool())
    //读地址接收就绪
    val arready = Input(Bool())

    // 2. 读响应通道 (R)
    val rid     = Input(UInt(4.W))
    val rdata   = Input(UInt(32.W))
    //读操作的响应状态（如 OKAY, SLVERR）。书中说明可忽略。
    val rresp   = Input(UInt(2.W))
    //标识当前是不是读突发传输的最后一拍数据。可忽略。
    val rlast   = Input(Bool())
    val rvalid  = Input(Bool())
    val rready  = Output(Bool())

    // 3. 写地址通道 (AW)
    val awid    = Output(UInt(4.W))
    val awaddr  = Output(UInt(32.W))
    val awlen   = Output(UInt(8.W))
    val awsize  = Output(UInt(3.W))
    val awburst = Output(UInt(2.W))
    val awlock  = Output(UInt(2.W))
    val awcache = Output(UInt(4.W))
    val awprot  = Output(UInt(3.W))
    val awvalid = Output(Bool())
    val awready = Input(Bool())

    // 4. 写数据通道 (W)
    val wid     = Output(UInt(4.W))
    val wdata   = Output(UInt(32.W))
    val wstrb   = Output(UInt(4.W))
    val wlast   = Output(Bool())
    val wvalid  = Output(Bool())
    val wready  = Input(Bool())

    // 5. 写响应通道 (B)
    val bid     = Input(UInt(4.W))   // 可忽略
    //写入结果状态（成功还是报错）。可忽略。
    //同 rresp，当前设计不处理总线物理错误。
    val bresp   = Input(UInt(2.W))
    val bvalid  = Input(Bool())
    val bready  = Output(Bool())
}
