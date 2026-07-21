package mycpu

import chisel3._

class PRF extends Module{
    val io = IO(new Bundle{
        val raddr1  = Input(UInt(Config.prfPtrWidth.W)); val rdata1  = Output(UInt(32.W))
        val raddr2  = Input(UInt(Config.prfPtrWidth.W)); val rdata2  = Output(UInt(32.W))
        val raddr3  = Input(UInt(Config.prfPtrWidth.W)); val rdata3  = Output(UInt(32.W))
        val raddr4  = Input(UInt(Config.prfPtrWidth.W)); val rdata4  = Output(UInt(32.W))
        val raddr5  = Input(UInt(Config.prfPtrWidth.W)); val rdata5  = Output(UInt(32.W))
        val raddr6  = Input(UInt(Config.prfPtrWidth.W)); val rdata6  = Output(UInt(32.W))
        val raddr7  = Input(UInt(Config.prfPtrWidth.W)); val rdata7  = Output(UInt(32.W))
        val raddr8  = Input(UInt(Config.prfPtrWidth.W)); val rdata8  = Output(UInt(32.W))

        val we1     = Input(Bool())
        val waddr1  = Input(UInt(Config.prfPtrWidth.W))
        val wdata1  = Input(UInt(32.W))
        
        val we2     = Input(Bool())
        val waddr2  = Input(UInt(Config.prfPtrWidth.W))
        val wdata2  = Input(UInt(32.W))
    })

    val regs = RegInit(VecInit(Seq.fill(Config.prfEntries)(0.U(32.W))))
    
    // 乱序写回逻辑
    when (io.we1 && io.waddr1 =/= 0.U) { regs(io.waddr1) := io.wdata1 }
    when (io.we2 && io.waddr2 =/= 0.U) { regs(io.waddr2) := io.wdata2 }
    
    // ★ 内部前递逻辑封装：如果刚才 CDB 写回了，当拍直接读出最新值！
    def readData(raddr: UInt): UInt = {
        Mux(raddr === 0.U, 0.U, 
            Mux(io.we2 && (io.waddr2 === raddr), io.wdata2, 
            Mux(io.we1 && (io.waddr1 === raddr), io.wdata1, 
            regs(raddr)))
        )
    }

    io.rdata1 := readData(io.raddr1)
    io.rdata2 := readData(io.raddr2)
    io.rdata3 := readData(io.raddr3)
    io.rdata4 := readData(io.raddr4)
    io.rdata5 := readData(io.raddr5)
    io.rdata6 := readData(io.raddr6)
    io.rdata7 := readData(io.raddr7)
    io.rdata8 := readData(io.raddr8)
}