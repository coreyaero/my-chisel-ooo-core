package mycpu

import chisel3._
import chisel3.util._



class FetchBuffer(depth: Int = 8) extends Module {
    val io = IO(new Bundle {
        val flush = Input(Bool())
        val in0   = Flipped(Decoupled(new PipelineData()))
        val in1   = Flipped(Decoupled(new PipelineData()))
        val out   = new FetchQueueOut()
    })
    
    val buffer = Reg(Vec(depth, new PipelineData()))
    val head   = RegInit(0.U(log2Ceil(depth).W))
    val tail   = RegInit(0.U(log2Ceil(depth).W))
    val count  = RegInit(0.U((log2Ceil(depth) + 1).W))

    val in_ready = count <= (depth - 2).U
    io.in0.ready := in_ready
    io.in1.ready := in_ready

    val enq0 = io.in0.valid && io.in0.ready
    val enq1 = io.in1.valid && io.in1.ready
    
    def wrapAdd(ptr: UInt, add: UInt) = {
        val sum = ptr + add
        Mux(sum >= depth.U, sum - depth.U, sum)(log2Ceil(depth)-1, 0)
    }

    io.out.valid0 := count > 0.U
    io.out.inst0  := buffer(head)
    io.out.valid1 := count > 1.U
    io.out.inst1  := buffer(wrapAdd(head, 1.U))

    when(io.flush) {
        head := 0.U; tail := 0.U; count := 0.U
    } .otherwise {
        head := wrapAdd(head, io.out.pop)
        
        val t0 = tail
        val t1 = wrapAdd(tail, 1.U)
        
        val t0_oh = UIntToOH(t0, depth)
        val t1_oh = UIntToOH(t1, depth)
        
        // ★ 核心手术点：解耦 Valid 与 CE 
        // 只要队列未满，强制打开 CE 大门。Valid 信号仅用于控制指针的自增。
        for (i <- 0 until depth) {
            val we0 = in_ready && t0_oh(i)
            val we1 = in_ready && t1_oh(i)
            
            when(we0) {
                buffer(i) := io.in0.bits
            } .elsewhen(we1) {
                buffer(i) := io.in1.bits
            }
        }
        
        val do_enq_cnt = Mux(enq1, 2.U, Mux(enq0, 1.U, 0.U))
        tail := wrapAdd(tail, do_enq_cnt)
        count := count + do_enq_cnt - io.out.pop
    }
}

class StageID extends Module {
    val io = IO(new Bundle {
        val in    = Flipped(new FetchQueueOut()) // 接 Fetch Buffer
        val out0  = Decoupled(new PipelineData())
        val out1  = Decoupled(new PipelineData())
        val flush = Input(Bool())
    })

    val dec0 = Module(new Decoder())
    val dec1 = Module(new Decoder())

    // ★ 终极防毒面具：LoongArch 的 NOP 指令 (addi.w $r0, $r0, 0)
    // 根据你的指令集格式，它的机器码是 0x02800000
    val NOP_INST = "h02800000".U(32.W) 

    // ★ 拦截清洗 0 号通道
    val safe_inst0_data = WireDefault(io.in.inst0)
    safe_inst0_data.inst := Mux(io.in.valid0, io.in.inst0.inst, NOP_INST)
    // 顺手把 Exception 也洗干净，防止幽灵异常冲刷流水线！
    safe_inst0_data.hasException := Mux(io.in.valid0, io.in.inst0.hasException, false.B)

    // ★ 拦截清洗 1 号通道 (彻底掐死 X 态源头)
    val safe_inst1_data = WireDefault(io.in.inst1)
    safe_inst1_data.inst := Mux(io.in.valid1, io.in.inst1.inst, NOP_INST)
    safe_inst1_data.hasException := Mux(io.in.valid1, io.in.inst1.hasException, false.B)
    dec0.io.inst := safe_inst0_data.inst
    dec1.io.inst := safe_inst1_data.inst

    // ★ 修复点：这里的类型从 DecoderOut 改成了 DecodeOut，与你的 Decoder 完全匹配
    def mapData(inData: PipelineData, decOut: DecodeOut) = {
        val out = WireDefault(inData)
        val op6 = inData.inst(31, 26)
        val is_store = (op6 === "b001010".U) && (inData.inst(24) === 1.U)
        val is_branch = (op6 === BitPat("b01011?")) || (op6 === BitPat("b0110??")) || (op6 === "b010100".U) || (op6 === "b010101".U) || (op6 === "b010011".U)
        val is_csr_write = (inData.inst(31, 24) === "h04".U) && (inData.inst(9, 5) =/= 0.U)
        val src2IsRd = is_store || is_branch || is_csr_write

        out.aluOp         := decOut.aluOp
        out.mduOp         := decOut.mduOp
        out.brType        := decOut.brType
        out.imm           := decOut.imm
        out.src1IsPC      := decOut.src1IsPC
        out.src2IsImm     := decOut.src2IsImm
        out.src2IsFour    := decOut.src2IsFour
        out.memWe         := decOut.memWe
        out.resFromMem    := decOut.resFromMem
        out.resFromMulDiv := decOut.resFromMulDiv
        out.lsOp          := decOut.lsOp
        out.regWriteEn    := decOut.regWe
        out.destReg       := decOut.destReg
        out.src1_addr     := inData.inst(9, 5)
        out.src2_addr     := Mux(src2IsRd, inData.inst(4, 0), inData.inst(14, 10))
        out.src1_read     := decOut.src1_read
        out.src2_read     := decOut.src2_read
        out.isCsr         := decOut.isCsr
        out.csrWe         := decOut.csrWe
        out.csrNum        := decOut.csrNum
        out.inst_ertn     := decOut.inst_ertn
        out.rdtimel       := decOut.rdtimel
        out.rdtimeh       := decOut.rdtimeh
        out.hasException  := inData.hasException || decOut.hasException
        out.ecode         := Mux(inData.hasException, inData.ecode, decOut.ecode)
        out.tlbOp         := decOut.tlbOp
        out.invtlb_op     := decOut.invtlb_op
        //out.is_refetch    := decOut.is_refetch
        out.is_refetch    := decOut.is_refetch || (inData.pred_taken && !is_branch)
        out.is_cacop      := decOut.is_cacop
        out.cacop_op      := decOut.cacop_op
        out.is_branch     := is_branch
        out
    }

    val d0 = mapData(safe_inst0_data, dec0.io.out)
    val d1 = mapData(safe_inst1_data, dec1.io.out)

    // ★ 序列化屏障仲裁：CSR/TLB/ERTN/CACOP 必须单发孤独执行！
    val is_ser0 = d0.isCsr || d0.inst_ertn || (d0.tlbOp =/= TlbOp.NOP) || d0.is_cacop
    val is_ser1 = d1.isCsr || d1.inst_ertn || (d1.tlbOp =/= TlbOp.NOP) || d1.is_cacop
    
    val allow_dual = !is_ser0 && !is_ser1

    val real_valid0 = io.in.valid0 && !io.flush
    val real_valid1 = io.in.valid1 && !io.flush && allow_dual

    io.out0.valid := real_valid0
    io.out0.bits  := d0
    io.out1.valid := real_valid1
    io.out1.bits  := d1

    // ★ 精确计算 Pop Count 反馈给 Fetch Buffer
    val pop0 = real_valid0 && io.out0.ready
    val pop1 = real_valid1 && io.out1.ready && pop0 // 只有 0 走了，1 才能走！
    
    io.in.pop := Mux(pop0 && pop1, 2.U, Mux(pop0, 1.U, 0.U))
}