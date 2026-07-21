package mycpu

import chisel3._
import chisel3.util._

private object Src1 extends ChiselEnum{val X, R, PC = Value}
private object Src2 extends ChiselEnum{val X, R, IMM = Value}
private object Dst  extends ChiselEnum{val X, RD, RJ, R1 = Value}
private object Imm  extends ChiselEnum{val X, SI12, UI12, SI16, SI20, SI26, UI5, FOUR = Value}

class DecodeOut extends Bundle{
    val aluOp           = UInt(12.W)
    val lsOp            = UInt(8.W)
    val mduOp           = UInt(7.W)
    val brType          = UInt(9.W)

    val imm             = UInt(32.W)
    val src1IsPC        = Bool()
    val src2IsImm       = Bool()
    val src2IsFour      = Bool()
    val destReg         = UInt(5.W)

    val regWe           = Bool()
    val memWe           = Bool()
    val resFromMem      = Bool()
    val resFromMulDiv   = Bool()
    
    val hasException    = Bool()
    val ecode           = UInt(6.W)
    val isCsr           = Bool()
    val csrWe           = Bool()
    val csrNum          = UInt(14.W)
    val inst_ertn       = Bool()
    
    val rdtimel         = Bool()
    val rdtimeh         = Bool()

    val src1_read       = Bool()
    val src2_read       = Bool()

    val tlbOp           = UInt(5.W)
    val invtlb_op       = UInt(5.W)
    val is_refetch      = Bool()

    val is_cacop    = Bool()
    val cacop_op    = UInt(5.W)
}

class Decoder extends Module{
    val io = IO(new Bundle{
        val inst = Input(UInt(32.W))
        val out  = Output(new DecodeOut())
    })
    val inst = io.inst

    def row(
        alu: UInt, ls: UInt, mdu: UInt, 
        s1:Src1.Type, s2:Src2.Type, i: Imm.Type, d: Dst.Type, 
        rWe: UInt, mWe: UInt, br: UInt,
        r1Re: UInt, r2Re: UInt
    ) : List[UInt] = {
        List(alu, ls, mdu, s1.asUInt, s2.asUInt, i.asUInt, d.asUInt, rWe.asUInt, mWe.asUInt, br, r1Re, r2Re)
    }
    val dflt = row(AluOp.NOP, LsOp.NOP, MduOp.NOP, Src1.R, Src2.R, Imm.X, Dst.X, 0.U, 0.U, BrType.NOP, 0.U, 0.U)
    val decodeTable = Array(
        //                 指令掩码 (BitPat)                          ALU          LS          MDU          S1       S2        IMM         DST    RWe  MWe    BrType     R1   R2
        BitPat("b000000_0000_01_00000_?????_?????_?????")   -> row(AluOp.ADD,   LsOp.NOP,   MduOp.NOP,   Src1.R,  Src2.R,    Imm.X,     Dst.RD,  1.U, 0.U, BrType.NOP, 1.U, 1.U), // add.w
        BitPat("b000000_0000_01_00010_?????_?????_?????")   -> row(AluOp.SUB,   LsOp.NOP,   MduOp.NOP,   Src1.R,  Src2.R,    Imm.X,     Dst.RD,  1.U, 0.U, BrType.NOP, 1.U, 1.U), // sub.w
        BitPat("b000000_0000_01_00100_?????_?????_?????")   -> row(AluOp.SLT,   LsOp.NOP,   MduOp.NOP,   Src1.R,  Src2.R,    Imm.X,     Dst.RD,  1.U, 0.U, BrType.NOP, 1.U, 1.U), // slt
        BitPat("b000000_0000_01_00101_?????_?????_?????")   -> row(AluOp.SLTU,  LsOp.NOP,   MduOp.NOP,   Src1.R,  Src2.R,    Imm.X,     Dst.RD,  1.U, 0.U, BrType.NOP, 1.U, 1.U), // sltu
        BitPat("b000000_0000_01_01000_?????_?????_?????")   -> row(AluOp.NOR,   LsOp.NOP,   MduOp.NOP,   Src1.R,  Src2.R,    Imm.X,     Dst.RD,  1.U, 0.U, BrType.NOP, 1.U, 1.U), // nor
        BitPat("b000000_0000_01_01001_?????_?????_?????")   -> row(AluOp.AND,   LsOp.NOP,   MduOp.NOP,   Src1.R,  Src2.R,    Imm.X,     Dst.RD,  1.U, 0.U, BrType.NOP, 1.U, 1.U), // and
        BitPat("b000000_0000_01_01010_?????_?????_?????")   -> row(AluOp.OR,    LsOp.NOP,   MduOp.NOP,   Src1.R,  Src2.R,    Imm.X,     Dst.RD,  1.U, 0.U, BrType.NOP, 1.U, 1.U), // or
        BitPat("b000000_0000_01_01011_?????_?????_?????")   -> row(AluOp.XOR,   LsOp.NOP,   MduOp.NOP,   Src1.R,  Src2.R,    Imm.X,     Dst.RD,  1.U, 0.U, BrType.NOP, 1.U, 1.U), // xor
        BitPat("b000000_0000_01_01110_?????_?????_?????")   -> row(AluOp.SLL,   LsOp.NOP,   MduOp.NOP,   Src1.R,  Src2.R,    Imm.X,     Dst.RD,  1.U, 0.U, BrType.NOP, 1.U, 1.U), // sll.w
        BitPat("b000000_0000_01_01111_?????_?????_?????")   -> row(AluOp.SRL,   LsOp.NOP,   MduOp.NOP,   Src1.R,  Src2.R,    Imm.X,     Dst.RD,  1.U, 0.U, BrType.NOP, 1.U, 1.U), // srl.w
        BitPat("b000000_0000_01_10000_?????_?????_?????")   -> row(AluOp.SRA,   LsOp.NOP,   MduOp.NOP,   Src1.R,  Src2.R,    Imm.X,     Dst.RD,  1.U, 0.U, BrType.NOP, 1.U, 1.U), // sra.w
        
        BitPat("b000000_0001_00_00001_?????_?????_?????")   -> row(AluOp.SLL,   LsOp.NOP,   MduOp.NOP,   Src1.R,  Src2.IMM,  Imm.UI5,   Dst.RD,  1.U, 0.U, BrType.NOP, 1.U, 0.U), // slli.w
        BitPat("b000000_0001_00_01001_?????_?????_?????")   -> row(AluOp.SRL,   LsOp.NOP,   MduOp.NOP,   Src1.R,  Src2.IMM,  Imm.UI5,   Dst.RD,  1.U, 0.U, BrType.NOP, 1.U, 0.U), // srli.w
        BitPat("b000000_0001_00_10001_?????_?????_?????")   -> row(AluOp.SRA,   LsOp.NOP,   MduOp.NOP,   Src1.R,  Src2.IMM,  Imm.UI5,   Dst.RD,  1.U, 0.U, BrType.NOP, 1.U, 0.U), // srai.w
        BitPat("b000000_1000_????_????_????_?????_?????")   -> row(AluOp.SLT,   LsOp.NOP,   MduOp.NOP,   Src1.R,  Src2.IMM,  Imm.SI12,  Dst.RD,  1.U, 0.U, BrType.NOP, 1.U, 0.U), // slti
        BitPat("b000000_1001_????_????_????_?????_?????")   -> row(AluOp.SLTU,  LsOp.NOP,   MduOp.NOP,   Src1.R,  Src2.IMM,  Imm.SI12,  Dst.RD,  1.U, 0.U, BrType.NOP, 1.U, 0.U), // sltui
        BitPat("b000000_1010_????_????_????_?????_?????")   -> row(AluOp.ADD,   LsOp.NOP,   MduOp.NOP,   Src1.R,  Src2.IMM,  Imm.SI12,  Dst.RD,  1.U, 0.U, BrType.NOP, 1.U, 0.U), // addi.w
        BitPat("b000000_1101_????_????_????_?????_?????")   -> row(AluOp.AND,   LsOp.NOP,   MduOp.NOP,   Src1.R,  Src2.IMM,  Imm.UI12,  Dst.RD,  1.U, 0.U, BrType.NOP, 1.U, 0.U), // andi
        BitPat("b000000_1110_????_????_????_?????_?????")   -> row(AluOp.OR,    LsOp.NOP,   MduOp.NOP,   Src1.R,  Src2.IMM,  Imm.UI12,  Dst.RD,  1.U, 0.U, BrType.NOP, 1.U, 0.U), // ori
        BitPat("b000000_1111_????_????_????_?????_?????")   -> row(AluOp.XOR,   LsOp.NOP,   MduOp.NOP,   Src1.R,  Src2.IMM,  Imm.UI12,  Dst.RD,  1.U, 0.U, BrType.NOP, 1.U, 0.U), // xori

        BitPat("b000101_0_????_????_????_????_????_?????")  -> row(AluOp.LUI,   LsOp.NOP,   MduOp.NOP,   Src1.X,  Src2.IMM,  Imm.SI20,  Dst.RD,  1.U, 0.U, BrType.NOP, 0.U, 0.U), // lu12i.w
        BitPat("b000111_0_????_????_????_????_????_?????")  -> row(AluOp.ADD,   LsOp.NOP,   MduOp.NOP,   Src1.PC, Src2.IMM,  Imm.SI20,  Dst.RD,  1.U, 0.U, BrType.NOP, 0.U, 0.U), // pcaddu12i
        
        BitPat("b000000_0000_01_11000_?????_?????_?????")   -> row(AluOp.NOP,   LsOp.NOP,   MduOp.MUL_W, Src1.R,  Src2.R,    Imm.X,     Dst.RD,  1.U, 0.U, BrType.NOP, 1.U, 1.U), // mul.w
        BitPat("b000000_0000_01_11001_?????_?????_?????")   -> row(AluOp.NOP,   LsOp.NOP,   MduOp.MULH_W,Src1.R,  Src2.R,    Imm.X,     Dst.RD,  1.U, 0.U, BrType.NOP, 1.U, 1.U), // mulh.w
        BitPat("b000000_0000_01_11010_?????_?????_?????")   -> row(AluOp.NOP,   LsOp.NOP,   MduOp.MULH_WU,Src1.R, Src2.R,    Imm.X,     Dst.RD,  1.U, 0.U, BrType.NOP, 1.U, 1.U), // mulh.wu
        BitPat("b000000_0000_10_00000_?????_?????_?????")   -> row(AluOp.NOP,   LsOp.NOP,   MduOp.DIV_W, Src1.R,  Src2.R,    Imm.X,     Dst.RD,  1.U, 0.U, BrType.NOP, 1.U, 1.U), // div.w
        BitPat("b000000_0000_10_00001_?????_?????_?????")   -> row(AluOp.NOP,   LsOp.NOP,   MduOp.MOD_W, Src1.R,  Src2.R,    Imm.X,     Dst.RD,  1.U, 0.U, BrType.NOP, 1.U, 1.U), // mod.w
        BitPat("b000000_0000_10_00010_?????_?????_?????")   -> row(AluOp.NOP,   LsOp.NOP,   MduOp.DIV_WU,Src1.R,  Src2.R,    Imm.X,     Dst.RD,  1.U, 0.U, BrType.NOP, 1.U, 1.U), // div.wu
        BitPat("b000000_0000_10_00011_?????_?????_?????")   -> row(AluOp.NOP,   LsOp.NOP,   MduOp.MOD_WU,Src1.R,  Src2.R,    Imm.X,     Dst.RD,  1.U, 0.U, BrType.NOP, 1.U, 1.U), // mod.wu
        
        BitPat("b001010_0000_????_????_????_?????_?????")   -> row(AluOp.ADD,   LsOp.LD_B,  MduOp.NOP,   Src1.R,  Src2.IMM,  Imm.SI12,  Dst.RD,  1.U, 0.U, BrType.NOP, 1.U, 0.U), // ld.b
        BitPat("b001010_0001_????_????_????_?????_?????")   -> row(AluOp.ADD,   LsOp.LD_H,  MduOp.NOP,   Src1.R,  Src2.IMM,  Imm.SI12,  Dst.RD,  1.U, 0.U, BrType.NOP, 1.U, 0.U), // ld.h
        BitPat("b001010_0010_????_????_????_?????_?????")   -> row(AluOp.ADD,   LsOp.LD_W,  MduOp.NOP,   Src1.R,  Src2.IMM,  Imm.SI12,  Dst.RD,  1.U, 0.U, BrType.NOP, 1.U, 0.U), // ld.w
        BitPat("b001010_1000_????_????_????_?????_?????")   -> row(AluOp.ADD,   LsOp.LD_BU, MduOp.NOP,   Src1.R,  Src2.IMM,  Imm.SI12,  Dst.RD,  1.U, 0.U, BrType.NOP, 1.U, 0.U), // ld.bu
        BitPat("b001010_1001_????_????_????_?????_?????")   -> row(AluOp.ADD,   LsOp.LD_HU, MduOp.NOP,   Src1.R,  Src2.IMM,  Imm.SI12,  Dst.RD,  1.U, 0.U, BrType.NOP, 1.U, 0.U), // ld.hu

        BitPat("b001010_0100_????_????_????_?????_?????")   -> row(AluOp.ADD,   LsOp.ST_B,  MduOp.NOP,   Src1.R,  Src2.IMM,  Imm.SI12,  Dst.X,   0.U, 1.U, BrType.NOP, 1.U, 1.U), // st.b
        BitPat("b001010_0101_????_????_????_?????_?????")   -> row(AluOp.ADD,   LsOp.ST_H,  MduOp.NOP,   Src1.R,  Src2.IMM,  Imm.SI12,  Dst.X,   0.U, 1.U, BrType.NOP, 1.U, 1.U), // st.h
        BitPat("b001010_0110_????_????_????_?????_?????")   -> row(AluOp.ADD,   LsOp.ST_W,  MduOp.NOP,   Src1.R,  Src2.IMM,  Imm.SI12,  Dst.X,   0.U, 1.U, BrType.NOP, 1.U, 1.U), // st.w

        BitPat("b010011_????_????_????_????_?????_?????")   -> row(AluOp.ADD,   LsOp.NOP,   MduOp.NOP,   Src1.PC, Src2.R,    Imm.SI16,  Dst.RD,  1.U, 0.U, BrType.JIRL,1.U, 0.U), // jirl
        BitPat("b010100_????_????_????_????_?????_?????")   -> row(AluOp.NOP,   LsOp.NOP,   MduOp.NOP,   Src1.R,  Src2.R,    Imm.SI26,  Dst.X,   0.U, 0.U, BrType.B,   0.U, 0.U),    // b
        BitPat("b010101_????_????_????_????_?????_?????")   -> row(AluOp.ADD,   LsOp.NOP,   MduOp.NOP,   Src1.PC, Src2.R,    Imm.SI26,  Dst.R1,  1.U, 0.U, BrType.BL,  0.U, 0.U),   // bl
        BitPat("b010110_????_????_????_????_?????_?????")   -> row(AluOp.NOP,   LsOp.NOP,   MduOp.NOP,   Src1.R,  Src2.R,    Imm.SI16,  Dst.X,   0.U, 0.U, BrType.BEQ, 1.U, 1.U),  // beq
        BitPat("b010111_????_????_????_????_?????_?????")   -> row(AluOp.NOP,   LsOp.NOP,   MduOp.NOP,   Src1.R,  Src2.R,    Imm.SI16,  Dst.X,   0.U, 0.U, BrType.BNE, 1.U, 1.U),  // bne
        BitPat("b011000_????_????_????_????_?????_?????")   -> row(AluOp.NOP,   LsOp.NOP,   MduOp.NOP,   Src1.R,  Src2.R,    Imm.SI16,  Dst.X,   0.U, 0.U, BrType.BLT, 1.U, 1.U),  // blt
        BitPat("b011001_????_????_????_????_?????_?????")   -> row(AluOp.NOP,   LsOp.NOP,   MduOp.NOP,   Src1.R,  Src2.R,    Imm.SI16,  Dst.X,   0.U, 0.U, BrType.BGE, 1.U, 1.U),  // bge
        BitPat("b011010_????_????_????_????_?????_?????")   -> row(AluOp.NOP,   LsOp.NOP,   MduOp.NOP,   Src1.R,  Src2.R,    Imm.SI16,  Dst.X,   0.U, 0.U, BrType.BLTU,1.U, 1.U), // bltu
        BitPat("b011011_????_????_????_????_?????_?????")   -> row(AluOp.NOP,   LsOp.NOP,   MduOp.NOP,   Src1.R,  Src2.R,    Imm.SI16,  Dst.X,   0.U, 0.U, BrType.BGEU,1.U, 1.U),  // bgeu

        BitPat("b000001_1001_00_10000_01010_00000_00000")   -> row(AluOp.NOP,   LsOp.NOP,   MduOp.NOP,   Src1.X,  Src2.X,    Imm.X,     Dst.X,   0.U, 0.U, BrType.NOP, 0.U, 0.U), // tlbsrch
        BitPat("b000001_1001_00_10000_01011_00000_00000")   -> row(AluOp.NOP,   LsOp.NOP,   MduOp.NOP,   Src1.X,  Src2.X,    Imm.X,     Dst.X,   0.U, 0.U, BrType.NOP, 0.U, 0.U), // tlbrd
        BitPat("b000001_1001_00_10000_01100_00000_00000")   -> row(AluOp.NOP,   LsOp.NOP,   MduOp.NOP,   Src1.X,  Src2.X,    Imm.X,     Dst.X,   0.U, 0.U, BrType.NOP, 0.U, 0.U), // tlbwr
        BitPat("b000001_1001_00_10000_01101_00000_00000")   -> row(AluOp.NOP,   LsOp.NOP,   MduOp.NOP,   Src1.X,  Src2.X,    Imm.X,     Dst.X,   0.U, 0.U, BrType.NOP, 0.U, 0.U), // tlbfill
        BitPat("b000001_1001_00_10011_?????_?????_?????")   -> row(AluOp.NOP,   LsOp.NOP,   MduOp.NOP,   Src1.R,  Src2.R,    Imm.X,     Dst.X,   0.U, 0.U, BrType.NOP, 1.U, 1.U), // invtlb 

        BitPat("b000001_1000_????_????_????_?????_?????")   -> row(AluOp.ADD,   LsOp.NOP,   MduOp.NOP,   Src1.R,  Src2.IMM,  Imm.SI12,  Dst.X,   0.U, 0.U, BrType.NOP, 1.U, 0.U), // cacop
    )
    
    val decoded = ListLookup(inst, dflt, decodeTable)
    val alu_s  = decoded(0)
    val ls_s   = decoded(1)
    val mdu_s  = decoded(2)
    val src1_s = decoded(3)
    val src2_s = decoded(4)
    val imm_s  = decoded(5)
    val dst_s  = decoded(6)
    val reg_we = decoded(7)
    val mem_we = decoded(8)
    val br_t   = decoded(9)
    val r1_re  = decoded(10)
    val r2_re  = decoded(11)

    val rj = inst(9, 5)
    val rd = inst(4, 0)
    val i12 = inst(21, 10)
    val i16 = inst(25, 10)
    val i20 = inst(24, 5)
    val i26 = Cat(inst(9,0), inst(25, 10))

    io.out.imm := Mux1H(Seq(
        (imm_s === Imm.UI5.asUInt)  -> Cat(0.U(27.W), inst(14, 10)),
        (imm_s === Imm.SI12.asUInt) -> Cat(Fill(20, i12(11)), i12),
        (imm_s === Imm.UI12.asUInt) -> Cat(0.U(20.W), i12),
        (imm_s === Imm.SI16.asUInt) -> Cat(Fill(14, i16(15)), i16, 0.U(2.W)),
        (imm_s === Imm.SI20.asUInt) -> Cat(i20, 0.U(12.W)),
        (imm_s === Imm.SI26.asUInt) -> Cat(Fill(4, i26(25)), i26, 0.U(2.W))
    ))


    io.out.aluOp        := alu_s
    io.out.lsOp         := ls_s
    io.out.mduOp        := mdu_s
    io.out.src1IsPC     := (src1_s === Src1.PC.asUInt)
    io.out.src2IsImm    := (src2_s === Src2.IMM.asUInt)
    io.out.src2IsFour   := (br_t === BrType.JIRL) || (br_t === BrType.BL)
    
    io.out.memWe        := mem_we === 1.U
    io.out.brType       := br_t
    io.out.resFromMem   := (ls_s =/= LsOp.NOP) && (mem_we === 0.U)
    io.out.resFromMulDiv:= (mdu_s =/= MduOp.NOP)

    val is_syscall      = inst === BitPat("b000000_0000_1010110_????_????_????_???")
    val is_break        = inst === BitPat("b000000_0000_1010100_????_????_????_???")
    val is_ertn         = inst === BitPat("b0000_0110_0100_1000_0011_1000_0000_0000")
    val is_csr          = inst(31, 24) === "h04".U

    val is_rdtime_base  = inst === BitPat("b0000_0000_0000_0000_0110_0???_????_????")
    val is_timer_l      = is_rdtime_base && inst(10) === 0.U
    val is_timer_h      = is_rdtime_base && inst(10) === 1.U
    val is_rdcntid      = is_rdtime_base && inst(10) === 0.U && rd === 0.U

    val is_tlbsrch = inst === BitPat("b000001_1001_00_10000_01010_00000_00000") // 06482800
    val is_tlbrd   = inst === BitPat("b000001_1001_00_10000_01011_00000_00000") // 06482C00
    val is_tlbwr   = inst === BitPat("b000001_1001_00_10000_01100_00000_00000") // 06483000
    val is_tlbfill = inst === BitPat("b000001_1001_00_10000_01101_00000_00000") // 06483400
    // invtlb 的 [19:15] 是 10011
    val is_invtlb  = inst === BitPat("b000001_1001_00_10011_?????_?????_?????") && inst(4, 0) <= 6.U

    io.out.tlbOp := MuxCase(TlbOp.NOP, Seq(
        is_tlbsrch -> TlbOp.SRCH,
        is_tlbrd   -> TlbOp.RD,
        is_tlbwr   -> TlbOp.WR,
        is_tlbfill -> TlbOp.FILL,
        is_invtlb  -> TlbOp.INV
    ))
    io.out.invtlb_op := Mux(is_invtlb, inst(4, 0), 0.U)

    //任何修改高级存储状态的指令（写 CRMD.DA/PG、DMW0/1、ASID 的 CSR 指令，以及除了 tlbsrch 外的所有 TLB 指令）
    //都会改变地址翻译行为，在写回级需要清除其后取进流水线的所有指令并重取
    val is_csr_mmu_write = is_csr && io.out.csrWe && (
        io.out.csrNum === "h00".U || // CRMD
        io.out.csrNum === "h18".U || // ASID
        io.out.csrNum === "h180".U|| // DMW0
        io.out.csrNum === "h181".U   // DMW1
    )
    io.out.is_refetch := is_csr_mmu_write || is_tlbrd || is_tlbwr || is_tlbfill || is_invtlb

    io.out.csrWe        := (is_csr && (rj =/= 0.U)) || is_tlbsrch
    io.out.isCsr        := is_csr || is_rdcntid
    io.out.csrNum       := Mux(is_tlbsrch, "h10".U(14.W), 
                           Mux(is_rdcntid, "h40".U(14.W), inst(23, 10)))
    io.out.inst_ertn    := is_ertn

    io.out.rdtimel      := is_timer_l && !is_rdcntid
    io.out.rdtimeh      := is_timer_h

    io.out.regWe        := (reg_we === 1.U) || is_csr || is_rdtime_base
    io.out.destReg :=   Mux(is_rdcntid, rj, 
                        Mux(is_rdtime_base || is_csr, rd, 
                        Mux1H(Seq(
                            (dst_s === Dst.RD.asUInt) -> rd,
                            (dst_s === Dst.RJ.asUInt) -> rj,
                            (dst_s === Dst.R1.asUInt) -> 1.U(5.W)
                        ))))

    val is_tlb_inst     = is_tlbsrch || is_tlbrd || is_tlbwr || is_tlbfill || is_invtlb

    val is_cacop = inst === BitPat("b000001_1000_????_????_????_?????_?????")
    io.out.is_cacop := is_cacop
    io.out.cacop_op := inst(4, 0)

    val inst_valid      =   (alu_s =/= AluOp.NOP) || (ls_s =/= LsOp.NOP) || (mdu_s =/= MduOp.NOP) || (br_t =/= BrType.NOP) || 
                            is_syscall || is_break || is_ertn || is_csr || is_rdtime_base || is_tlb_inst || is_cacop

    io.out.hasException := !inst_valid || is_syscall || is_break
    io.out.ecode        :=  Mux(is_syscall, "h0B".U(6.W), 
                            Mux(is_break,   "h0C".U(6.W), 
                            Mux(!inst_valid,"h0D".U(6.W), 0.U)))

    val rj_is_zero = (rj === 0.U)
    val rj_is_one  = (rj === 1.U)
    val csr_reads_src1 = is_csr && !rj_is_zero && !rj_is_one
    val csr_reads_src2 = is_csr && !rj_is_zero

    io.out.src1_read := inst_valid && (r1_re === 1.U || csr_reads_src1)
    io.out.src2_read := inst_valid && (r2_re === 1.U || csr_reads_src2)
}