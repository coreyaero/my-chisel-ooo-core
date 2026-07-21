package mycpu

import chisel3._
import chisel3.util._

class ALU extends Module{
    val io = IO(new Bundle{
        val aluOp   = Input(UInt(12.W))
        val src1    = Input(UInt(32.W))
        val src2    = Input(UInt(32.W))
        val res     = Output(UInt(32.W))
    })

    val Seq(opAdd, opSub, opSlt, opSltu, opAnd, opNor, opOr, opXor, opSll, opSrl, opSra, opLui) = io.aluOp.asBools

    val adderCin = opSub | opSlt | opSltu
    val adderB = Mux(adderCin, ~io.src2, io.src2)
    val adderRes = io.src1 +& adderB + adderCin

    val addsubRes = adderRes(31, 0)
    val adderCout = adderRes(32)

    val sltBit0 = (io.src1(31) & ~io.src2(31)) | ((io.src1(31) === io.src2(31)) & addsubRes(31))
    val sltRes = Cat(0.U(31.W), sltBit0)
    val sltuRes = Cat(0.U(31.W), ~adderCout)

    val shamt = io.src2(4, 0)
    val sllRes = io.src1 << shamt
    val srlRes = io.src1 >> shamt
    val sraRes = (io.src1.asSInt >> shamt).asUInt

    io.res := Mux1H(Seq(
        opAdd  -> addsubRes,
        opSub  -> addsubRes,
        opSlt  -> sltRes,
        opSltu -> sltuRes,
        opAnd  -> (io.src1 & io.src2),
        opNor  -> ~(io.src1 | io.src2),
        opOr   -> (io.src1 | io.src2),
        opXor  -> (io.src1 ^ io.src2),
        opSll  -> sllRes,
        opSrl  -> srlRes,
        opSra  -> sraRes,
        opLui  -> io.src2
    ))
}