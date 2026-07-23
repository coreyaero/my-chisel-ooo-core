package mycpu

import chisel3._
import chisel3.util._

class ALU extends Module{
    val io = IO(new Bundle{
        val aluOp   = Input(UInt(13.W))
        val src1    = Input(UInt(32.W))
        val src2    = Input(UInt(32.W))
        val res     = Output(UInt(32.W))
    })

    val Seq(opAdd, opSub, opSlt, opSltu, opAnd, opNor, opOr, opXor, opSll, opSrl, opSra, opLui, opCpucfg) = io.aluOp.asBools

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

    // ★ 新增：Cache 拓扑结构常数生成 (4路、256组、32B行长)
    // =========================================================================
        // ★ CPUCFG 满血配置表 (龙芯 LA32R 规范)
        // =========================================================================
        val cpucfgRes = MuxLookup(io.src1, 0.U(32.W))(Seq(
            // --- 0x00 ~ 0x05: 基础架构与扩展能力 ---
            "h00".U -> "h0014C010".U, // PRID: 处理器标识 (LA32R 特征码)
            "h01".U -> "h00000001".U, // ISA: 标明为 LA32 架构
            "h02".U -> "h00000000".U, // Extensions: 纯整数基础指令集 (无浮点、向量扩展)
            
            // --- 0x03: MMU 配置 (极其重要) ---
            // [14:12] DMW数量-1 = 1 (你有 dmw0 和 dmw1)
            // [11:0]  TLB表项-1 = 15 (你的 tlb_table 是 16 项)
            "h03".U -> "h0000100F".U, 
            
            "h04".U -> "h00000000".U, // OS Features: 无硬件页表漫游等高级特性
            "h05".U -> "h00000000".U, // OS Features

            // --- 0x10 ~ 0x14: 缓存拓扑结构 ---
            "h10".U -> "h00000005".U, // L1 Cache 存在性: ICache(bit 0) = 1, DCache(bit 2) = 1
            
            // L1 ICache: Offset=5 (32B), Index=8 (256 Sets), Ways=3 (4 Ways) -> 0x05080003
            "h11".U -> "h05080003".U, 
            
            // L1 DCache: Offset=5 (32B), Index=8 (256 Sets), Ways=3 (4 Ways) -> 0x05080003
            "h12".U -> "h05080003".U, 
            
            // L2 / L3 Cache 必须明确返回 0，彻底阻断软件去初始化不存在的外缓！
            "h13".U -> "h00000000".U, 
            "h14".U -> "h00000000".U  
        ))

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
        opLui  -> io.src2,
        opCpucfg -> cpucfgRes // ★ 新增：输出配置结果
    ))
}