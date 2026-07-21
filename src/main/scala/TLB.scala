package mycpu

import chisel3._
import chisel3.util._
import _root_.circt.stage.ChiselStage

class TlbEntry extends Bundle {
    val e       = Bool()        //Entry enable
    val ps4MB   = Bool()        //1: 2MB page, 0: 4KB page
    val vppn    = UInt(19.W)    //Virtual Page Number
    val asid    = UInt(10.W)    //Address Space ID
    val g       = Bool()        //Global flag (ANDed from lo0 and lo1)
    
    val lo0     = new TlbeloReg()
    val lo1     = new TlbeloReg()
}

class tlb extends Module {
    val io = IO(new Bundle {
        //For IF
        //Input
        val s0_vppn     = Input(UInt(19.W))
        //Distinguish left/right page
        val s0_va_bit12 = Input(Bool())
        val s0_asid     = Input(UInt(10.W))
        //Output
        //Is it founded?
        val s0_found    = Output(Bool())
        //0-15
        val s0_index    = Output(UInt(4.W))
        val s0_ppn      = Output(UInt(20.W))
        //Pagetable Size
        //If 4kb, we use 2^12 (001100)
        //If 2mb, we use 2^21 (010101)
        val s0_ps       = Output(UInt(6.W))
        val s0_plv      = Output(UInt(2.W))
        val s0_mat      = Output(UInt(2.W))
        val s0_d        = Output(Bool())
        val s0_v        = Output(Bool())

        //For EX/MEM
        val s1_vppn     = Input(UInt(19.W))
        val s1_va_bit12 = Input(Bool())
        val s1_asid     = Input(UInt(10.W))
        val s1_found    = Output(Bool())
        val s1_index    = Output(UInt(4.W))
        val s1_ppn      = Output(UInt(20.W))
        val s1_ps       = Output(UInt(6.W))
        val s1_plv      = Output(UInt(2.W))
        val s1_mat      = Output(UInt(2.W))
        val s1_d        = Output(Bool())
        val s1_v        = Output(Bool())
    
        //For INVTLB to delete some of the PTE
        val invtlb_valid = Input(Bool())
        val invtlb_op    = Input(UInt(5.W))
        //0, 1: Invalidate all TLB entries (both global and non-global)
        //4:    Invalidate non-global TLB entries where ASID matches the input
        //5:    Invalidate non-global TLB entries where both ASID and VPN match
        //6:    Invalidate TLB entries where VPN matches

        //For TLBWR / TLBFILL to write PTE
        val we      = Input(Bool())
        val w_index = Input(UInt(4.W))
        val w_dat   = Input(new TlbEntry())

        //For TLBRD to read PTE
        val r_index = Input(UInt(4.W))
        val r_dat   = Output(new TlbEntry())
    })
    val tlb_table = Reg(Vec(16, new TlbEntry()))
    //Write a PTE
    when(io.we) { tlb_table(io.w_index) := io.w_dat }
    //Read a PTE
    io.r_dat := tlb_table(io.r_index)

    //Search Port 0
    val match0 = Wire(Vec(16, Bool()))
    for (i <- 0 until 16) {
        val entry = tlb_table(i)
        val vppn_match = (io.s0_vppn(18, 9) === entry.vppn(18, 9)) && (entry.ps4MB || (io.s0_vppn(8, 0) === entry.vppn(8, 0)))
        match0(i) := entry.e && vppn_match && (entry.asid === io.s0_asid || entry.g)
    }

    io.s0_found := match0.asUInt =/= 0.U
    io.s0_index := PriorityEncoder(match0) // 保留这个给 TLBSRCH 用
    
    // ★ 时序杀招：跨过编码器，直接独热码捞数据！
    val hit0 = Mux1H(match0, tlb_table)
    val sel0 = Mux(hit0.ps4MB, io.s0_vppn(8), io.s0_va_bit12)
    val selected_lo0 = Mux(sel0, hit0.lo1, hit0.lo0)

    io.s0_ppn := selected_lo0.ppn
    io.s0_plv := selected_lo0.plv
    io.s0_mat := selected_lo0.mat
    io.s0_d   := selected_lo0.d
    io.s0_v   := selected_lo0.v
    io.s0_ps  := Mux(hit0.ps4MB, 21.U(6.W), 12.U(6.W))

    //Search Port 1
    val match1 = Wire(Vec(16, Bool()))
    for (i <- 0 until 16) {
        val entry = tlb_table(i)
        val vppn_match = (io.s1_vppn(18, 9) === entry.vppn(18, 9)) &&  (entry.ps4MB || (io.s1_vppn(8, 0) === entry.vppn(8, 0)))
        match1(i) := entry.e && vppn_match && (entry.asid === io.s1_asid || entry.g)
    }

    io.s1_found := match1.asUInt =/= 0.U
    io.s1_index := PriorityEncoder(match1)
    
    // ★ 时序杀招：跨过编码器，直接独热码捞数据！
    val hit1 = Mux1H(match1, tlb_table)
    val sel1 = Mux(hit1.ps4MB, io.s1_vppn(8), io.s1_va_bit12)
    val selected_lo1 = Mux(sel1, hit1.lo1, hit1.lo0)
    
    io.s1_ppn := selected_lo1.ppn
    io.s1_plv := selected_lo1.plv
    io.s1_mat := selected_lo1.mat
    io.s1_d   := selected_lo1.d
    io.s1_v   := selected_lo1.v
    io.s1_ps  := Mux(hit1.ps4MB, 21.U(6.W), 12.U(6.W))

    // INVTLB
    when(io.invtlb_valid) {
        for (i <- 0 until 16) {
            val entry = tlb_table(i)
            val cond1 = !entry.g
            val cond2 = entry.g
            val cond3 = (io.s1_asid === entry.asid)
            val cond4 = (io.s1_vppn(18, 9) === entry.vppn(18, 9)) && (entry.ps4MB || (io.s1_vppn(8, 0) === entry.vppn(8, 0)))

            val should_inv = MuxLookup(io.invtlb_op, false.B)(Seq(
                0.U -> (cond1 || cond2),
                1.U -> (cond1 || cond2),
                2.U -> cond2,
                3.U -> cond1,
                4.U -> (cond1 && cond3),
                5.U -> (cond1 && cond3 && cond4),
                6.U -> ((cond2 || cond3) && cond4)
            ))
            when(should_inv) {
                val updated_entry = WireDefault(entry)
                updated_entry.e := false.B
                tlb_table(i) := updated_entry
            }
        }
    }
}