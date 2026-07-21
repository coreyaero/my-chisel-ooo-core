package mycpu

import chisel3._
import chisel3.util._

class Ctrl extends Module {
    val io = IO(new Bundle {
        val ex_branch_req = Input(Bool())
        val ex_branch_pc  = Input(UInt(32.W))
        
        val wb_flush      = Input(Bool())
        val wb_target_pc  = Input(UInt(32.W))

        val next_pc_flush = Output(Bool())
        val next_pc       = Output(UInt(32.W))

        val flush_if      = Output(Bool())
        val flush_id      = Output(Bool())
        val flush_ex      = Output(Bool())
        val flush_mem     = Output(Bool())
        val flush_wb      = Output(Bool())
    })

    val do_flush = io.wb_flush || io.ex_branch_req
    val target_pc = Mux(io.wb_flush, io.wb_target_pc, io.ex_branch_pc)

    io.next_pc_flush := do_flush
    io.next_pc       := target_pc

    io.flush_if  := do_flush
    io.flush_id  := do_flush
    
    io.flush_ex  := io.wb_flush 
    io.flush_mem := io.wb_flush
    io.flush_wb  := false.B 
}