
package mycpu

import chisel3._
import chisel3.util._


class div_gen_0 extends ExtModule {
    val io = FlatIO(new Bundle {
        val aclk                    = Input(Clock())
        val s_axis_divisor_tvalid   = Input(Bool())
        val s_axis_divisor_tdata    = Input(UInt(32.W))
        val s_axis_dividend_tvalid  = Input(Bool())
        val s_axis_dividend_tdata   = Input(UInt(32.W))
        val m_axis_dout_tvalid      = Output(Bool())
        val m_axis_dout_tdata       = Output(UInt(64.W))
    })

    setInline("div_gen_0.v",
        """module div_gen_0(
        |  input         aclk,
        |  input         s_axis_divisor_tvalid,
        |  input  [31:0] s_axis_divisor_tdata,
        |  input         s_axis_dividend_tvalid,
        |  input  [31:0] s_axis_dividend_tdata,
        |  output        m_axis_dout_tvalid,
        |  output [63:0] m_axis_dout_tdata
        |);
        |  
        |  reg [33:0] valid_pipe;
        |  reg [63:0] data_pipe [0:33];
        |  
        |  integer i;
        |  
        |  initial begin
        |      valid_pipe = 34'b0;
        |      for (i = 0; i < 34; i = i + 1) data_pipe[i] = 64'b0;
        |  end
        |  
        |  always @(posedge aclk) begin
        |      valid_pipe[0] <= s_axis_divisor_tvalid & s_axis_dividend_tvalid;
        |      
        |      if (s_axis_divisor_tvalid & s_axis_dividend_tvalid) begin
        |          if (s_axis_divisor_tdata == 32'b0) begin
        |              data_pipe[0] <= {32'hffffffff, s_axis_dividend_tdata};
        |          end else begin
        |              data_pipe[0] <= { (s_axis_dividend_tdata / s_axis_divisor_tdata), 
        |                                (s_axis_dividend_tdata % s_axis_divisor_tdata) };
        |          end
        |      end
        |      
        |      for (i = 1; i < 34; i = i + 1) begin
        |          valid_pipe[i] <= valid_pipe[i-1];
        |          data_pipe[i]  <= data_pipe[i-1];
        |      end
        |  end
        |  
        |  assign m_axis_dout_tvalid = valid_pipe[33];
        |  assign m_axis_dout_tdata  = data_pipe[33];
        |  
        |endmodule
        |""".stripMargin)
}
class Multiplier extends Module {
    val io = IO(new Bundle {
        val src1     = Input(UInt(32.W))
        val src2     = Input(UInt(32.W))
        val isSigned = Input(Bool())
        val result64 = Output(UInt(64.W))
    })
    val signedRes   = RegNext(io.src1.asSInt * io.src2.asSInt).asUInt
    val unsignedRes = RegNext(io.src1 * io.src2)
    io.result64    := Mux(io.isSigned, signedRes, unsignedRes)
}

class Divider extends Module {
    val io = IO(new Bundle {
        val enable = Input(Bool())
        val a      = Input(UInt(32.W))
        val b      = Input(UInt(32.W))
        val q      = Output(UInt(32.W))
        val r      = Output(UInt(32.W))
        val done   = Output(Bool())
    })

    val div_ip = Module(new div_gen_0())
    div_ip.io.aclk                   := clock
    div_ip.io.s_axis_dividend_tvalid := io.enable
    div_ip.io.s_axis_dividend_tdata  := io.a
    div_ip.io.s_axis_divisor_tvalid  := io.enable
    div_ip.io.s_axis_divisor_tdata   := io.b
    
    io.done := div_ip.io.m_axis_dout_tvalid
    io.q    := div_ip.io.m_axis_dout_tdata(63, 32)
    io.r    := div_ip.io.m_axis_dout_tdata(31, 0)
}