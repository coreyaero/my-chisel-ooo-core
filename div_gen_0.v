module div_gen_0(
  input         aclk,
  input         aresetn,
  input         s_axis_divisor_tvalid,
  input  [31:0] s_axis_divisor_tdata,
  input         s_axis_dividend_tvalid,
  input  [31:0] s_axis_dividend_tdata,
  output        m_axis_dout_tvalid,
  output [63:0] m_axis_dout_tdata
);
  
  reg [33:0] valid_pipe;
  reg [63:0] data_pipe [0:33];
  
  integer i;
  
  initial begin
      valid_pipe = 34'b0;
      for (i = 0; i < 34; i = i + 1) data_pipe[i] = 64'b0;
  end
  
  always @(posedge aclk) begin
      if (!aresetn) begin
          valid_pipe <= 34'b0;
      end else begin
          valid_pipe[0] <= s_axis_divisor_tvalid & s_axis_dividend_tvalid;
          
          if (s_axis_divisor_tvalid & s_axis_dividend_tvalid) begin
              if (s_axis_divisor_tdata == 32'b0) begin
                  data_pipe[0] <= {32'hffffffff, s_axis_dividend_tdata};
              end else begin
                  data_pipe[0] <= { (s_axis_dividend_tdata / s_axis_divisor_tdata), 
                                    (s_axis_dividend_tdata % s_axis_divisor_tdata) };
              end
          end
          
          for (i = 1; i < 34; i = i + 1) begin
              valid_pipe[i] <= valid_pipe[i-1];
              data_pipe[i]  <= data_pipe[i-1];
          end
      end
  end
  
  assign m_axis_dout_tvalid = valid_pipe[33];
  assign m_axis_dout_tdata  = data_pipe[33];
  
endmodule

