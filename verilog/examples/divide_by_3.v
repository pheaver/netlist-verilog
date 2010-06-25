//-----------------------------------------------------
// Design Name : divide_by_3 
// File Name   : divide_by_3.v
// Function    : Divide By 3
// Coder       : Deepak Kumar Tala
//-----------------------------------------------------
module divide_by_3 (
clk_in       , //Input Clock
reset        , // Reset Input
clk_out        // Output Clock
);
//-----------Input Ports---------------
input clk_in;
input  reset;
//-----------Output Ports---------------
output clk_out;
//------------Internal Variables--------
reg [1:0] pos_cnt;
reg [1:0] neg_cnt;
//-------------Code Start-----------------
// Posedge counter
always @ (posedge clk_in)
if (reset) begin
  pos_cnt <= 0;
end else begin
  pos_cnt <= (pos_cnt == 2) ? 0 : pos_cnt + 1;
end
// Neg edge counter
always @ (negedge clk_in)
if (reset) begin
  neg_cnt <= 0;
end else begin
  neg_cnt <= (neg_cnt == 2) ? 0 : neg_cnt + 1;
end

assign clk_out = ((pos_cnt != 2) && (neg_cnt != 2));

endmodule

// Testbench to check the divide_by_3 logic
module test();
reg reset, clk_in;
wire clk_out;
divide_by_3 U (
  .clk_in (clk_in),
  .reset  (reset),
  .clk_out (clk_out)
);

initial begin
   clk_in = 0;
   reset = 0;
   #2 reset = 1;
   #2 reset = 0;
   #100 $finish;
end

always #1 clk_in = ~clk_in;

endmodule
