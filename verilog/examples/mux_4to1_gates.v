//-----------------------------------------------------
// Design Name : mux_4to1_gates
// File Name   : mux_4to1_gates.v
// Function    : 4:1 Mux Using Gates
// Coder       : Deepak Kumar Tala
//-----------------------------------------------------
module mux_4to1_gates(a,b,c,d,sel,y);
input a,b,c,d;
input [1:0] sel;
output y;

wire mux_1,mux_2;

mux_2to1_gates U_mux1 (a,b,sel[0],mux_1),
               U_mux2 (c,d,sel[0],mux_2),
               U_mux3 (mux_1,mux_2,sel[1],y);

endmodule
