//-----------------------------------------------------
// Design Name : dff_udp 
// File Name   : dff_udp.v
// Function    : D Flip Flop
// Coder       : Deepak Kumar Tala
//-----------------------------------------------------
primitive dff_udp (q,clk,d);
  input clk,d;
  output q;
  reg q;
  table
    // clk d : q : q+
        r  0 : ? : 0 ;
        r  1 : ? : 1 ;
        f  ? : ? : - ;
        ?  * : ? : - ;
  endtable
endprimitive
