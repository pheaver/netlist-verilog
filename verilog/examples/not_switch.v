//-----------------------------------------------------
// Design Name : not_switch
// File Name   : not_switch.v
// Function    : NOT Gate Using Switch Primitives
// Coder       : Deepak Kumar Tala
//-----------------------------------------------------
module not_switch (out, in);
   output  out;               
   input   in;                
   
   supply1 power;             
   supply0 ground;            
   
   pmos (out, power, in);     
   nmos (out, ground, in);    
   
endmodule
