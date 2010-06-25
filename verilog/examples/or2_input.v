//-----------------------------------------------------
// Design Name : or2_input
// File Name   : or2_input.v
// Function    : 2 Input OR Gate Using UDP
// Coder       : Deepak Kumar Tala
//-----------------------------------------------------
primitive or2_input (c,a,b);
  output c;
  input a,b;
  table
    //a b : c
    1 ? : 1;
    ? 1 : 1;
    0 0 : 0;
    0 x : x;
    x 0 : x;
  endtable
endprimitive
