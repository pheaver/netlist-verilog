primitive xor2_input (c,a,b);
  output c;
  input a,b;
  table
     0 0 : 0;
     0 1 : 1;
     1 0 : 1;
     1 1 : 0;
     x 1 : x;
     1 x : x;
     x 0 : x;
     0 x : x;
     x x : x;
  endtable
endprimitive
