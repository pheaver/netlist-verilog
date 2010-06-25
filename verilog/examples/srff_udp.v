primitive srff_udp (q,s,r);
output q;
input s,r;

reg q;

initial q = 1'b1;

table
  // s r q q+
  1 0 : ? : 1 ;
  f 0 : 1 : - ;
  0 r : ? : 0 ;
  0 f : 0 : - ;
  1 1 : ? : 0 ;
endtable

endprimitive
