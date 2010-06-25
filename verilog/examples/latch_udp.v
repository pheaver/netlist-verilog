primitive latch_udp(q, clock, data) ;
output q; reg q ;
input clock, data;
table
  // clock data   q   q+ 
      0    1    : ? : 1 ;
      0    0    : ? : 0 ;
      1    ?    : ? : - ; // - = no change
endtable
endprimitive
