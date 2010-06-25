module not_switch (out, in);
   output  out;               
   input   in;                
   
   supply1 power;             
   supply0 ground;            
   
   pmos (out, power, in);     
   nmos (out, ground, in);    
   
endmodule

module d_latch_switch(clk,d,q);
input clk,d;
output q;

wire clkn, net1, qn;

not_switch i1 (clkn,clk);

pmos p1 (d,q,clkn);
nmos n1 (d,q,clk);

pmos p2 (q,net1,clk);
nmos n2 (q,net1,clkn);

not_switch i2 (qn,q);
not_switch i3 (net1,qn);

endmodule
