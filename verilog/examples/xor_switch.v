module nor2_switch (a,b,y);
input a, b;
output y;

supply1 power;
supply0 ground;

wire  connect;

nmos (y,ground,a);
nmos (y,ground,b);
pmos (y,connect,b);
pmos (power,connect,a);

endmodule
