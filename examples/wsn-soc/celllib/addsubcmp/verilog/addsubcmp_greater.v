module AddSubCmp_Greater (
  input[15:0] A_i,
  input[15:0] B_i,
  output Greater_o
);

  wire [15:0] D_s;
  wire        Carry_s;
  wire        Zero_s;
  wire        Sign_s;
  wire        Overflow_s;

  AddSubCmp ThisAddSubCmp (
    .AddOrSub_i(1'b1),
    .A_i       (A_i),
    .B_i       (B_i),
    .D_o       (D_s),
    .Carry_i   (1'b0),
    .Carry_o   (Carry_s),
    .Zero_o    (Zero_s),
    .Sign_o    (Sign_s),
    .Overflow_o(Overflow_s)
  );

  assign Greater_o = Carry_s & ~Zero_s;

endmodule // AddSubCmp_Greater

module AddSubCmp_Greater_Direct (
  input[15:0] A_i,
  input[15:0] B_i,
  output Greater_o
);

  assign Greater_o = (A_i > B_i) ? 1'b1 : 1'b0;

endmodule // AddSubCmp_Greater_Direct
