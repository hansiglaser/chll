module AddSubCmp_Less (
  input[15:0] A_i,
  input[15:0] B_i,
  output Less_o
);

  wire [15:0] D_s;
  wire        Carry_s;
  wire        Zero_s;
  wire        Sign_s;
  wire        Overflow_s;

  AddSubCmp ThisAddSubCmp (
    .AddOrSub_i(1'b1),
    .A_i       (B_i),
    .B_i       (A_i),
    .D_o       (D_s),
    .Carry_i   (1'b0),
    .Carry_o   (Carry_s),
    .Zero_o    (Zero_s),
    .Sign_o    (Sign_s),
    .Overflow_o(Overflow_s)
  );

  assign Less_o = Carry_s & ~Zero_s;

endmodule // AddSubCmp_Less

module AddSubCmp_Less_Direct (
  input[15:0] A_i,
  input[15:0] B_i,
  output Less_o
);

  assign Less_o = (A_i < B_i) ? 1'b1 : 1'b0;

endmodule // AddSubCmp_Less
