module AddSubCmp_Add (
  input[15:0] A_i,
  input[15:0] B_i,
  output[15:0] D_o
);

  wire        Carry_s;
  wire        Zero_s;
  wire        Sign_s;
  wire        Overflow_s;

  AddSubCmp ThisAddSubCmp (
    .AddOrSub_i(1'b0),
    .A_i       (A_i),
    .B_i       (B_i),
    .D_o       (D_o),
    .Carry_i   (1'b0),
    .Carry_o   (Carry_s),
    .Zero_o    (Zero_s),
    .Sign_o    (Sign_s),
    .Overflow_o(Overflow_s)
  );

endmodule // AddSubCmp_Add

module AddSubCmp_Add_Direct (
  input[15:0] A_i,
  input[15:0] B_i,
  output[15:0] D_o
);

  assign D_o = A_i + B_i;

endmodule // AddSubCmp_Add_Direct

