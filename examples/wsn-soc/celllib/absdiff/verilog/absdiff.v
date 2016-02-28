module AbsDiff (
  (* intersynth_conntype = "Word" *)
  input[15:0] A_i,
  (* intersynth_conntype = "Word" *)
  input[15:0] B_i,
  (* intersynth_conntype = "Word" *)
  output[15:0] D_o
);

  wire [16 : 0] DiffAB;
  wire [15 : 0] DiffBA;
  assign DiffAB = {1'b0, A_i} - {1'b0, B_i};
  assign DiffBA =        B_i  -        A_i;
  assign D_o = DiffAB[16] ? DiffBA : DiffAB[15 : 0];

endmodule
