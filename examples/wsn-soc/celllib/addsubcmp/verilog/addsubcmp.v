module AddSubCmp #(
  parameter Width = 16
) (
  (* intersynth_conntype = "Bit" *)
  input AddOrSub_i,
  (* intersynth_conntype = "Word" *)
  input[Width-1:0] A_i,
  (* intersynth_conntype = "Word" *)
  input[Width-1:0] B_i,
  (* intersynth_conntype = "Word" *)
  output[Width-1:0] D_o,
  (* intersynth_conntype = "Bit" *)
  input Carry_i,
  (* intersynth_conntype = "Bit" *)
  output Carry_o,
  (* intersynth_conntype = "Bit" *)
  output Zero_o,
  (* intersynth_conntype = "Bit" *)
  output Sign_o,
  (* intersynth_conntype = "Bit" *)
  output Overflow_o
);
  wire [Width:0]   A;
  wire [Width:0]   B;
  wire [Width:0]   Result;
  wire             Carry;
  wire [Width-1:0] D;

  assign A     = {1'b0, A_i};
  assign B     = (AddOrSub_i == 1'b0 ? {1'b0, B_i} : {1'b0, ~B_i});
  assign Carry = (AddOrSub_i == 1'b0 ? Carry_i : ~Carry_i);

  assign Result = A + B + Carry;

  assign D = Result[Width-1:0];
  assign D_o = D;
  
  
  assign Zero_o     = (D == 0 ? 1'b1 : 1'b0);
  assign Sign_o     = Result[Width-1];
  assign Carry_o    = Result[Width];
  assign Overflow_o = ((A[Width-1] == B[Width-1]) && (Result[Width-1] != A[Width-1]) ? 1'b1 : 1'b0);

endmodule
