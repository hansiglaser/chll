module WordMuxDual (
  (* intersynth_conntype = "Word" *)
  input[15:0] A_i,
  (* intersynth_conntype = "Word" *)
  input[15:0] B_i,
  (* intersynth_conntype = "Bit" *)
  input S_i,
  (* intersynth_conntype = "Word" *)
  output[15:0] Y_o
);

assign     Y_o = S_i ? B_i : A_i;

endmodule
