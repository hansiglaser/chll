module ByteMuxDual (
  (* intersynth_conntype = "Byte" *)
  input[7:0] A_i,
  (* intersynth_conntype = "Byte" *)
  input[7:0] B_i,
  (* intersynth_conntype = "Bit" *)
  input S_i,
  (* intersynth_conntype = "Byte" *)
  output[7:0] Y_o
);

assign     Y_o = S_i ? B_i : A_i;

endmodule
