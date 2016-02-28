module ByteMuxQuad (
  (* intersynth_conntype = "Byte" *)
  input[7:0] A_i,
  (* intersynth_conntype = "Byte" *)
  input[7:0] B_i,
  (* intersynth_conntype = "Byte" *)
  input[7:0] C_i,
  (* intersynth_conntype = "Byte" *)
  input[7:0] D_i,
  (* intersynth_conntype = "Bit" *)
  input SAB_i,
  (* intersynth_conntype = "Bit" *)
  input SC_i,
  (* intersynth_conntype = "Bit" *)
  input SD_i,
  (* intersynth_conntype = "Byte" *)
  output[7:0] Y_o
);

wire [7:0] AB  = SAB_i ? B_i : A_i;
wire [7:0] ABC = SC_i ? C_i : AB;
assign     Y_o = SD_i ? D_i : ABC;

endmodule
