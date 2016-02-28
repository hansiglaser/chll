module Byte2Word (
  (* intersynth_conntype = "Byte" *)
  input[7:0] H_i,
  (* intersynth_conntype = "Byte" *)
  input[7:0] L_i,
  (* intersynth_conntype = "Word" *)
  output[15:0] Y_o
);

  assign Y_o = {H_i, L_i};

endmodule
