module Byte2WordSel (
  (* intersynth_conntype = "Byte" *)
  input[7:0] H_i,
  (* intersynth_conntype = "Byte" *)
  input[7:0] L_i,
  (* intersynth_conntype = "Word" *)
  output[15:0] Y_o,
  (* intersynth_config = 4 *)
  input[3:0] Shift_i,
  (* intersynth_config = 4 *)
  input[3:0] Mask_i
);
  wire [15:0] Concat  = { H_i, L_i };
  wire [15:0] Shifted = Concat >> Shift_i;
  wire [15:0] Mask    = 16'hFFFF >> (15 - Mask_i);
  wire [15:0] Masked  = Shifted & Mask;

  assign Y_o = Masked;

endmodule
