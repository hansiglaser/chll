module Byte2WordSel_11MSB (
  input[7:0] H_i,
  input[7:0] L_i,
  output[15:0] Y_o,
);

  wire [3:0] CfgShift_s = 4'd5;
  wire [3:0] CfgMask_s  = 4'd11;

  Byte2WordSel DUT (
    .H_i    (H_i),
    .L_i    (L_i),
    .Y_o    (Y_o),
    .Shift_i(CfgShift_s),
    .Mask_i (CfgMask_s)
  );

endmodule // Byte2WordSel_11MSB

module Byte2WordSel_11MSB_Direct (
  input[7:0] H_i,
  input[7:0] L_i,
  output[15:0] Y_o,
);

  assign Y_o = { 5'b00000, H_i, L_i[7:5] };

endmodule // Byte2WordSel_11MSB
