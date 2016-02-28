module Counter32_RV1 (
  input Reset_n_i,
  input Clk_i,
  input Preset_i,
  input Enable_i,
  input[31:0] PresetVal_i,
  output Zero_o
);

  wire ResetSig_s;
  wire Direction_s;
  wire [15:0] DH_s;
  wire [15:0] DL_s;
  wire Overflow_s;

  assign ResetSig_s  = 1'b0;
  assign Direction_s = 1'b1;

  Counter32 ThisCounter (
    .Reset_n_i   (Reset_n_i),
    .Clk_i       (Clk_i),
    .ResetSig_i  (ResetSig_s),
    .Preset_i    (Preset_i),
    .Enable_i    (Enable_i),
    .Direction_i (Direction_s),
    .PresetValH_i(PresetVal_i[31:16]),
    .PresetValL_i(PresetVal_i[15:0]),
    .DH_o        (DH_s),
    .DL_o        (DL_s),
    .Overflow_o  (Overflow_s),
    .Zero_o      (Zero_o)
  );

endmodule

module Counter32_RV1_Timer (
  input Reset_n_i,
  input Clk_i,
  input Preset_i,
  input Enable_i,
  input[31:0] PresetVal_i,
  output Zero_o
);

  reg [31:0] Value;

  always @(negedge Reset_n_i or posedge Clk_i)
  begin
    if (!Reset_n_i)
    begin
      Value <= 'd0;
    end
    else
    begin
      if (Preset_i) begin
        Value <= PresetVal_i;
      end else if (Enable_i)
      begin
        Value <= Value - 1'b1;
      end
    end
  end

  assign Zero_o = (Value == 0 ? 1'b1 : 1'b0);

endmodule



