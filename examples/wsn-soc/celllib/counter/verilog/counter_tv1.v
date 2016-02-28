module Counter_TV1 #(
  parameter Width = 16
) (
  (* intersynth_port = "Reset_n_i" *)
  input Reset_n_i,
  (* intersynth_port = "Clk_i" *)
  input Clk_i,
  (* intersynth_conntype = "Bit" *)
  input ResetSig_i,
  (* intersynth_conntype = "Bit" *)
  input Preset_i,
  (* intersynth_conntype = "Bit" *)
  input Enable_i,
  (* intersynth_conntype = "Bit" *)
  input Direction_i,
  (* intersynth_conntype = "Word" *)
  input[Width-1:0] PresetVal_i,
  (* intersynth_conntype = "Word" *)
  output[Width-1:0] D_o,
  (* intersynth_conntype = "Bit" *)
  output Overflow_o,
  (* intersynth_conntype = "Bit" *)
  output Zero_o
);

  reg [Width-1:0] Value;
  reg             Ovfl;

  always @(negedge Reset_n_i or posedge Clk_i)
  begin
    if (!Reset_n_i)
    begin
      Value   <=  'd0;
      Ovfl    <= 1'd0;
    end
    else
    begin
      if (ResetSig_i) begin
        Value <=  'd0;
        Ovfl  <= 1'b0;
      end else if (Preset_i) begin
        Value <= PresetVal_i;
        Ovfl  <= 1'b0;
      end else if (Enable_i)
      begin
        if (!Direction_i)
          {Ovfl, Value} <= Value + 1'b1;
        else
          {Ovfl, Value} <= Value - 1'b1;
      end
    end
  end

  assign D_o = Value;

  assign Zero_o = (Value == 0 ? 1'b1 : 1'b0);

  assign Overflow_o = Ovfl;

endmodule
