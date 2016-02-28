module ByteRegister (
  (* intersynth_port = "Reset_n_i" *)
  input Reset_n_i,
  (* intersynth_port = "Clk_i" *)
  input Clk_i,
  (* intersynth_conntype = "Byte" *)
  input[7:0] D_i,
  (* intersynth_conntype = "Byte" *)
  output reg [7:0] Q_o,
  (* intersynth_conntype = "Bit" *)
  input Enable_i
);

  always @(negedge Reset_n_i or posedge Clk_i)
  begin
    if (!Reset_n_i)
    begin
      Q_o <= 8'd0;
    end
    else
    begin
      if (Enable_i)
      begin
        Q_o <= D_i;
      end
    end  
  end

endmodule
