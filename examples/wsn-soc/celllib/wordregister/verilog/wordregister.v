module WordRegister (
  (* intersynth_port = "Reset_n_i" *)
  input Reset_n_i,
  (* intersynth_port = "Clk_i" *)
  input Clk_i,
  (* intersynth_conntype = "Word" *)
  input[15:0] D_i,
  (* intersynth_conntype = "Word" *)
  output reg [15:0] Q_o,
  (* intersynth_conntype = "Bit" *)
  input Enable_i
);

  always @(negedge Reset_n_i or posedge Clk_i)
  begin
    if (!Reset_n_i)
    begin
      Q_o <= 16'd0;
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
