/**
* The ex.app MAX6682Mean implements an accumulator, which gets either a new
* value or adds to its old value (or keeps the old value). This is implemented
* with two dedicated signals "StoreValue" and "AddValue" (compare the proces
* with both enable signals below in WordRegister_Mux_Direct). Yosys creates
* a MUX topology where one MUX selects between the old value and the added
* value, and the second MUX decides between this result and the new value.
*
* This is doesn't directly fit our simple "Enable" WordRegister, because its
* MUX (directly at the input of the register) selects between the old and the
* new value.
*
* Here we define a "reduced variant" (in reality it is an extended variant)
* which swaps these two MUXes and generates the apropriate control signals.
* The introduced OR gate for the two Enable signals will be swallowed by the
* controlling FSM and the remaining MUX will be coverted by the cell
* "WordMuxDual".
*/
module WordRegister_Mux (
  input Reset_n_i,
  input Clk_i,
  input[15:0] D1_i,
  input[15:0] D2_i,
  output[15:0] Q_o,
  input Enable1_i,
  input Enable2_i
);

  wire Enable_s;
  wire [15:0] D_s;

  assign Enable_s = Enable1_i | Enable2_i;
  assign D_s = (Enable1_i ? D1_i : D2_i);

  WordRegister ThisWordRegister (
    .Reset_n_i(Reset_n_i),
    .Clk_i    (Clk_i),
    .D_i      (D_s),
    .Q_o      (Q_o),
    .Enable_i (Enable_s)
  );

endmodule // WordRegister_Mux

module WordRegister_Mux_Direct (
  input Reset_n_i,
  input Clk_i,
  input[15:0] D1_i,
  input[15:0] D2_i,
  output reg [15:0] Q_o,
  input Enable1_i,
  input Enable2_i
);

  always @(negedge Reset_n_i or posedge Clk_i)
  begin
    if (!Reset_n_i)
    begin
      Q_o <= 16'd0;
    end
    else
    begin
      if (Enable1_i)
      begin
        Q_o <= D1_i;
      end
      else if (Enable2_i)
      begin
        Q_o <= D2_i;
      end
    end  
  end

endmodule // WordRegister_Mux_Direct
