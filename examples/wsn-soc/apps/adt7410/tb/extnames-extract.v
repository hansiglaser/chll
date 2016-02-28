module ExtNames (
  output I2CFSM_Done
);
  assign I2CFSM_Done = adt7410_tb.DUT.\I2CFSM_1.Done_o ;
endmodule
