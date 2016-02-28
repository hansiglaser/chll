module ExtNames (
  output I2CFSM_Done
);
  assign I2CFSM_Done = tmp421_tb.DUT.I2CFSM_Done_s;
endmodule
