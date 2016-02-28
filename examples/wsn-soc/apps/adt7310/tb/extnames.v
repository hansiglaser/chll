module ExtNames (
  output SPIFSM_Done
);
  assign SPIFSM_Done = adt7310_tb.DUT.SPIFSM_Done_s;
endmodule
