module ExtNames (
  output SPIFSM_Done
);
  assign SPIFSM_Done = adt7310p32ls16_tb.DUT.SPIFSM_Done_s;
endmodule
