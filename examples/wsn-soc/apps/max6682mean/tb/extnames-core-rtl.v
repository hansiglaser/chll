module ExtNames (
  output SPIFSM_Done,
  output CpuIntr,
  output[15:0] SensorValue,
  output Enable
);
  assign SPIFSM_Done = core_tb.DUT.MyReconfigLogic_0.MAX6682_0.SPI_FSM_Done;
  assign CpuIntr     = core_tb.DUT.MyReconfigLogic_0.MAX6682_0.CpuIntr_o;
  assign SensorValue = core_tb.DUT.MyReconfigLogic_0.MAX6682_0.SensorValue_o;
  assign Enable      = core_tb.DUT.MyReconfigLogic_0.MAX6682_0.Enable_i;
endmodule
