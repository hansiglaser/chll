module ExtNames (
  output SPIFSM_Done,
  output CpuIntr,
  output[15:0] SensorValue,
  output Enable
);
  assign SPIFSM_Done = chip_tb.DUT.core_1.MyReconfigLogic_0.MAX6682_0.SPI_FSM_Done;
  assign CpuIntr     = chip_tb.DUT.core_1.MyReconfigLogic_0.MAX6682_0.CpuIntr_o;
  assign SensorValue = chip_tb.DUT.core_1.MyReconfigLogic_0.MAX6682_0.SensorValue_o;
  assign Enable      = chip_tb.DUT.core_1.MyReconfigLogic_0.MAX6682_0.Enable_i;
endmodule
