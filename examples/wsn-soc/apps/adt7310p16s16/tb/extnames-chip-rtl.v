module ExtNames (
  output SPIFSM_Done,
  output CpuIntr,
  output [15:0] SensorValue
);
  assign SPIFSM_Done = chip_tb.DUT.core_1.MyReconfigLogic_0.ADT7310_0.SPIFSM_Done_s;
  assign CpuIntr     = chip_tb.DUT.core_1.MyReconfigLogic_0.ADT7310_0.CpuIntr_o;
  assign SensorValue = chip_tb.DUT.core_1.MyReconfigLogic_0.ADT7310_0.SensorValue_o;
endmodule
