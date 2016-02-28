module ExtNames (
  output I2CFSM_Done,
  output CpuIntr,
  output[15:0] SensorValue
);
  assign I2CFSM_Done = chip_tb.DUT.core_1.MyReconfigLogic_0.ADT7410_0.I2CFSM_Done_s;
  assign CpuIntr     = chip_tb.DUT.core_1.MyReconfigLogic_0.ADT7410_0.CpuIntr_o;
  assign SensorValue = chip_tb.DUT.core_1.MyReconfigLogic_0.ADT7410_0.SensorValue_o;
endmodule
