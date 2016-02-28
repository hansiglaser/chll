module ExtNames (
  output I2CFSM_Done,
  output CpuIntr,
  output[15:0] SensorValue
);
  assign I2CFSM_Done = core_tb.DUT.MyReconfigLogic_0.ADT7410_0.I2CFSM_Done_s;
  assign CpuIntr     = core_tb.DUT.MyReconfigLogic_0.ADT7410_0.CpuIntr_o;
  assign SensorValue = core_tb.DUT.MyReconfigLogic_0.ADT7410_0.SensorValue_o;
endmodule
