module ExtNames (
  output SPIFSM_Done,
  output CpuIntr,
  output [15:0] SensorValue
);
  assign SPIFSM_Done = core_tb.DUT.MyReconfigLogic_0.ADT7310_0.SPIFSM_Done_s;
  assign CpuIntr     = core_tb.DUT.MyReconfigLogic_0.ADT7310_0.CpuIntr_o;
  assign SensorValue = core_tb.DUT.MyReconfigLogic_0.ADT7310_0.SensorValue_o;
endmodule
