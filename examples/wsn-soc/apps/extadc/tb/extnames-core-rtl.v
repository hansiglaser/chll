module ExtNames (
  output CpuIntr,
  output[15:0] SensorValue,
  output Enable
);
  assign CpuIntr     = core_tb.DUT.MyReconfigLogic_0.ExtADC_0.CpuIntr_o;
  assign SensorValue = core_tb.DUT.MyReconfigLogic_0.ExtADC_0.SensorValue_o;
  assign Enable      = core_tb.DUT.MyReconfigLogic_0.ExtADC_0.Enable_i;
endmodule
