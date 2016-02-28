// Access internal signals from testbench
// Auto-generated by ../scripts/reconf-module-postproc.tcl

module ExtNames (
  output CpuIntr,
  output Enable,
  output[15:0] SensorValue
);
  assign CpuIntr = core_tb.DUT.MyReconfigLogic_0.MyInterSynthModule_0.cell_80.Out0_o;
  assign Enable = core_tb.DUT.MyReconfigLogic_0.MyInterSynthModule_0.cell_80.In1_i;
  assign SensorValue = core_tb.DUT.MyReconfigLogic_0.MyInterSynthModule_0.cell_66.Q_o;
endmodule
