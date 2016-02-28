(* src = "../../verilog/extadc.v:1", top = 1 *)
module ExtADC (
  (* intersynth_port = "Reset_n_i", src = "../../verilog/extadc.v:3" *)
  input Reset_n_i,
  (* intersynth_port = "Clk_i", src = "../../verilog/extadc.v:5" *)
  input Clk_i,
  (* intersynth_conntype = "Bit", intersynth_port = "ReconfModuleIn_s", src = "../../verilog/extadc.v:7" *)
  input Enable_i,
  (* intersynth_conntype = "Bit", intersynth_port = "ReconfModuleIRQs_s", src = "../../verilog/extadc.v:9" *)
  output CpuIntr_o,
  (* intersynth_conntype = "Bit", intersynth_port = "Outputs_o", src = "../../verilog/extadc.v:11" *)
  output SensorPower_o,
  (* intersynth_conntype = "Bit", intersynth_port = "Outputs_o", src = "../../verilog/extadc.v:13" *)
  output SensorStart_o,
  (* intersynth_conntype = "Bit", intersynth_port = "Inputs_i", src = "../../verilog/extadc.v:15" *)
  input SensorReady_i,
  (* intersynth_conntype = "Bit", intersynth_port = "AdcDoConvert_o", src = "../../verilog/extadc.v:17" *)
  output AdcStart_o,
  (* intersynth_conntype = "Bit", intersynth_port = "AdcConvComplete_i", src = "../../verilog/extadc.v:19" *)
  input AdcDone_i,
  (* intersynth_conntype = "Word", intersynth_port = "AdcValue_i", src = "../../verilog/extadc.v:21" *)
  input[15:0] AdcValue_i,
  (* intersynth_conntype = "Word", intersynth_param = "PeriodCounterPreset_i", src = "../../verilog/extadc.v:23" *)
  input[15:0] PeriodCounterPreset_i,
  (* intersynth_conntype = "Word", intersynth_param = "SensorValue_o", src = "../../verilog/extadc.v:25" *)
  output[15:0] SensorValue_o,
  (* intersynth_conntype = "Word", intersynth_param = "Threshold_i", src = "../../verilog/extadc.v:27" *)
  input[15:0] Threshold_i
);

  (* src = "../../../../addsubcmp/verilog/addsubcmp_greater.v:8" *)
  wire \$extract$\AddSubCmp_Greater_Direct$728.Carry_s ;
  (* src = "../../../../addsubcmp/verilog/addsubcmp_greater.v:7" *)
  wire [15:0] \$extract$\AddSubCmp_Greater_Direct$728.D_s ;
  (* src = "../../../../addsubcmp/verilog/addsubcmp_greater.v:11" *)
  wire \$extract$\AddSubCmp_Greater_Direct$728.Overflow_s ;
  (* src = "../../../../addsubcmp/verilog/addsubcmp_greater.v:10" *)
  wire \$extract$\AddSubCmp_Greater_Direct$728.Sign_s ;
  (* src = "../../../../addsubcmp/verilog/addsubcmp_greater.v:9" *)
  wire \$extract$\AddSubCmp_Greater_Direct$728.Zero_s ;
  (* src = "../../../../counter/verilog/counter_rv1.v:14" *)
  wire [15:0] \$extract$\Counter_RV1_Timer$725.D_s ;
  (* src = "../../../../counter/verilog/counter_rv1.v:15" *)
  wire \$extract$\Counter_RV1_Timer$725.Overflow_s ;
  (* src = "../../verilog/extadc.v:167" *)
  wire [15:0] AbsDiffResult;
  (* src = "../../verilog/extadc.v:43" *)
  wire StoreNewValue;
  (* src = "../../verilog/extadc.v:41" *)
  wire TimerEnable;
  (* src = "../../verilog/extadc.v:39" *)
  wire TimerOvfl;
  (* src = "../../verilog/extadc.v:40" *)
  wire TimerPreset;
  wire FSM_1_Out7_s;
  wire FSM_1_Out8_s;
  wire FSM_1_Out9_s;
  wire FSM_1_Out10_s;
  wire FSM_1_Out11_s;
  wire FSM_1_Out12_s;
  wire FSM_1_Out13_s;
  wire FSM_1_Out14_s;
  wire FSM_1_CfgMode_s;
  wire FSM_1_CfgClk_s;
  wire FSM_1_CfgShift_s;
  wire FSM_1_CfgDataIn_s;
  wire FSM_1_CfgDataOut_s;


  AbsDiff \$extract$\AbsDiff$726  (
    .A_i(AdcValue_i),
    .B_i(SensorValue_o),
    .D_o(AbsDiffResult)
  );

  (* src = "../../../../addsubcmp/verilog/addsubcmp_greater.v:13" *)
  AddSubCmp \$extract$\AddSubCmp_Greater_Direct$728.ThisAddSubCmp  (
    .A_i(AbsDiffResult),
    .AddOrSub_i(1'b1),
    .B_i(Threshold_i),
    .Carry_i(1'b0),
    .Carry_o(\$extract$\AddSubCmp_Greater_Direct$728.Carry_s ),
    .D_o(\$extract$\AddSubCmp_Greater_Direct$728.D_s ),
    .Overflow_o(\$extract$\AddSubCmp_Greater_Direct$728.Overflow_s ),
    .Sign_o(\$extract$\AddSubCmp_Greater_Direct$728.Sign_s ),
    .Zero_o(\$extract$\AddSubCmp_Greater_Direct$728.Zero_s )
  );

  (* src = "../../../../counter/verilog/counter_rv1.v:20" *)
  Counter \$extract$\Counter_RV1_Timer$725.ThisCounter  (
    .Clk_i(Clk_i),
    .D_o(\$extract$\Counter_RV1_Timer$725.D_s ),
    .Direction_i(1'b1),
    .Enable_i(TimerEnable),
    .Overflow_o(\$extract$\Counter_RV1_Timer$725.Overflow_s ),
    .PresetVal_i(PeriodCounterPreset_i),
    .Preset_i(TimerPreset),
    .ResetSig_i(1'b0),
    .Reset_n_i(Reset_n_i),
    .Zero_o(TimerOvfl)
  );

  WordRegister \$extract$\WordRegister$727  (
    .Clk_i(Clk_i),
    .D_i(AdcValue_i),
    .Enable_i(StoreNewValue),
    .Q_o(SensorValue_o),
    .Reset_n_i(Reset_n_i)
  );

  FSM FSM_1 (
    .Reset_n_i(Reset_n_i),
    .Clk_i(Clk_i),
    .In0_i(AdcDone_i),
    .In1_i(Enable_i),
    .In2_i(SensorReady_i),
    .In3_i(TimerOvfl),
    .In4_i(\$extract$\AddSubCmp_Greater_Direct$728.Carry_s ),
    .In5_i(\$extract$\AddSubCmp_Greater_Direct$728.Zero_s ),
    .In6_i(1'b0),
    .In7_i(1'b0),
    .Out0_o(CpuIntr_o),
    .Out1_o(SensorStart_o),
    .Out2_o(StoreNewValue),
    .Out3_o(AdcStart_o),
    .Out4_o(SensorPower_o),
    .Out5_o(TimerEnable),
    .Out6_o(TimerPreset),
    .Out7_o(FSM_1_Out7_s),
    .Out8_o(FSM_1_Out8_s),
    .Out9_o(FSM_1_Out9_s),
    .Out10_o(FSM_1_Out10_s),
    .Out11_o(FSM_1_Out11_s),
    .Out12_o(FSM_1_Out12_s),
    .Out13_o(FSM_1_Out13_s),
    .Out14_o(FSM_1_Out14_s),
    .CfgMode_i(FSM_1_CfgMode_s),
    .CfgClk_i(FSM_1_CfgClk_s),
    .CfgShift_i(FSM_1_CfgShift_s),
    .CfgDataIn_i(FSM_1_CfgDataIn_s),
    .CfgDataOut_o(FSM_1_CfgDataOut_s)
  );
  assign FSM_1_CfgMode_s = 1'b0;
  assign FSM_1_CfgClk_s = 1'b0;
  assign FSM_1_CfgShift_s = 1'b0;
  assign FSM_1_CfgDataIn_s = 1'b0;

endmodule
