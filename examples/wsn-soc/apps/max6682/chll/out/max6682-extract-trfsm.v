(* src = "../../verilog/max6682.v:133", top = 1 *)
module MAX6682 (
  (* intersynth_port = "Reset_n_i", src = "../../verilog/max6682.v:135" *)
  input Reset_n_i,
  (* intersynth_port = "Clk_i", src = "../../verilog/max6682.v:137" *)
  input Clk_i,
  (* intersynth_conntype = "Bit", intersynth_port = "ReconfModuleIn_s", src = "../../verilog/max6682.v:139" *)
  input Enable_i,
  (* intersynth_conntype = "Bit", intersynth_port = "ReconfModuleIRQs_s", src = "../../verilog/max6682.v:141" *)
  output CpuIntr_o,
  (* intersynth_conntype = "Bit", intersynth_port = "Outputs_o", src = "../../verilog/max6682.v:143" *)
  output MAX6682CS_n_o,
  (* intersynth_conntype = "Byte", intersynth_port = "SPI_DataOut", src = "../../verilog/max6682.v:145" *)
  input[7:0] SPI_Data_i,
  (* intersynth_conntype = "Bit", intersynth_port = "SPI_Write", src = "../../verilog/max6682.v:147" *)
  output SPI_Write_o,
  (* intersynth_conntype = "Bit", intersynth_port = "SPI_ReadNext", src = "../../verilog/max6682.v:149" *)
  output SPI_ReadNext_o,
  (* intersynth_conntype = "Byte", intersynth_port = "SPI_DataIn", src = "../../verilog/max6682.v:151" *)
  output[7:0] SPI_Data_o,
  (* intersynth_conntype = "Bit", intersynth_port = "SPI_FIFOFull", src = "../../verilog/max6682.v:153" *)
  input SPI_FIFOFull_i,
  (* intersynth_conntype = "Bit", intersynth_port = "SPI_FIFOEmpty", src = "../../verilog/max6682.v:155" *)
  input SPI_FIFOEmpty_i,
  (* intersynth_conntype = "Bit", intersynth_port = "SPI_Transmission", src = "../../verilog/max6682.v:157" *)
  input SPI_Transmission_i,
  (* intersynth_conntype = "Word", intersynth_param = "PeriodCounterPresetH_i", src = "../../verilog/max6682.v:159" *)
  input[15:0] PeriodCounterPresetH_i,
  (* intersynth_conntype = "Word", intersynth_param = "PeriodCounterPresetL_i", src = "../../verilog/max6682.v:161" *)
  input[15:0] PeriodCounterPresetL_i,
  (* intersynth_conntype = "Word", intersynth_param = "SensorValue_o", src = "../../verilog/max6682.v:163" *)
  output[15:0] SensorValue_o,
  (* intersynth_conntype = "Word", intersynth_param = "Threshold_i", src = "../../verilog/max6682.v:165" *)
  input[15:0] Threshold_i,
  (* intersynth_conntype = "Bit", intersynth_port = "SPI_CPOL", src = "../../verilog/max6682.v:167" *)
  output SPI_CPOL_o,
  (* intersynth_conntype = "Bit", intersynth_port = "SPI_CPHA", src = "../../verilog/max6682.v:169" *)
  output SPI_CPHA_o,
  (* intersynth_conntype = "Bit", intersynth_port = "SPI_LSBFE", src = "../../verilog/max6682.v:171" *)
  output SPI_LSBFE_o
);

  (* src = "../../../../addsubcmp/verilog/addsubcmp_greater.v:8" *)
  wire \$extract$\AddSubCmp_Greater_Direct$773.Carry_s ;
  (* src = "../../../../addsubcmp/verilog/addsubcmp_greater.v:7" *)
  wire [15:0] \$extract$\AddSubCmp_Greater_Direct$773.D_s ;
  (* src = "../../../../addsubcmp/verilog/addsubcmp_greater.v:11" *)
  wire \$extract$\AddSubCmp_Greater_Direct$773.Overflow_s ;
  (* src = "../../../../addsubcmp/verilog/addsubcmp_greater.v:10" *)
  wire \$extract$\AddSubCmp_Greater_Direct$773.Sign_s ;
  (* src = "../../../../addsubcmp/verilog/addsubcmp_greater.v:9" *)
  wire \$extract$\AddSubCmp_Greater_Direct$773.Zero_s ;
  (* src = "../../../../counter32/verilog/counter32_rv1.v:12" *)
  wire [15:0] \$extract$\Counter32_RV1_Timer$768.DH_s ;
  (* src = "../../../../counter32/verilog/counter32_rv1.v:13" *)
  wire [15:0] \$extract$\Counter32_RV1_Timer$768.DL_s ;
  (* src = "../../../../counter32/verilog/counter32_rv1.v:14" *)
  wire \$extract$\Counter32_RV1_Timer$768.Overflow_s ;
  (* src = "../../verilog/max6682.v:323" *)
  wire [15:0] AbsDiffResult;
  (* src = "../../verilog/max6682.v:184" *)
  wire [7:0] Byte0;
  (* src = "../../verilog/max6682.v:185" *)
  wire [7:0] Byte1;
  (* src = "../../verilog/max6682.v:9" *)
  wire \MAX6682_SPI_FSM_1.SPI_FSM_Done ;
  (* src = "../../verilog/max6682.v:4" *)
  wire \MAX6682_SPI_FSM_1.SPI_FSM_Start ;
  (* src = "../../verilog/max6682.v:24" *)
  wire \MAX6682_SPI_FSM_1.SPI_FSM_Wr0 ;
  (* src = "../../verilog/max6682.v:23" *)
  wire \MAX6682_SPI_FSM_1.SPI_FSM_Wr1 ;
  (* src = "../../verilog/max6682.v:216" *)
  wire SensorFSM_StoreNewValue;
  (* src = "../../verilog/max6682.v:214" *)
  wire SensorFSM_TimerEnable;
  (* src = "../../verilog/max6682.v:212" *)
  wire SensorFSM_TimerOvfl;
  (* src = "../../verilog/max6682.v:213" *)
  wire SensorFSM_TimerPreset;
  (* src = "../../verilog/max6682.v:321" *)
  wire [15:0] SensorValue;
  wire SPIFSM_1_Out6_s;
  wire SPIFSM_1_Out7_s;
  wire SPIFSM_1_Out8_s;
  wire SPIFSM_1_Out9_s;
  wire SPIFSM_1_Out10_s;
  wire SPIFSM_1_Out11_s;
  wire SPIFSM_1_Out12_s;
  wire SPIFSM_1_Out13_s;
  wire SPIFSM_1_Out14_s;
  wire SPIFSM_1_CfgMode_s;
  wire SPIFSM_1_CfgClk_s;
  wire SPIFSM_1_CfgShift_s;
  wire SPIFSM_1_CfgDataIn_s;
  wire SPIFSM_1_CfgDataOut_s;
  wire SensorFSM_1_Out5_s;
  wire SensorFSM_1_Out6_s;
  wire SensorFSM_1_Out7_s;
  wire SensorFSM_1_Out8_s;
  wire SensorFSM_1_Out9_s;
  wire SensorFSM_1_CfgMode_s;
  wire SensorFSM_1_CfgClk_s;
  wire SensorFSM_1_CfgShift_s;
  wire SensorFSM_1_CfgDataIn_s;
  wire SensorFSM_1_CfgDataOut_s;


  AbsDiff \$extract$\AbsDiff$769  (
    .A_i(SensorValue),
    .B_i(SensorValue_o),
    .D_o(AbsDiffResult)
  );

  (* src = "../../../../addsubcmp/verilog/addsubcmp_greater.v:13" *)
  AddSubCmp \$extract$\AddSubCmp_Greater_Direct$773.ThisAddSubCmp  (
    .A_i(AbsDiffResult),
    .AddOrSub_i(1'b1),
    .B_i(Threshold_i),
    .Carry_i(1'b0),
    .Carry_o(\$extract$\AddSubCmp_Greater_Direct$773.Carry_s ),
    .D_o(\$extract$\AddSubCmp_Greater_Direct$773.D_s ),
    .Overflow_o(\$extract$\AddSubCmp_Greater_Direct$773.Overflow_s ),
    .Sign_o(\$extract$\AddSubCmp_Greater_Direct$773.Sign_s ),
    .Zero_o(\$extract$\AddSubCmp_Greater_Direct$773.Zero_s )
  );

  (* src = "../../../../byte2wordsel/verilog/byte2wordsel_11msb.v:10" *)
  Byte2WordSel \$extract$\Byte2WordSel_11MSB_Direct$781.DUT  (
    .H_i(Byte1),
    .L_i(Byte0),
    .Mask_i(4'b1011),
    .Shift_i(4'b0101),
    .Y_o(SensorValue)
  );

  (* src = "../../../../counter32/verilog/counter32_rv1.v:19" *)
  Counter32 \$extract$\Counter32_RV1_Timer$768.ThisCounter  (
    .Clk_i(Clk_i),
    .DH_o(\$extract$\Counter32_RV1_Timer$768.DH_s ),
    .DL_o(\$extract$\Counter32_RV1_Timer$768.DL_s ),
    .Direction_i(1'b1),
    .Enable_i(SensorFSM_TimerEnable),
    .Overflow_o(\$extract$\Counter32_RV1_Timer$768.Overflow_s ),
    .PresetValH_i(PeriodCounterPresetH_i),
    .PresetValL_i(PeriodCounterPresetL_i),
    .Preset_i(SensorFSM_TimerPreset),
    .ResetSig_i(1'b0),
    .Reset_n_i(Reset_n_i),
    .Zero_o(SensorFSM_TimerOvfl)
  );

  WordRegister \$extract$\WordRegister$770  (
    .Clk_i(Clk_i),
    .D_i(SensorValue),
    .Enable_i(SensorFSM_StoreNewValue),
    .Q_o(SensorValue_o),
    .Reset_n_i(Reset_n_i)
  );

  SensorFSM SensorFSM_1 (
    .Reset_n_i(Reset_n_i),
    .Clk_i(Clk_i),
    .In0_i(Enable_i),
    .In1_i(\MAX6682_SPI_FSM_1.SPI_FSM_Done ),
    .In2_i(SensorFSM_TimerOvfl),
    .In3_i(\$extract$\AddSubCmp_Greater_Direct$773.Carry_s ),
    .In4_i(\$extract$\AddSubCmp_Greater_Direct$773.Zero_s ),
    .In5_i(1'b0),
    .In6_i(1'b0),
    .In7_i(1'b0),
    .In8_i(1'b0),
    .In9_i(1'b0),
    .Out0_o(\MAX6682_SPI_FSM_1.SPI_FSM_Start ),
    .Out1_o(SensorFSM_StoreNewValue),
    .Out2_o(SensorFSM_TimerEnable),
    .Out3_o(SensorFSM_TimerPreset),
    .Out4_o(CpuIntr_o),
    .Out5_o(SensorFSM_1_Out5_s),
    .Out6_o(SensorFSM_1_Out6_s),
    .Out7_o(SensorFSM_1_Out7_s),
    .Out8_o(SensorFSM_1_Out8_s),
    .Out9_o(SensorFSM_1_Out9_s),
    .CfgMode_i(SensorFSM_1_CfgMode_s),
    .CfgClk_i(SensorFSM_1_CfgClk_s),
    .CfgShift_i(SensorFSM_1_CfgShift_s),
    .CfgDataIn_i(SensorFSM_1_CfgDataIn_s),
    .CfgDataOut_o(SensorFSM_1_CfgDataOut_s)
  );

  ByteRegister \$techmap\MAX6682_SPI_FSM_1.$extract$\ByteRegister$771  (
    .Clk_i(Clk_i),
    .D_i(SPI_Data_i),
    .Enable_i(\MAX6682_SPI_FSM_1.SPI_FSM_Wr0 ),
    .Q_o(Byte0),
    .Reset_n_i(Reset_n_i)
  );

  ByteRegister \$techmap\MAX6682_SPI_FSM_1.$extract$\ByteRegister$772  (
    .Clk_i(Clk_i),
    .D_i(SPI_Data_i),
    .Enable_i(\MAX6682_SPI_FSM_1.SPI_FSM_Wr1 ),
    .Q_o(Byte1),
    .Reset_n_i(Reset_n_i)
  );

  SPIFSM SPIFSM_1 (
    .Reset_n_i(Reset_n_i),
    .Clk_i(Clk_i),
    .In0_i(\MAX6682_SPI_FSM_1.SPI_FSM_Start ),
    .In1_i(SPI_Transmission_i),
    .In2_i(1'b0),
    .In3_i(1'b0),
    .In4_i(1'b0),
    .In5_i(1'b0),
    .In6_i(1'b0),
    .In7_i(1'b0),
    .Out0_o(\MAX6682_SPI_FSM_1.SPI_FSM_Wr0 ),
    .Out1_o(\MAX6682_SPI_FSM_1.SPI_FSM_Done ),
    .Out2_o(\MAX6682_SPI_FSM_1.SPI_FSM_Wr1 ),
    .Out3_o(SPI_ReadNext_o),
    .Out4_o(MAX6682CS_n_o),
    .Out5_o(SPI_Write_o),
    .Out6_o(SPIFSM_1_Out6_s),
    .Out7_o(SPIFSM_1_Out7_s),
    .Out8_o(SPIFSM_1_Out8_s),
    .Out9_o(SPIFSM_1_Out9_s),
    .Out10_o(SPIFSM_1_Out10_s),
    .Out11_o(SPIFSM_1_Out11_s),
    .Out12_o(SPIFSM_1_Out12_s),
    .Out13_o(SPIFSM_1_Out13_s),
    .Out14_o(SPIFSM_1_Out14_s),
    .CfgMode_i(SPIFSM_1_CfgMode_s),
    .CfgClk_i(SPIFSM_1_CfgClk_s),
    .CfgShift_i(SPIFSM_1_CfgShift_s),
    .CfgDataIn_i(SPIFSM_1_CfgDataIn_s),
    .CfgDataOut_o(SPIFSM_1_CfgDataOut_s)
  );
  assign SPI_CPHA_o = 1'b0;
  assign SPI_CPOL_o = 1'b0;
  assign SPI_Data_o = 8'b00000000;
  assign SPI_LSBFE_o = 1'b0;
  assign SPIFSM_1_CfgMode_s = 1'b0;
  assign SPIFSM_1_CfgClk_s = 1'b0;
  assign SPIFSM_1_CfgShift_s = 1'b0;
  assign SPIFSM_1_CfgDataIn_s = 1'b0;
  assign SensorFSM_1_CfgMode_s = 1'b0;
  assign SensorFSM_1_CfgClk_s = 1'b0;
  assign SensorFSM_1_CfgShift_s = 1'b0;
  assign SensorFSM_1_CfgDataIn_s = 1'b0;

endmodule
