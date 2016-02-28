(* src = "../../verilog/max6682mean.v:1", top = 1 *)
module MAX6682Mean (
  (* intersynth_port = "Reset_n_i", src = "../../verilog/max6682mean.v:3" *)
  input Reset_n_i,
  (* intersynth_port = "Clk_i", src = "../../verilog/max6682mean.v:5" *)
  input Clk_i,
  (* intersynth_conntype = "Bit", intersynth_port = "ReconfModuleIn_s", src = "../../verilog/max6682mean.v:7" *)
  input Enable_i,
  (* intersynth_conntype = "Bit", intersynth_port = "ReconfModuleIRQs_s", src = "../../verilog/max6682mean.v:9" *)
  output CpuIntr_o,
  (* intersynth_conntype = "Bit", intersynth_port = "Outputs_o", src = "../../verilog/max6682mean.v:11" *)
  output MAX6682CS_n_o,
  (* intersynth_conntype = "Byte", intersynth_port = "SPI_DataOut", src = "../../verilog/max6682mean.v:13" *)
  input[7:0] SPI_Data_i,
  (* intersynth_conntype = "Bit", intersynth_port = "SPI_Write", src = "../../verilog/max6682mean.v:15" *)
  output SPI_Write_o,
  (* intersynth_conntype = "Bit", intersynth_port = "SPI_ReadNext", src = "../../verilog/max6682mean.v:17" *)
  output SPI_ReadNext_o,
  (* intersynth_conntype = "Byte", intersynth_port = "SPI_DataIn", src = "../../verilog/max6682mean.v:19" *)
  output[7:0] SPI_Data_o,
  (* intersynth_conntype = "Bit", intersynth_port = "SPI_FIFOFull", src = "../../verilog/max6682mean.v:21" *)
  input SPI_FIFOFull_i,
  (* intersynth_conntype = "Bit", intersynth_port = "SPI_FIFOEmpty", src = "../../verilog/max6682mean.v:23" *)
  input SPI_FIFOEmpty_i,
  (* intersynth_conntype = "Bit", intersynth_port = "SPI_Transmission", src = "../../verilog/max6682mean.v:25" *)
  input SPI_Transmission_i,
  (* intersynth_conntype = "Word", intersynth_param = "PauseCounterPreset_i", src = "../../verilog/max6682mean.v:27" *)
  input[15:0] PauseCounterPreset_i,
  (* intersynth_conntype = "Word", intersynth_param = "PeriodCounterPresetH_i", src = "../../verilog/max6682mean.v:29" *)
  input[15:0] PeriodCounterPresetH_i,
  (* intersynth_conntype = "Word", intersynth_param = "PeriodCounterPresetL_i", src = "../../verilog/max6682mean.v:31" *)
  input[15:0] PeriodCounterPresetL_i,
  (* intersynth_conntype = "Word", intersynth_param = "SensorValue_o", src = "../../verilog/max6682mean.v:33" *)
  output[15:0] SensorValue_o,
  (* intersynth_conntype = "Word", intersynth_param = "Threshold_i", src = "../../verilog/max6682mean.v:35" *)
  input[15:0] Threshold_i,
  (* intersynth_conntype = "Bit", intersynth_port = "SPI_CPOL", src = "../../verilog/max6682mean.v:37" *)
  output SPI_CPOL_o,
  (* intersynth_conntype = "Bit", intersynth_port = "SPI_CPHA", src = "../../verilog/max6682mean.v:39" *)
  output SPI_CPHA_o,
  (* intersynth_conntype = "Bit", intersynth_port = "SPI_LSBFE", src = "../../verilog/max6682mean.v:41" *)
  output SPI_LSBFE_o
);

  (* src = "../../verilog/max6682mean.v:303" *)
  wire [15:0] \$add$../../verilog/max6682mean.v:303$35_Y ;
  (* src = "../../../../addsubcmp/verilog/addsubcmp_add.v:7" *)
  wire \$extract$\AddSubCmp_Add_Direct$2146.Carry_s ;
  (* src = "../../../../addsubcmp/verilog/addsubcmp_add.v:10" *)
  wire \$extract$\AddSubCmp_Add_Direct$2146.Overflow_s ;
  (* src = "../../../../addsubcmp/verilog/addsubcmp_add.v:9" *)
  wire \$extract$\AddSubCmp_Add_Direct$2146.Sign_s ;
  (* src = "../../../../addsubcmp/verilog/addsubcmp_add.v:8" *)
  wire \$extract$\AddSubCmp_Add_Direct$2146.Zero_s ;
  (* src = "../../../../addsubcmp/verilog/addsubcmp_greater.v:8" *)
  wire \$extract$\AddSubCmp_Greater_Direct$2147.Carry_s ;
  (* src = "../../../../addsubcmp/verilog/addsubcmp_greater.v:7" *)
  wire [15:0] \$extract$\AddSubCmp_Greater_Direct$2147.D_s ;
  (* src = "../../../../addsubcmp/verilog/addsubcmp_greater.v:11" *)
  wire \$extract$\AddSubCmp_Greater_Direct$2147.Overflow_s ;
  (* src = "../../../../addsubcmp/verilog/addsubcmp_greater.v:10" *)
  wire \$extract$\AddSubCmp_Greater_Direct$2147.Sign_s ;
  (* src = "../../../../addsubcmp/verilog/addsubcmp_greater.v:9" *)
  wire \$extract$\AddSubCmp_Greater_Direct$2147.Zero_s ;
  (* src = "../../../../counter32/verilog/counter32_rv1.v:12" *)
  wire [15:0] \$extract$\Counter32_RV1_Timer$2140.DH_s ;
  (* src = "../../../../counter32/verilog/counter32_rv1.v:13" *)
  wire [15:0] \$extract$\Counter32_RV1_Timer$2140.DL_s ;
  (* src = "../../../../counter32/verilog/counter32_rv1.v:14" *)
  wire \$extract$\Counter32_RV1_Timer$2140.Overflow_s ;
  (* src = "../../../../counter/verilog/counter_rv1.v:14" *)
  wire [15:0] \$extract$\Counter_RV1_Timer$2139.D_s ;
  (* src = "../../../../counter/verilog/counter_rv1.v:15" *)
  wire \$extract$\Counter_RV1_Timer$2139.Overflow_s ;
  (* src = "../../../../wordregister/verilog/wordregister_mux.v:30" *)
  wire [15:0] \$extract$\WordRegister_Mux_Direct$2143.D_s ;
  (* src = "../../../../wordregister/verilog/wordregister_mux.v:29" *)
  wire \$extract$\WordRegister_Mux_Direct$2143.Enable_s ;
  (* src = "../../verilog/max6682mean.v:283" *)
  wire [15:0] AbsDiffResult;
  (* src = "../../verilog/max6682mean.v:281" *)
  wire [15:0] Accumulator;
  (* src = "../../verilog/max6682mean.v:54" *)
  wire [7:0] Byte0;
  (* src = "../../verilog/max6682mean.v:55" *)
  wire [7:0] Byte1;
  (* src = "../../verilog/max6682mean.v:89" *)
  wire PauseTimerEnable;
  (* src = "../../verilog/max6682mean.v:90" *)
  wire PauseTimerOvfl;
  (* src = "../../verilog/max6682mean.v:88" *)
  wire PauseTimerPreset;
  (* src = "../../verilog/spifsm.v:9" *)
  wire \SPI_FSM_1.SPI_FSM_Done ;
  (* src = "../../verilog/spifsm.v:4" *)
  wire \SPI_FSM_1.SPI_FSM_Start ;
  (* src = "../../verilog/spifsm.v:24" *)
  wire \SPI_FSM_1.SPI_FSM_Wr0 ;
  (* src = "../../verilog/spifsm.v:23" *)
  wire \SPI_FSM_1.SPI_FSM_Wr1 ;
  (* src = "../../verilog/max6682mean.v:95" *)
  wire SensorFSM_StoreValue;
  (* src = "../../verilog/max6682mean.v:93" *)
  wire SensorFSM_TimerEnable;
  (* src = "../../verilog/max6682mean.v:91" *)
  wire SensorFSM_TimerOvfl;
  (* src = "../../verilog/max6682mean.v:92" *)
  wire SensorFSM_TimerPreset;
  (* src = "../../verilog/max6682mean.v:280" *)
  wire [15:0] SensorValue;
  wire TRFSM1_1_Out8_s;
  wire TRFSM1_1_Out9_s;
  wire TRFSM1_1_Out10_s;
  wire TRFSM1_1_Out11_s;
  wire TRFSM1_1_Out12_s;
  wire TRFSM1_1_Out13_s;
  wire TRFSM1_1_Out14_s;
  wire TRFSM1_1_CfgMode_s;
  wire TRFSM1_1_CfgClk_s;
  wire TRFSM1_1_CfgShift_s;
  wire TRFSM1_1_CfgDataIn_s;
  wire TRFSM1_1_CfgDataOut_s;
  wire TRFSM0_1_Out6_s;
  wire TRFSM0_1_Out7_s;
  wire TRFSM0_1_Out8_s;
  wire TRFSM0_1_Out9_s;
  wire TRFSM0_1_CfgMode_s;
  wire TRFSM0_1_CfgClk_s;
  wire TRFSM0_1_CfgShift_s;
  wire TRFSM0_1_CfgDataIn_s;
  wire TRFSM0_1_CfgDataOut_s;


  AbsDiff \$extract$\AbsDiff$2141  (
    .A_i(SensorValue_o),
    .B_i(Accumulator),
    .D_o(AbsDiffResult)
  );

  (* src = "../../../../addsubcmp/verilog/addsubcmp_add.v:12" *)
  AddSubCmp \$extract$\AddSubCmp_Add_Direct$2146.ThisAddSubCmp  (
    .A_i(Accumulator),
    .AddOrSub_i(1'b0),
    .B_i(SensorValue),
    .Carry_i(1'b0),
    .Carry_o(\$extract$\AddSubCmp_Add_Direct$2146.Carry_s ),
    .D_o(\$add$../../verilog/max6682mean.v:303$35_Y ),
    .Overflow_o(\$extract$\AddSubCmp_Add_Direct$2146.Overflow_s ),
    .Sign_o(\$extract$\AddSubCmp_Add_Direct$2146.Sign_s ),
    .Zero_o(\$extract$\AddSubCmp_Add_Direct$2146.Zero_s )
  );

  (* src = "../../../../addsubcmp/verilog/addsubcmp_greater.v:13" *)
  AddSubCmp \$extract$\AddSubCmp_Greater_Direct$2147.ThisAddSubCmp  (
    .A_i(AbsDiffResult),
    .AddOrSub_i(1'b1),
    .B_i(Threshold_i),
    .Carry_i(1'b0),
    .Carry_o(\$extract$\AddSubCmp_Greater_Direct$2147.Carry_s ),
    .D_o(\$extract$\AddSubCmp_Greater_Direct$2147.D_s ),
    .Overflow_o(\$extract$\AddSubCmp_Greater_Direct$2147.Overflow_s ),
    .Sign_o(\$extract$\AddSubCmp_Greater_Direct$2147.Sign_s ),
    .Zero_o(\$extract$\AddSubCmp_Greater_Direct$2147.Zero_s )
  );

  (* src = "../../../../byte2wordsel/verilog/byte2wordsel_11msb.v:10" *)
  Byte2WordSel \$extract$\Byte2WordSel_11MSB_Direct$2155.DUT  (
    .H_i(Byte1),
    .L_i(Byte0),
    .Mask_i(4'b1011),
    .Shift_i(4'b0101),
    .Y_o(SensorValue)
  );

  (* src = "../../../../counter32/verilog/counter32_rv1.v:19" *)
  Counter32 \$extract$\Counter32_RV1_Timer$2140.ThisCounter  (
    .Clk_i(Clk_i),
    .DH_o(\$extract$\Counter32_RV1_Timer$2140.DH_s ),
    .DL_o(\$extract$\Counter32_RV1_Timer$2140.DL_s ),
    .Direction_i(1'b1),
    .Enable_i(SensorFSM_TimerEnable),
    .Overflow_o(\$extract$\Counter32_RV1_Timer$2140.Overflow_s ),
    .PresetValH_i(PeriodCounterPresetH_i),
    .PresetValL_i(PeriodCounterPresetL_i),
    .Preset_i(SensorFSM_TimerPreset),
    .ResetSig_i(1'b0),
    .Reset_n_i(Reset_n_i),
    .Zero_o(SensorFSM_TimerOvfl)
  );

  (* src = "../../../../counter/verilog/counter_rv1.v:20" *)
  Counter \$extract$\Counter_RV1_Timer$2139.ThisCounter  (
    .Clk_i(Clk_i),
    .D_o(\$extract$\Counter_RV1_Timer$2139.D_s ),
    .Direction_i(1'b1),
    .Enable_i(PauseTimerEnable),
    .Overflow_o(\$extract$\Counter_RV1_Timer$2139.Overflow_s ),
    .PresetVal_i(PauseCounterPreset_i),
    .Preset_i(PauseTimerPreset),
    .ResetSig_i(1'b0),
    .Reset_n_i(Reset_n_i),
    .Zero_o(PauseTimerOvfl)
  );

  WordMuxDual \$extract$\WordMuxDual$2156  (
    .A_i(\$add$../../verilog/max6682mean.v:303$35_Y ),
    .B_i(SensorValue),
    .S_i(SensorFSM_StoreValue),
    .Y_o(\$extract$\WordRegister_Mux_Direct$2143.D_s )
  );

  WordRegister \$extract$\WordRegister$2142  (
    .Clk_i(Clk_i),
    .D_i(Accumulator),
    .Enable_i(CpuIntr_o),
    .Q_o(SensorValue_o),
    .Reset_n_i(Reset_n_i)
  );

  (* src = "../../../../wordregister/verilog/wordregister_mux.v:35" *)
  WordRegister \$extract$\WordRegister_Mux_Direct$2143.ThisWordRegister  (
    .Clk_i(Clk_i),
    .D_i(\$extract$\WordRegister_Mux_Direct$2143.D_s ),
    .Enable_i(\$extract$\WordRegister_Mux_Direct$2143.Enable_s ),
    .Q_o(Accumulator),
    .Reset_n_i(Reset_n_i)
  );

  TRFSM1 TRFSM1_1 (
    .Reset_n_i(Reset_n_i),
    .Clk_i(Clk_i),
    .In0_i(Enable_i),
    .In1_i(PauseTimerOvfl),
    .In2_i(\SPI_FSM_1.SPI_FSM_Done ),
    .In3_i(SensorFSM_TimerOvfl),
    .In4_i(\$extract$\AddSubCmp_Greater_Direct$2147.Carry_s ),
    .In5_i(\$extract$\AddSubCmp_Greater_Direct$2147.Zero_s ),
    .In6_i(1'b0),
    .In7_i(1'b0),
    .In8_i(1'b0),
    .In9_i(1'b0),
    .Out0_o(CpuIntr_o),
    .Out1_o(SensorFSM_StoreValue),
    .Out2_o(\SPI_FSM_1.SPI_FSM_Start ),
    .Out3_o(SensorFSM_TimerEnable),
    .Out4_o(SensorFSM_TimerPreset),
    .Out5_o(\$extract$\WordRegister_Mux_Direct$2143.Enable_s ),
    .Out6_o(PauseTimerEnable),
    .Out7_o(PauseTimerPreset),
    .Out8_o(TRFSM1_1_Out8_s),
    .Out9_o(TRFSM1_1_Out9_s),
    .Out10_o(TRFSM1_1_Out10_s),
    .Out11_o(TRFSM1_1_Out11_s),
    .Out12_o(TRFSM1_1_Out12_s),
    .Out13_o(TRFSM1_1_Out13_s),
    .Out14_o(TRFSM1_1_Out14_s),
    .CfgMode_i(TRFSM1_1_CfgMode_s),
    .CfgClk_i(TRFSM1_1_CfgClk_s),
    .CfgShift_i(TRFSM1_1_CfgShift_s),
    .CfgDataIn_i(TRFSM1_1_CfgDataIn_s),
    .CfgDataOut_o(TRFSM1_1_CfgDataOut_s)
  );

  ByteRegister \$techmap\SPI_FSM_1.$extract$\ByteRegister$2144  (
    .Clk_i(Clk_i),
    .D_i(SPI_Data_i),
    .Enable_i(\SPI_FSM_1.SPI_FSM_Wr0 ),
    .Q_o(Byte0),
    .Reset_n_i(Reset_n_i)
  );

  ByteRegister \$techmap\SPI_FSM_1.$extract$\ByteRegister$2145  (
    .Clk_i(Clk_i),
    .D_i(SPI_Data_i),
    .Enable_i(\SPI_FSM_1.SPI_FSM_Wr1 ),
    .Q_o(Byte1),
    .Reset_n_i(Reset_n_i)
  );

  TRFSM0 TRFSM0_1 (
    .Reset_n_i(Reset_n_i),
    .Clk_i(Clk_i),
    .In0_i(\SPI_FSM_1.SPI_FSM_Start ),
    .In1_i(SPI_Transmission_i),
    .In2_i(1'b0),
    .In3_i(1'b0),
    .In4_i(1'b0),
    .In5_i(1'b0),
    .Out0_o(\SPI_FSM_1.SPI_FSM_Done ),
    .Out1_o(\SPI_FSM_1.SPI_FSM_Wr0 ),
    .Out2_o(\SPI_FSM_1.SPI_FSM_Wr1 ),
    .Out3_o(SPI_ReadNext_o),
    .Out4_o(SPI_Write_o),
    .Out5_o(MAX6682CS_n_o),
    .Out6_o(TRFSM0_1_Out6_s),
    .Out7_o(TRFSM0_1_Out7_s),
    .Out8_o(TRFSM0_1_Out8_s),
    .Out9_o(TRFSM0_1_Out9_s),
    .CfgMode_i(TRFSM0_1_CfgMode_s),
    .CfgClk_i(TRFSM0_1_CfgClk_s),
    .CfgShift_i(TRFSM0_1_CfgShift_s),
    .CfgDataIn_i(TRFSM0_1_CfgDataIn_s),
    .CfgDataOut_o(TRFSM0_1_CfgDataOut_s)
  );
  assign SPI_CPHA_o = 1'b0;
  assign SPI_CPOL_o = 1'b0;
  assign SPI_Data_o = 8'b00000000;
  assign SPI_LSBFE_o = 1'b0;
  assign TRFSM1_1_CfgMode_s = 1'b0;
  assign TRFSM1_1_CfgClk_s = 1'b0;
  assign TRFSM1_1_CfgShift_s = 1'b0;
  assign TRFSM1_1_CfgDataIn_s = 1'b0;
  assign TRFSM0_1_CfgMode_s = 1'b0;
  assign TRFSM0_1_CfgClk_s = 1'b0;
  assign TRFSM0_1_CfgShift_s = 1'b0;
  assign TRFSM0_1_CfgDataIn_s = 1'b0;

endmodule
