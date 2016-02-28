(* src = "../../verilog/adt7310.v:1", top = 1 *)
module ADT7310 (
  (* intersynth_port = "Reset_n_i", src = "../../verilog/adt7310.v:3" *)
  input Reset_n_i,
  (* intersynth_port = "Clk_i", src = "../../verilog/adt7310.v:5" *)
  input Clk_i,
  (* intersynth_conntype = "Bit", intersynth_port = "ReconfModuleIn_s", src = "../../verilog/adt7310.v:7" *)
  input Enable_i,
  (* intersynth_conntype = "Bit", intersynth_port = "ReconfModuleIRQs_s", src = "../../verilog/adt7310.v:9" *)
  output CpuIntr_o,
  (* intersynth_conntype = "Bit", intersynth_port = "Outputs_o", src = "../../verilog/adt7310.v:11" *)
  output ADT7310CS_n_o,
  (* intersynth_conntype = "Byte", intersynth_port = "SPI_DataOut", src = "../../verilog/adt7310.v:13" *)
  input[7:0] SPI_Data_i,
  (* intersynth_conntype = "Bit", intersynth_port = "SPI_Write", src = "../../verilog/adt7310.v:15" *)
  output SPI_Write_o,
  (* intersynth_conntype = "Bit", intersynth_port = "SPI_ReadNext", src = "../../verilog/adt7310.v:17" *)
  output SPI_ReadNext_o,
  (* intersynth_conntype = "Byte", intersynth_port = "SPI_DataIn", src = "../../verilog/adt7310.v:19" *)
  output[7:0] SPI_Data_o,
  (* intersynth_conntype = "Bit", intersynth_port = "SPI_FIFOFull", src = "../../verilog/adt7310.v:21" *)
  input SPI_FIFOFull_i,
  (* intersynth_conntype = "Bit", intersynth_port = "SPI_FIFOEmpty", src = "../../verilog/adt7310.v:23" *)
  input SPI_FIFOEmpty_i,
  (* intersynth_conntype = "Bit", intersynth_port = "SPI_Transmission", src = "../../verilog/adt7310.v:25" *)
  input SPI_Transmission_i,
  (* intersynth_conntype = "Word", intersynth_param = "SPICounterPresetH_i", src = "../../verilog/adt7310.v:27" *)
  input[15:0] SPICounterPresetH_i,
  (* intersynth_conntype = "Word", intersynth_param = "SPICounterPresetL_i", src = "../../verilog/adt7310.v:29" *)
  input[15:0] SPICounterPresetL_i,
  (* intersynth_conntype = "Word", intersynth_param = "Threshold_i", src = "../../verilog/adt7310.v:31" *)
  input[15:0] Threshold_i,
  (* intersynth_conntype = "Word", intersynth_param = "PeriodCounterPreset_i", src = "../../verilog/adt7310.v:33" *)
  input[15:0] PeriodCounterPreset_i,
  (* intersynth_conntype = "Word", intersynth_param = "SensorValue_o", src = "../../verilog/adt7310.v:35" *)
  output[15:0] SensorValue_o,
  (* intersynth_conntype = "Bit", intersynth_port = "SPI_CPOL", src = "../../verilog/adt7310.v:37" *)
  output SPI_CPOL_o,
  (* intersynth_conntype = "Bit", intersynth_port = "SPI_CPHA", src = "../../verilog/adt7310.v:39" *)
  output SPI_CPHA_o,
  (* intersynth_conntype = "Bit", intersynth_port = "SPI_LSBFE", src = "../../verilog/adt7310.v:41" *)
  output SPI_LSBFE_o
);

  wire \$techmap\SPIFSM_1.$auto$opt_reduce.cc:126:opt_mux$2459 ;
  (* src = "../../../../counter32/verilog/counter32_rv1.v:12" *)
  wire [15:0] \$techmap\SPIFSM_1.$extract$\Counter32_RV1_Timer$2533.DH_s ;
  (* src = "../../../../counter32/verilog/counter32_rv1.v:13" *)
  wire [15:0] \$techmap\SPIFSM_1.$extract$\Counter32_RV1_Timer$2533.DL_s ;
  (* src = "../../../../counter32/verilog/counter32_rv1.v:14" *)
  wire \$techmap\SPIFSM_1.$extract$\Counter32_RV1_Timer$2533.Overflow_s ;
  wire \$techmap\SPIFSM_1.$procmux$297_CMP ;
  wire \$techmap\SPIFSM_1.$procmux$302_CMP ;
  (* src = "../../../../addsubcmp/verilog/addsubcmp_greater.v:8" *)
  wire \$techmap\SensorFSM_1.$extract$\AddSubCmp_Greater_Direct$2538.Carry_s ;
  (* src = "../../../../addsubcmp/verilog/addsubcmp_greater.v:7" *)
  wire [15:0] \$techmap\SensorFSM_1.$extract$\AddSubCmp_Greater_Direct$2538.D_s ;
  (* src = "../../../../addsubcmp/verilog/addsubcmp_greater.v:11" *)
  wire \$techmap\SensorFSM_1.$extract$\AddSubCmp_Greater_Direct$2538.Overflow_s ;
  (* src = "../../../../addsubcmp/verilog/addsubcmp_greater.v:10" *)
  wire \$techmap\SensorFSM_1.$extract$\AddSubCmp_Greater_Direct$2538.Sign_s ;
  (* src = "../../../../addsubcmp/verilog/addsubcmp_greater.v:9" *)
  wire \$techmap\SensorFSM_1.$extract$\AddSubCmp_Greater_Direct$2538.Zero_s ;
  (* src = "../../../../counter/verilog/counter_rv1.v:14" *)
  wire [15:0] \$techmap\SensorFSM_1.$extract$\Counter_RV1_Timer$2532.D_s ;
  (* src = "../../../../counter/verilog/counter_rv1.v:15" *)
  wire \$techmap\SensorFSM_1.$extract$\Counter_RV1_Timer$2532.Overflow_s ;
  (* src = "../../verilog/spifsm.v:45" *)
  wire \SPIFSM_1.SPI_FSM_TimerEnable ;
  (* src = "../../verilog/spifsm.v:43" *)
  wire \SPIFSM_1.SPI_FSM_TimerOvfl ;
  (* src = "../../verilog/spifsm.v:44" *)
  wire \SPIFSM_1.SPI_FSM_TimerPreset ;
  (* src = "../../verilog/spifsm.v:47" *)
  wire \SPIFSM_1.SPI_FSM_Wr0 ;
  (* src = "../../verilog/spifsm.v:46" *)
  wire \SPIFSM_1.SPI_FSM_Wr1 ;
  (* keep = 1, src = "../../verilog/adt7310.v:56" *)
  wire [7:0] SPIFSM_Byte0_s;
  (* keep = 1, src = "../../verilog/adt7310.v:58" *)
  wire [7:0] SPIFSM_Byte1_s;
  (* keep = 1, src = "../../verilog/adt7310.v:54" *)
  wire SPIFSM_Done_s;
  (* keep = 1, src = "../../verilog/adt7310.v:52" *)
  wire SPIFSM_Start_s;
  (* src = "../../verilog/sensorfsm.v:39" *)
  wire [15:0] \SensorFSM_1.AbsDiffResult ;
  (* src = "../../verilog/sensorfsm.v:33" *)
  wire \SensorFSM_1.SensorFSM_StoreNewValue ;
  (* src = "../../verilog/sensorfsm.v:31" *)
  wire \SensorFSM_1.SensorFSM_TimerEnable ;
  (* src = "../../verilog/sensorfsm.v:29" *)
  wire \SensorFSM_1.SensorFSM_TimerOvfl ;
  (* src = "../../verilog/sensorfsm.v:30" *)
  wire \SensorFSM_1.SensorFSM_TimerPreset ;
  (* src = "../../verilog/sensorfsm.v:37" *)
  wire [15:0] \SensorFSM_1.SensorValue ;
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


  Byte2Word \$extract$\Byte2Word$2543  (
    .H_i(SPIFSM_Byte1_s),
    .L_i(SPIFSM_Byte0_s),
    .Y_o(\SensorFSM_1.SensorValue )
  );

  ByteMuxQuad \$techmap\SPIFSM_1.$extract$\ByteMuxQuad$2539  (
    .A_i(8'b00001000),
    .B_i(8'b11111111),
    .C_i(8'b01010000),
    .D_i(8'b00100000),
    .SAB_i(\$techmap\SPIFSM_1.$auto$opt_reduce.cc:126:opt_mux$2459 ),
    .SC_i(\$techmap\SPIFSM_1.$procmux$297_CMP ),
    .SD_i(\$techmap\SPIFSM_1.$procmux$302_CMP ),
    .Y_o(SPI_Data_o)
  );

  ByteRegister \$techmap\SPIFSM_1.$extract$\ByteRegister$2536  (
    .Clk_i(Clk_i),
    .D_i(SPI_Data_i),
    .Enable_i(\SPIFSM_1.SPI_FSM_Wr0 ),
    .Q_o(SPIFSM_Byte0_s),
    .Reset_n_i(Reset_n_i)
  );

  ByteRegister \$techmap\SPIFSM_1.$extract$\ByteRegister$2537  (
    .Clk_i(Clk_i),
    .D_i(SPI_Data_i),
    .Enable_i(\SPIFSM_1.SPI_FSM_Wr1 ),
    .Q_o(SPIFSM_Byte1_s),
    .Reset_n_i(Reset_n_i)
  );

  (* src = "../../../../counter32/verilog/counter32_rv1.v:19" *)
  Counter32 \$techmap\SPIFSM_1.$extract$\Counter32_RV1_Timer$2533.ThisCounter  (
    .Clk_i(Clk_i),
    .DH_o(\$techmap\SPIFSM_1.$extract$\Counter32_RV1_Timer$2533.DH_s ),
    .DL_o(\$techmap\SPIFSM_1.$extract$\Counter32_RV1_Timer$2533.DL_s ),
    .Direction_i(1'b1),
    .Enable_i(\SPIFSM_1.SPI_FSM_TimerEnable ),
    .Overflow_o(\$techmap\SPIFSM_1.$extract$\Counter32_RV1_Timer$2533.Overflow_s ),
    .PresetValH_i(SPICounterPresetH_i),
    .PresetValL_i(SPICounterPresetL_i),
    .Preset_i(\SPIFSM_1.SPI_FSM_TimerPreset ),
    .ResetSig_i(1'b0),
    .Reset_n_i(Reset_n_i),
    .Zero_o(\SPIFSM_1.SPI_FSM_TimerOvfl )
  );

  SPIFSM SPIFSM_1 (
    .Reset_n_i(Reset_n_i),
    .Clk_i(Clk_i),
    .In0_i(\SPIFSM_1.SPI_FSM_TimerOvfl ),
    .In1_i(SPI_Transmission_i),
    .In2_i(SPIFSM_Start_s),
    .In3_i(1'b0),
    .In4_i(1'b0),
    .In5_i(1'b0),
    .In6_i(1'b0),
    .In7_i(1'b0),
    .Out0_o(\$techmap\SPIFSM_1.$procmux$297_CMP ),
    .Out1_o(\$techmap\SPIFSM_1.$procmux$302_CMP ),
    .Out2_o(\SPIFSM_1.SPI_FSM_Wr0 ),
    .Out3_o(\SPIFSM_1.SPI_FSM_Wr1 ),
    .Out4_o(\$techmap\SPIFSM_1.$auto$opt_reduce.cc:126:opt_mux$2459 ),
    .Out5_o(\SPIFSM_1.SPI_FSM_TimerEnable ),
    .Out6_o(\SPIFSM_1.SPI_FSM_TimerPreset ),
    .Out7_o(ADT7310CS_n_o),
    .Out8_o(SPIFSM_Done_s),
    .Out9_o(SPI_Write_o),
    .Out10_o(SPI_ReadNext_o),
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

  AbsDiff \$techmap\SensorFSM_1.$extract$\AbsDiff$2534  (
    .A_i(\SensorFSM_1.SensorValue ),
    .B_i(SensorValue_o),
    .D_o(\SensorFSM_1.AbsDiffResult )
  );

  (* src = "../../../../addsubcmp/verilog/addsubcmp_greater.v:13" *)
  AddSubCmp \$techmap\SensorFSM_1.$extract$\AddSubCmp_Greater_Direct$2538.ThisAddSubCmp  (
    .A_i(\SensorFSM_1.AbsDiffResult ),
    .AddOrSub_i(1'b1),
    .B_i(Threshold_i),
    .Carry_i(1'b0),
    .Carry_o(\$techmap\SensorFSM_1.$extract$\AddSubCmp_Greater_Direct$2538.Carry_s ),
    .D_o(\$techmap\SensorFSM_1.$extract$\AddSubCmp_Greater_Direct$2538.D_s ),
    .Overflow_o(\$techmap\SensorFSM_1.$extract$\AddSubCmp_Greater_Direct$2538.Overflow_s ),
    .Sign_o(\$techmap\SensorFSM_1.$extract$\AddSubCmp_Greater_Direct$2538.Sign_s ),
    .Zero_o(\$techmap\SensorFSM_1.$extract$\AddSubCmp_Greater_Direct$2538.Zero_s )
  );

  (* src = "../../../../counter/verilog/counter_rv1.v:20" *)
  Counter \$techmap\SensorFSM_1.$extract$\Counter_RV1_Timer$2532.ThisCounter  (
    .Clk_i(Clk_i),
    .D_o(\$techmap\SensorFSM_1.$extract$\Counter_RV1_Timer$2532.D_s ),
    .Direction_i(1'b1),
    .Enable_i(\SensorFSM_1.SensorFSM_TimerEnable ),
    .Overflow_o(\$techmap\SensorFSM_1.$extract$\Counter_RV1_Timer$2532.Overflow_s ),
    .PresetVal_i(PeriodCounterPreset_i),
    .Preset_i(\SensorFSM_1.SensorFSM_TimerPreset ),
    .ResetSig_i(1'b0),
    .Reset_n_i(Reset_n_i),
    .Zero_o(\SensorFSM_1.SensorFSM_TimerOvfl )
  );

  WordRegister \$techmap\SensorFSM_1.$extract$\WordRegister$2535  (
    .Clk_i(Clk_i),
    .D_i(\SensorFSM_1.SensorValue ),
    .Enable_i(\SensorFSM_1.SensorFSM_StoreNewValue ),
    .Q_o(SensorValue_o),
    .Reset_n_i(Reset_n_i)
  );

  SensorFSM SensorFSM_1 (
    .Reset_n_i(Reset_n_i),
    .Clk_i(Clk_i),
    .In0_i(Enable_i),
    .In1_i(SPIFSM_Done_s),
    .In2_i(\SensorFSM_1.SensorFSM_TimerOvfl ),
    .In3_i(\$techmap\SensorFSM_1.$extract$\AddSubCmp_Greater_Direct$2538.Carry_s ),
    .In4_i(\$techmap\SensorFSM_1.$extract$\AddSubCmp_Greater_Direct$2538.Zero_s ),
    .In5_i(1'b0),
    .In6_i(1'b0),
    .In7_i(1'b0),
    .In8_i(1'b0),
    .In9_i(1'b0),
    .Out0_o(SPIFSM_Start_s),
    .Out1_o(\SensorFSM_1.SensorFSM_StoreNewValue ),
    .Out2_o(\SensorFSM_1.SensorFSM_TimerEnable ),
    .Out3_o(\SensorFSM_1.SensorFSM_TimerPreset ),
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
  assign SPI_CPHA_o = 1'b1;
  assign SPI_CPOL_o = 1'b1;
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
