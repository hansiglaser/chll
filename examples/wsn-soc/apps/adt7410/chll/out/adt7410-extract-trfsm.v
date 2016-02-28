(* src = "../../verilog/adt7410.v:1", top = 1 *)
module ADT7410 (
  (* intersynth_port = "Reset_n_i", src = "../../verilog/adt7410.v:3" *)
  input Reset_n_i,
  (* intersynth_port = "Clk_i", src = "../../verilog/adt7410.v:5" *)
  input Clk_i,
  (* intersynth_conntype = "Bit", intersynth_port = "ReconfModuleIn_s", src = "../../verilog/adt7410.v:7" *)
  input Enable_i,
  (* intersynth_conntype = "Bit", intersynth_port = "ReconfModuleIRQs_s", src = "../../verilog/adt7410.v:9" *)
  output CpuIntr_o,
  (* intersynth_conntype = "Bit", intersynth_port = "I2C_ReceiveSend_n", src = "../../verilog/adt7410.v:11" *)
  output I2C_ReceiveSend_n_o,
  (* intersynth_conntype = "Byte", intersynth_port = "I2C_ReadCount", src = "../../verilog/adt7410.v:13" *)
  output[7:0] I2C_ReadCount_o,
  (* intersynth_conntype = "Bit", intersynth_port = "I2C_StartProcess", src = "../../verilog/adt7410.v:15" *)
  output I2C_StartProcess_o,
  (* intersynth_conntype = "Bit", intersynth_port = "I2C_Busy", src = "../../verilog/adt7410.v:17" *)
  input I2C_Busy_i,
  (* intersynth_conntype = "Bit", intersynth_port = "I2C_FIFOReadNext", src = "../../verilog/adt7410.v:19" *)
  output I2C_FIFOReadNext_o,
  (* intersynth_conntype = "Bit", intersynth_port = "I2C_FIFOWrite", src = "../../verilog/adt7410.v:21" *)
  output I2C_FIFOWrite_o,
  (* intersynth_conntype = "Byte", intersynth_port = "I2C_DataIn", src = "../../verilog/adt7410.v:23" *)
  output[7:0] I2C_Data_o,
  (* intersynth_conntype = "Byte", intersynth_port = "I2C_DataOut", src = "../../verilog/adt7410.v:25" *)
  input[7:0] I2C_Data_i,
  (* intersynth_conntype = "Bit", intersynth_port = "I2C_Error", src = "../../verilog/adt7410.v:27" *)
  input I2C_Error_i,
  (* intersynth_conntype = "Word", intersynth_param = "PeriodCounterPreset_i", src = "../../verilog/adt7410.v:29" *)
  input[15:0] PeriodCounterPreset_i,
  (* intersynth_conntype = "Word", intersynth_param = "SensorValue_o", src = "../../verilog/adt7410.v:31" *)
  output[15:0] SensorValue_o,
  (* intersynth_conntype = "Word", intersynth_param = "Threshold_i", src = "../../verilog/adt7410.v:33" *)
  input[15:0] Threshold_i,
  (* intersynth_conntype = "Word", intersynth_param = "WaitCounterPreset_i", src = "../../verilog/adt7410.v:35" *)
  input[15:0] WaitCounterPreset_i
);

  wire \$techmap\I2CFSM_1.$auto$opt_reduce.cc:126:opt_mux$2832 ;
  (* src = "../../../../counter/verilog/counter_rv1.v:14" *)
  wire [15:0] \$techmap\I2CFSM_1.$extract$\Counter_RV1_Timer$2903.D_s ;
  (* src = "../../../../counter/verilog/counter_rv1.v:15" *)
  wire \$techmap\I2CFSM_1.$extract$\Counter_RV1_Timer$2903.Overflow_s ;
  wire \$techmap\I2CFSM_1.$procmux$1156_CMP ;
  wire \$techmap\I2CFSM_1.$procmux$1168_CMP ;
  wire \$techmap\I2CFSM_1.$procmux$1169_CMP ;
  wire [7:0] \$techmap\I2CFSM_1.$techmap$procmux$1425.$procmux$2880_Y ;
  (* src = "../../../../addsubcmp/verilog/addsubcmp_greater.v:8" *)
  wire \$techmap\SensorFSM_1.$extract$\AddSubCmp_Greater_Direct$2908.Carry_s ;
  (* src = "../../../../addsubcmp/verilog/addsubcmp_greater.v:7" *)
  wire [15:0] \$techmap\SensorFSM_1.$extract$\AddSubCmp_Greater_Direct$2908.D_s ;
  (* src = "../../../../addsubcmp/verilog/addsubcmp_greater.v:11" *)
  wire \$techmap\SensorFSM_1.$extract$\AddSubCmp_Greater_Direct$2908.Overflow_s ;
  (* src = "../../../../addsubcmp/verilog/addsubcmp_greater.v:10" *)
  wire \$techmap\SensorFSM_1.$extract$\AddSubCmp_Greater_Direct$2908.Sign_s ;
  (* src = "../../../../addsubcmp/verilog/addsubcmp_greater.v:9" *)
  wire \$techmap\SensorFSM_1.$extract$\AddSubCmp_Greater_Direct$2908.Zero_s ;
  (* src = "../../../../counter/verilog/counter_rv1.v:14" *)
  wire [15:0] \$techmap\SensorFSM_1.$extract$\Counter_RV1_Timer$2902.D_s ;
  (* src = "../../../../counter/verilog/counter_rv1.v:15" *)
  wire \$techmap\SensorFSM_1.$extract$\Counter_RV1_Timer$2902.Overflow_s ;
  (* src = "../../verilog/i2cfsm.v:10" *)
  wire [7:0] \I2CFSM_1.Byte0_o ;
  (* src = "../../verilog/i2cfsm.v:11" *)
  wire [7:0] \I2CFSM_1.Byte1_o ;
  (* src = "../../verilog/i2cfsm.v:8" *)
  wire \I2CFSM_1.Done_o ;
  (* src = "../../verilog/i2cfsm.v:9" *)
  wire \I2CFSM_1.Error_o ;
  (* src = "../../verilog/i2cfsm.v:77" *)
  wire \I2CFSM_1.I2C_FSM_TimerEnable ;
  (* src = "../../verilog/i2cfsm.v:75" *)
  wire \I2CFSM_1.I2C_FSM_TimerOvfl ;
  (* src = "../../verilog/i2cfsm.v:76" *)
  wire \I2CFSM_1.I2C_FSM_TimerPreset ;
  (* src = "../../verilog/i2cfsm.v:79" *)
  wire \I2CFSM_1.I2C_FSM_Wr0 ;
  (* src = "../../verilog/i2cfsm.v:78" *)
  wire \I2CFSM_1.I2C_FSM_Wr1 ;
  (* src = "../../verilog/i2cfsm.v:7" *)
  wire \I2CFSM_1.Start_i ;
  (* src = "../../verilog/sensorfsm.v:41" *)
  wire [15:0] \SensorFSM_1.AbsDiffResult ;
  (* src = "../../verilog/sensorfsm.v:35" *)
  wire \SensorFSM_1.SensorFSM_StoreNewValue ;
  (* src = "../../verilog/sensorfsm.v:33" *)
  wire \SensorFSM_1.SensorFSM_TimerEnable ;
  (* src = "../../verilog/sensorfsm.v:31" *)
  wire \SensorFSM_1.SensorFSM_TimerOvfl ;
  (* src = "../../verilog/sensorfsm.v:32" *)
  wire \SensorFSM_1.SensorFSM_TimerPreset ;
  (* src = "../../verilog/sensorfsm.v:39" *)
  wire [15:0] \SensorFSM_1.SensorValue ;
  wire I2CFSM_1_Out14_s;
  wire I2CFSM_1_CfgMode_s;
  wire I2CFSM_1_CfgClk_s;
  wire I2CFSM_1_CfgShift_s;
  wire I2CFSM_1_CfgDataIn_s;
  wire I2CFSM_1_CfgDataOut_s;
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


  Byte2Word \$extract$\Byte2Word$2915  (
    .H_i(\I2CFSM_1.Byte1_o ),
    .L_i(\I2CFSM_1.Byte0_o ),
    .Y_o(\SensorFSM_1.SensorValue )
  );

  ByteMuxDual \$techmap\I2CFSM_1.$extract$\ByteMuxDual$2910  (
    .A_i(8'b00000000),
    .B_i(8'b00000010),
    .S_i(I2C_ReceiveSend_n_o),
    .Y_o(I2C_ReadCount_o)
  );

  ByteMuxDual \$techmap\I2CFSM_1.$extract$\ByteMuxDual$2911  (
    .A_i(\$techmap\I2CFSM_1.$techmap$procmux$1425.$procmux$2880_Y ),
    .B_i(8'b00000011),
    .S_i(\$techmap\I2CFSM_1.$procmux$1169_CMP ),
    .Y_o(I2C_Data_o)
  );

  ByteMuxQuad \$techmap\I2CFSM_1.$extract$\ByteMuxQuad$2909  (
    .A_i(8'b00000000),
    .B_i(8'b10010001),
    .C_i(8'b10010000),
    .D_i(8'b00100000),
    .SAB_i(\$techmap\I2CFSM_1.$procmux$1156_CMP ),
    .SC_i(\$techmap\I2CFSM_1.$auto$opt_reduce.cc:126:opt_mux$2832 ),
    .SD_i(\$techmap\I2CFSM_1.$procmux$1168_CMP ),
    .Y_o(\$techmap\I2CFSM_1.$techmap$procmux$1425.$procmux$2880_Y )
  );

  ByteRegister \$techmap\I2CFSM_1.$extract$\ByteRegister$2906  (
    .Clk_i(Clk_i),
    .D_i(I2C_Data_i),
    .Enable_i(\I2CFSM_1.I2C_FSM_Wr0 ),
    .Q_o(\I2CFSM_1.Byte0_o ),
    .Reset_n_i(Reset_n_i)
  );

  ByteRegister \$techmap\I2CFSM_1.$extract$\ByteRegister$2907  (
    .Clk_i(Clk_i),
    .D_i(I2C_Data_i),
    .Enable_i(\I2CFSM_1.I2C_FSM_Wr1 ),
    .Q_o(\I2CFSM_1.Byte1_o ),
    .Reset_n_i(Reset_n_i)
  );

  (* src = "../../../../counter/verilog/counter_rv1.v:20" *)
  Counter \$techmap\I2CFSM_1.$extract$\Counter_RV1_Timer$2903.ThisCounter  (
    .Clk_i(Clk_i),
    .D_o(\$techmap\I2CFSM_1.$extract$\Counter_RV1_Timer$2903.D_s ),
    .Direction_i(1'b1),
    .Enable_i(\I2CFSM_1.I2C_FSM_TimerEnable ),
    .Overflow_o(\$techmap\I2CFSM_1.$extract$\Counter_RV1_Timer$2903.Overflow_s ),
    .PresetVal_i(WaitCounterPreset_i),
    .Preset_i(\I2CFSM_1.I2C_FSM_TimerPreset ),
    .ResetSig_i(1'b0),
    .Reset_n_i(Reset_n_i),
    .Zero_o(\I2CFSM_1.I2C_FSM_TimerOvfl )
  );

  I2CFSM I2CFSM_1 (
    .Reset_n_i(Reset_n_i),
    .Clk_i(Clk_i),
    .In0_i(I2C_Busy_i),
    .In1_i(I2C_Error_i),
    .In2_i(\I2CFSM_1.I2C_FSM_TimerOvfl ),
    .In3_i(\I2CFSM_1.Start_i ),
    .In4_i(1'b0),
    .In5_i(1'b0),
    .In6_i(1'b0),
    .In7_i(1'b0),
    .Out0_o(\$techmap\I2CFSM_1.$procmux$1156_CMP ),
    .Out1_o(\$techmap\I2CFSM_1.$procmux$1168_CMP ),
    .Out2_o(\$techmap\I2CFSM_1.$procmux$1169_CMP ),
    .Out3_o(\I2CFSM_1.Done_o ),
    .Out4_o(\I2CFSM_1.I2C_FSM_Wr0 ),
    .Out5_o(I2C_ReceiveSend_n_o),
    .Out6_o(I2C_StartProcess_o),
    .Out7_o(\$techmap\I2CFSM_1.$auto$opt_reduce.cc:126:opt_mux$2832 ),
    .Out8_o(\I2CFSM_1.Error_o ),
    .Out9_o(\I2CFSM_1.I2C_FSM_Wr1 ),
    .Out10_o(I2C_FIFOReadNext_o),
    .Out11_o(\I2CFSM_1.I2C_FSM_TimerEnable ),
    .Out12_o(\I2CFSM_1.I2C_FSM_TimerPreset ),
    .Out13_o(I2C_FIFOWrite_o),
    .Out14_o(I2CFSM_1_Out14_s),
    .CfgMode_i(I2CFSM_1_CfgMode_s),
    .CfgClk_i(I2CFSM_1_CfgClk_s),
    .CfgShift_i(I2CFSM_1_CfgShift_s),
    .CfgDataIn_i(I2CFSM_1_CfgDataIn_s),
    .CfgDataOut_o(I2CFSM_1_CfgDataOut_s)
  );

  AbsDiff \$techmap\SensorFSM_1.$extract$\AbsDiff$2904  (
    .A_i(\SensorFSM_1.SensorValue ),
    .B_i(SensorValue_o),
    .D_o(\SensorFSM_1.AbsDiffResult )
  );

  (* src = "../../../../addsubcmp/verilog/addsubcmp_greater.v:13" *)
  AddSubCmp \$techmap\SensorFSM_1.$extract$\AddSubCmp_Greater_Direct$2908.ThisAddSubCmp  (
    .A_i(\SensorFSM_1.AbsDiffResult ),
    .AddOrSub_i(1'b1),
    .B_i(Threshold_i),
    .Carry_i(1'b0),
    .Carry_o(\$techmap\SensorFSM_1.$extract$\AddSubCmp_Greater_Direct$2908.Carry_s ),
    .D_o(\$techmap\SensorFSM_1.$extract$\AddSubCmp_Greater_Direct$2908.D_s ),
    .Overflow_o(\$techmap\SensorFSM_1.$extract$\AddSubCmp_Greater_Direct$2908.Overflow_s ),
    .Sign_o(\$techmap\SensorFSM_1.$extract$\AddSubCmp_Greater_Direct$2908.Sign_s ),
    .Zero_o(\$techmap\SensorFSM_1.$extract$\AddSubCmp_Greater_Direct$2908.Zero_s )
  );

  (* src = "../../../../counter/verilog/counter_rv1.v:20" *)
  Counter \$techmap\SensorFSM_1.$extract$\Counter_RV1_Timer$2902.ThisCounter  (
    .Clk_i(Clk_i),
    .D_o(\$techmap\SensorFSM_1.$extract$\Counter_RV1_Timer$2902.D_s ),
    .Direction_i(1'b1),
    .Enable_i(\SensorFSM_1.SensorFSM_TimerEnable ),
    .Overflow_o(\$techmap\SensorFSM_1.$extract$\Counter_RV1_Timer$2902.Overflow_s ),
    .PresetVal_i(PeriodCounterPreset_i),
    .Preset_i(\SensorFSM_1.SensorFSM_TimerPreset ),
    .ResetSig_i(1'b0),
    .Reset_n_i(Reset_n_i),
    .Zero_o(\SensorFSM_1.SensorFSM_TimerOvfl )
  );

  WordRegister \$techmap\SensorFSM_1.$extract$\WordRegister$2905  (
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
    .In1_i(\I2CFSM_1.Done_o ),
    .In2_i(\I2CFSM_1.Error_o ),
    .In3_i(\SensorFSM_1.SensorFSM_TimerOvfl ),
    .In4_i(\$techmap\SensorFSM_1.$extract$\AddSubCmp_Greater_Direct$2908.Carry_s ),
    .In5_i(\$techmap\SensorFSM_1.$extract$\AddSubCmp_Greater_Direct$2908.Zero_s ),
    .In6_i(1'b0),
    .In7_i(1'b0),
    .In8_i(1'b0),
    .In9_i(1'b0),
    .Out0_o(\I2CFSM_1.Start_i ),
    .Out1_o(\SensorFSM_1.SensorFSM_StoreNewValue ),
    .Out2_o(CpuIntr_o),
    .Out3_o(\SensorFSM_1.SensorFSM_TimerEnable ),
    .Out4_o(\SensorFSM_1.SensorFSM_TimerPreset ),
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
  assign I2CFSM_1_CfgMode_s = 1'b0;
  assign I2CFSM_1_CfgClk_s = 1'b0;
  assign I2CFSM_1_CfgShift_s = 1'b0;
  assign I2CFSM_1_CfgDataIn_s = 1'b0;
  assign SensorFSM_1_CfgMode_s = 1'b0;
  assign SensorFSM_1_CfgClk_s = 1'b0;
  assign SensorFSM_1_CfgShift_s = 1'b0;
  assign SensorFSM_1_CfgDataIn_s = 1'b0;

endmodule
