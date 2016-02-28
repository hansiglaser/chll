(* src = "../../verilog/slowadt7410.v:1" *)
module SlowADT7410 (
  (* intersynth_port = "Reset_n_i", src = "../../verilog/slowadt7410.v:3" *)
  input Reset_n_i,
  (* intersynth_port = "Clk_i", src = "../../verilog/slowadt7410.v:5" *)
  input Clk_i,
  (* intersynth_conntype = "Bit", intersynth_port = "ReconfModuleIn_s", src = "../../verilog/slowadt7410.v:7" *)
  input Enable_i,
  (* intersynth_conntype = "Bit", intersynth_port = "ReconfModuleIRQs_s", src = "../../verilog/slowadt7410.v:9" *)
  output CpuIntr_o,
  (* intersynth_conntype = "Bit", intersynth_port = "I2C_ReceiveSend_n", src = "../../verilog/slowadt7410.v:11" *)
  output I2C_ReceiveSend_n_o,
  (* intersynth_conntype = "Byte", intersynth_port = "I2C_ReadCount", src = "../../verilog/slowadt7410.v:13" *)
  output[7:0] I2C_ReadCount_o,
  (* intersynth_conntype = "Bit", intersynth_port = "I2C_StartProcess", src = "../../verilog/slowadt7410.v:15" *)
  output I2C_StartProcess_o,
  (* intersynth_conntype = "Bit", intersynth_port = "I2C_Busy", src = "../../verilog/slowadt7410.v:17" *)
  input I2C_Busy_i,
  (* intersynth_conntype = "Bit", intersynth_port = "I2C_FIFOReadNext", src = "../../verilog/slowadt7410.v:19" *)
  output I2C_FIFOReadNext_o,
  (* intersynth_conntype = "Bit", intersynth_port = "I2C_FIFOWrite", src = "../../verilog/slowadt7410.v:21" *)
  output I2C_FIFOWrite_o,
  (* intersynth_conntype = "Byte", intersynth_port = "I2C_DataIn", src = "../../verilog/slowadt7410.v:23" *)
  output[7:0] I2C_Data_o,
  (* intersynth_conntype = "Byte", intersynth_port = "I2C_DataOut", src = "../../verilog/slowadt7410.v:25" *)
  input[7:0] I2C_Data_i,
  (* intersynth_conntype = "Bit", intersynth_port = "I2C_Error", src = "../../verilog/slowadt7410.v:27" *)
  input I2C_Error_i,
  (* intersynth_conntype = "Word", intersynth_param = "PeriodCounterPresetH_i", src = "../../verilog/slowadt7410.v:29" *)
  input[15:0] PeriodCounterPresetH_i,
  (* intersynth_conntype = "Word", intersynth_param = "PeriodCounterPresetL_i", src = "../../verilog/slowadt7410.v:31" *)
  input[15:0] PeriodCounterPresetL_i,
  (* intersynth_conntype = "Word", intersynth_param = "SensorValue_o", src = "../../verilog/slowadt7410.v:33" *)
  output[15:0] SensorValue_o,
  (* intersynth_conntype = "Word", intersynth_param = "Threshold_i", src = "../../verilog/slowadt7410.v:35" *)
  input[15:0] Threshold_i,
  (* intersynth_conntype = "Word", intersynth_param = "WaitCounterPresetH_i", src = "../../verilog/slowadt7410.v:37" *)
  input[15:0] WaitCounterPresetH_i,
  (* intersynth_conntype = "Word", intersynth_param = "WaitCounterPresetL_i", src = "../../verilog/slowadt7410.v:39" *)
  input[15:0] WaitCounterPresetL_i
);

  (* src = "../../verilog/i2cfsm.v:241" *)
  wire [7:0] \$techmap\I2CFSM_1.$0\Byte0_o[7:0] ;
  (* src = "../../verilog/i2cfsm.v:241" *)
  wire [7:0] \$techmap\I2CFSM_1.$0\Byte1_o[7:0] ;
  (* src = "../../verilog/i2cfsm.v:271" *)
  wire [31:0] \$techmap\I2CFSM_1.$0\I2C_FSM_Timer[31:0] ;
  (* src = "../../verilog/i2cfsm.v:98" *)
  wire \$techmap\I2CFSM_1.$2\I2C_FIFOReadNext_o[0:0] ;
  (* src = "../../verilog/i2cfsm.v:98" *)
  wire \$techmap\I2CFSM_1.$2\I2C_FSM_TimerEnable[0:0] ;
  (* src = "../../verilog/i2cfsm.v:98" *)
  wire \$techmap\I2CFSM_1.$2\I2C_FSM_TimerPreset[0:0] ;
  (* src = "../../verilog/i2cfsm.v:98" *)
  wire \$techmap\I2CFSM_1.$4\I2C_FSM_TimerEnable[0:0] ;
  wire \$techmap\I2CFSM_1.$auto$opt_reduce.cc:126:opt_mux$2832 ;
  wire \$techmap\I2CFSM_1.$auto$opt_reduce.cc:126:opt_mux$2834 ;
  wire \$techmap\I2CFSM_1.$procmux$1152_CMP ;
  wire \$techmap\I2CFSM_1.$procmux$1153_CMP ;
  wire \$techmap\I2CFSM_1.$procmux$1156_CMP ;
  wire \$techmap\I2CFSM_1.$procmux$1157_CMP ;
  wire \$techmap\I2CFSM_1.$procmux$1158_CMP ;
  wire \$techmap\I2CFSM_1.$procmux$1161_CMP ;
  wire \$techmap\I2CFSM_1.$procmux$1166_CMP ;
  wire \$techmap\I2CFSM_1.$procmux$1167_CMP ;
  wire \$techmap\I2CFSM_1.$procmux$1168_CMP ;
  wire \$techmap\I2CFSM_1.$procmux$1169_CMP ;
  wire \$techmap\I2CFSM_1.$procmux$1172_CMP ;
  wire [31:0] \$techmap\I2CFSM_1.$procmux$803_Y ;
  (* src = "../../verilog/i2cfsm.v:285" *)
  wire [31:0] \$techmap\I2CFSM_1.$sub$../../verilog/i2cfsm.v:285$14_Y ;
  (* src = "../../verilog/sensorfsm.v:146" *)
  wire [31:0] \$techmap\SensorFSM_1.$0\SensorFSM_Timer[31:0] ;
  (* src = "../../verilog/sensorfsm.v:169" *)
  wire [15:0] \$techmap\SensorFSM_1.$0\Word0[15:0] ;
  (* src = "../../verilog/sensorfsm.v:60" *)
  wire \$techmap\SensorFSM_1.$2\MeasureFSM_Start_o[0:0] ;
  (* src = "../../verilog/sensorfsm.v:60" *)
  wire \$techmap\SensorFSM_1.$2\SensorFSM_StoreNewValue[0:0] ;
  (* src = "../../verilog/sensorfsm.v:60" *)
  wire \$techmap\SensorFSM_1.$2\SensorFSM_TimerPreset[0:0] ;
  (* src = "../../verilog/sensorfsm.v:60" *)
  wire \$techmap\SensorFSM_1.$3\SensorFSM_StoreNewValue[0:0] ;
  (* src = "../../verilog/sensorfsm.v:60" *)
  wire \$techmap\SensorFSM_1.$3\SensorFSM_TimerPreset[0:0] ;
  (* src = "../../verilog/sensorfsm.v:60" *)
  wire \$techmap\SensorFSM_1.$4\SensorFSM_TimerPreset[0:0] ;
  (* src = "../../verilog/sensorfsm.v:60" *)
  wire \$techmap\SensorFSM_1.$5\SensorFSM_TimerPreset[0:0] ;
  wire \$techmap\SensorFSM_1.$procmux$195_CMP ;
  wire \$techmap\SensorFSM_1.$procmux$202_CMP ;
  wire \$techmap\SensorFSM_1.$procmux$207_CMP ;
  wire \$techmap\SensorFSM_1.$procmux$210_CMP ;
  wire [31:0] \$techmap\SensorFSM_1.$procmux$62_Y ;
  (* src = "../../verilog/sensorfsm.v:160" *)
  wire [31:0] \$techmap\SensorFSM_1.$sub$../../verilog/sensorfsm.v:160$51_Y ;
  (* src = "../../verilog/i2cfsm.v:10" *)
  wire [7:0] \I2CFSM_1.Byte0_o ;
  (* src = "../../verilog/i2cfsm.v:11" *)
  wire [7:0] \I2CFSM_1.Byte1_o ;
  (* src = "../../verilog/i2cfsm.v:5" *)
  wire \I2CFSM_1.Clk_i ;
  (* src = "../../verilog/i2cfsm.v:8" *)
  wire \I2CFSM_1.Done_o ;
  (* src = "../../verilog/i2cfsm.v:9" *)
  wire \I2CFSM_1.Error_o ;
  (* src = "../../verilog/i2cfsm.v:17" *)
  wire \I2CFSM_1.I2C_Busy_i ;
  (* src = "../../verilog/i2cfsm.v:22" *)
  wire [7:0] \I2CFSM_1.I2C_Data_i ;
  (* src = "../../verilog/i2cfsm.v:21" *)
  wire [7:0] \I2CFSM_1.I2C_Data_o ;
  (* src = "../../verilog/i2cfsm.v:24" *)
  wire \I2CFSM_1.I2C_Error_i ;
  (* src = "../../verilog/i2cfsm.v:19" *)
  wire \I2CFSM_1.I2C_FIFOReadNext_o ;
  (* src = "../../verilog/i2cfsm.v:20" *)
  wire \I2CFSM_1.I2C_FIFOWrite_o ;
  (* src = "../../verilog/i2cfsm.v:269" *)
  wire [31:0] \I2CFSM_1.I2C_FSM_Timer ;
  (* src = "../../verilog/i2cfsm.v:78" *)
  wire \I2CFSM_1.I2C_FSM_TimerEnable ;
  (* src = "../../verilog/i2cfsm.v:76" *)
  wire \I2CFSM_1.I2C_FSM_TimerOvfl ;
  (* src = "../../verilog/i2cfsm.v:77" *)
  wire \I2CFSM_1.I2C_FSM_TimerPreset ;
  (* src = "../../verilog/i2cfsm.v:80" *)
  wire \I2CFSM_1.I2C_FSM_Wr0 ;
  (* src = "../../verilog/i2cfsm.v:79" *)
  wire \I2CFSM_1.I2C_FSM_Wr1 ;
  (* src = "../../verilog/i2cfsm.v:15" *)
  wire [7:0] \I2CFSM_1.I2C_ReadCount_o ;
  (* src = "../../verilog/i2cfsm.v:14" *)
  wire \I2CFSM_1.I2C_ReceiveSend_n_o ;
  (* src = "../../verilog/i2cfsm.v:16" *)
  wire \I2CFSM_1.I2C_StartProcess_o ;
  (* src = "../../verilog/i2cfsm.v:26" *)
  wire [15:0] \I2CFSM_1.ParamCounterPresetH_i ;
  (* src = "../../verilog/i2cfsm.v:27" *)
  wire [15:0] \I2CFSM_1.ParamCounterPresetL_i ;
  (* src = "../../verilog/i2cfsm.v:4" *)
  wire \I2CFSM_1.Reset_n_i ;
  (* src = "../../verilog/i2cfsm.v:7" *)
  wire \I2CFSM_1.Start_i ;
  (* src = "../../verilog/slowadt7410.v:45" *)
  wire [7:0] I2CFSM_Byte0_s;
  (* src = "../../verilog/slowadt7410.v:46" *)
  wire [7:0] I2CFSM_Byte1_s;
  (* src = "../../verilog/slowadt7410.v:43" *)
  wire I2CFSM_Done_s;
  (* src = "../../verilog/slowadt7410.v:44" *)
  wire I2CFSM_Error_s;
  (* src = "../../verilog/slowadt7410.v:42" *)
  wire I2CFSM_Start_s;
  (* src = "../../verilog/sensorfsm.v:42" *)
  wire [15:0] \SensorFSM_1.AbsDiffResult ;
  (* src = "../../verilog/sensorfsm.v:7" *)
  wire \SensorFSM_1.Clk_i ;
  (* src = "../../verilog/sensorfsm.v:10" *)
  wire \SensorFSM_1.CpuIntr_o ;
  (* src = "../../verilog/sensorfsm.v:184" *)
  wire [16:0] \SensorFSM_1.DiffAB ;
  (* src = "../../verilog/sensorfsm.v:185" *)
  wire [15:0] \SensorFSM_1.DiffBA ;
  (* src = "../../verilog/sensorfsm.v:9" *)
  wire \SensorFSM_1.Enable_i ;
  (* src = "../../verilog/sensorfsm.v:16" *)
  wire [7:0] \SensorFSM_1.MeasureFSM_Byte0_i ;
  (* src = "../../verilog/sensorfsm.v:17" *)
  wire [7:0] \SensorFSM_1.MeasureFSM_Byte1_i ;
  (* src = "../../verilog/sensorfsm.v:14" *)
  wire \SensorFSM_1.MeasureFSM_Done_i ;
  (* src = "../../verilog/sensorfsm.v:15" *)
  wire \SensorFSM_1.MeasureFSM_Error_i ;
  (* src = "../../verilog/sensorfsm.v:13" *)
  wire \SensorFSM_1.MeasureFSM_Start_o ;
  (* src = "../../verilog/sensorfsm.v:20" *)
  wire [15:0] \SensorFSM_1.ParamCounterPresetH_i ;
  (* src = "../../verilog/sensorfsm.v:21" *)
  wire [15:0] \SensorFSM_1.ParamCounterPresetL_i ;
  (* src = "../../verilog/sensorfsm.v:19" *)
  wire [15:0] \SensorFSM_1.ParamThreshold_i ;
  (* src = "../../verilog/sensorfsm.v:6" *)
  wire \SensorFSM_1.Reset_n_i ;
  (* src = "../../verilog/sensorfsm.v:35" *)
  wire \SensorFSM_1.SensorFSM_DiffTooLarge ;
  (* src = "../../verilog/sensorfsm.v:36" *)
  wire \SensorFSM_1.SensorFSM_StoreNewValue ;
  (* src = "../../verilog/sensorfsm.v:144" *)
  wire [31:0] \SensorFSM_1.SensorFSM_Timer ;
  (* src = "../../verilog/sensorfsm.v:34" *)
  wire \SensorFSM_1.SensorFSM_TimerEnable ;
  (* src = "../../verilog/sensorfsm.v:32" *)
  wire \SensorFSM_1.SensorFSM_TimerOvfl ;
  (* src = "../../verilog/sensorfsm.v:33" *)
  wire \SensorFSM_1.SensorFSM_TimerPreset ;
  (* src = "../../verilog/sensorfsm.v:40" *)
  wire [15:0] \SensorFSM_1.SensorValue ;
  (* src = "../../verilog/sensorfsm.v:11" *)
  wire [15:0] \SensorFSM_1.SensorValue_o ;
  (* src = "../../verilog/sensorfsm.v:41" *)
  wire [15:0] \SensorFSM_1.Word0 ;
  wire I2CFSM_1_Out13_s;
  wire I2CFSM_1_Out14_s;
  wire I2CFSM_1_CfgMode_s;
  wire I2CFSM_1_CfgClk_s;
  wire I2CFSM_1_CfgShift_s;
  wire I2CFSM_1_CfgDataIn_s;
  wire I2CFSM_1_CfgDataOut_s;
  wire SensorFSM_1_Out4_s;
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


  \$reduce_or  #(
    .A_SIGNED(0),
    .A_WIDTH(2),
    .Y_WIDTH(1)
  ) \$techmap\I2CFSM_1.$auto$opt_reduce.cc:130:opt_mux$2827  (
    .A({ \$techmap\I2CFSM_1.$procmux$1153_CMP , \$techmap\I2CFSM_1.$procmux$1152_CMP  }),
    .Y(\I2CFSM_1.I2C_ReceiveSend_n_o )
  );

  \$reduce_or  #(
    .A_SIGNED(0),
    .A_WIDTH(3),
    .Y_WIDTH(1)
  ) \$techmap\I2CFSM_1.$auto$opt_reduce.cc:130:opt_mux$2831  (
    .A({ \$techmap\I2CFSM_1.$procmux$1167_CMP , \$techmap\I2CFSM_1.$procmux$1157_CMP , \$techmap\I2CFSM_1.$procmux$1153_CMP  }),
    .Y(\I2CFSM_1.I2C_StartProcess_o )
  );

  \$reduce_or  #(
    .A_SIGNED(0),
    .A_WIDTH(2),
    .Y_WIDTH(1)
  ) \$techmap\I2CFSM_1.$auto$opt_reduce.cc:130:opt_mux$2833  (
    .A({ \$techmap\I2CFSM_1.$procmux$1172_CMP , \$techmap\I2CFSM_1.$procmux$1161_CMP  }),
    .Y(\$techmap\I2CFSM_1.$auto$opt_reduce.cc:126:opt_mux$2832 )
  );

  \$reduce_or  #(
    .A_SIGNED(0),
    .A_WIDTH(3),
    .Y_WIDTH(1)
  ) \$techmap\I2CFSM_1.$auto$opt_reduce.cc:130:opt_mux$2835  (
    .A({ \$techmap\I2CFSM_1.$procmux$1169_CMP , \$techmap\I2CFSM_1.$procmux$1168_CMP , \$techmap\I2CFSM_1.$procmux$1158_CMP  }),
    .Y(\$techmap\I2CFSM_1.$auto$opt_reduce.cc:126:opt_mux$2834 )
  );

  (* src = "../../verilog/i2cfsm.v:290" *)
  \$eq  #(
    .A_SIGNED(0),
    .A_WIDTH(32),
    .B_SIGNED(0),
    .B_WIDTH(32),
    .Y_WIDTH(1)
  ) \$techmap\I2CFSM_1.$eq$../../verilog/i2cfsm.v:290$15  (
    .A(\I2CFSM_1.I2C_FSM_Timer ),
    .B(0),
    .Y(\I2CFSM_1.I2C_FSM_TimerOvfl )
  );

  I2CFSM I2CFSM_1 (
    .Reset_n_i(\I2CFSM_1.Reset_n_i ),
    .Clk_i(\I2CFSM_1.Clk_i ),
    .In0_i(\I2CFSM_1.I2C_Busy_i ),
    .In1_i(\I2CFSM_1.I2C_Error_i ),
    .In2_i(\I2CFSM_1.I2C_FSM_TimerOvfl ),
    .In3_i(\I2CFSM_1.Start_i ),
    .In4_i(1'b0),
    .In5_i(1'b0),
    .In6_i(1'b0),
    .In7_i(1'b0),
    .Out0_o(\$techmap\I2CFSM_1.$procmux$1152_CMP ),
    .Out1_o(\$techmap\I2CFSM_1.$procmux$1153_CMP ),
    .Out2_o(\$techmap\I2CFSM_1.$procmux$1156_CMP ),
    .Out3_o(\$techmap\I2CFSM_1.$procmux$1157_CMP ),
    .Out4_o(\$techmap\I2CFSM_1.$procmux$1158_CMP ),
    .Out5_o(\$techmap\I2CFSM_1.$procmux$1161_CMP ),
    .Out6_o(\$techmap\I2CFSM_1.$procmux$1166_CMP ),
    .Out7_o(\$techmap\I2CFSM_1.$procmux$1167_CMP ),
    .Out8_o(\$techmap\I2CFSM_1.$procmux$1168_CMP ),
    .Out9_o(\$techmap\I2CFSM_1.$procmux$1169_CMP ),
    .Out10_o(\$techmap\I2CFSM_1.$procmux$1172_CMP ),
    .Out11_o(\I2CFSM_1.Done_o ),
    .Out12_o(\I2CFSM_1.I2C_FSM_Wr0 ),
    .Out13_o(I2CFSM_1_Out13_s),
    .Out14_o(I2CFSM_1_Out14_s),
    .CfgMode_i(I2CFSM_1_CfgMode_s),
    .CfgClk_i(I2CFSM_1_CfgClk_s),
    .CfgShift_i(I2CFSM_1_CfgShift_s),
    .CfgDataIn_i(I2CFSM_1_CfgDataIn_s),
    .CfgDataOut_o(I2CFSM_1_CfgDataOut_s)
  );

  (* src = "../../verilog/i2cfsm.v:241" *)
  \$adff  #(
    .ARST_POLARITY(1'b0),
    .ARST_VALUE(8'b00000000),
    .CLK_POLARITY(1'b1),
    .WIDTH(8)
  ) \$techmap\I2CFSM_1.$procdff$2819  (
    .ARST(\I2CFSM_1.Reset_n_i ),
    .CLK(\I2CFSM_1.Clk_i ),
    .D(\$techmap\I2CFSM_1.$0\Byte0_o[7:0] ),
    .Q(\I2CFSM_1.Byte0_o )
  );

  (* src = "../../verilog/i2cfsm.v:241" *)
  \$adff  #(
    .ARST_POLARITY(1'b0),
    .ARST_VALUE(8'b00000000),
    .CLK_POLARITY(1'b1),
    .WIDTH(8)
  ) \$techmap\I2CFSM_1.$procdff$2820  (
    .ARST(\I2CFSM_1.Reset_n_i ),
    .CLK(\I2CFSM_1.Clk_i ),
    .D(\$techmap\I2CFSM_1.$0\Byte1_o[7:0] ),
    .Q(\I2CFSM_1.Byte1_o )
  );

  (* src = "../../verilog/i2cfsm.v:271" *)
  \$adff  #(
    .ARST_POLARITY(1'b0),
    .ARST_VALUE(0),
    .CLK_POLARITY(1'b1),
    .WIDTH(32)
  ) \$techmap\I2CFSM_1.$procdff$2821  (
    .ARST(\I2CFSM_1.Reset_n_i ),
    .CLK(\I2CFSM_1.Clk_i ),
    .D(\$techmap\I2CFSM_1.$0\I2C_FSM_Timer[31:0] ),
    .Q(\I2CFSM_1.I2C_FSM_Timer )
  );

  \$and  #(
    .A_SIGNED(0),
    .A_WIDTH(1),
    .B_SIGNED(0),
    .B_WIDTH(1),
    .Y_WIDTH(1)
  ) \$techmap\I2CFSM_1.$procmux$1192  (
    .A(\$techmap\I2CFSM_1.$procmux$1166_CMP ),
    .B(\I2CFSM_1.I2C_Error_i ),
    .Y(\I2CFSM_1.Error_o )
  );

  \$pmux  #(
    .S_WIDTH(2),
    .WIDTH(1)
  ) \$techmap\I2CFSM_1.$procmux$1202  (
    .A(1'b0),
    .B({ \$techmap\I2CFSM_1.$2\I2C_FIFOReadNext_o[0:0] , 1'b1 }),
    .S({ \$techmap\I2CFSM_1.$procmux$1152_CMP , \I2CFSM_1.I2C_FSM_Wr0  }),
    .Y(\I2CFSM_1.I2C_FIFOReadNext_o )
  );

  \$pmux  #(
    .S_WIDTH(2),
    .WIDTH(1)
  ) \$techmap\I2CFSM_1.$procmux$1241  (
    .A(1'b0),
    .B({ \$techmap\I2CFSM_1.$2\I2C_FSM_TimerEnable[0:0] , \$techmap\I2CFSM_1.$4\I2C_FSM_TimerEnable[0:0]  }),
    .S({ \$techmap\I2CFSM_1.$procmux$1166_CMP , \$techmap\I2CFSM_1.$procmux$1161_CMP  }),
    .Y(\I2CFSM_1.I2C_FSM_TimerEnable )
  );

  \$pmux  #(
    .S_WIDTH(2),
    .WIDTH(1)
  ) \$techmap\I2CFSM_1.$procmux$1268  (
    .A(1'b1),
    .B({ \$techmap\I2CFSM_1.$2\I2C_FSM_TimerPreset[0:0] , \I2CFSM_1.I2C_FSM_TimerOvfl  }),
    .S({ \$techmap\I2CFSM_1.$procmux$1166_CMP , \$techmap\I2CFSM_1.$procmux$1161_CMP  }),
    .Y(\I2CFSM_1.I2C_FSM_TimerPreset )
  );

  \$and  #(
    .A_SIGNED(0),
    .A_WIDTH(1),
    .B_SIGNED(0),
    .B_WIDTH(1),
    .Y_WIDTH(1)
  ) \$techmap\I2CFSM_1.$procmux$1313  (
    .A(\$techmap\I2CFSM_1.$procmux$1152_CMP ),
    .B(\$techmap\I2CFSM_1.$2\I2C_FIFOReadNext_o[0:0] ),
    .Y(\I2CFSM_1.I2C_FSM_Wr1 )
  );

  \$mux  #(
    .WIDTH(8)
  ) \$techmap\I2CFSM_1.$procmux$1340  (
    .A(8'b00000000),
    .B(8'b00000010),
    .S(\I2CFSM_1.I2C_ReceiveSend_n_o ),
    .Y(\I2CFSM_1.I2C_ReadCount_o )
  );

  \$pmux  #(
    .S_WIDTH(4),
    .WIDTH(8)
  ) \$techmap\I2CFSM_1.$procmux$1425  (
    .A(8'b00000000),
    .B(52465809),
    .S({ \$techmap\I2CFSM_1.$procmux$1169_CMP , \$techmap\I2CFSM_1.$procmux$1168_CMP , \$techmap\I2CFSM_1.$auto$opt_reduce.cc:126:opt_mux$2832 , \$techmap\I2CFSM_1.$procmux$1156_CMP  }),
    .Y(\I2CFSM_1.I2C_Data_o )
  );

  \$pmux  #(
    .S_WIDTH(4),
    .WIDTH(1)
  ) \$techmap\I2CFSM_1.$procmux$1452  (
    .A(1'b0),
    .B({ \I2CFSM_1.Start_i , \I2CFSM_1.I2C_FSM_TimerOvfl , 1'b1, \$techmap\I2CFSM_1.$2\I2C_FIFOReadNext_o[0:0]  }),
    .S({ \$techmap\I2CFSM_1.$procmux$1172_CMP , \$techmap\I2CFSM_1.$procmux$1161_CMP , \$techmap\I2CFSM_1.$auto$opt_reduce.cc:126:opt_mux$2834 , \$techmap\I2CFSM_1.$procmux$1156_CMP  }),
    .Y(\I2CFSM_1.I2C_FIFOWrite_o )
  );

  \$mux  #(
    .WIDTH(1)
  ) \$techmap\I2CFSM_1.$procmux$1598  (
    .A(\$techmap\I2CFSM_1.$2\I2C_FIFOReadNext_o[0:0] ),
    .B(1'b0),
    .S(\I2CFSM_1.I2C_Error_i ),
    .Y(\$techmap\I2CFSM_1.$2\I2C_FSM_TimerEnable[0:0] )
  );

  \$or  #(
    .A_SIGNED(0),
    .A_WIDTH(1),
    .B_SIGNED(0),
    .B_WIDTH(1),
    .Y_WIDTH(1)
  ) \$techmap\I2CFSM_1.$procmux$1626  (
    .A(\I2CFSM_1.I2C_Busy_i ),
    .B(\I2CFSM_1.I2C_Error_i ),
    .Y(\$techmap\I2CFSM_1.$2\I2C_FSM_TimerPreset[0:0] )
  );

  \$not  #(
    .A_SIGNED(0),
    .A_WIDTH(1),
    .Y_WIDTH(1)
  ) \$techmap\I2CFSM_1.$procmux$1766  (
    .A(\I2CFSM_1.I2C_Busy_i ),
    .Y(\$techmap\I2CFSM_1.$2\I2C_FIFOReadNext_o[0:0] )
  );

  \$not  #(
    .A_SIGNED(0),
    .A_WIDTH(1),
    .Y_WIDTH(1)
  ) \$techmap\I2CFSM_1.$procmux$1876  (
    .A(\I2CFSM_1.I2C_FSM_TimerOvfl ),
    .Y(\$techmap\I2CFSM_1.$4\I2C_FSM_TimerEnable[0:0] )
  );

  \$mux  #(
    .WIDTH(8)
  ) \$techmap\I2CFSM_1.$procmux$793  (
    .A(\I2CFSM_1.Byte0_o ),
    .B(\I2CFSM_1.I2C_Data_i ),
    .S(\I2CFSM_1.I2C_FSM_Wr0 ),
    .Y(\$techmap\I2CFSM_1.$0\Byte0_o[7:0] )
  );

  \$mux  #(
    .WIDTH(8)
  ) \$techmap\I2CFSM_1.$procmux$800  (
    .A(\I2CFSM_1.Byte1_o ),
    .B(\I2CFSM_1.I2C_Data_i ),
    .S(\I2CFSM_1.I2C_FSM_Wr1 ),
    .Y(\$techmap\I2CFSM_1.$0\Byte1_o[7:0] )
  );

  \$mux  #(
    .WIDTH(32)
  ) \$techmap\I2CFSM_1.$procmux$803  (
    .A(\I2CFSM_1.I2C_FSM_Timer ),
    .B(\$techmap\I2CFSM_1.$sub$../../verilog/i2cfsm.v:285$14_Y ),
    .S(\I2CFSM_1.I2C_FSM_TimerEnable ),
    .Y(\$techmap\I2CFSM_1.$procmux$803_Y )
  );

  \$mux  #(
    .WIDTH(32)
  ) \$techmap\I2CFSM_1.$procmux$806  (
    .A(\$techmap\I2CFSM_1.$procmux$803_Y ),
    .B({ \I2CFSM_1.ParamCounterPresetH_i , \I2CFSM_1.ParamCounterPresetL_i  }),
    .S(\I2CFSM_1.I2C_FSM_TimerPreset ),
    .Y(\$techmap\I2CFSM_1.$0\I2C_FSM_Timer[31:0] )
  );

  (* src = "../../verilog/i2cfsm.v:285" *)
  \$sub  #(
    .A_SIGNED(0),
    .A_WIDTH(32),
    .B_SIGNED(0),
    .B_WIDTH(1),
    .Y_WIDTH(32)
  ) \$techmap\I2CFSM_1.$sub$../../verilog/i2cfsm.v:285$14  (
    .A(\I2CFSM_1.I2C_FSM_Timer ),
    .B(1'b1),
    .Y(\$techmap\I2CFSM_1.$sub$../../verilog/i2cfsm.v:285$14_Y )
  );

  (* src = "../../verilog/sensorfsm.v:165" *)
  \$eq  #(
    .A_SIGNED(0),
    .A_WIDTH(32),
    .B_SIGNED(0),
    .B_WIDTH(32),
    .Y_WIDTH(1)
  ) \$techmap\SensorFSM_1.$eq$../../verilog/sensorfsm.v:165$52  (
    .A(\SensorFSM_1.SensorFSM_Timer ),
    .B(0),
    .Y(\SensorFSM_1.SensorFSM_TimerOvfl )
  );

  SensorFSM SensorFSM_1 (
    .Reset_n_i(\SensorFSM_1.Reset_n_i ),
    .Clk_i(\SensorFSM_1.Clk_i ),
    .In0_i(\SensorFSM_1.Enable_i ),
    .In1_i(\SensorFSM_1.MeasureFSM_Done_i ),
    .In2_i(\SensorFSM_1.MeasureFSM_Error_i ),
    .In3_i(\SensorFSM_1.SensorFSM_DiffTooLarge ),
    .In4_i(\SensorFSM_1.SensorFSM_TimerOvfl ),
    .In5_i(1'b0),
    .In6_i(1'b0),
    .In7_i(1'b0),
    .In8_i(1'b0),
    .In9_i(1'b0),
    .Out0_o(\$techmap\SensorFSM_1.$procmux$195_CMP ),
    .Out1_o(\$techmap\SensorFSM_1.$procmux$202_CMP ),
    .Out2_o(\$techmap\SensorFSM_1.$procmux$207_CMP ),
    .Out3_o(\$techmap\SensorFSM_1.$procmux$210_CMP ),
    .Out4_o(SensorFSM_1_Out4_s),
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

  (* src = "../../verilog/sensorfsm.v:190" *)
  \$gt  #(
    .A_SIGNED(0),
    .A_WIDTH(16),
    .B_SIGNED(0),
    .B_WIDTH(16),
    .Y_WIDTH(1)
  ) \$techmap\SensorFSM_1.$gt$../../verilog/sensorfsm.v:190$59  (
    .A(\SensorFSM_1.AbsDiffResult ),
    .B(\SensorFSM_1.ParamThreshold_i ),
    .Y(\SensorFSM_1.SensorFSM_DiffTooLarge )
  );

  (* src = "../../verilog/sensorfsm.v:146" *)
  \$adff  #(
    .ARST_POLARITY(1'b0),
    .ARST_VALUE(0),
    .CLK_POLARITY(1'b1),
    .WIDTH(32)
  ) \$techmap\SensorFSM_1.$procdff$2816  (
    .ARST(\SensorFSM_1.Reset_n_i ),
    .CLK(\SensorFSM_1.Clk_i ),
    .D(\$techmap\SensorFSM_1.$0\SensorFSM_Timer[31:0] ),
    .Q(\SensorFSM_1.SensorFSM_Timer )
  );

  (* src = "../../verilog/sensorfsm.v:169" *)
  \$adff  #(
    .ARST_POLARITY(1'b0),
    .ARST_VALUE(16'b0000000000000000),
    .CLK_POLARITY(1'b1),
    .WIDTH(16)
  ) \$techmap\SensorFSM_1.$procdff$2817  (
    .ARST(\SensorFSM_1.Reset_n_i ),
    .CLK(\SensorFSM_1.Clk_i ),
    .D(\$techmap\SensorFSM_1.$0\Word0[15:0] ),
    .Q(\SensorFSM_1.Word0 )
  );

  \$pmux  #(
    .S_WIDTH(2),
    .WIDTH(1)
  ) \$techmap\SensorFSM_1.$procmux$194  (
    .A(1'b0),
    .B({ \SensorFSM_1.MeasureFSM_Error_i , 1'b1 }),
    .S({ \$techmap\SensorFSM_1.$procmux$202_CMP , \$techmap\SensorFSM_1.$procmux$195_CMP  }),
    .Y(\SensorFSM_1.CpuIntr_o )
  );

  \$and  #(
    .A_SIGNED(0),
    .A_WIDTH(1),
    .B_SIGNED(0),
    .B_WIDTH(1),
    .Y_WIDTH(1)
  ) \$techmap\SensorFSM_1.$procmux$227  (
    .A(\$techmap\SensorFSM_1.$procmux$207_CMP ),
    .B(\$techmap\SensorFSM_1.$2\MeasureFSM_Start_o[0:0] ),
    .Y(\SensorFSM_1.MeasureFSM_Start_o )
  );

  \$and  #(
    .A_SIGNED(0),
    .A_WIDTH(1),
    .B_SIGNED(0),
    .B_WIDTH(1),
    .Y_WIDTH(1)
  ) \$techmap\SensorFSM_1.$procmux$243  (
    .A(\$techmap\SensorFSM_1.$procmux$202_CMP ),
    .B(\$techmap\SensorFSM_1.$2\SensorFSM_StoreNewValue[0:0] ),
    .Y(\SensorFSM_1.SensorFSM_StoreNewValue )
  );

  \$pmux  #(
    .S_WIDTH(3),
    .WIDTH(1)
  ) \$techmap\SensorFSM_1.$procmux$285  (
    .A(1'b0),
    .B({ \SensorFSM_1.Enable_i , 1'b1, \$techmap\SensorFSM_1.$2\SensorFSM_StoreNewValue[0:0]  }),
    .S({ \$techmap\SensorFSM_1.$procmux$210_CMP , \$techmap\SensorFSM_1.$procmux$207_CMP , \$techmap\SensorFSM_1.$procmux$202_CMP  }),
    .Y(\SensorFSM_1.SensorFSM_TimerEnable )
  );

  \$pmux  #(
    .S_WIDTH(3),
    .WIDTH(1)
  ) \$techmap\SensorFSM_1.$procmux$306  (
    .A(1'b1),
    .B({ \$techmap\SensorFSM_1.$2\SensorFSM_TimerPreset[0:0] , 1'b0, \$techmap\SensorFSM_1.$3\SensorFSM_TimerPreset[0:0]  }),
    .S({ \$techmap\SensorFSM_1.$procmux$210_CMP , \$techmap\SensorFSM_1.$procmux$207_CMP , \$techmap\SensorFSM_1.$procmux$202_CMP  }),
    .Y(\SensorFSM_1.SensorFSM_TimerPreset )
  );

  \$not  #(
    .A_SIGNED(0),
    .A_WIDTH(1),
    .Y_WIDTH(1)
  ) \$techmap\SensorFSM_1.$procmux$356  (
    .A(\SensorFSM_1.Enable_i ),
    .Y(\$techmap\SensorFSM_1.$2\SensorFSM_TimerPreset[0:0] )
  );

  \$and  #(
    .A_SIGNED(0),
    .A_WIDTH(1),
    .B_SIGNED(0),
    .B_WIDTH(1),
    .Y_WIDTH(1)
  ) \$techmap\SensorFSM_1.$procmux$397  (
    .A(\SensorFSM_1.Enable_i ),
    .B(\SensorFSM_1.SensorFSM_TimerOvfl ),
    .Y(\$techmap\SensorFSM_1.$2\MeasureFSM_Start_o[0:0] )
  );

  \$mux  #(
    .WIDTH(1)
  ) \$techmap\SensorFSM_1.$procmux$482  (
    .A(\$techmap\SensorFSM_1.$3\SensorFSM_StoreNewValue[0:0] ),
    .B(1'b0),
    .S(\SensorFSM_1.MeasureFSM_Error_i ),
    .Y(\$techmap\SensorFSM_1.$2\SensorFSM_StoreNewValue[0:0] )
  );

  \$or  #(
    .A_SIGNED(0),
    .A_WIDTH(1),
    .B_SIGNED(0),
    .B_WIDTH(1),
    .Y_WIDTH(1)
  ) \$techmap\SensorFSM_1.$procmux$526  (
    .A(\$techmap\SensorFSM_1.$4\SensorFSM_TimerPreset[0:0] ),
    .B(\SensorFSM_1.MeasureFSM_Error_i ),
    .Y(\$techmap\SensorFSM_1.$3\SensorFSM_TimerPreset[0:0] )
  );

  \$and  #(
    .A_SIGNED(0),
    .A_WIDTH(1),
    .B_SIGNED(0),
    .B_WIDTH(1),
    .Y_WIDTH(1)
  ) \$techmap\SensorFSM_1.$procmux$613  (
    .A(\SensorFSM_1.MeasureFSM_Done_i ),
    .B(\SensorFSM_1.SensorFSM_DiffTooLarge ),
    .Y(\$techmap\SensorFSM_1.$3\SensorFSM_StoreNewValue[0:0] )
  );

  \$mux  #(
    .WIDTH(32)
  ) \$techmap\SensorFSM_1.$procmux$62  (
    .A(\SensorFSM_1.SensorFSM_Timer ),
    .B(\$techmap\SensorFSM_1.$sub$../../verilog/sensorfsm.v:160$51_Y ),
    .S(\SensorFSM_1.SensorFSM_TimerEnable ),
    .Y(\$techmap\SensorFSM_1.$procmux$62_Y )
  );

  \$mux  #(
    .WIDTH(32)
  ) \$techmap\SensorFSM_1.$procmux$65  (
    .A(\$techmap\SensorFSM_1.$procmux$62_Y ),
    .B({ \SensorFSM_1.ParamCounterPresetH_i , \SensorFSM_1.ParamCounterPresetL_i  }),
    .S(\SensorFSM_1.SensorFSM_TimerPreset ),
    .Y(\$techmap\SensorFSM_1.$0\SensorFSM_Timer[31:0] )
  );

  \$mux  #(
    .WIDTH(1)
  ) \$techmap\SensorFSM_1.$procmux$659  (
    .A(1'b1),
    .B(\$techmap\SensorFSM_1.$5\SensorFSM_TimerPreset[0:0] ),
    .S(\SensorFSM_1.MeasureFSM_Done_i ),
    .Y(\$techmap\SensorFSM_1.$4\SensorFSM_TimerPreset[0:0] )
  );

  \$mux  #(
    .WIDTH(16)
  ) \$techmap\SensorFSM_1.$procmux$68  (
    .A(\SensorFSM_1.Word0 ),
    .B({ \SensorFSM_1.MeasureFSM_Byte1_i , \SensorFSM_1.MeasureFSM_Byte0_i  }),
    .S(\SensorFSM_1.SensorFSM_StoreNewValue ),
    .Y(\$techmap\SensorFSM_1.$0\Word0[15:0] )
  );

  \$not  #(
    .A_SIGNED(0),
    .A_WIDTH(1),
    .Y_WIDTH(1)
  ) \$techmap\SensorFSM_1.$procmux$705  (
    .A(\SensorFSM_1.SensorFSM_DiffTooLarge ),
    .Y(\$techmap\SensorFSM_1.$5\SensorFSM_TimerPreset[0:0] )
  );

  (* src = "../../verilog/sensorfsm.v:160" *)
  \$sub  #(
    .A_SIGNED(0),
    .A_WIDTH(32),
    .B_SIGNED(0),
    .B_WIDTH(1),
    .Y_WIDTH(32)
  ) \$techmap\SensorFSM_1.$sub$../../verilog/sensorfsm.v:160$51  (
    .A(\SensorFSM_1.SensorFSM_Timer ),
    .B(1'b1),
    .Y(\$techmap\SensorFSM_1.$sub$../../verilog/sensorfsm.v:160$51_Y )
  );

  (* src = "../../verilog/sensorfsm.v:186" *)
  \$sub  #(
    .A_SIGNED(0),
    .A_WIDTH(17),
    .B_SIGNED(0),
    .B_WIDTH(17),
    .Y_WIDTH(17)
  ) \$techmap\SensorFSM_1.$sub$../../verilog/sensorfsm.v:186$56  (
    .A({ 1'b0, \SensorFSM_1.MeasureFSM_Byte1_i , \SensorFSM_1.MeasureFSM_Byte0_i  }),
    .B({ 1'b0, \SensorFSM_1.Word0  }),
    .Y(\SensorFSM_1.DiffAB )
  );

  (* src = "../../verilog/sensorfsm.v:187" *)
  \$sub  #(
    .A_SIGNED(0),
    .A_WIDTH(16),
    .B_SIGNED(0),
    .B_WIDTH(16),
    .Y_WIDTH(16)
  ) \$techmap\SensorFSM_1.$sub$../../verilog/sensorfsm.v:187$57  (
    .A(\SensorFSM_1.Word0 ),
    .B({ \SensorFSM_1.MeasureFSM_Byte1_i , \SensorFSM_1.MeasureFSM_Byte0_i  }),
    .Y(\SensorFSM_1.DiffBA )
  );

  (* src = "../../verilog/sensorfsm.v:188" *)
  \$mux  #(
    .WIDTH(16)
  ) \$techmap\SensorFSM_1.$ternary$../../verilog/sensorfsm.v:188$58  (
    .A(\SensorFSM_1.DiffAB [15:0]),
    .B(\SensorFSM_1.DiffBA ),
    .S(\SensorFSM_1.DiffAB [16]),
    .Y(\SensorFSM_1.AbsDiffResult )
  );
  assign I2CFSM_Byte0_s = \I2CFSM_1.Byte0_o ;
  assign I2CFSM_Byte1_s = \I2CFSM_1.Byte1_o ;
  assign \I2CFSM_1.Clk_i  = Clk_i;
  assign I2CFSM_Done_s = \I2CFSM_1.Done_o ;
  assign I2CFSM_Error_s = \I2CFSM_1.Error_o ;
  assign \I2CFSM_1.I2C_Busy_i  = I2C_Busy_i;
  assign \I2CFSM_1.I2C_Data_i  = I2C_Data_i;
  assign I2C_Data_o = \I2CFSM_1.I2C_Data_o ;
  assign \I2CFSM_1.I2C_Error_i  = I2C_Error_i;
  assign I2C_FIFOReadNext_o = \I2CFSM_1.I2C_FIFOReadNext_o ;
  assign I2C_FIFOWrite_o = \I2CFSM_1.I2C_FIFOWrite_o ;
  assign I2C_ReadCount_o = \I2CFSM_1.I2C_ReadCount_o ;
  assign I2C_ReceiveSend_n_o = \I2CFSM_1.I2C_ReceiveSend_n_o ;
  assign I2C_StartProcess_o = \I2CFSM_1.I2C_StartProcess_o ;
  assign \I2CFSM_1.ParamCounterPresetH_i  = WaitCounterPresetH_i;
  assign \I2CFSM_1.ParamCounterPresetL_i  = WaitCounterPresetL_i;
  assign \I2CFSM_1.Reset_n_i  = Reset_n_i;
  assign \I2CFSM_1.Start_i  = I2CFSM_Start_s;
  assign \SensorFSM_1.Clk_i  = Clk_i;
  assign CpuIntr_o = \SensorFSM_1.CpuIntr_o ;
  assign \SensorFSM_1.Enable_i  = Enable_i;
  assign \SensorFSM_1.MeasureFSM_Byte0_i  = I2CFSM_Byte0_s;
  assign \SensorFSM_1.MeasureFSM_Byte1_i  = I2CFSM_Byte1_s;
  assign \SensorFSM_1.MeasureFSM_Done_i  = I2CFSM_Done_s;
  assign \SensorFSM_1.MeasureFSM_Error_i  = I2CFSM_Error_s;
  assign I2CFSM_Start_s = \SensorFSM_1.MeasureFSM_Start_o ;
  assign \SensorFSM_1.ParamCounterPresetH_i  = PeriodCounterPresetH_i;
  assign \SensorFSM_1.ParamCounterPresetL_i  = PeriodCounterPresetL_i;
  assign \SensorFSM_1.ParamThreshold_i  = Threshold_i;
  assign \SensorFSM_1.Reset_n_i  = Reset_n_i;
  assign SensorValue_o = \SensorFSM_1.SensorValue_o ;
  assign \SensorFSM_1.SensorValue  = { \SensorFSM_1.MeasureFSM_Byte1_i , \SensorFSM_1.MeasureFSM_Byte0_i  };
  assign \SensorFSM_1.SensorValue_o  = \SensorFSM_1.Word0 ;
  assign I2CFSM_1_CfgMode_s = 1'b0;
  assign I2CFSM_1_CfgClk_s = 1'b0;
  assign I2CFSM_1_CfgShift_s = 1'b0;
  assign I2CFSM_1_CfgDataIn_s = 1'b0;
  assign SensorFSM_1_CfgMode_s = 1'b0;
  assign SensorFSM_1_CfgClk_s = 1'b0;
  assign SensorFSM_1_CfgShift_s = 1'b0;
  assign SensorFSM_1_CfgDataIn_s = 1'b0;

endmodule
