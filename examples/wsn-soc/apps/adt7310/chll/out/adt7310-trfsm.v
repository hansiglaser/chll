(* src = "../../verilog/adt7310.v:1" *)
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

  (* src = "../../verilog/spifsm.v:187" *)
  wire [7:0] \$techmap\SPIFSM_1.$0\Byte0_o[7:0] ;
  (* src = "../../verilog/spifsm.v:187" *)
  wire [7:0] \$techmap\SPIFSM_1.$0\Byte1_o[7:0] ;
  (* src = "../../verilog/spifsm.v:217" *)
  wire [31:0] \$techmap\SPIFSM_1.$0\SPI_FSM_Timer[31:0] ;
  (* src = "../../verilog/spifsm.v:65" *)
  wire \$techmap\SPIFSM_1.$2\ADT7310CS_n_o[0:0] ;
  (* src = "../../verilog/spifsm.v:65" *)
  wire \$techmap\SPIFSM_1.$2\SPI_FSM_TimerEnable[0:0] ;
  (* src = "../../verilog/spifsm.v:65" *)
  wire \$techmap\SPIFSM_1.$2\SPI_ReadNext_o[0:0] ;
  wire \$techmap\SPIFSM_1.$auto$opt_reduce.cc:126:opt_mux$2453 ;
  wire \$techmap\SPIFSM_1.$auto$opt_reduce.cc:126:opt_mux$2455 ;
  wire \$techmap\SPIFSM_1.$auto$opt_reduce.cc:126:opt_mux$2457 ;
  wire \$techmap\SPIFSM_1.$auto$opt_reduce.cc:126:opt_mux$2459 ;
  wire \$techmap\SPIFSM_1.$auto$opt_reduce.cc:126:opt_mux$2461 ;
  wire \$techmap\SPIFSM_1.$auto$opt_reduce.cc:126:opt_mux$2477 ;
  wire \$techmap\SPIFSM_1.$procmux$297_CMP ;
  wire \$techmap\SPIFSM_1.$procmux$298_CMP ;
  wire \$techmap\SPIFSM_1.$procmux$301_CMP ;
  wire \$techmap\SPIFSM_1.$procmux$302_CMP ;
  wire \$techmap\SPIFSM_1.$procmux$305_CMP ;
  wire \$techmap\SPIFSM_1.$procmux$334_CMP ;
  wire \$techmap\SPIFSM_1.$procmux$335_CMP ;
  wire \$techmap\SPIFSM_1.$procmux$336_CMP ;
  wire [31:0] \$techmap\SPIFSM_1.$procmux$80_Y ;
  (* src = "../../verilog/spifsm.v:231" *)
  wire [31:0] \$techmap\SPIFSM_1.$sub$../../verilog/spifsm.v:231$46_Y ;
  (* src = "../../verilog/sensorfsm.v:130" *)
  wire [15:0] \$techmap\SensorFSM_1.$0\SensorFSM_Timer[15:0] ;
  (* src = "../../verilog/sensorfsm.v:153" *)
  wire [15:0] \$techmap\SensorFSM_1.$0\Word0[15:0] ;
  (* src = "../../verilog/sensorfsm.v:57" *)
  wire \$techmap\SensorFSM_1.$2\MeasureFSM_Start_o[0:0] ;
  (* src = "../../verilog/sensorfsm.v:57" *)
  wire \$techmap\SensorFSM_1.$2\SensorFSM_StoreNewValue[0:0] ;
  (* src = "../../verilog/sensorfsm.v:57" *)
  wire \$techmap\SensorFSM_1.$2\SensorFSM_TimerPreset[0:0] ;
  (* src = "../../verilog/sensorfsm.v:57" *)
  wire \$techmap\SensorFSM_1.$3\SensorFSM_TimerPreset[0:0] ;
  (* src = "../../verilog/sensorfsm.v:57" *)
  wire \$techmap\SensorFSM_1.$4\SensorFSM_TimerPreset[0:0] ;
  wire \$techmap\SensorFSM_1.$auto$opt_reduce.cc:126:opt_mux$2463 ;
  wire \$techmap\SensorFSM_1.$procmux$1004_CMP ;
  wire \$techmap\SensorFSM_1.$procmux$1007_CMP ;
  wire \$techmap\SensorFSM_1.$procmux$1129_CMP ;
  wire [15:0] \$techmap\SensorFSM_1.$procmux$826_Y ;
  (* src = "../../verilog/sensorfsm.v:144" *)
  wire [15:0] \$techmap\SensorFSM_1.$sub$../../verilog/sensorfsm.v:144$59_Y ;
  (* src = "../../verilog/spifsm.v:24" *)
  wire \SPIFSM_1.ADT7310CS_n_o ;
  (* src = "../../verilog/spifsm.v:13" *)
  wire [7:0] \SPIFSM_1.Byte0_o ;
  (* src = "../../verilog/spifsm.v:14" *)
  wire [7:0] \SPIFSM_1.Byte1_o ;
  (* src = "../../verilog/spifsm.v:9" *)
  wire \SPIFSM_1.Clk_i ;
  (* src = "../../verilog/spifsm.v:12" *)
  wire \SPIFSM_1.Done_o ;
  (* src = "../../verilog/spifsm.v:26" *)
  wire [31:0] \SPIFSM_1.ParamCounterPreset_i ;
  (* src = "../../verilog/spifsm.v:8" *)
  wire \SPIFSM_1.Reset_n_i ;
  (* src = "../../verilog/spifsm.v:20" *)
  wire [7:0] \SPIFSM_1.SPI_Data_i ;
  (* src = "../../verilog/spifsm.v:19" *)
  wire [7:0] \SPIFSM_1.SPI_Data_o ;
  (* src = "../../verilog/spifsm.v:22" *)
  wire \SPIFSM_1.SPI_FIFOEmpty_i ;
  (* src = "../../verilog/spifsm.v:21" *)
  wire \SPIFSM_1.SPI_FIFOFull_i ;
  (* src = "../../verilog/spifsm.v:215" *)
  wire [31:0] \SPIFSM_1.SPI_FSM_Timer ;
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
  (* src = "../../verilog/spifsm.v:18" *)
  wire \SPIFSM_1.SPI_ReadNext_o ;
  (* src = "../../verilog/spifsm.v:16" *)
  wire \SPIFSM_1.SPI_Transmission_i ;
  (* src = "../../verilog/spifsm.v:17" *)
  wire \SPIFSM_1.SPI_Write_o ;
  (* src = "../../verilog/spifsm.v:11" *)
  wire \SPIFSM_1.Start_i ;
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
  (* src = "../../verilog/sensorfsm.v:7" *)
  wire \SensorFSM_1.Clk_i ;
  (* src = "../../verilog/sensorfsm.v:10" *)
  wire \SensorFSM_1.CpuIntr_o ;
  (* src = "../../verilog/sensorfsm.v:168" *)
  wire [16:0] \SensorFSM_1.DiffAB ;
  (* src = "../../verilog/sensorfsm.v:169" *)
  wire [15:0] \SensorFSM_1.DiffBA ;
  (* src = "../../verilog/sensorfsm.v:9" *)
  wire \SensorFSM_1.Enable_i ;
  (* src = "../../verilog/sensorfsm.v:15" *)
  wire [7:0] \SensorFSM_1.MeasureFSM_Byte0_i ;
  (* src = "../../verilog/sensorfsm.v:16" *)
  wire [7:0] \SensorFSM_1.MeasureFSM_Byte1_i ;
  (* src = "../../verilog/sensorfsm.v:14" *)
  wire \SensorFSM_1.MeasureFSM_Done_i ;
  (* src = "../../verilog/sensorfsm.v:13" *)
  wire \SensorFSM_1.MeasureFSM_Start_o ;
  (* src = "../../verilog/sensorfsm.v:19" *)
  wire [15:0] \SensorFSM_1.ParamCounterPreset_i ;
  (* src = "../../verilog/sensorfsm.v:18" *)
  wire [15:0] \SensorFSM_1.ParamThreshold_i ;
  (* src = "../../verilog/sensorfsm.v:6" *)
  wire \SensorFSM_1.Reset_n_i ;
  (* src = "../../verilog/sensorfsm.v:32" *)
  wire \SensorFSM_1.SensorFSM_DiffTooLarge ;
  (* src = "../../verilog/sensorfsm.v:33" *)
  wire \SensorFSM_1.SensorFSM_StoreNewValue ;
  (* src = "../../verilog/sensorfsm.v:128" *)
  wire [15:0] \SensorFSM_1.SensorFSM_Timer ;
  (* src = "../../verilog/sensorfsm.v:31" *)
  wire \SensorFSM_1.SensorFSM_TimerEnable ;
  (* src = "../../verilog/sensorfsm.v:29" *)
  wire \SensorFSM_1.SensorFSM_TimerOvfl ;
  (* src = "../../verilog/sensorfsm.v:30" *)
  wire \SensorFSM_1.SensorFSM_TimerPreset ;
  (* src = "../../verilog/sensorfsm.v:37" *)
  wire [15:0] \SensorFSM_1.SensorValue ;
  (* src = "../../verilog/sensorfsm.v:11" *)
  wire [15:0] \SensorFSM_1.SensorValue_o ;
  (* src = "../../verilog/sensorfsm.v:38" *)
  wire [15:0] \SensorFSM_1.Word0 ;
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
  wire SensorFSM_1_Out3_s;
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
    .A_WIDTH(3),
    .Y_WIDTH(1)
  ) \$techmap\SPIFSM_1.$auto$opt_reduce.cc:130:opt_mux$2454  (
    .A({ \SPIFSM_1.SPI_FSM_Wr1 , \SPIFSM_1.SPI_FSM_Wr0 , \$techmap\SPIFSM_1.$procmux$298_CMP  }),
    .Y(\$techmap\SPIFSM_1.$auto$opt_reduce.cc:126:opt_mux$2453 )
  );

  \$reduce_or  #(
    .A_SIGNED(0),
    .A_WIDTH(6),
    .Y_WIDTH(1)
  ) \$techmap\SPIFSM_1.$auto$opt_reduce.cc:130:opt_mux$2456  (
    .A({ \$techmap\SPIFSM_1.$procmux$336_CMP , \$techmap\SPIFSM_1.$procmux$335_CMP , \$techmap\SPIFSM_1.$procmux$334_CMP , \$techmap\SPIFSM_1.$procmux$302_CMP , \$techmap\SPIFSM_1.$procmux$301_CMP , \$techmap\SPIFSM_1.$procmux$298_CMP  }),
    .Y(\$techmap\SPIFSM_1.$auto$opt_reduce.cc:126:opt_mux$2455 )
  );

  \$reduce_or  #(
    .A_SIGNED(0),
    .A_WIDTH(9),
    .Y_WIDTH(1)
  ) \$techmap\SPIFSM_1.$auto$opt_reduce.cc:130:opt_mux$2458  (
    .A({ \SPIFSM_1.SPI_FSM_Wr1 , \SPIFSM_1.SPI_FSM_Wr0 , \$techmap\SPIFSM_1.$procmux$336_CMP , \$techmap\SPIFSM_1.$procmux$335_CMP , \$techmap\SPIFSM_1.$procmux$334_CMP , \$techmap\SPIFSM_1.$procmux$302_CMP , \$techmap\SPIFSM_1.$procmux$301_CMP , \$techmap\SPIFSM_1.$procmux$298_CMP , \$techmap\SPIFSM_1.$procmux$297_CMP  }),
    .Y(\$techmap\SPIFSM_1.$auto$opt_reduce.cc:126:opt_mux$2457 )
  );

  \$reduce_or  #(
    .A_SIGNED(0),
    .A_WIDTH(2),
    .Y_WIDTH(1)
  ) \$techmap\SPIFSM_1.$auto$opt_reduce.cc:130:opt_mux$2460  (
    .A({ \$techmap\SPIFSM_1.$procmux$336_CMP , \$techmap\SPIFSM_1.$procmux$335_CMP  }),
    .Y(\$techmap\SPIFSM_1.$auto$opt_reduce.cc:126:opt_mux$2459 )
  );

  \$reduce_or  #(
    .A_SIGNED(0),
    .A_WIDTH(3),
    .Y_WIDTH(1)
  ) \$techmap\SPIFSM_1.$auto$opt_reduce.cc:130:opt_mux$2462  (
    .A({ \$techmap\SPIFSM_1.$procmux$336_CMP , \$techmap\SPIFSM_1.$procmux$335_CMP , \$techmap\SPIFSM_1.$procmux$302_CMP  }),
    .Y(\$techmap\SPIFSM_1.$auto$opt_reduce.cc:126:opt_mux$2461 )
  );

  \$reduce_or  #(
    .A_SIGNED(0),
    .A_WIDTH(2),
    .Y_WIDTH(1)
  ) \$techmap\SPIFSM_1.$auto$opt_reduce.cc:130:opt_mux$2478  (
    .A({ \$techmap\SPIFSM_1.$procmux$334_CMP , \$techmap\SPIFSM_1.$procmux$301_CMP  }),
    .Y(\$techmap\SPIFSM_1.$auto$opt_reduce.cc:126:opt_mux$2477 )
  );

  (* src = "../../verilog/spifsm.v:236" *)
  \$eq  #(
    .A_SIGNED(0),
    .A_WIDTH(32),
    .B_SIGNED(0),
    .B_WIDTH(32),
    .Y_WIDTH(1)
  ) \$techmap\SPIFSM_1.$eq$../../verilog/spifsm.v:236$47  (
    .A(\SPIFSM_1.SPI_FSM_Timer ),
    .B(0),
    .Y(\SPIFSM_1.SPI_FSM_TimerOvfl )
  );

  SPIFSM SPIFSM_1 (
    .Reset_n_i(\SPIFSM_1.Reset_n_i ),
    .Clk_i(\SPIFSM_1.Clk_i ),
    .In0_i(\SPIFSM_1.SPI_FSM_TimerOvfl ),
    .In1_i(\SPIFSM_1.SPI_Transmission_i ),
    .In2_i(\SPIFSM_1.Start_i ),
    .In3_i(1'b0),
    .In4_i(1'b0),
    .In5_i(1'b0),
    .In6_i(1'b0),
    .In7_i(1'b0),
    .Out0_o(\$techmap\SPIFSM_1.$procmux$297_CMP ),
    .Out1_o(\$techmap\SPIFSM_1.$procmux$298_CMP ),
    .Out2_o(\$techmap\SPIFSM_1.$procmux$301_CMP ),
    .Out3_o(\$techmap\SPIFSM_1.$procmux$302_CMP ),
    .Out4_o(\$techmap\SPIFSM_1.$procmux$305_CMP ),
    .Out5_o(\$techmap\SPIFSM_1.$procmux$334_CMP ),
    .Out6_o(\$techmap\SPIFSM_1.$procmux$335_CMP ),
    .Out7_o(\$techmap\SPIFSM_1.$procmux$336_CMP ),
    .Out8_o(\SPIFSM_1.SPI_FSM_Wr0 ),
    .Out9_o(\SPIFSM_1.SPI_FSM_Wr1 ),
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

  (* src = "../../verilog/spifsm.v:187" *)
  \$adff  #(
    .ARST_POLARITY(1'b0),
    .ARST_VALUE(8'b00000000),
    .CLK_POLARITY(1'b1),
    .WIDTH(8)
  ) \$techmap\SPIFSM_1.$procdff$2439  (
    .ARST(\SPIFSM_1.Reset_n_i ),
    .CLK(\SPIFSM_1.Clk_i ),
    .D(\$techmap\SPIFSM_1.$0\Byte0_o[7:0] ),
    .Q(\SPIFSM_1.Byte0_o )
  );

  (* src = "../../verilog/spifsm.v:187" *)
  \$adff  #(
    .ARST_POLARITY(1'b0),
    .ARST_VALUE(8'b00000000),
    .CLK_POLARITY(1'b1),
    .WIDTH(8)
  ) \$techmap\SPIFSM_1.$procdff$2440  (
    .ARST(\SPIFSM_1.Reset_n_i ),
    .CLK(\SPIFSM_1.Clk_i ),
    .D(\$techmap\SPIFSM_1.$0\Byte1_o[7:0] ),
    .Q(\SPIFSM_1.Byte1_o )
  );

  (* src = "../../verilog/spifsm.v:217" *)
  \$adff  #(
    .ARST_POLARITY(1'b0),
    .ARST_VALUE(0),
    .CLK_POLARITY(1'b1),
    .WIDTH(32)
  ) \$techmap\SPIFSM_1.$procdff$2441  (
    .ARST(\SPIFSM_1.Reset_n_i ),
    .CLK(\SPIFSM_1.Clk_i ),
    .D(\$techmap\SPIFSM_1.$0\SPI_FSM_Timer[31:0] ),
    .Q(\SPIFSM_1.SPI_FSM_Timer )
  );

  \$pmux  #(
    .S_WIDTH(2),
    .WIDTH(1)
  ) \$techmap\SPIFSM_1.$procmux$296  (
    .A(1'b0),
    .B({ 1'b1, \$techmap\SPIFSM_1.$2\SPI_FSM_TimerEnable[0:0]  }),
    .S({ \$techmap\SPIFSM_1.$procmux$298_CMP , \$techmap\SPIFSM_1.$procmux$297_CMP  }),
    .Y(\SPIFSM_1.SPI_FSM_TimerEnable )
  );

  \$pmux  #(
    .S_WIDTH(2),
    .WIDTH(1)
  ) \$techmap\SPIFSM_1.$procmux$317  (
    .A(1'b1),
    .B({ 1'b0, \SPIFSM_1.SPI_FSM_TimerOvfl  }),
    .S({ \$techmap\SPIFSM_1.$procmux$298_CMP , \$techmap\SPIFSM_1.$procmux$297_CMP  }),
    .Y(\SPIFSM_1.SPI_FSM_TimerPreset )
  );

  \$pmux  #(
    .S_WIDTH(2),
    .WIDTH(1)
  ) \$techmap\SPIFSM_1.$procmux$371  (
    .A(1'b0),
    .B({ \$techmap\SPIFSM_1.$2\SPI_ReadNext_o[0:0] , 1'b1 }),
    .S({ \$techmap\SPIFSM_1.$auto$opt_reduce.cc:126:opt_mux$2477 , \$techmap\SPIFSM_1.$auto$opt_reduce.cc:126:opt_mux$2453  }),
    .Y(\SPIFSM_1.SPI_ReadNext_o )
  );

  \$pmux  #(
    .S_WIDTH(3),
    .WIDTH(1)
  ) \$techmap\SPIFSM_1.$procmux$396  (
    .A(1'b1),
    .B({ \$techmap\SPIFSM_1.$2\ADT7310CS_n_o[0:0] , \$techmap\SPIFSM_1.$2\SPI_FSM_TimerEnable[0:0] , 1'b0 }),
    .S({ \$techmap\SPIFSM_1.$procmux$305_CMP , \$techmap\SPIFSM_1.$procmux$297_CMP , \$techmap\SPIFSM_1.$auto$opt_reduce.cc:126:opt_mux$2455  }),
    .Y(\SPIFSM_1.ADT7310CS_n_o )
  );

  \$pmux  #(
    .S_WIDTH(2),
    .WIDTH(1)
  ) \$techmap\SPIFSM_1.$procmux$413  (
    .A(1'b1),
    .B({ \$techmap\SPIFSM_1.$2\ADT7310CS_n_o[0:0] , 1'b0 }),
    .S({ \$techmap\SPIFSM_1.$procmux$305_CMP , \$techmap\SPIFSM_1.$auto$opt_reduce.cc:126:opt_mux$2457  }),
    .Y(\SPIFSM_1.Done_o )
  );

  \$pmux  #(
    .S_WIDTH(3),
    .WIDTH(8)
  ) \$techmap\SPIFSM_1.$procmux$439  (
    .A(8'b00001000),
    .B(24'b001000000101000011111111),
    .S({ \$techmap\SPIFSM_1.$procmux$302_CMP , \$techmap\SPIFSM_1.$procmux$297_CMP , \$techmap\SPIFSM_1.$auto$opt_reduce.cc:126:opt_mux$2459  }),
    .Y(\SPIFSM_1.SPI_Data_o )
  );

  \$pmux  #(
    .S_WIDTH(3),
    .WIDTH(1)
  ) \$techmap\SPIFSM_1.$procmux$481  (
    .A(1'b0),
    .B({ \SPIFSM_1.Start_i , \SPIFSM_1.SPI_FSM_TimerOvfl , 1'b1 }),
    .S({ \$techmap\SPIFSM_1.$procmux$305_CMP , \$techmap\SPIFSM_1.$procmux$297_CMP , \$techmap\SPIFSM_1.$auto$opt_reduce.cc:126:opt_mux$2461  }),
    .Y(\SPIFSM_1.SPI_Write_o )
  );

  \$not  #(
    .A_SIGNED(0),
    .A_WIDTH(1),
    .Y_WIDTH(1)
  ) \$techmap\SPIFSM_1.$procmux$535  (
    .A(\SPIFSM_1.Start_i ),
    .Y(\$techmap\SPIFSM_1.$2\ADT7310CS_n_o[0:0] )
  );

  \$not  #(
    .A_SIGNED(0),
    .A_WIDTH(1),
    .Y_WIDTH(1)
  ) \$techmap\SPIFSM_1.$procmux$641  (
    .A(\SPIFSM_1.SPI_Transmission_i ),
    .Y(\$techmap\SPIFSM_1.$2\SPI_ReadNext_o[0:0] )
  );

  \$not  #(
    .A_SIGNED(0),
    .A_WIDTH(1),
    .Y_WIDTH(1)
  ) \$techmap\SPIFSM_1.$procmux$659  (
    .A(\SPIFSM_1.SPI_FSM_TimerOvfl ),
    .Y(\$techmap\SPIFSM_1.$2\SPI_FSM_TimerEnable[0:0] )
  );

  \$mux  #(
    .WIDTH(8)
  ) \$techmap\SPIFSM_1.$procmux$70  (
    .A(\SPIFSM_1.Byte0_o ),
    .B(\SPIFSM_1.SPI_Data_i ),
    .S(\SPIFSM_1.SPI_FSM_Wr0 ),
    .Y(\$techmap\SPIFSM_1.$0\Byte0_o[7:0] )
  );

  \$mux  #(
    .WIDTH(8)
  ) \$techmap\SPIFSM_1.$procmux$77  (
    .A(\SPIFSM_1.Byte1_o ),
    .B(\SPIFSM_1.SPI_Data_i ),
    .S(\SPIFSM_1.SPI_FSM_Wr1 ),
    .Y(\$techmap\SPIFSM_1.$0\Byte1_o[7:0] )
  );

  \$mux  #(
    .WIDTH(32)
  ) \$techmap\SPIFSM_1.$procmux$80  (
    .A(\SPIFSM_1.SPI_FSM_Timer ),
    .B(\$techmap\SPIFSM_1.$sub$../../verilog/spifsm.v:231$46_Y ),
    .S(\SPIFSM_1.SPI_FSM_TimerEnable ),
    .Y(\$techmap\SPIFSM_1.$procmux$80_Y )
  );

  \$mux  #(
    .WIDTH(32)
  ) \$techmap\SPIFSM_1.$procmux$83  (
    .A(\$techmap\SPIFSM_1.$procmux$80_Y ),
    .B(\SPIFSM_1.ParamCounterPreset_i ),
    .S(\SPIFSM_1.SPI_FSM_TimerPreset ),
    .Y(\$techmap\SPIFSM_1.$0\SPI_FSM_Timer[31:0] )
  );

  (* src = "../../verilog/spifsm.v:231" *)
  \$sub  #(
    .A_SIGNED(0),
    .A_WIDTH(32),
    .B_SIGNED(0),
    .B_WIDTH(1),
    .Y_WIDTH(32)
  ) \$techmap\SPIFSM_1.$sub$../../verilog/spifsm.v:231$46  (
    .A(\SPIFSM_1.SPI_FSM_Timer ),
    .B(1'b1),
    .Y(\$techmap\SPIFSM_1.$sub$../../verilog/spifsm.v:231$46_Y )
  );

  \$reduce_or  #(
    .A_SIGNED(0),
    .A_WIDTH(3),
    .Y_WIDTH(1)
  ) \$techmap\SensorFSM_1.$auto$opt_reduce.cc:130:opt_mux$2464  (
    .A({ \$techmap\SensorFSM_1.$procmux$1129_CMP , \$techmap\SensorFSM_1.$procmux$1007_CMP , \$techmap\SensorFSM_1.$procmux$1004_CMP  }),
    .Y(\$techmap\SensorFSM_1.$auto$opt_reduce.cc:126:opt_mux$2463 )
  );

  (* src = "../../verilog/sensorfsm.v:149" *)
  \$eq  #(
    .A_SIGNED(0),
    .A_WIDTH(16),
    .B_SIGNED(0),
    .B_WIDTH(16),
    .Y_WIDTH(1)
  ) \$techmap\SensorFSM_1.$eq$../../verilog/sensorfsm.v:149$60  (
    .A(\SensorFSM_1.SensorFSM_Timer ),
    .B(16'b0000000000000000),
    .Y(\SensorFSM_1.SensorFSM_TimerOvfl )
  );

  SensorFSM SensorFSM_1 (
    .Reset_n_i(\SensorFSM_1.Reset_n_i ),
    .Clk_i(\SensorFSM_1.Clk_i ),
    .In0_i(\SensorFSM_1.Enable_i ),
    .In1_i(\SensorFSM_1.MeasureFSM_Done_i ),
    .In2_i(\SensorFSM_1.SensorFSM_DiffTooLarge ),
    .In3_i(\SensorFSM_1.SensorFSM_TimerOvfl ),
    .In4_i(1'b0),
    .In5_i(1'b0),
    .In6_i(1'b0),
    .In7_i(1'b0),
    .In8_i(1'b0),
    .In9_i(1'b0),
    .Out0_o(\$techmap\SensorFSM_1.$procmux$1004_CMP ),
    .Out1_o(\$techmap\SensorFSM_1.$procmux$1007_CMP ),
    .Out2_o(\$techmap\SensorFSM_1.$procmux$1129_CMP ),
    .Out3_o(SensorFSM_1_Out3_s),
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

  (* src = "../../verilog/sensorfsm.v:174" *)
  \$gt  #(
    .A_SIGNED(0),
    .A_WIDTH(16),
    .B_SIGNED(0),
    .B_WIDTH(16),
    .Y_WIDTH(1)
  ) \$techmap\SensorFSM_1.$gt$../../verilog/sensorfsm.v:174$67  (
    .A(\SensorFSM_1.AbsDiffResult ),
    .B(\SensorFSM_1.ParamThreshold_i ),
    .Y(\SensorFSM_1.SensorFSM_DiffTooLarge )
  );

  (* src = "../../verilog/sensorfsm.v:130" *)
  \$adff  #(
    .ARST_POLARITY(1'b0),
    .ARST_VALUE(16'b0000000000000000),
    .CLK_POLARITY(1'b1),
    .WIDTH(16)
  ) \$techmap\SensorFSM_1.$procdff$2443  (
    .ARST(\SensorFSM_1.Reset_n_i ),
    .CLK(\SensorFSM_1.Clk_i ),
    .D(\$techmap\SensorFSM_1.$0\SensorFSM_Timer[15:0] ),
    .Q(\SensorFSM_1.SensorFSM_Timer )
  );

  (* src = "../../verilog/sensorfsm.v:153" *)
  \$adff  #(
    .ARST_POLARITY(1'b0),
    .ARST_VALUE(16'b0000000000000000),
    .CLK_POLARITY(1'b1),
    .WIDTH(16)
  ) \$techmap\SensorFSM_1.$procdff$2444  (
    .ARST(\SensorFSM_1.Reset_n_i ),
    .CLK(\SensorFSM_1.Clk_i ),
    .D(\$techmap\SensorFSM_1.$0\Word0[15:0] ),
    .Q(\SensorFSM_1.Word0 )
  );

  \$not  #(
    .A_SIGNED(0),
    .A_WIDTH(1),
    .Y_WIDTH(1)
  ) \$techmap\SensorFSM_1.$procmux$1036  (
    .A(\SensorFSM_1.Enable_i ),
    .Y(\$techmap\SensorFSM_1.$2\SensorFSM_TimerPreset[0:0] )
  );

  \$and  #(
    .A_SIGNED(0),
    .A_WIDTH(1),
    .B_SIGNED(0),
    .B_WIDTH(1),
    .Y_WIDTH(1)
  ) \$techmap\SensorFSM_1.$procmux$1065  (
    .A(\SensorFSM_1.Enable_i ),
    .B(\SensorFSM_1.SensorFSM_TimerOvfl ),
    .Y(\$techmap\SensorFSM_1.$2\MeasureFSM_Start_o[0:0] )
  );

  \$and  #(
    .A_SIGNED(0),
    .A_WIDTH(1),
    .B_SIGNED(0),
    .B_WIDTH(1),
    .Y_WIDTH(1)
  ) \$techmap\SensorFSM_1.$procmux$1142  (
    .A(\SensorFSM_1.MeasureFSM_Done_i ),
    .B(\SensorFSM_1.SensorFSM_DiffTooLarge ),
    .Y(\$techmap\SensorFSM_1.$2\SensorFSM_StoreNewValue[0:0] )
  );

  \$mux  #(
    .WIDTH(1)
  ) \$techmap\SensorFSM_1.$procmux$1174  (
    .A(1'b1),
    .B(\$techmap\SensorFSM_1.$4\SensorFSM_TimerPreset[0:0] ),
    .S(\SensorFSM_1.MeasureFSM_Done_i ),
    .Y(\$techmap\SensorFSM_1.$3\SensorFSM_TimerPreset[0:0] )
  );

  \$not  #(
    .A_SIGNED(0),
    .A_WIDTH(1),
    .Y_WIDTH(1)
  ) \$techmap\SensorFSM_1.$procmux$1206  (
    .A(\SensorFSM_1.SensorFSM_DiffTooLarge ),
    .Y(\$techmap\SensorFSM_1.$4\SensorFSM_TimerPreset[0:0] )
  );

  \$mux  #(
    .WIDTH(16)
  ) \$techmap\SensorFSM_1.$procmux$826  (
    .A(\SensorFSM_1.SensorFSM_Timer ),
    .B(\$techmap\SensorFSM_1.$sub$../../verilog/sensorfsm.v:144$59_Y ),
    .S(\SensorFSM_1.SensorFSM_TimerEnable ),
    .Y(\$techmap\SensorFSM_1.$procmux$826_Y )
  );

  \$mux  #(
    .WIDTH(16)
  ) \$techmap\SensorFSM_1.$procmux$829  (
    .A(\$techmap\SensorFSM_1.$procmux$826_Y ),
    .B(\SensorFSM_1.ParamCounterPreset_i ),
    .S(\SensorFSM_1.SensorFSM_TimerPreset ),
    .Y(\$techmap\SensorFSM_1.$0\SensorFSM_Timer[15:0] )
  );

  \$mux  #(
    .WIDTH(16)
  ) \$techmap\SensorFSM_1.$procmux$832  (
    .A(\SensorFSM_1.Word0 ),
    .B({ \SensorFSM_1.MeasureFSM_Byte1_i , \SensorFSM_1.MeasureFSM_Byte0_i  }),
    .S(\SensorFSM_1.SensorFSM_StoreNewValue ),
    .Y(\$techmap\SensorFSM_1.$0\Word0[15:0] )
  );

  \$not  #(
    .A_SIGNED(0),
    .A_WIDTH(1),
    .Y_WIDTH(1)
  ) \$techmap\SensorFSM_1.$procmux$923  (
    .A(\$techmap\SensorFSM_1.$auto$opt_reduce.cc:126:opt_mux$2463 ),
    .Y(\SensorFSM_1.CpuIntr_o )
  );

  \$and  #(
    .A_SIGNED(0),
    .A_WIDTH(1),
    .B_SIGNED(0),
    .B_WIDTH(1),
    .Y_WIDTH(1)
  ) \$techmap\SensorFSM_1.$procmux$943  (
    .A(\$techmap\SensorFSM_1.$procmux$1004_CMP ),
    .B(\$techmap\SensorFSM_1.$2\MeasureFSM_Start_o[0:0] ),
    .Y(\SensorFSM_1.MeasureFSM_Start_o )
  );

  \$and  #(
    .A_SIGNED(0),
    .A_WIDTH(1),
    .B_SIGNED(0),
    .B_WIDTH(1),
    .Y_WIDTH(1)
  ) \$techmap\SensorFSM_1.$procmux$953  (
    .A(\$techmap\SensorFSM_1.$procmux$1129_CMP ),
    .B(\$techmap\SensorFSM_1.$2\SensorFSM_StoreNewValue[0:0] ),
    .Y(\SensorFSM_1.SensorFSM_StoreNewValue )
  );

  \$pmux  #(
    .S_WIDTH(3),
    .WIDTH(1)
  ) \$techmap\SensorFSM_1.$procmux$983  (
    .A(1'b0),
    .B({ \SensorFSM_1.Enable_i , 1'b1, \$techmap\SensorFSM_1.$2\SensorFSM_StoreNewValue[0:0]  }),
    .S({ \$techmap\SensorFSM_1.$procmux$1007_CMP , \$techmap\SensorFSM_1.$procmux$1004_CMP , \$techmap\SensorFSM_1.$procmux$1129_CMP  }),
    .Y(\SensorFSM_1.SensorFSM_TimerEnable )
  );

  \$pmux  #(
    .S_WIDTH(3),
    .WIDTH(1)
  ) \$techmap\SensorFSM_1.$procmux$998  (
    .A(1'b1),
    .B({ \$techmap\SensorFSM_1.$2\SensorFSM_TimerPreset[0:0] , 1'b0, \$techmap\SensorFSM_1.$3\SensorFSM_TimerPreset[0:0]  }),
    .S({ \$techmap\SensorFSM_1.$procmux$1007_CMP , \$techmap\SensorFSM_1.$procmux$1004_CMP , \$techmap\SensorFSM_1.$procmux$1129_CMP  }),
    .Y(\SensorFSM_1.SensorFSM_TimerPreset )
  );

  (* src = "../../verilog/sensorfsm.v:144" *)
  \$sub  #(
    .A_SIGNED(0),
    .A_WIDTH(16),
    .B_SIGNED(0),
    .B_WIDTH(1),
    .Y_WIDTH(16)
  ) \$techmap\SensorFSM_1.$sub$../../verilog/sensorfsm.v:144$59  (
    .A(\SensorFSM_1.SensorFSM_Timer ),
    .B(1'b1),
    .Y(\$techmap\SensorFSM_1.$sub$../../verilog/sensorfsm.v:144$59_Y )
  );

  (* src = "../../verilog/sensorfsm.v:170" *)
  \$sub  #(
    .A_SIGNED(0),
    .A_WIDTH(17),
    .B_SIGNED(0),
    .B_WIDTH(17),
    .Y_WIDTH(17)
  ) \$techmap\SensorFSM_1.$sub$../../verilog/sensorfsm.v:170$64  (
    .A({ 1'b0, \SensorFSM_1.MeasureFSM_Byte1_i , \SensorFSM_1.MeasureFSM_Byte0_i  }),
    .B({ 1'b0, \SensorFSM_1.Word0  }),
    .Y(\SensorFSM_1.DiffAB )
  );

  (* src = "../../verilog/sensorfsm.v:171" *)
  \$sub  #(
    .A_SIGNED(0),
    .A_WIDTH(16),
    .B_SIGNED(0),
    .B_WIDTH(16),
    .Y_WIDTH(16)
  ) \$techmap\SensorFSM_1.$sub$../../verilog/sensorfsm.v:171$65  (
    .A(\SensorFSM_1.Word0 ),
    .B({ \SensorFSM_1.MeasureFSM_Byte1_i , \SensorFSM_1.MeasureFSM_Byte0_i  }),
    .Y(\SensorFSM_1.DiffBA )
  );

  (* src = "../../verilog/sensorfsm.v:172" *)
  \$mux  #(
    .WIDTH(16)
  ) \$techmap\SensorFSM_1.$ternary$../../verilog/sensorfsm.v:172$66  (
    .A(\SensorFSM_1.DiffAB [15:0]),
    .B(\SensorFSM_1.DiffBA ),
    .S(\SensorFSM_1.DiffAB [16]),
    .Y(\SensorFSM_1.AbsDiffResult )
  );
  assign SPI_CPHA_o = 1'b1;
  assign SPI_CPOL_o = 1'b1;
  assign SPI_LSBFE_o = 1'b0;
  assign ADT7310CS_n_o = \SPIFSM_1.ADT7310CS_n_o ;
  assign SPIFSM_Byte0_s = \SPIFSM_1.Byte0_o ;
  assign SPIFSM_Byte1_s = \SPIFSM_1.Byte1_o ;
  assign \SPIFSM_1.Clk_i  = Clk_i;
  assign SPIFSM_Done_s = \SPIFSM_1.Done_o ;
  assign \SPIFSM_1.ParamCounterPreset_i  = { SPICounterPresetH_i, SPICounterPresetL_i };
  assign \SPIFSM_1.Reset_n_i  = Reset_n_i;
  assign \SPIFSM_1.SPI_Data_i  = SPI_Data_i;
  assign SPI_Data_o = \SPIFSM_1.SPI_Data_o ;
  assign \SPIFSM_1.SPI_FIFOEmpty_i  = SPI_FIFOEmpty_i;
  assign \SPIFSM_1.SPI_FIFOFull_i  = SPI_FIFOFull_i;
  assign SPI_ReadNext_o = \SPIFSM_1.SPI_ReadNext_o ;
  assign \SPIFSM_1.SPI_Transmission_i  = SPI_Transmission_i;
  assign SPI_Write_o = \SPIFSM_1.SPI_Write_o ;
  assign \SPIFSM_1.Start_i  = SPIFSM_Start_s;
  assign \SensorFSM_1.Clk_i  = Clk_i;
  assign CpuIntr_o = \SensorFSM_1.CpuIntr_o ;
  assign \SensorFSM_1.Enable_i  = Enable_i;
  assign \SensorFSM_1.MeasureFSM_Byte0_i  = SPIFSM_Byte0_s;
  assign \SensorFSM_1.MeasureFSM_Byte1_i  = SPIFSM_Byte1_s;
  assign \SensorFSM_1.MeasureFSM_Done_i  = SPIFSM_Done_s;
  assign SPIFSM_Start_s = \SensorFSM_1.MeasureFSM_Start_o ;
  assign \SensorFSM_1.ParamCounterPreset_i  = PeriodCounterPreset_i;
  assign \SensorFSM_1.ParamThreshold_i  = Threshold_i;
  assign \SensorFSM_1.Reset_n_i  = Reset_n_i;
  assign SensorValue_o = \SensorFSM_1.SensorValue_o ;
  assign \SensorFSM_1.SensorValue  = { \SensorFSM_1.MeasureFSM_Byte1_i , \SensorFSM_1.MeasureFSM_Byte0_i  };
  assign \SensorFSM_1.SensorValue_o  = \SensorFSM_1.Word0 ;
  assign SPIFSM_1_CfgMode_s = 1'b0;
  assign SPIFSM_1_CfgClk_s = 1'b0;
  assign SPIFSM_1_CfgShift_s = 1'b0;
  assign SPIFSM_1_CfgDataIn_s = 1'b0;
  assign SensorFSM_1_CfgMode_s = 1'b0;
  assign SensorFSM_1_CfgClk_s = 1'b0;
  assign SensorFSM_1_CfgShift_s = 1'b0;
  assign SensorFSM_1_CfgDataIn_s = 1'b0;

endmodule
