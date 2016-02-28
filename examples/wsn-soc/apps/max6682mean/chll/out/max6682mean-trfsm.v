(* src = "../../verilog/max6682mean.v:1" *)
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

  (* src = "../../verilog/max6682mean.v:287" *)
  wire [15:0] \$0\Accumulator[15:0] ;
  (* src = "../../verilog/max6682mean.v:287" *)
  wire [15:0] \$0\LastValue[15:0] ;
  (* src = "../../verilog/max6682mean.v:231" *)
  wire [15:0] \$0\PauseTimer[15:0] ;
  (* src = "../../verilog/max6682mean.v:256" *)
  wire [31:0] \$0\SensorFSM_Timer[31:0] ;
  (* src = "../../verilog/max6682mean.v:112" *)
  wire \$2\PauseTimerPreset[0:0] ;
  (* src = "../../verilog/max6682mean.v:112" *)
  wire \$2\SPI_FSM_Start[0:0] ;
  (* src = "../../verilog/max6682mean.v:112" *)
  wire \$2\SensorFSM_TimerPreset[0:0] ;
  (* src = "../../verilog/max6682mean.v:303" *)
  wire [15:0] \$add$../../verilog/max6682mean.v:303$35_Y ;
  wire \$auto$opt_reduce.cc:126:opt_mux$2097 ;
  wire \$auto$opt_reduce.cc:126:opt_mux$2103 ;
  wire \$auto$opt_reduce.cc:126:opt_mux$2109 ;
  wire \$procmux$1019_CMP ;
  wire \$procmux$1024_CMP ;
  wire \$procmux$1027_CMP ;
  wire \$procmux$1121_CMP ;
  wire \$procmux$1188_CMP ;
  wire \$procmux$1325_CMP ;
  wire \$procmux$1392_CMP ;
  wire \$procmux$1529_CMP ;
  wire \$procmux$1596_CMP ;
  wire \$procmux$1663_CMP ;
  wire [15:0] \$procmux$1729_Y ;
  wire [31:0] \$procmux$1735_Y ;
  wire [15:0] \$procmux$1741_Y ;
  (* src = "../../verilog/max6682mean.v:245" *)
  wire [15:0] \$sub$../../verilog/max6682mean.v:245$25_Y ;
  (* src = "../../verilog/max6682mean.v:270" *)
  wire [31:0] \$sub$../../verilog/max6682mean.v:270$30_Y ;
  (* src = "../../verilog/spifsm.v:117" *)
  wire [7:0] \$techmap\SPI_FSM_1.$0\Byte0[7:0] ;
  (* src = "../../verilog/spifsm.v:117" *)
  wire [7:0] \$techmap\SPI_FSM_1.$0\Byte1[7:0] ;
  (* src = "../../verilog/spifsm.v:50" *)
  wire \$techmap\SPI_FSM_1.$2\MAX6682CS_n_o[0:0] ;
  (* src = "../../verilog/spifsm.v:50" *)
  wire \$techmap\SPI_FSM_1.$2\SPI_FSM_Wr1[0:0] ;
  wire \$techmap\SPI_FSM_1.$auto$opt_reduce.cc:126:opt_mux$2101 ;
  wire \$techmap\SPI_FSM_1.$auto$opt_reduce.cc:126:opt_mux$2111 ;
  wire \$techmap\SPI_FSM_1.$procmux$1864_CMP ;
  wire \$techmap\SPI_FSM_1.$procmux$1865_CMP ;
  wire \$techmap\SPI_FSM_1.$procmux$1866_CMP ;
  wire \$techmap\SPI_FSM_1.$procmux$1869_CMP ;
  (* src = "../../verilog/max6682mean.v:283" *)
  wire [15:0] AbsDiffResult;
  (* src = "../../verilog/max6682mean.v:281" *)
  wire [15:0] Accumulator;
  (* src = "../../verilog/max6682mean.v:54" *)
  wire [7:0] Byte0;
  (* src = "../../verilog/max6682mean.v:55" *)
  wire [7:0] Byte1;
  (* src = "../../verilog/max6682mean.v:313" *)
  wire [16:0] DiffAB;
  (* src = "../../verilog/max6682mean.v:314" *)
  wire [15:0] DiffBA;
  (* src = "../../verilog/max6682mean.v:282" *)
  wire [15:0] LastValue;
  (* src = "../../verilog/max6682mean.v:229" *)
  wire [15:0] PauseTimer;
  (* src = "../../verilog/max6682mean.v:89" *)
  wire PauseTimerEnable;
  (* src = "../../verilog/max6682mean.v:90" *)
  wire PauseTimerOvfl;
  (* src = "../../verilog/max6682mean.v:88" *)
  wire PauseTimerPreset;
  (* src = "../../verilog/spifsm.v:11" *)
  wire [7:0] \SPI_FSM_1.Byte0 ;
  (* src = "../../verilog/spifsm.v:12" *)
  wire [7:0] \SPI_FSM_1.Byte1 ;
  (* src = "../../verilog/spifsm.v:3" *)
  wire \SPI_FSM_1.Clk_i ;
  (* src = "../../verilog/spifsm.v:6" *)
  wire \SPI_FSM_1.MAX6682CS_n_o ;
  (* src = "../../verilog/spifsm.v:2" *)
  wire \SPI_FSM_1.Reset_n_i ;
  (* src = "../../verilog/spifsm.v:10" *)
  wire [7:0] \SPI_FSM_1.SPI_Data_i ;
  (* src = "../../verilog/spifsm.v:9" *)
  wire \SPI_FSM_1.SPI_FSM_Done ;
  (* src = "../../verilog/spifsm.v:4" *)
  wire \SPI_FSM_1.SPI_FSM_Start ;
  (* src = "../../verilog/spifsm.v:24" *)
  wire \SPI_FSM_1.SPI_FSM_Wr0 ;
  (* src = "../../verilog/spifsm.v:23" *)
  wire \SPI_FSM_1.SPI_FSM_Wr1 ;
  (* src = "../../verilog/spifsm.v:8" *)
  wire \SPI_FSM_1.SPI_ReadNext_o ;
  (* src = "../../verilog/spifsm.v:5" *)
  wire \SPI_FSM_1.SPI_Transmission_i ;
  (* src = "../../verilog/spifsm.v:7" *)
  wire \SPI_FSM_1.SPI_Write_o ;
  (* src = "../../verilog/max6682mean.v:53" *)
  wire SPI_FSM_Done;
  (* src = "../../verilog/max6682mean.v:52" *)
  wire SPI_FSM_Start;
  (* src = "../../verilog/max6682mean.v:96" *)
  wire SensorFSM_AddValue;
  (* src = "../../verilog/max6682mean.v:94" *)
  wire SensorFSM_DiffTooLarge;
  (* src = "../../verilog/max6682mean.v:97" *)
  wire SensorFSM_StoreNewValue;
  (* src = "../../verilog/max6682mean.v:95" *)
  wire SensorFSM_StoreValue;
  (* src = "../../verilog/max6682mean.v:254" *)
  wire [31:0] SensorFSM_Timer;
  (* src = "../../verilog/max6682mean.v:93" *)
  wire SensorFSM_TimerEnable;
  (* src = "../../verilog/max6682mean.v:91" *)
  wire SensorFSM_TimerOvfl;
  (* src = "../../verilog/max6682mean.v:92" *)
  wire SensorFSM_TimerPreset;
  (* src = "../../verilog/max6682mean.v:280" *)
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
  wire SensorFSM_1_CfgMode_s;
  wire SensorFSM_1_CfgClk_s;
  wire SensorFSM_1_CfgShift_s;
  wire SensorFSM_1_CfgDataIn_s;
  wire SensorFSM_1_CfgDataOut_s;


  (* src = "../../verilog/max6682mean.v:303" *)
  \$add  #(
    .A_SIGNED(0),
    .A_WIDTH(16),
    .B_SIGNED(0),
    .B_WIDTH(16),
    .Y_WIDTH(16)
  ) \$add$../../verilog/max6682mean.v:303$35  (
    .A(Accumulator),
    .B({ 5'b00000, Byte1, Byte0[7:5] }),
    .Y(\$add$../../verilog/max6682mean.v:303$35_Y )
  );

  \$reduce_or  #(
    .A_SIGNED(0),
    .A_WIDTH(3),
    .Y_WIDTH(1)
  ) \$auto$opt_reduce.cc:130:opt_mux$2098  (
    .A({ \$procmux$1529_CMP , \$procmux$1325_CMP , \$procmux$1121_CMP  }),
    .Y(\$auto$opt_reduce.cc:126:opt_mux$2097 )
  );

  \$reduce_or  #(
    .A_SIGNED(0),
    .A_WIDTH(3),
    .Y_WIDTH(1)
  ) \$auto$opt_reduce.cc:130:opt_mux$2104  (
    .A({ \$procmux$1392_CMP , \$procmux$1188_CMP , \$procmux$1019_CMP  }),
    .Y(\$auto$opt_reduce.cc:126:opt_mux$2103 )
  );

  \$reduce_or  #(
    .A_SIGNED(0),
    .A_WIDTH(3),
    .Y_WIDTH(1)
  ) \$auto$opt_reduce.cc:130:opt_mux$2110  (
    .A({ \$procmux$1596_CMP , \$procmux$1392_CMP , \$procmux$1188_CMP  }),
    .Y(\$auto$opt_reduce.cc:126:opt_mux$2109 )
  );

  (* src = "../../verilog/max6682mean.v:250" *)
  \$eq  #(
    .A_SIGNED(0),
    .A_WIDTH(16),
    .B_SIGNED(0),
    .B_WIDTH(16),
    .Y_WIDTH(1)
  ) \$eq$../../verilog/max6682mean.v:250$26  (
    .A(PauseTimer),
    .B(16'b0000000000000000),
    .Y(PauseTimerOvfl)
  );

  (* src = "../../verilog/max6682mean.v:275" *)
  \$eq  #(
    .A_SIGNED(0),
    .A_WIDTH(32),
    .B_SIGNED(0),
    .B_WIDTH(32),
    .Y_WIDTH(1)
  ) \$eq$../../verilog/max6682mean.v:275$31  (
    .A(SensorFSM_Timer),
    .B(0),
    .Y(SensorFSM_TimerOvfl)
  );

  SensorFSM SensorFSM_1 (
    .Reset_n_i(Reset_n_i),
    .Clk_i(Clk_i),
    .In0_i(Enable_i),
    .In1_i(PauseTimerOvfl),
    .In2_i(SPI_FSM_Done),
    .In3_i(SensorFSM_TimerOvfl),
    .In4_i(1'b0),
    .In5_i(1'b0),
    .In6_i(1'b0),
    .In7_i(1'b0),
    .In8_i(1'b0),
    .In9_i(1'b0),
    .Out0_o(\$procmux$1019_CMP ),
    .Out1_o(\$procmux$1024_CMP ),
    .Out2_o(\$procmux$1027_CMP ),
    .Out3_o(\$procmux$1121_CMP ),
    .Out4_o(\$procmux$1188_CMP ),
    .Out5_o(\$procmux$1325_CMP ),
    .Out6_o(\$procmux$1392_CMP ),
    .Out7_o(\$procmux$1529_CMP ),
    .Out8_o(\$procmux$1596_CMP ),
    .Out9_o(\$procmux$1663_CMP ),
    .CfgMode_i(SensorFSM_1_CfgMode_s),
    .CfgClk_i(SensorFSM_1_CfgClk_s),
    .CfgShift_i(SensorFSM_1_CfgShift_s),
    .CfgDataIn_i(SensorFSM_1_CfgDataIn_s),
    .CfgDataOut_o(SensorFSM_1_CfgDataOut_s)
  );

  (* src = "../../verilog/max6682mean.v:319" *)
  \$gt  #(
    .A_SIGNED(0),
    .A_WIDTH(16),
    .B_SIGNED(0),
    .B_WIDTH(16),
    .Y_WIDTH(1)
  ) \$gt$../../verilog/max6682mean.v:319$39  (
    .A(AbsDiffResult),
    .B(Threshold_i),
    .Y(SensorFSM_DiffTooLarge)
  );

  (* src = "../../verilog/max6682mean.v:231" *)
  \$adff  #(
    .ARST_POLARITY(1'b0),
    .ARST_VALUE(16'b0000000000000000),
    .CLK_POLARITY(1'b1),
    .WIDTH(16)
  ) \$procdff$2089  (
    .ARST(Reset_n_i),
    .CLK(Clk_i),
    .D(\$0\PauseTimer[15:0] ),
    .Q(PauseTimer)
  );

  (* src = "../../verilog/max6682mean.v:256" *)
  \$adff  #(
    .ARST_POLARITY(1'b0),
    .ARST_VALUE(0),
    .CLK_POLARITY(1'b1),
    .WIDTH(32)
  ) \$procdff$2090  (
    .ARST(Reset_n_i),
    .CLK(Clk_i),
    .D(\$0\SensorFSM_Timer[31:0] ),
    .Q(SensorFSM_Timer)
  );

  (* src = "../../verilog/max6682mean.v:287" *)
  \$adff  #(
    .ARST_POLARITY(1'b0),
    .ARST_VALUE(16'b0000000000000000),
    .CLK_POLARITY(1'b1),
    .WIDTH(16)
  ) \$procdff$2091  (
    .ARST(Reset_n_i),
    .CLK(Clk_i),
    .D(\$0\Accumulator[15:0] ),
    .Q(Accumulator)
  );

  (* src = "../../verilog/max6682mean.v:287" *)
  \$adff  #(
    .ARST_POLARITY(1'b0),
    .ARST_VALUE(16'b0000000000000000),
    .CLK_POLARITY(1'b1),
    .WIDTH(16)
  ) \$procdff$2092  (
    .ARST(Reset_n_i),
    .CLK(Clk_i),
    .D(\$0\LastValue[15:0] ),
    .Q(LastValue)
  );

  \$not  #(
    .A_SIGNED(0),
    .A_WIDTH(1),
    .Y_WIDTH(1)
  ) \$procmux$1051  (
    .A(SPI_FSM_Done),
    .Y(\$2\PauseTimerPreset[0:0] )
  );

  \$mux  #(
    .WIDTH(16)
  ) \$procmux$1729  (
    .A(PauseTimer),
    .B(\$sub$../../verilog/max6682mean.v:245$25_Y ),
    .S(PauseTimerEnable),
    .Y(\$procmux$1729_Y )
  );

  \$mux  #(
    .WIDTH(16)
  ) \$procmux$1732  (
    .A(\$procmux$1729_Y ),
    .B(PauseCounterPreset_i),
    .S(PauseTimerPreset),
    .Y(\$0\PauseTimer[15:0] )
  );

  \$mux  #(
    .WIDTH(32)
  ) \$procmux$1735  (
    .A(SensorFSM_Timer),
    .B(\$sub$../../verilog/max6682mean.v:270$30_Y ),
    .S(SensorFSM_TimerEnable),
    .Y(\$procmux$1735_Y )
  );

  \$mux  #(
    .WIDTH(32)
  ) \$procmux$1738  (
    .A(\$procmux$1735_Y ),
    .B({ PeriodCounterPresetH_i, PeriodCounterPresetL_i }),
    .S(SensorFSM_TimerPreset),
    .Y(\$0\SensorFSM_Timer[31:0] )
  );

  \$mux  #(
    .WIDTH(16)
  ) \$procmux$1741  (
    .A(Accumulator),
    .B(\$add$../../verilog/max6682mean.v:303$35_Y ),
    .S(SensorFSM_AddValue),
    .Y(\$procmux$1741_Y )
  );

  \$mux  #(
    .WIDTH(16)
  ) \$procmux$1744  (
    .A(\$procmux$1741_Y ),
    .B({ 5'b00000, Byte1, Byte0[7:5] }),
    .S(SensorFSM_StoreValue),
    .Y(\$0\Accumulator[15:0] )
  );

  \$mux  #(
    .WIDTH(16)
  ) \$procmux$1753  (
    .A(LastValue),
    .B(Accumulator),
    .S(CpuIntr_o),
    .Y(\$0\LastValue[15:0] )
  );

  \$and  #(
    .A_SIGNED(0),
    .A_WIDTH(1),
    .B_SIGNED(0),
    .B_WIDTH(1),
    .Y_WIDTH(1)
  ) \$procmux$374  (
    .A(\$procmux$1663_CMP ),
    .B(SensorFSM_DiffTooLarge),
    .Y(CpuIntr_o)
  );

  \$pmux  #(
    .S_WIDTH(2),
    .WIDTH(1)
  ) \$procmux$414  (
    .A(1'b0),
    .B({ SPI_FSM_Done, 1'b1 }),
    .S({ \$auto$opt_reduce.cc:126:opt_mux$2103 , \$auto$opt_reduce.cc:126:opt_mux$2097  }),
    .Y(PauseTimerEnable)
  );

  \$pmux  #(
    .S_WIDTH(2),
    .WIDTH(1)
  ) \$procmux$448  (
    .A(1'b1),
    .B({ \$2\PauseTimerPreset[0:0] , 1'b0 }),
    .S({ \$auto$opt_reduce.cc:126:opt_mux$2103 , \$auto$opt_reduce.cc:126:opt_mux$2097  }),
    .Y(PauseTimerPreset)
  );

  \$pmux  #(
    .S_WIDTH(2),
    .WIDTH(1)
  ) \$procmux$482  (
    .A(1'b0),
    .B({ \$2\SPI_FSM_Start[0:0] , PauseTimerOvfl }),
    .S({ \$procmux$1024_CMP , \$auto$opt_reduce.cc:126:opt_mux$2097  }),
    .Y(SPI_FSM_Start)
  );

  \$and  #(
    .A_SIGNED(0),
    .A_WIDTH(1),
    .B_SIGNED(0),
    .B_WIDTH(1),
    .Y_WIDTH(1)
  ) \$procmux$513  (
    .A(\$auto$opt_reduce.cc:126:opt_mux$2109 ),
    .B(SPI_FSM_Done),
    .Y(SensorFSM_AddValue)
  );

  \$and  #(
    .A_SIGNED(0),
    .A_WIDTH(1),
    .B_SIGNED(0),
    .B_WIDTH(1),
    .Y_WIDTH(1)
  ) \$procmux$599  (
    .A(\$procmux$1019_CMP ),
    .B(SPI_FSM_Done),
    .Y(SensorFSM_StoreValue)
  );

  \$pmux  #(
    .S_WIDTH(2),
    .WIDTH(1)
  ) \$procmux$672  (
    .A(1'b0),
    .B({ Enable_i, 1'b1 }),
    .S({ \$procmux$1027_CMP , \$procmux$1024_CMP  }),
    .Y(SensorFSM_TimerEnable)
  );

  \$pmux  #(
    .S_WIDTH(2),
    .WIDTH(1)
  ) \$procmux$706  (
    .A(1'b1),
    .B({ \$2\SensorFSM_TimerPreset[0:0] , 1'b0 }),
    .S({ \$procmux$1027_CMP , \$procmux$1024_CMP  }),
    .Y(SensorFSM_TimerPreset)
  );

  \$not  #(
    .A_SIGNED(0),
    .A_WIDTH(1),
    .Y_WIDTH(1)
  ) \$procmux$777  (
    .A(Enable_i),
    .Y(\$2\SensorFSM_TimerPreset[0:0] )
  );

  \$and  #(
    .A_SIGNED(0),
    .A_WIDTH(1),
    .B_SIGNED(0),
    .B_WIDTH(1),
    .Y_WIDTH(1)
  ) \$procmux$844  (
    .A(Enable_i),
    .B(SensorFSM_TimerOvfl),
    .Y(\$2\SPI_FSM_Start[0:0] )
  );

  (* src = "../../verilog/max6682mean.v:245" *)
  \$sub  #(
    .A_SIGNED(0),
    .A_WIDTH(16),
    .B_SIGNED(0),
    .B_WIDTH(1),
    .Y_WIDTH(16)
  ) \$sub$../../verilog/max6682mean.v:245$25  (
    .A(PauseTimer),
    .B(1'b1),
    .Y(\$sub$../../verilog/max6682mean.v:245$25_Y )
  );

  (* src = "../../verilog/max6682mean.v:270" *)
  \$sub  #(
    .A_SIGNED(0),
    .A_WIDTH(32),
    .B_SIGNED(0),
    .B_WIDTH(1),
    .Y_WIDTH(32)
  ) \$sub$../../verilog/max6682mean.v:270$30  (
    .A(SensorFSM_Timer),
    .B(1'b1),
    .Y(\$sub$../../verilog/max6682mean.v:270$30_Y )
  );

  (* src = "../../verilog/max6682mean.v:315" *)
  \$sub  #(
    .A_SIGNED(0),
    .A_WIDTH(17),
    .B_SIGNED(0),
    .B_WIDTH(17),
    .Y_WIDTH(17)
  ) \$sub$../../verilog/max6682mean.v:315$36  (
    .A({ 1'b0, LastValue }),
    .B({ 1'b0, Accumulator }),
    .Y(DiffAB)
  );

  (* src = "../../verilog/max6682mean.v:316" *)
  \$sub  #(
    .A_SIGNED(0),
    .A_WIDTH(16),
    .B_SIGNED(0),
    .B_WIDTH(16),
    .Y_WIDTH(16)
  ) \$sub$../../verilog/max6682mean.v:316$37  (
    .A(Accumulator),
    .B(LastValue),
    .Y(DiffBA)
  );

  \$reduce_or  #(
    .A_SIGNED(0),
    .A_WIDTH(4),
    .Y_WIDTH(1)
  ) \$techmap\SPI_FSM_1.$auto$opt_reduce.cc:130:opt_mux$2102  (
    .A({ \SPI_FSM_1.SPI_FSM_Wr0 , \$techmap\SPI_FSM_1.$procmux$1866_CMP , \$techmap\SPI_FSM_1.$procmux$1865_CMP , \$techmap\SPI_FSM_1.$procmux$1864_CMP  }),
    .Y(\$techmap\SPI_FSM_1.$auto$opt_reduce.cc:126:opt_mux$2101 )
  );

  \$reduce_or  #(
    .A_SIGNED(0),
    .A_WIDTH(2),
    .Y_WIDTH(1)
  ) \$techmap\SPI_FSM_1.$auto$opt_reduce.cc:130:opt_mux$2112  (
    .A({ \SPI_FSM_1.SPI_FSM_Done , \$techmap\SPI_FSM_1.$procmux$1869_CMP  }),
    .Y(\$techmap\SPI_FSM_1.$auto$opt_reduce.cc:126:opt_mux$2111 )
  );

  SPIFSM SPIFSM_1 (
    .Reset_n_i(\SPI_FSM_1.Reset_n_i ),
    .Clk_i(\SPI_FSM_1.Clk_i ),
    .In0_i(\SPI_FSM_1.SPI_FSM_Start ),
    .In1_i(\SPI_FSM_1.SPI_Transmission_i ),
    .In2_i(1'b0),
    .In3_i(1'b0),
    .In4_i(1'b0),
    .In5_i(1'b0),
    .In6_i(1'b0),
    .In7_i(1'b0),
    .Out0_o(\$techmap\SPI_FSM_1.$procmux$1864_CMP ),
    .Out1_o(\$techmap\SPI_FSM_1.$procmux$1865_CMP ),
    .Out2_o(\$techmap\SPI_FSM_1.$procmux$1866_CMP ),
    .Out3_o(\$techmap\SPI_FSM_1.$procmux$1869_CMP ),
    .Out4_o(\SPI_FSM_1.SPI_FSM_Done ),
    .Out5_o(\SPI_FSM_1.SPI_FSM_Wr0 ),
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

  (* src = "../../verilog/spifsm.v:117" *)
  \$adff  #(
    .ARST_POLARITY(1'b0),
    .ARST_VALUE(8'b00000000),
    .CLK_POLARITY(1'b1),
    .WIDTH(8)
  ) \$techmap\SPI_FSM_1.$procdff$2094  (
    .ARST(\SPI_FSM_1.Reset_n_i ),
    .CLK(\SPI_FSM_1.Clk_i ),
    .D(\$techmap\SPI_FSM_1.$0\Byte0[7:0] ),
    .Q(\SPI_FSM_1.Byte0 )
  );

  (* src = "../../verilog/spifsm.v:117" *)
  \$adff  #(
    .ARST_POLARITY(1'b0),
    .ARST_VALUE(8'b00000000),
    .CLK_POLARITY(1'b1),
    .WIDTH(8)
  ) \$techmap\SPI_FSM_1.$procdff$2095  (
    .ARST(\SPI_FSM_1.Reset_n_i ),
    .CLK(\SPI_FSM_1.Clk_i ),
    .D(\$techmap\SPI_FSM_1.$0\Byte1[7:0] ),
    .Q(\SPI_FSM_1.Byte1 )
  );

  \$mux  #(
    .WIDTH(8)
  ) \$techmap\SPI_FSM_1.$procmux$1756  (
    .A(\SPI_FSM_1.Byte0 ),
    .B(\SPI_FSM_1.SPI_Data_i ),
    .S(\SPI_FSM_1.SPI_FSM_Wr0 ),
    .Y(\$techmap\SPI_FSM_1.$0\Byte0[7:0] )
  );

  \$mux  #(
    .WIDTH(8)
  ) \$techmap\SPI_FSM_1.$procmux$1763  (
    .A(\SPI_FSM_1.Byte1 ),
    .B(\SPI_FSM_1.SPI_Data_i ),
    .S(\SPI_FSM_1.SPI_FSM_Wr1 ),
    .Y(\$techmap\SPI_FSM_1.$0\Byte1[7:0] )
  );

  \$and  #(
    .A_SIGNED(0),
    .A_WIDTH(1),
    .B_SIGNED(0),
    .B_WIDTH(1),
    .Y_WIDTH(1)
  ) \$techmap\SPI_FSM_1.$procmux$1891  (
    .A(\$techmap\SPI_FSM_1.$procmux$1864_CMP ),
    .B(\$techmap\SPI_FSM_1.$2\SPI_FSM_Wr1[0:0] ),
    .Y(\SPI_FSM_1.SPI_FSM_Wr1 )
  );

  \$pmux  #(
    .S_WIDTH(2),
    .WIDTH(1)
  ) \$techmap\SPI_FSM_1.$procmux$1902  (
    .A(1'b0),
    .B({ \$techmap\SPI_FSM_1.$2\SPI_FSM_Wr1[0:0] , 1'b1 }),
    .S({ \$techmap\SPI_FSM_1.$procmux$1864_CMP , \SPI_FSM_1.SPI_FSM_Wr0  }),
    .Y(\SPI_FSM_1.SPI_ReadNext_o )
  );

  \$pmux  #(
    .S_WIDTH(2),
    .WIDTH(1)
  ) \$techmap\SPI_FSM_1.$procmux$1915  (
    .A(1'b1),
    .B({ 1'b0, \$techmap\SPI_FSM_1.$2\MAX6682CS_n_o[0:0]  }),
    .S({ \$techmap\SPI_FSM_1.$auto$opt_reduce.cc:126:opt_mux$2101 , \$techmap\SPI_FSM_1.$auto$opt_reduce.cc:126:opt_mux$2111  }),
    .Y(\SPI_FSM_1.MAX6682CS_n_o )
  );

  \$pmux  #(
    .S_WIDTH(2),
    .WIDTH(1)
  ) \$techmap\SPI_FSM_1.$procmux$1943  (
    .A(1'b0),
    .B({ 1'b1, \SPI_FSM_1.SPI_FSM_Start  }),
    .S({ \$techmap\SPI_FSM_1.$procmux$1866_CMP , \$techmap\SPI_FSM_1.$auto$opt_reduce.cc:126:opt_mux$2111  }),
    .Y(\SPI_FSM_1.SPI_Write_o )
  );

  \$not  #(
    .A_SIGNED(0),
    .A_WIDTH(1),
    .Y_WIDTH(1)
  ) \$techmap\SPI_FSM_1.$procmux$1980  (
    .A(\SPI_FSM_1.SPI_FSM_Start ),
    .Y(\$techmap\SPI_FSM_1.$2\MAX6682CS_n_o[0:0] )
  );

  \$not  #(
    .A_SIGNED(0),
    .A_WIDTH(1),
    .Y_WIDTH(1)
  ) \$techmap\SPI_FSM_1.$procmux$2020  (
    .A(\SPI_FSM_1.SPI_Transmission_i ),
    .Y(\$techmap\SPI_FSM_1.$2\SPI_FSM_Wr1[0:0] )
  );

  (* src = "../../verilog/max6682mean.v:317" *)
  \$mux  #(
    .WIDTH(16)
  ) \$ternary$../../verilog/max6682mean.v:317$38  (
    .A(DiffAB[15:0]),
    .B(DiffBA),
    .S(DiffAB[16]),
    .Y(AbsDiffResult)
  );
  assign SPI_CPHA_o = 1'b0;
  assign SPI_CPOL_o = 1'b0;
  assign SPI_Data_o = 8'b00000000;
  assign SPI_LSBFE_o = 1'b0;
  assign SensorFSM_StoreNewValue = CpuIntr_o;
  assign SensorValue = { 5'b00000, Byte1, Byte0[7:5] };
  assign SensorValue_o = LastValue;
  assign Byte0 = \SPI_FSM_1.Byte0 ;
  assign Byte1 = \SPI_FSM_1.Byte1 ;
  assign \SPI_FSM_1.Clk_i  = Clk_i;
  assign MAX6682CS_n_o = \SPI_FSM_1.MAX6682CS_n_o ;
  assign \SPI_FSM_1.Reset_n_i  = Reset_n_i;
  assign \SPI_FSM_1.SPI_Data_i  = SPI_Data_i;
  assign SPI_FSM_Done = \SPI_FSM_1.SPI_FSM_Done ;
  assign \SPI_FSM_1.SPI_FSM_Start  = SPI_FSM_Start;
  assign SPI_ReadNext_o = \SPI_FSM_1.SPI_ReadNext_o ;
  assign \SPI_FSM_1.SPI_Transmission_i  = SPI_Transmission_i;
  assign SPI_Write_o = \SPI_FSM_1.SPI_Write_o ;
  assign SPIFSM_1_CfgMode_s = 1'b0;
  assign SPIFSM_1_CfgClk_s = 1'b0;
  assign SPIFSM_1_CfgShift_s = 1'b0;
  assign SPIFSM_1_CfgDataIn_s = 1'b0;
  assign SensorFSM_1_CfgMode_s = 1'b0;
  assign SensorFSM_1_CfgClk_s = 1'b0;
  assign SensorFSM_1_CfgShift_s = 1'b0;
  assign SensorFSM_1_CfgDataIn_s = 1'b0;

endmodule
