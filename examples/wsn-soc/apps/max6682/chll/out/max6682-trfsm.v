(* src = "../../verilog/max6682.v:133" *)
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

  (* src = "../../verilog/max6682.v:297" *)
  wire [31:0] \$0\SensorFSM_Timer[31:0] ;
  (* src = "../../verilog/max6682.v:327" *)
  wire [15:0] \$0\Word0[15:0] ;
  (* src = "../../verilog/max6682.v:231" *)
  wire \$2\SPI_FSM_Start[0:0] ;
  (* src = "../../verilog/max6682.v:231" *)
  wire \$2\SensorFSM_StoreNewValue[0:0] ;
  (* src = "../../verilog/max6682.v:231" *)
  wire \$2\SensorFSM_TimerPreset[0:0] ;
  (* src = "../../verilog/max6682.v:231" *)
  wire \$3\SensorFSM_TimerPreset[0:0] ;
  (* src = "../../verilog/max6682.v:231" *)
  wire \$4\SensorFSM_TimerPreset[0:0] ;
  wire \$auto$opt_reduce.cc:126:opt_mux$732 ;
  wire \$procmux$118_CMP ;
  wire \$procmux$123_CMP ;
  wire \$procmux$126_CMP ;
  wire [31:0] \$procmux$449_Y ;
  (* src = "../../verilog/max6682.v:311" *)
  wire [31:0] \$sub$../../verilog/max6682.v:311$18_Y ;
  (* src = "../../verilog/max6682.v:111" *)
  wire [7:0] \$techmap\MAX6682_SPI_FSM_1.$0\Byte0[7:0] ;
  (* src = "../../verilog/max6682.v:111" *)
  wire [7:0] \$techmap\MAX6682_SPI_FSM_1.$0\Byte1[7:0] ;
  (* src = "../../verilog/max6682.v:50" *)
  wire \$techmap\MAX6682_SPI_FSM_1.$2\MAX6682CS_n_o[0:0] ;
  (* src = "../../verilog/max6682.v:50" *)
  wire \$techmap\MAX6682_SPI_FSM_1.$2\SPI_FSM_Wr1[0:0] ;
  wire \$techmap\MAX6682_SPI_FSM_1.$auto$opt_reduce.cc:126:opt_mux$736 ;
  wire \$techmap\MAX6682_SPI_FSM_1.$procmux$553_CMP ;
  wire \$techmap\MAX6682_SPI_FSM_1.$procmux$554_CMP ;
  wire \$techmap\MAX6682_SPI_FSM_1.$procmux$558_CMP ;
  wire \$techmap\MAX6682_SPI_FSM_1.$procmux$559_CMP ;
  wire \$techmap\MAX6682_SPI_FSM_1.$procmux$560_CMP ;
  wire \$techmap\MAX6682_SPI_FSM_1.$procmux$563_CMP ;
  (* src = "../../verilog/max6682.v:323" *)
  wire [15:0] AbsDiffResult;
  (* src = "../../verilog/max6682.v:184" *)
  wire [7:0] Byte0;
  (* src = "../../verilog/max6682.v:185" *)
  wire [7:0] Byte1;
  (* src = "../../verilog/max6682.v:342" *)
  wire [16:0] DiffAB;
  (* src = "../../verilog/max6682.v:343" *)
  wire [15:0] DiffBA;
  (* src = "../../verilog/max6682.v:11" *)
  wire [7:0] \MAX6682_SPI_FSM_1.Byte0 ;
  (* src = "../../verilog/max6682.v:12" *)
  wire [7:0] \MAX6682_SPI_FSM_1.Byte1 ;
  (* src = "../../verilog/max6682.v:3" *)
  wire \MAX6682_SPI_FSM_1.Clk_i ;
  (* src = "../../verilog/max6682.v:6" *)
  wire \MAX6682_SPI_FSM_1.MAX6682CS_n_o ;
  (* src = "../../verilog/max6682.v:2" *)
  wire \MAX6682_SPI_FSM_1.Reset_n_i ;
  (* src = "../../verilog/max6682.v:10" *)
  wire [7:0] \MAX6682_SPI_FSM_1.SPI_Data_i ;
  (* src = "../../verilog/max6682.v:9" *)
  wire \MAX6682_SPI_FSM_1.SPI_FSM_Done ;
  (* src = "../../verilog/max6682.v:4" *)
  wire \MAX6682_SPI_FSM_1.SPI_FSM_Start ;
  (* src = "../../verilog/max6682.v:24" *)
  wire \MAX6682_SPI_FSM_1.SPI_FSM_Wr0 ;
  (* src = "../../verilog/max6682.v:23" *)
  wire \MAX6682_SPI_FSM_1.SPI_FSM_Wr1 ;
  (* src = "../../verilog/max6682.v:8" *)
  wire \MAX6682_SPI_FSM_1.SPI_ReadNext_o ;
  (* src = "../../verilog/max6682.v:5" *)
  wire \MAX6682_SPI_FSM_1.SPI_Transmission_i ;
  (* src = "../../verilog/max6682.v:7" *)
  wire \MAX6682_SPI_FSM_1.SPI_Write_o ;
  (* src = "../../verilog/max6682.v:183" *)
  wire SPI_FSM_Done;
  (* src = "../../verilog/max6682.v:182" *)
  wire SPI_FSM_Start;
  (* src = "../../verilog/max6682.v:215" *)
  wire SensorFSM_DiffTooLarge;
  (* src = "../../verilog/max6682.v:216" *)
  wire SensorFSM_StoreNewValue;
  (* src = "../../verilog/max6682.v:295" *)
  wire [31:0] SensorFSM_Timer;
  (* src = "../../verilog/max6682.v:214" *)
  wire SensorFSM_TimerEnable;
  (* src = "../../verilog/max6682.v:212" *)
  wire SensorFSM_TimerOvfl;
  (* src = "../../verilog/max6682.v:213" *)
  wire SensorFSM_TimerPreset;
  (* src = "../../verilog/max6682.v:321" *)
  wire [15:0] SensorValue;
  (* src = "../../verilog/max6682.v:322" *)
  wire [15:0] Word0;
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
  ) \$auto$opt_reduce.cc:130:opt_mux$733  (
    .A({ \$procmux$126_CMP , \$procmux$123_CMP , \$procmux$118_CMP  }),
    .Y(\$auto$opt_reduce.cc:126:opt_mux$732 )
  );

  (* src = "../../verilog/max6682.v:316" *)
  \$eq  #(
    .A_SIGNED(0),
    .A_WIDTH(32),
    .B_SIGNED(0),
    .B_WIDTH(32),
    .Y_WIDTH(1)
  ) \$eq$../../verilog/max6682.v:316$19  (
    .A(SensorFSM_Timer),
    .B(0),
    .Y(SensorFSM_TimerOvfl)
  );

  SensorFSM SensorFSM_1 (
    .Reset_n_i(Reset_n_i),
    .Clk_i(Clk_i),
    .In0_i(Enable_i),
    .In1_i(SPI_FSM_Done),
    .In2_i(SensorFSM_DiffTooLarge),
    .In3_i(SensorFSM_TimerOvfl),
    .In4_i(1'b0),
    .In5_i(1'b0),
    .In6_i(1'b0),
    .In7_i(1'b0),
    .In8_i(1'b0),
    .In9_i(1'b0),
    .Out0_o(\$procmux$118_CMP ),
    .Out1_o(\$procmux$123_CMP ),
    .Out2_o(\$procmux$126_CMP ),
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

  (* src = "../../verilog/max6682.v:348" *)
  \$gt  #(
    .A_SIGNED(0),
    .A_WIDTH(16),
    .B_SIGNED(0),
    .B_WIDTH(16),
    .Y_WIDTH(1)
  ) \$gt$../../verilog/max6682.v:348$26  (
    .A(AbsDiffResult),
    .B(Threshold_i),
    .Y(SensorFSM_DiffTooLarge)
  );

  (* src = "../../verilog/max6682.v:297" *)
  \$adff  #(
    .ARST_POLARITY(1'b0),
    .ARST_VALUE(0),
    .CLK_POLARITY(1'b1),
    .WIDTH(32)
  ) \$procdff$727  (
    .ARST(Reset_n_i),
    .CLK(Clk_i),
    .D(\$0\SensorFSM_Timer[31:0] ),
    .Q(SensorFSM_Timer)
  );

  (* src = "../../verilog/max6682.v:327" *)
  \$adff  #(
    .ARST_POLARITY(1'b0),
    .ARST_VALUE(16'b0000000000000000),
    .CLK_POLARITY(1'b1),
    .WIDTH(16)
  ) \$procdff$728  (
    .ARST(Reset_n_i),
    .CLK(Clk_i),
    .D(\$0\Word0[15:0] ),
    .Q(Word0)
  );

  \$not  #(
    .A_SIGNED(0),
    .A_WIDTH(1),
    .Y_WIDTH(1)
  ) \$procmux$117  (
    .A(\$auto$opt_reduce.cc:126:opt_mux$732 ),
    .Y(CpuIntr_o)
  );

  \$and  #(
    .A_SIGNED(0),
    .A_WIDTH(1),
    .B_SIGNED(0),
    .B_WIDTH(1),
    .Y_WIDTH(1)
  ) \$procmux$137  (
    .A(\$procmux$123_CMP ),
    .B(\$2\SPI_FSM_Start[0:0] ),
    .Y(SPI_FSM_Start)
  );

  \$and  #(
    .A_SIGNED(0),
    .A_WIDTH(1),
    .B_SIGNED(0),
    .B_WIDTH(1),
    .Y_WIDTH(1)
  ) \$procmux$147  (
    .A(\$procmux$118_CMP ),
    .B(\$2\SensorFSM_StoreNewValue[0:0] ),
    .Y(SensorFSM_StoreNewValue)
  );

  \$pmux  #(
    .S_WIDTH(3),
    .WIDTH(1)
  ) \$procmux$177  (
    .A(1'b0),
    .B({ Enable_i, 1'b1, \$2\SensorFSM_StoreNewValue[0:0]  }),
    .S({ \$procmux$126_CMP , \$procmux$123_CMP , \$procmux$118_CMP  }),
    .Y(SensorFSM_TimerEnable)
  );

  \$pmux  #(
    .S_WIDTH(3),
    .WIDTH(1)
  ) \$procmux$192  (
    .A(1'b1),
    .B({ \$2\SensorFSM_TimerPreset[0:0] , 1'b0, \$3\SensorFSM_TimerPreset[0:0]  }),
    .S({ \$procmux$126_CMP , \$procmux$123_CMP , \$procmux$118_CMP  }),
    .Y(SensorFSM_TimerPreset)
  );

  \$not  #(
    .A_SIGNED(0),
    .A_WIDTH(1),
    .Y_WIDTH(1)
  ) \$procmux$230  (
    .A(Enable_i),
    .Y(\$2\SensorFSM_TimerPreset[0:0] )
  );

  \$and  #(
    .A_SIGNED(0),
    .A_WIDTH(1),
    .B_SIGNED(0),
    .B_WIDTH(1),
    .Y_WIDTH(1)
  ) \$procmux$259  (
    .A(Enable_i),
    .B(SensorFSM_TimerOvfl),
    .Y(\$2\SPI_FSM_Start[0:0] )
  );

  \$and  #(
    .A_SIGNED(0),
    .A_WIDTH(1),
    .B_SIGNED(0),
    .B_WIDTH(1),
    .Y_WIDTH(1)
  ) \$procmux$336  (
    .A(SPI_FSM_Done),
    .B(SensorFSM_DiffTooLarge),
    .Y(\$2\SensorFSM_StoreNewValue[0:0] )
  );

  \$mux  #(
    .WIDTH(1)
  ) \$procmux$368  (
    .A(1'b1),
    .B(\$4\SensorFSM_TimerPreset[0:0] ),
    .S(SPI_FSM_Done),
    .Y(\$3\SensorFSM_TimerPreset[0:0] )
  );

  \$not  #(
    .A_SIGNED(0),
    .A_WIDTH(1),
    .Y_WIDTH(1)
  ) \$procmux$400  (
    .A(SensorFSM_DiffTooLarge),
    .Y(\$4\SensorFSM_TimerPreset[0:0] )
  );

  \$mux  #(
    .WIDTH(32)
  ) \$procmux$449  (
    .A(SensorFSM_Timer),
    .B(\$sub$../../verilog/max6682.v:311$18_Y ),
    .S(SensorFSM_TimerEnable),
    .Y(\$procmux$449_Y )
  );

  \$mux  #(
    .WIDTH(32)
  ) \$procmux$452  (
    .A(\$procmux$449_Y ),
    .B({ PeriodCounterPresetH_i, PeriodCounterPresetL_i }),
    .S(SensorFSM_TimerPreset),
    .Y(\$0\SensorFSM_Timer[31:0] )
  );

  \$mux  #(
    .WIDTH(16)
  ) \$procmux$455  (
    .A(Word0),
    .B({ 5'b00000, Byte1, Byte0[7:5] }),
    .S(SensorFSM_StoreNewValue),
    .Y(\$0\Word0[15:0] )
  );

  (* src = "../../verilog/max6682.v:311" *)
  \$sub  #(
    .A_SIGNED(0),
    .A_WIDTH(32),
    .B_SIGNED(0),
    .B_WIDTH(1),
    .Y_WIDTH(32)
  ) \$sub$../../verilog/max6682.v:311$18  (
    .A(SensorFSM_Timer),
    .B(1'b1),
    .Y(\$sub$../../verilog/max6682.v:311$18_Y )
  );

  (* src = "../../verilog/max6682.v:344" *)
  \$sub  #(
    .A_SIGNED(0),
    .A_WIDTH(17),
    .B_SIGNED(0),
    .B_WIDTH(17),
    .Y_WIDTH(17)
  ) \$sub$../../verilog/max6682.v:344$23  (
    .A({ 6'b000000, Byte1, Byte0[7:5] }),
    .B({ 1'b0, Word0 }),
    .Y(DiffAB)
  );

  (* src = "../../verilog/max6682.v:345" *)
  \$sub  #(
    .A_SIGNED(0),
    .A_WIDTH(16),
    .B_SIGNED(0),
    .B_WIDTH(16),
    .Y_WIDTH(16)
  ) \$sub$../../verilog/max6682.v:345$24  (
    .A(Word0),
    .B({ 5'b00000, Byte1, Byte0[7:5] }),
    .Y(DiffBA)
  );

  \$reduce_or  #(
    .A_SIGNED(0),
    .A_WIDTH(2),
    .Y_WIDTH(1)
  ) \$techmap\MAX6682_SPI_FSM_1.$auto$opt_reduce.cc:130:opt_mux$735  (
    .A({ \$techmap\MAX6682_SPI_FSM_1.$procmux$554_CMP , \$techmap\MAX6682_SPI_FSM_1.$procmux$553_CMP  }),
    .Y(\MAX6682_SPI_FSM_1.SPI_FSM_Done )
  );

  \$reduce_or  #(
    .A_SIGNED(0),
    .A_WIDTH(4),
    .Y_WIDTH(1)
  ) \$techmap\MAX6682_SPI_FSM_1.$auto$opt_reduce.cc:130:opt_mux$737  (
    .A({ \MAX6682_SPI_FSM_1.SPI_FSM_Wr0 , \$techmap\MAX6682_SPI_FSM_1.$procmux$560_CMP , \$techmap\MAX6682_SPI_FSM_1.$procmux$559_CMP , \$techmap\MAX6682_SPI_FSM_1.$procmux$558_CMP  }),
    .Y(\$techmap\MAX6682_SPI_FSM_1.$auto$opt_reduce.cc:126:opt_mux$736 )
  );

  SPIFSM SPIFSM_1 (
    .Reset_n_i(\MAX6682_SPI_FSM_1.Reset_n_i ),
    .Clk_i(\MAX6682_SPI_FSM_1.Clk_i ),
    .In0_i(\MAX6682_SPI_FSM_1.SPI_FSM_Start ),
    .In1_i(\MAX6682_SPI_FSM_1.SPI_Transmission_i ),
    .In2_i(1'b0),
    .In3_i(1'b0),
    .In4_i(1'b0),
    .In5_i(1'b0),
    .In6_i(1'b0),
    .In7_i(1'b0),
    .Out0_o(\$techmap\MAX6682_SPI_FSM_1.$procmux$553_CMP ),
    .Out1_o(\$techmap\MAX6682_SPI_FSM_1.$procmux$554_CMP ),
    .Out2_o(\$techmap\MAX6682_SPI_FSM_1.$procmux$558_CMP ),
    .Out3_o(\$techmap\MAX6682_SPI_FSM_1.$procmux$559_CMP ),
    .Out4_o(\$techmap\MAX6682_SPI_FSM_1.$procmux$560_CMP ),
    .Out5_o(\$techmap\MAX6682_SPI_FSM_1.$procmux$563_CMP ),
    .Out6_o(\MAX6682_SPI_FSM_1.SPI_FSM_Wr0 ),
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

  (* src = "../../verilog/max6682.v:111" *)
  \$adff  #(
    .ARST_POLARITY(1'b0),
    .ARST_VALUE(8'b00000000),
    .CLK_POLARITY(1'b1),
    .WIDTH(8)
  ) \$techmap\MAX6682_SPI_FSM_1.$procdff$729  (
    .ARST(\MAX6682_SPI_FSM_1.Reset_n_i ),
    .CLK(\MAX6682_SPI_FSM_1.Clk_i ),
    .D(\$techmap\MAX6682_SPI_FSM_1.$0\Byte0[7:0] ),
    .Q(\MAX6682_SPI_FSM_1.Byte0 )
  );

  (* src = "../../verilog/max6682.v:111" *)
  \$adff  #(
    .ARST_POLARITY(1'b0),
    .ARST_VALUE(8'b00000000),
    .CLK_POLARITY(1'b1),
    .WIDTH(8)
  ) \$techmap\MAX6682_SPI_FSM_1.$procdff$730  (
    .ARST(\MAX6682_SPI_FSM_1.Reset_n_i ),
    .CLK(\MAX6682_SPI_FSM_1.Clk_i ),
    .D(\$techmap\MAX6682_SPI_FSM_1.$0\Byte1[7:0] ),
    .Q(\MAX6682_SPI_FSM_1.Byte1 )
  );

  \$mux  #(
    .WIDTH(8)
  ) \$techmap\MAX6682_SPI_FSM_1.$procmux$458  (
    .A(\MAX6682_SPI_FSM_1.Byte0 ),
    .B(\MAX6682_SPI_FSM_1.SPI_Data_i ),
    .S(\MAX6682_SPI_FSM_1.SPI_FSM_Wr0 ),
    .Y(\$techmap\MAX6682_SPI_FSM_1.$0\Byte0[7:0] )
  );

  \$mux  #(
    .WIDTH(8)
  ) \$techmap\MAX6682_SPI_FSM_1.$procmux$465  (
    .A(\MAX6682_SPI_FSM_1.Byte1 ),
    .B(\MAX6682_SPI_FSM_1.SPI_Data_i ),
    .S(\MAX6682_SPI_FSM_1.SPI_FSM_Wr1 ),
    .Y(\$techmap\MAX6682_SPI_FSM_1.$0\Byte1[7:0] )
  );

  \$and  #(
    .A_SIGNED(0),
    .A_WIDTH(1),
    .B_SIGNED(0),
    .B_WIDTH(1),
    .Y_WIDTH(1)
  ) \$techmap\MAX6682_SPI_FSM_1.$procmux$583  (
    .A(\$techmap\MAX6682_SPI_FSM_1.$procmux$558_CMP ),
    .B(\$techmap\MAX6682_SPI_FSM_1.$2\SPI_FSM_Wr1[0:0] ),
    .Y(\MAX6682_SPI_FSM_1.SPI_FSM_Wr1 )
  );

  \$pmux  #(
    .S_WIDTH(2),
    .WIDTH(1)
  ) \$techmap\MAX6682_SPI_FSM_1.$procmux$593  (
    .A(1'b0),
    .B({ \$techmap\MAX6682_SPI_FSM_1.$2\SPI_FSM_Wr1[0:0] , 1'b1 }),
    .S({ \$techmap\MAX6682_SPI_FSM_1.$procmux$558_CMP , \MAX6682_SPI_FSM_1.SPI_FSM_Wr0  }),
    .Y(\MAX6682_SPI_FSM_1.SPI_ReadNext_o )
  );

  \$pmux  #(
    .S_WIDTH(2),
    .WIDTH(1)
  ) \$techmap\MAX6682_SPI_FSM_1.$procmux$606  (
    .A(1'b1),
    .B({ \$techmap\MAX6682_SPI_FSM_1.$2\MAX6682CS_n_o[0:0] , 1'b0 }),
    .S({ \$techmap\MAX6682_SPI_FSM_1.$procmux$563_CMP , \$techmap\MAX6682_SPI_FSM_1.$auto$opt_reduce.cc:126:opt_mux$736  }),
    .Y(\MAX6682_SPI_FSM_1.MAX6682CS_n_o )
  );

  \$pmux  #(
    .S_WIDTH(2),
    .WIDTH(1)
  ) \$techmap\MAX6682_SPI_FSM_1.$procmux$637  (
    .A(1'b0),
    .B({ \MAX6682_SPI_FSM_1.SPI_FSM_Start , 1'b1 }),
    .S({ \$techmap\MAX6682_SPI_FSM_1.$procmux$563_CMP , \$techmap\MAX6682_SPI_FSM_1.$procmux$560_CMP  }),
    .Y(\MAX6682_SPI_FSM_1.SPI_Write_o )
  );

  \$not  #(
    .A_SIGNED(0),
    .A_WIDTH(1),
    .Y_WIDTH(1)
  ) \$techmap\MAX6682_SPI_FSM_1.$procmux$666  (
    .A(\MAX6682_SPI_FSM_1.SPI_FSM_Start ),
    .Y(\$techmap\MAX6682_SPI_FSM_1.$2\MAX6682CS_n_o[0:0] )
  );

  \$not  #(
    .A_SIGNED(0),
    .A_WIDTH(1),
    .Y_WIDTH(1)
  ) \$techmap\MAX6682_SPI_FSM_1.$procmux$703  (
    .A(\MAX6682_SPI_FSM_1.SPI_Transmission_i ),
    .Y(\$techmap\MAX6682_SPI_FSM_1.$2\SPI_FSM_Wr1[0:0] )
  );

  (* src = "../../verilog/max6682.v:346" *)
  \$mux  #(
    .WIDTH(16)
  ) \$ternary$../../verilog/max6682.v:346$25  (
    .A(DiffAB[15:0]),
    .B(DiffBA),
    .S(DiffAB[16]),
    .Y(AbsDiffResult)
  );
  assign SPI_CPHA_o = 1'b0;
  assign SPI_CPOL_o = 1'b0;
  assign SPI_Data_o = 8'b00000000;
  assign SPI_LSBFE_o = 1'b0;
  assign SensorValue = { 5'b00000, Byte1, Byte0[7:5] };
  assign SensorValue_o = Word0;
  assign Byte0 = \MAX6682_SPI_FSM_1.Byte0 ;
  assign Byte1 = \MAX6682_SPI_FSM_1.Byte1 ;
  assign \MAX6682_SPI_FSM_1.Clk_i  = Clk_i;
  assign MAX6682CS_n_o = \MAX6682_SPI_FSM_1.MAX6682CS_n_o ;
  assign \MAX6682_SPI_FSM_1.Reset_n_i  = Reset_n_i;
  assign \MAX6682_SPI_FSM_1.SPI_Data_i  = SPI_Data_i;
  assign SPI_FSM_Done = \MAX6682_SPI_FSM_1.SPI_FSM_Done ;
  assign \MAX6682_SPI_FSM_1.SPI_FSM_Start  = SPI_FSM_Start;
  assign SPI_ReadNext_o = \MAX6682_SPI_FSM_1.SPI_ReadNext_o ;
  assign \MAX6682_SPI_FSM_1.SPI_Transmission_i  = SPI_Transmission_i;
  assign SPI_Write_o = \MAX6682_SPI_FSM_1.SPI_Write_o ;
  assign SPIFSM_1_CfgMode_s = 1'b0;
  assign SPIFSM_1_CfgClk_s = 1'b0;
  assign SPIFSM_1_CfgShift_s = 1'b0;
  assign SPIFSM_1_CfgDataIn_s = 1'b0;
  assign SensorFSM_1_CfgMode_s = 1'b0;
  assign SensorFSM_1_CfgClk_s = 1'b0;
  assign SensorFSM_1_CfgShift_s = 1'b0;
  assign SensorFSM_1_CfgDataIn_s = 1'b0;

endmodule
