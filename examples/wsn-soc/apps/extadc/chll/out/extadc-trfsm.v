(* src = "../../verilog/extadc.v:1" *)
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

  (* src = "../../verilog/extadc.v:142" *)
  wire [15:0] \$0\Timer[15:0] ;
  (* src = "../../verilog/extadc.v:169" *)
  wire [15:0] \$0\Word0[15:0] ;
  (* src = "../../verilog/extadc.v:58" *)
  wire \$2\SensorPower_o[0:0] ;
  (* src = "../../verilog/extadc.v:58" *)
  wire \$2\StoreNewValue[0:0] ;
  (* src = "../../verilog/extadc.v:58" *)
  wire \$2\TimerPreset[0:0] ;
  (* src = "../../verilog/extadc.v:58" *)
  wire \$3\TimerEnable[0:0] ;
  (* src = "../../verilog/extadc.v:58" *)
  wire \$3\TimerPreset[0:0] ;
  (* src = "../../verilog/extadc.v:58" *)
  wire \$4\TimerEnable[0:0] ;
  wire \$procmux$190_CMP ;
  wire \$procmux$193_CMP ;
  wire \$procmux$194_CMP ;
  wire \$procmux$199_CMP ;
  wire \$procmux$202_CMP ;
  wire [15:0] \$procmux$23_Y ;
  (* src = "../../verilog/extadc.v:156" *)
  wire [15:0] \$sub$../../verilog/extadc.v:156$12_Y ;
  (* src = "../../verilog/extadc.v:167" *)
  wire [15:0] AbsDiffResult;
  (* src = "../../verilog/extadc.v:184" *)
  wire [16:0] DiffAB;
  (* src = "../../verilog/extadc.v:185" *)
  wire [15:0] DiffBA;
  (* src = "../../verilog/extadc.v:42" *)
  wire DiffTooLarge;
  (* src = "../../verilog/extadc.v:43" *)
  wire StoreNewValue;
  (* src = "../../verilog/extadc.v:140" *)
  wire [15:0] Timer;
  (* src = "../../verilog/extadc.v:41" *)
  wire TimerEnable;
  (* src = "../../verilog/extadc.v:39" *)
  wire TimerOvfl;
  (* src = "../../verilog/extadc.v:40" *)
  wire TimerPreset;
  (* src = "../../verilog/extadc.v:166" *)
  wire [15:0] Word0;
  wire FSM_1_Out6_s;
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


  \$reduce_or  #(
    .A_SIGNED(0),
    .A_WIDTH(3),
    .Y_WIDTH(1)
  ) \$auto$opt_reduce.cc:130:opt_mux$710  (
    .A({ \$procmux$194_CMP , \$procmux$193_CMP , \$procmux$190_CMP  }),
    .Y(SensorStart_o)
  );

  (* src = "../../verilog/extadc.v:161" *)
  \$eq  #(
    .A_SIGNED(0),
    .A_WIDTH(16),
    .B_SIGNED(0),
    .B_WIDTH(16),
    .Y_WIDTH(1)
  ) \$eq$../../verilog/extadc.v:161$13  (
    .A(Timer),
    .B(16'b0000000000000000),
    .Y(TimerOvfl)
  );

  FSM FSM_1 (
    .Reset_n_i(Reset_n_i),
    .Clk_i(Clk_i),
    .In0_i(AdcDone_i),
    .In1_i(DiffTooLarge),
    .In2_i(Enable_i),
    .In3_i(SensorReady_i),
    .In4_i(TimerOvfl),
    .In5_i(1'b0),
    .In6_i(1'b0),
    .In7_i(1'b0),
    .Out0_o(\$procmux$190_CMP ),
    .Out1_o(\$procmux$193_CMP ),
    .Out2_o(\$procmux$194_CMP ),
    .Out3_o(\$procmux$199_CMP ),
    .Out4_o(\$procmux$202_CMP ),
    .Out5_o(CpuIntr_o),
    .Out6_o(FSM_1_Out6_s),
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

  (* src = "../../verilog/extadc.v:190" *)
  \$gt  #(
    .A_SIGNED(0),
    .A_WIDTH(16),
    .B_SIGNED(0),
    .B_WIDTH(16),
    .Y_WIDTH(1)
  ) \$gt$../../verilog/extadc.v:190$20  (
    .A(AbsDiffResult),
    .B(Threshold_i),
    .Y(DiffTooLarge)
  );

  (* src = "../../verilog/extadc.v:142" *)
  \$adff  #(
    .ARST_POLARITY(1'b0),
    .ARST_VALUE(16'b0000000000000000),
    .CLK_POLARITY(1'b1),
    .WIDTH(16)
  ) \$procdff$706  (
    .ARST(Reset_n_i),
    .CLK(Clk_i),
    .D(\$0\Timer[15:0] ),
    .Q(Timer)
  );

  (* src = "../../verilog/extadc.v:169" *)
  \$adff  #(
    .ARST_POLARITY(1'b0),
    .ARST_VALUE(16'b0000000000000000),
    .CLK_POLARITY(1'b1),
    .WIDTH(16)
  ) \$procdff$707  (
    .ARST(Reset_n_i),
    .CLK(Clk_i),
    .D(\$0\Word0[15:0] ),
    .Q(Word0)
  );

  \$pmux  #(
    .S_WIDTH(2),
    .WIDTH(1)
  ) \$procmux$189  (
    .A(1'b0),
    .B({ SensorReady_i, 1'b1 }),
    .S({ \$procmux$193_CMP , \$procmux$190_CMP  }),
    .Y(AdcStart_o)
  );

  \$pmux  #(
    .S_WIDTH(2),
    .WIDTH(1)
  ) \$procmux$229  (
    .A(1'b0),
    .B({ \$2\SensorPower_o[0:0] , 1'b1 }),
    .S({ \$procmux$199_CMP , SensorStart_o }),
    .Y(SensorPower_o)
  );

  \$mux  #(
    .WIDTH(16)
  ) \$procmux$23  (
    .A(Timer),
    .B(\$sub$../../verilog/extadc.v:156$12_Y ),
    .S(TimerEnable),
    .Y(\$procmux$23_Y )
  );

  \$mux  #(
    .WIDTH(16)
  ) \$procmux$26  (
    .A(\$procmux$23_Y ),
    .B(PeriodCounterPreset_i),
    .S(TimerPreset),
    .Y(\$0\Timer[15:0] )
  );

  \$and  #(
    .A_SIGNED(0),
    .A_WIDTH(1),
    .B_SIGNED(0),
    .B_WIDTH(1),
    .Y_WIDTH(1)
  ) \$procmux$269  (
    .A(\$procmux$190_CMP ),
    .B(\$2\StoreNewValue[0:0] ),
    .Y(StoreNewValue)
  );

  \$mux  #(
    .WIDTH(16)
  ) \$procmux$29  (
    .A(Word0),
    .B(AdcValue_i),
    .S(StoreNewValue),
    .Y(\$0\Word0[15:0] )
  );

  \$pmux  #(
    .S_WIDTH(2),
    .WIDTH(1)
  ) \$procmux$318  (
    .A(1'b0),
    .B({ Enable_i, \$3\TimerEnable[0:0]  }),
    .S({ \$procmux$202_CMP , \$procmux$199_CMP  }),
    .Y(TimerEnable)
  );

  \$pmux  #(
    .S_WIDTH(2),
    .WIDTH(1)
  ) \$procmux$338  (
    .A(1'b1),
    .B({ \$2\TimerPreset[0:0] , \$3\TimerPreset[0:0]  }),
    .S({ \$procmux$202_CMP , \$procmux$199_CMP  }),
    .Y(TimerPreset)
  );

  \$not  #(
    .A_SIGNED(0),
    .A_WIDTH(1),
    .Y_WIDTH(1)
  ) \$procmux$381  (
    .A(Enable_i),
    .Y(\$2\TimerPreset[0:0] )
  );

  \$and  #(
    .A_SIGNED(0),
    .A_WIDTH(1),
    .B_SIGNED(0),
    .B_WIDTH(1),
    .Y_WIDTH(1)
  ) \$procmux$420  (
    .A(Enable_i),
    .B(TimerOvfl),
    .Y(\$2\SensorPower_o[0:0] )
  );

  \$and  #(
    .A_SIGNED(0),
    .A_WIDTH(1),
    .B_SIGNED(0),
    .B_WIDTH(1),
    .Y_WIDTH(1)
  ) \$procmux$441  (
    .A(Enable_i),
    .B(\$4\TimerEnable[0:0] ),
    .Y(\$3\TimerEnable[0:0] )
  );

  \$mux  #(
    .WIDTH(1)
  ) \$procmux$462  (
    .A(1'b1),
    .B(TimerOvfl),
    .S(Enable_i),
    .Y(\$3\TimerPreset[0:0] )
  );

  \$not  #(
    .A_SIGNED(0),
    .A_WIDTH(1),
    .Y_WIDTH(1)
  ) \$procmux$502  (
    .A(TimerOvfl),
    .Y(\$4\TimerEnable[0:0] )
  );

  \$and  #(
    .A_SIGNED(0),
    .A_WIDTH(1),
    .B_SIGNED(0),
    .B_WIDTH(1),
    .Y_WIDTH(1)
  ) \$procmux$646  (
    .A(AdcDone_i),
    .B(DiffTooLarge),
    .Y(\$2\StoreNewValue[0:0] )
  );

  (* src = "../../verilog/extadc.v:156" *)
  \$sub  #(
    .A_SIGNED(0),
    .A_WIDTH(16),
    .B_SIGNED(0),
    .B_WIDTH(1),
    .Y_WIDTH(16)
  ) \$sub$../../verilog/extadc.v:156$12  (
    .A(Timer),
    .B(1'b1),
    .Y(\$sub$../../verilog/extadc.v:156$12_Y )
  );

  (* src = "../../verilog/extadc.v:186" *)
  \$sub  #(
    .A_SIGNED(0),
    .A_WIDTH(17),
    .B_SIGNED(0),
    .B_WIDTH(17),
    .Y_WIDTH(17)
  ) \$sub$../../verilog/extadc.v:186$17  (
    .A({ 1'b0, AdcValue_i }),
    .B({ 1'b0, Word0 }),
    .Y(DiffAB)
  );

  (* src = "../../verilog/extadc.v:187" *)
  \$sub  #(
    .A_SIGNED(0),
    .A_WIDTH(16),
    .B_SIGNED(0),
    .B_WIDTH(16),
    .Y_WIDTH(16)
  ) \$sub$../../verilog/extadc.v:187$18  (
    .A(Word0),
    .B(AdcValue_i),
    .Y(DiffBA)
  );

  (* src = "../../verilog/extadc.v:188" *)
  \$mux  #(
    .WIDTH(16)
  ) \$ternary$../../verilog/extadc.v:188$19  (
    .A(DiffAB[15:0]),
    .B(DiffBA),
    .S(DiffAB[16]),
    .Y(AbsDiffResult)
  );
  assign SensorValue_o = Word0;
  assign FSM_1_CfgMode_s = 1'b0;
  assign FSM_1_CfgClk_s = 1'b0;
  assign FSM_1_CfgShift_s = 1'b0;
  assign FSM_1_CfgDataIn_s = 1'b0;

endmodule
