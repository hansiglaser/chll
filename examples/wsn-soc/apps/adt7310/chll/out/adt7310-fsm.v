/* Generated by Yosys 0.3.0+ (git sha1 3b52121) */

(* src = "../../verilog/spifsm.v:3" *)
module \$paramod\SPIFSM\SPPRWidth=4\SPRWidth=4\DataWidth=8 (Reset_n_i, Clk_i, Start_i, Done_o, Byte0_o, Byte1_o, SPI_Transmission_i, SPI_Write_o, SPI_ReadNext_o, SPI_Data_o, SPI_Data_i, SPI_FIFOFull_i, SPI_FIFOEmpty_i, ADT7310CS_n_o, ParamCounterPreset_i);
  (* src = "../../verilog/spifsm.v:187" *)
  wire [7:0] \$0\Byte0_o[7:0] ;
  (* src = "../../verilog/spifsm.v:187" *)
  wire [7:0] \$0\Byte1_o[7:0] ;
  (* src = "../../verilog/spifsm.v:217" *)
  wire [31:0] \$0\SPI_FSM_Timer[31:0] ;
  (* src = "../../verilog/spifsm.v:65" *)
  wire \$2\ADT7310CS_n_o[0:0] ;
  (* src = "../../verilog/spifsm.v:65" *)
  wire \$2\SPI_FSM_TimerEnable[0:0] ;
  (* src = "../../verilog/spifsm.v:65" *)
  wire \$2\SPI_ReadNext_o[0:0] ;
  wire \$auto$opt_reduce.cc:126:opt_mux$2453 ;
  wire \$auto$opt_reduce.cc:126:opt_mux$2455 ;
  wire \$auto$opt_reduce.cc:126:opt_mux$2457 ;
  wire \$auto$opt_reduce.cc:126:opt_mux$2459 ;
  wire \$auto$opt_reduce.cc:126:opt_mux$2461 ;
  wire \$auto$opt_reduce.cc:126:opt_mux$2477 ;
  wire \$procmux$297_CMP ;
  wire \$procmux$298_CMP ;
  wire \$procmux$301_CMP ;
  wire \$procmux$302_CMP ;
  wire \$procmux$305_CMP ;
  wire \$procmux$334_CMP ;
  wire \$procmux$335_CMP ;
  wire \$procmux$336_CMP ;
  wire [31:0] \$procmux$80_Y ;
  (* src = "../../verilog/spifsm.v:231" *)
  wire [31:0] \$sub$../../verilog/spifsm.v:231$46_Y ;
  (* src = "../../verilog/spifsm.v:24" *)
  output ADT7310CS_n_o;
  (* src = "../../verilog/spifsm.v:13" *)
  output [7:0] Byte0_o;
  (* src = "../../verilog/spifsm.v:14" *)
  output [7:0] Byte1_o;
  (* src = "../../verilog/spifsm.v:9" *)
  input Clk_i;
  (* src = "../../verilog/spifsm.v:12" *)
  output Done_o;
  (* src = "../../verilog/spifsm.v:26" *)
  input [31:0] ParamCounterPreset_i;
  (* src = "../../verilog/spifsm.v:8" *)
  input Reset_n_i;
  (* src = "../../verilog/spifsm.v:20" *)
  input [7:0] SPI_Data_i;
  (* src = "../../verilog/spifsm.v:19" *)
  output [7:0] SPI_Data_o;
  (* src = "../../verilog/spifsm.v:22" *)
  input SPI_FIFOEmpty_i;
  (* src = "../../verilog/spifsm.v:21" *)
  input SPI_FIFOFull_i;
  (* src = "../../verilog/spifsm.v:215" *)
  wire [31:0] SPI_FSM_Timer;
  (* src = "../../verilog/spifsm.v:45" *)
  wire SPI_FSM_TimerEnable;
  (* src = "../../verilog/spifsm.v:43" *)
  wire SPI_FSM_TimerOvfl;
  (* src = "../../verilog/spifsm.v:44" *)
  wire SPI_FSM_TimerPreset;
  (* src = "../../verilog/spifsm.v:47" *)
  wire SPI_FSM_Wr0;
  (* src = "../../verilog/spifsm.v:46" *)
  wire SPI_FSM_Wr1;
  (* src = "../../verilog/spifsm.v:18" *)
  output SPI_ReadNext_o;
  (* src = "../../verilog/spifsm.v:16" *)
  input SPI_Transmission_i;
  (* src = "../../verilog/spifsm.v:17" *)
  output SPI_Write_o;
  (* src = "../../verilog/spifsm.v:11" *)
  input Start_i;
  \$reduce_or  #(
    .A_SIGNED(32'b00000000000000000000000000000000),
    .A_WIDTH(32'b00000000000000000000000000000011),
    .Y_WIDTH(32'b00000000000000000000000000000001)
  ) \$auto$opt_reduce.cc:130:opt_mux$2454  (
    .A({ SPI_FSM_Wr1, SPI_FSM_Wr0, \$procmux$298_CMP  }),
    .Y(\$auto$opt_reduce.cc:126:opt_mux$2453 )
  );
  \$reduce_or  #(
    .A_SIGNED(32'b00000000000000000000000000000000),
    .A_WIDTH(32'b00000000000000000000000000000110),
    .Y_WIDTH(32'b00000000000000000000000000000001)
  ) \$auto$opt_reduce.cc:130:opt_mux$2456  (
    .A({ \$procmux$336_CMP , \$procmux$335_CMP , \$procmux$334_CMP , \$procmux$302_CMP , \$procmux$301_CMP , \$procmux$298_CMP  }),
    .Y(\$auto$opt_reduce.cc:126:opt_mux$2455 )
  );
  \$reduce_or  #(
    .A_SIGNED(32'b00000000000000000000000000000000),
    .A_WIDTH(32'b00000000000000000000000000001001),
    .Y_WIDTH(32'b00000000000000000000000000000001)
  ) \$auto$opt_reduce.cc:130:opt_mux$2458  (
    .A({ SPI_FSM_Wr1, SPI_FSM_Wr0, \$procmux$336_CMP , \$procmux$335_CMP , \$procmux$334_CMP , \$procmux$302_CMP , \$procmux$301_CMP , \$procmux$298_CMP , \$procmux$297_CMP  }),
    .Y(\$auto$opt_reduce.cc:126:opt_mux$2457 )
  );
  \$reduce_or  #(
    .A_SIGNED(32'b00000000000000000000000000000000),
    .A_WIDTH(32'b00000000000000000000000000000010),
    .Y_WIDTH(32'b00000000000000000000000000000001)
  ) \$auto$opt_reduce.cc:130:opt_mux$2460  (
    .A({ \$procmux$336_CMP , \$procmux$335_CMP  }),
    .Y(\$auto$opt_reduce.cc:126:opt_mux$2459 )
  );
  \$reduce_or  #(
    .A_SIGNED(32'b00000000000000000000000000000000),
    .A_WIDTH(32'b00000000000000000000000000000011),
    .Y_WIDTH(32'b00000000000000000000000000000001)
  ) \$auto$opt_reduce.cc:130:opt_mux$2462  (
    .A({ \$procmux$336_CMP , \$procmux$335_CMP , \$procmux$302_CMP  }),
    .Y(\$auto$opt_reduce.cc:126:opt_mux$2461 )
  );
  \$reduce_or  #(
    .A_SIGNED(32'b00000000000000000000000000000000),
    .A_WIDTH(32'b00000000000000000000000000000010),
    .Y_WIDTH(32'b00000000000000000000000000000001)
  ) \$auto$opt_reduce.cc:130:opt_mux$2478  (
    .A({ \$procmux$334_CMP , \$procmux$301_CMP  }),
    .Y(\$auto$opt_reduce.cc:126:opt_mux$2477 )
  );
  (* src = "../../verilog/spifsm.v:236" *)
  \$eq  #(
    .A_SIGNED(32'b00000000000000000000000000000000),
    .A_WIDTH(32'b00000000000000000000000000100000),
    .B_SIGNED(32'b00000000000000000000000000000000),
    .B_WIDTH(32'b00000000000000000000000000100000),
    .Y_WIDTH(32'b00000000000000000000000000000001)
  ) \$eq$../../verilog/spifsm.v:236$47  (
    .A(SPI_FSM_Timer),
    .B(0),
    .Y(SPI_FSM_TimerOvfl)
  );
  (* fsm_encoding = "auto" *)
  (* src = "../../verilog/spifsm.v:41" *)
  \$fsm  #(
    .ARST_POLARITY(1'b0),
    .CLK_POLARITY(1'b1),
    .CTRL_IN_WIDTH(32'b00000000000000000000000000000011),
    .CTRL_OUT_WIDTH(32'b00000000000000000000000000001010),
    .NAME("\\SPI_FSM_State"),
    .STATE_BITS(32'b00000000000000000000000000000100),
    .STATE_NUM(32'b00000000000000000000000000001011),
    .STATE_NUM_LOG2(32'b00000000000000000000000000000100),
    .STATE_RST(32'b00000000000000000000000000000000),
    .STATE_TABLE(44'b01110011010110010001011010100010010010000000),
    .TRANS_NUM(32'b00000000000000000000000000001111),
    .TRANS_TABLE(315'b1010z1z101000001000001010z0z000100001000001001zzz001000000000101000zzz010100100000000111zzz010001000000000110zzz001100000010000101zzz101000010000000100zzz000000000000000011z0z100100000001000011z1z001100000001000010zz1100000000000010010zz0001000000000010001zzz0111100000000000001zz0110000001000000000zz00000000010000)
  ) \$fsm$\SPI_FSM_State$2481  (
    .ARST(Reset_n_i),
    .CLK(Clk_i),
    .CTRL_IN({ Start_i, SPI_Transmission_i, SPI_FSM_TimerOvfl }),
    .CTRL_OUT({ SPI_FSM_Wr1, SPI_FSM_Wr0, \$procmux$336_CMP , \$procmux$335_CMP , \$procmux$334_CMP , \$procmux$305_CMP , \$procmux$302_CMP , \$procmux$301_CMP , \$procmux$298_CMP , \$procmux$297_CMP  })
  );
  (* src = "../../verilog/spifsm.v:187" *)
  \$adff  #(
    .ARST_POLARITY(1'b0),
    .ARST_VALUE(8'b00000000),
    .CLK_POLARITY(1'b1),
    .WIDTH(32'b00000000000000000000000000001000)
  ) \$procdff$2439  (
    .ARST(Reset_n_i),
    .CLK(Clk_i),
    .D(\$0\Byte0_o[7:0] ),
    .Q(Byte0_o)
  );
  (* src = "../../verilog/spifsm.v:187" *)
  \$adff  #(
    .ARST_POLARITY(1'b0),
    .ARST_VALUE(8'b00000000),
    .CLK_POLARITY(1'b1),
    .WIDTH(32'b00000000000000000000000000001000)
  ) \$procdff$2440  (
    .ARST(Reset_n_i),
    .CLK(Clk_i),
    .D(\$0\Byte1_o[7:0] ),
    .Q(Byte1_o)
  );
  (* src = "../../verilog/spifsm.v:217" *)
  \$adff  #(
    .ARST_POLARITY(1'b0),
    .ARST_VALUE(32'b00000000000000000000000000000000),
    .CLK_POLARITY(1'b1),
    .WIDTH(32'b00000000000000000000000000100000)
  ) \$procdff$2441  (
    .ARST(Reset_n_i),
    .CLK(Clk_i),
    .D(\$0\SPI_FSM_Timer[31:0] ),
    .Q(SPI_FSM_Timer)
  );
  \$pmux  #(
    .S_WIDTH(32'b00000000000000000000000000000010),
    .WIDTH(32'b00000000000000000000000000000001)
  ) \$procmux$296  (
    .A(1'b0),
    .B({ 1'b1, \$2\SPI_FSM_TimerEnable[0:0]  }),
    .S({ \$procmux$298_CMP , \$procmux$297_CMP  }),
    .Y(SPI_FSM_TimerEnable)
  );
  \$pmux  #(
    .S_WIDTH(32'b00000000000000000000000000000010),
    .WIDTH(32'b00000000000000000000000000000001)
  ) \$procmux$317  (
    .A(1'b1),
    .B({ 1'b0, SPI_FSM_TimerOvfl }),
    .S({ \$procmux$298_CMP , \$procmux$297_CMP  }),
    .Y(SPI_FSM_TimerPreset)
  );
  \$pmux  #(
    .S_WIDTH(32'b00000000000000000000000000000010),
    .WIDTH(32'b00000000000000000000000000000001)
  ) \$procmux$371  (
    .A(1'b0),
    .B({ \$2\SPI_ReadNext_o[0:0] , 1'b1 }),
    .S({ \$auto$opt_reduce.cc:126:opt_mux$2477 , \$auto$opt_reduce.cc:126:opt_mux$2453  }),
    .Y(SPI_ReadNext_o)
  );
  \$pmux  #(
    .S_WIDTH(32'b00000000000000000000000000000011),
    .WIDTH(32'b00000000000000000000000000000001)
  ) \$procmux$396  (
    .A(1'b1),
    .B({ \$2\ADT7310CS_n_o[0:0] , \$2\SPI_FSM_TimerEnable[0:0] , 1'b0 }),
    .S({ \$procmux$305_CMP , \$procmux$297_CMP , \$auto$opt_reduce.cc:126:opt_mux$2455  }),
    .Y(ADT7310CS_n_o)
  );
  \$pmux  #(
    .S_WIDTH(32'b00000000000000000000000000000010),
    .WIDTH(32'b00000000000000000000000000000001)
  ) \$procmux$413  (
    .A(1'b1),
    .B({ \$2\ADT7310CS_n_o[0:0] , 1'b0 }),
    .S({ \$procmux$305_CMP , \$auto$opt_reduce.cc:126:opt_mux$2457  }),
    .Y(Done_o)
  );
  \$pmux  #(
    .S_WIDTH(32'b00000000000000000000000000000011),
    .WIDTH(32'b00000000000000000000000000001000)
  ) \$procmux$439  (
    .A(8'b00001000),
    .B(24'b001000000101000011111111),
    .S({ \$procmux$302_CMP , \$procmux$297_CMP , \$auto$opt_reduce.cc:126:opt_mux$2459  }),
    .Y(SPI_Data_o)
  );
  \$pmux  #(
    .S_WIDTH(32'b00000000000000000000000000000011),
    .WIDTH(32'b00000000000000000000000000000001)
  ) \$procmux$481  (
    .A(1'b0),
    .B({ Start_i, SPI_FSM_TimerOvfl, 1'b1 }),
    .S({ \$procmux$305_CMP , \$procmux$297_CMP , \$auto$opt_reduce.cc:126:opt_mux$2461  }),
    .Y(SPI_Write_o)
  );
  \$not  #(
    .A_SIGNED(32'b00000000000000000000000000000000),
    .A_WIDTH(32'b00000000000000000000000000000001),
    .Y_WIDTH(32'b00000000000000000000000000000001)
  ) \$procmux$535  (
    .A(Start_i),
    .Y(\$2\ADT7310CS_n_o[0:0] )
  );
  \$not  #(
    .A_SIGNED(32'b00000000000000000000000000000000),
    .A_WIDTH(32'b00000000000000000000000000000001),
    .Y_WIDTH(32'b00000000000000000000000000000001)
  ) \$procmux$641  (
    .A(SPI_Transmission_i),
    .Y(\$2\SPI_ReadNext_o[0:0] )
  );
  \$not  #(
    .A_SIGNED(32'b00000000000000000000000000000000),
    .A_WIDTH(32'b00000000000000000000000000000001),
    .Y_WIDTH(32'b00000000000000000000000000000001)
  ) \$procmux$659  (
    .A(SPI_FSM_TimerOvfl),
    .Y(\$2\SPI_FSM_TimerEnable[0:0] )
  );
  \$mux  #(
    .WIDTH(32'b00000000000000000000000000001000)
  ) \$procmux$70  (
    .A(Byte0_o),
    .B(SPI_Data_i),
    .S(SPI_FSM_Wr0),
    .Y(\$0\Byte0_o[7:0] )
  );
  \$mux  #(
    .WIDTH(32'b00000000000000000000000000001000)
  ) \$procmux$77  (
    .A(Byte1_o),
    .B(SPI_Data_i),
    .S(SPI_FSM_Wr1),
    .Y(\$0\Byte1_o[7:0] )
  );
  \$mux  #(
    .WIDTH(32'b00000000000000000000000000100000)
  ) \$procmux$80  (
    .A(SPI_FSM_Timer),
    .B(\$sub$../../verilog/spifsm.v:231$46_Y ),
    .S(SPI_FSM_TimerEnable),
    .Y(\$procmux$80_Y )
  );
  \$mux  #(
    .WIDTH(32'b00000000000000000000000000100000)
  ) \$procmux$83  (
    .A(\$procmux$80_Y ),
    .B(ParamCounterPreset_i),
    .S(SPI_FSM_TimerPreset),
    .Y(\$0\SPI_FSM_Timer[31:0] )
  );
  (* src = "../../verilog/spifsm.v:231" *)
  \$sub  #(
    .A_SIGNED(32'b00000000000000000000000000000000),
    .A_WIDTH(32'b00000000000000000000000000100000),
    .B_SIGNED(32'b00000000000000000000000000000000),
    .B_WIDTH(32'b00000000000000000000000000000001),
    .Y_WIDTH(32'b00000000000000000000000000100000)
  ) \$sub$../../verilog/spifsm.v:231$46  (
    .A(SPI_FSM_Timer),
    .B(1'b1),
    .Y(\$sub$../../verilog/spifsm.v:231$46_Y )
  );
endmodule

(* src = "../../verilog/sensorfsm.v:3" *)
module \$paramod\SensorFSM\DataWidth=8 (Reset_n_i, Clk_i, Enable_i, CpuIntr_o, SensorValue_o, MeasureFSM_Start_o, MeasureFSM_Done_i, MeasureFSM_Byte0_i, MeasureFSM_Byte1_i, ParamThreshold_i, ParamCounterPreset_i);
  (* src = "../../verilog/sensorfsm.v:130" *)
  wire [15:0] \$0\SensorFSM_Timer[15:0] ;
  (* src = "../../verilog/sensorfsm.v:153" *)
  wire [15:0] \$0\Word0[15:0] ;
  (* src = "../../verilog/sensorfsm.v:57" *)
  wire \$2\MeasureFSM_Start_o[0:0] ;
  (* src = "../../verilog/sensorfsm.v:57" *)
  wire \$2\SensorFSM_StoreNewValue[0:0] ;
  (* src = "../../verilog/sensorfsm.v:57" *)
  wire \$2\SensorFSM_TimerPreset[0:0] ;
  (* src = "../../verilog/sensorfsm.v:57" *)
  wire \$3\SensorFSM_TimerPreset[0:0] ;
  (* src = "../../verilog/sensorfsm.v:57" *)
  wire \$4\SensorFSM_TimerPreset[0:0] ;
  wire \$auto$opt_reduce.cc:126:opt_mux$2463 ;
  wire \$procmux$1004_CMP ;
  wire \$procmux$1007_CMP ;
  wire \$procmux$1129_CMP ;
  wire [15:0] \$procmux$826_Y ;
  (* src = "../../verilog/sensorfsm.v:144" *)
  wire [15:0] \$sub$../../verilog/sensorfsm.v:144$59_Y ;
  (* src = "../../verilog/sensorfsm.v:39" *)
  wire [15:0] AbsDiffResult;
  (* src = "../../verilog/sensorfsm.v:7" *)
  input Clk_i;
  (* src = "../../verilog/sensorfsm.v:10" *)
  output CpuIntr_o;
  (* src = "../../verilog/sensorfsm.v:168" *)
  wire [16:0] DiffAB;
  (* src = "../../verilog/sensorfsm.v:169" *)
  wire [15:0] DiffBA;
  (* src = "../../verilog/sensorfsm.v:9" *)
  input Enable_i;
  (* src = "../../verilog/sensorfsm.v:15" *)
  input [7:0] MeasureFSM_Byte0_i;
  (* src = "../../verilog/sensorfsm.v:16" *)
  input [7:0] MeasureFSM_Byte1_i;
  (* src = "../../verilog/sensorfsm.v:14" *)
  input MeasureFSM_Done_i;
  (* src = "../../verilog/sensorfsm.v:13" *)
  output MeasureFSM_Start_o;
  (* src = "../../verilog/sensorfsm.v:19" *)
  input [15:0] ParamCounterPreset_i;
  (* src = "../../verilog/sensorfsm.v:18" *)
  input [15:0] ParamThreshold_i;
  (* src = "../../verilog/sensorfsm.v:6" *)
  input Reset_n_i;
  (* src = "../../verilog/sensorfsm.v:32" *)
  wire SensorFSM_DiffTooLarge;
  (* src = "../../verilog/sensorfsm.v:33" *)
  wire SensorFSM_StoreNewValue;
  (* src = "../../verilog/sensorfsm.v:128" *)
  wire [15:0] SensorFSM_Timer;
  (* src = "../../verilog/sensorfsm.v:31" *)
  wire SensorFSM_TimerEnable;
  (* src = "../../verilog/sensorfsm.v:29" *)
  wire SensorFSM_TimerOvfl;
  (* src = "../../verilog/sensorfsm.v:30" *)
  wire SensorFSM_TimerPreset;
  (* src = "../../verilog/sensorfsm.v:37" *)
  wire [15:0] SensorValue;
  (* src = "../../verilog/sensorfsm.v:11" *)
  output [15:0] SensorValue_o;
  (* src = "../../verilog/sensorfsm.v:38" *)
  wire [15:0] Word0;
  \$reduce_or  #(
    .A_SIGNED(32'b00000000000000000000000000000000),
    .A_WIDTH(32'b00000000000000000000000000000011),
    .Y_WIDTH(32'b00000000000000000000000000000001)
  ) \$auto$opt_reduce.cc:130:opt_mux$2464  (
    .A({ \$procmux$1129_CMP , \$procmux$1007_CMP , \$procmux$1004_CMP  }),
    .Y(\$auto$opt_reduce.cc:126:opt_mux$2463 )
  );
  (* src = "../../verilog/sensorfsm.v:149" *)
  \$eq  #(
    .A_SIGNED(32'b00000000000000000000000000000000),
    .A_WIDTH(32'b00000000000000000000000000010000),
    .B_SIGNED(32'b00000000000000000000000000000000),
    .B_WIDTH(32'b00000000000000000000000000010000),
    .Y_WIDTH(32'b00000000000000000000000000000001)
  ) \$eq$../../verilog/sensorfsm.v:149$60  (
    .A(SensorFSM_Timer),
    .B(16'b0000000000000000),
    .Y(SensorFSM_TimerOvfl)
  );
  (* fsm_encoding = "auto" *)
  (* src = "../../verilog/sensorfsm.v:27" *)
  \$fsm  #(
    .ARST_POLARITY(1'b0),
    .CLK_POLARITY(1'b1),
    .CTRL_IN_WIDTH(32'b00000000000000000000000000000100),
    .CTRL_OUT_WIDTH(32'b00000000000000000000000000000011),
    .NAME("\\SensorFSM_State"),
    .STATE_BITS(32'b00000000000000000000000000000010),
    .STATE_NUM(32'b00000000000000000000000000000100),
    .STATE_NUM_LOG2(32'b00000000000000000000000000000011),
    .STATE_RST(32'b00000000000000000000000000000000),
    .STATE_TABLE(8'b11011000),
    .TRANS_NUM(32'b00000000000000000000000000001001),
    .TRANS_TABLE(117'b011zzzz0100000100zz10100010101zz1001001010zzz0000001001z11z011100001z01z010100001zz0z001100000zzz1010010000zzz0000010)
  ) \$fsm$\SensorFSM_State$2494  (
    .ARST(Reset_n_i),
    .CLK(Clk_i),
    .CTRL_IN({ SensorFSM_TimerOvfl, SensorFSM_DiffTooLarge, MeasureFSM_Done_i, Enable_i }),
    .CTRL_OUT({ \$procmux$1129_CMP , \$procmux$1007_CMP , \$procmux$1004_CMP  })
  );
  (* src = "../../verilog/sensorfsm.v:174" *)
  \$gt  #(
    .A_SIGNED(32'b00000000000000000000000000000000),
    .A_WIDTH(32'b00000000000000000000000000010000),
    .B_SIGNED(32'b00000000000000000000000000000000),
    .B_WIDTH(32'b00000000000000000000000000010000),
    .Y_WIDTH(32'b00000000000000000000000000000001)
  ) \$gt$../../verilog/sensorfsm.v:174$67  (
    .A(AbsDiffResult),
    .B(ParamThreshold_i),
    .Y(SensorFSM_DiffTooLarge)
  );
  (* src = "../../verilog/sensorfsm.v:130" *)
  \$adff  #(
    .ARST_POLARITY(1'b0),
    .ARST_VALUE(16'b0000000000000000),
    .CLK_POLARITY(1'b1),
    .WIDTH(32'b00000000000000000000000000010000)
  ) \$procdff$2443  (
    .ARST(Reset_n_i),
    .CLK(Clk_i),
    .D(\$0\SensorFSM_Timer[15:0] ),
    .Q(SensorFSM_Timer)
  );
  (* src = "../../verilog/sensorfsm.v:153" *)
  \$adff  #(
    .ARST_POLARITY(1'b0),
    .ARST_VALUE(16'b0000000000000000),
    .CLK_POLARITY(1'b1),
    .WIDTH(32'b00000000000000000000000000010000)
  ) \$procdff$2444  (
    .ARST(Reset_n_i),
    .CLK(Clk_i),
    .D(\$0\Word0[15:0] ),
    .Q(Word0)
  );
  \$not  #(
    .A_SIGNED(32'b00000000000000000000000000000000),
    .A_WIDTH(32'b00000000000000000000000000000001),
    .Y_WIDTH(32'b00000000000000000000000000000001)
  ) \$procmux$1036  (
    .A(Enable_i),
    .Y(\$2\SensorFSM_TimerPreset[0:0] )
  );
  \$and  #(
    .A_SIGNED(32'b00000000000000000000000000000000),
    .A_WIDTH(32'b00000000000000000000000000000001),
    .B_SIGNED(32'b00000000000000000000000000000000),
    .B_WIDTH(32'b00000000000000000000000000000001),
    .Y_WIDTH(32'b00000000000000000000000000000001)
  ) \$procmux$1065  (
    .A(Enable_i),
    .B(SensorFSM_TimerOvfl),
    .Y(\$2\MeasureFSM_Start_o[0:0] )
  );
  \$and  #(
    .A_SIGNED(32'b00000000000000000000000000000000),
    .A_WIDTH(32'b00000000000000000000000000000001),
    .B_SIGNED(32'b00000000000000000000000000000000),
    .B_WIDTH(32'b00000000000000000000000000000001),
    .Y_WIDTH(32'b00000000000000000000000000000001)
  ) \$procmux$1142  (
    .A(MeasureFSM_Done_i),
    .B(SensorFSM_DiffTooLarge),
    .Y(\$2\SensorFSM_StoreNewValue[0:0] )
  );
  \$mux  #(
    .WIDTH(32'b00000000000000000000000000000001)
  ) \$procmux$1174  (
    .A(1'b1),
    .B(\$4\SensorFSM_TimerPreset[0:0] ),
    .S(MeasureFSM_Done_i),
    .Y(\$3\SensorFSM_TimerPreset[0:0] )
  );
  \$not  #(
    .A_SIGNED(32'b00000000000000000000000000000000),
    .A_WIDTH(32'b00000000000000000000000000000001),
    .Y_WIDTH(32'b00000000000000000000000000000001)
  ) \$procmux$1206  (
    .A(SensorFSM_DiffTooLarge),
    .Y(\$4\SensorFSM_TimerPreset[0:0] )
  );
  \$mux  #(
    .WIDTH(32'b00000000000000000000000000010000)
  ) \$procmux$826  (
    .A(SensorFSM_Timer),
    .B(\$sub$../../verilog/sensorfsm.v:144$59_Y ),
    .S(SensorFSM_TimerEnable),
    .Y(\$procmux$826_Y )
  );
  \$mux  #(
    .WIDTH(32'b00000000000000000000000000010000)
  ) \$procmux$829  (
    .A(\$procmux$826_Y ),
    .B(ParamCounterPreset_i),
    .S(SensorFSM_TimerPreset),
    .Y(\$0\SensorFSM_Timer[15:0] )
  );
  \$mux  #(
    .WIDTH(32'b00000000000000000000000000010000)
  ) \$procmux$832  (
    .A(Word0),
    .B({ MeasureFSM_Byte1_i, MeasureFSM_Byte0_i }),
    .S(SensorFSM_StoreNewValue),
    .Y(\$0\Word0[15:0] )
  );
  \$not  #(
    .A_SIGNED(32'b00000000000000000000000000000000),
    .A_WIDTH(32'b00000000000000000000000000000001),
    .Y_WIDTH(32'b00000000000000000000000000000001)
  ) \$procmux$923  (
    .A(\$auto$opt_reduce.cc:126:opt_mux$2463 ),
    .Y(CpuIntr_o)
  );
  \$and  #(
    .A_SIGNED(32'b00000000000000000000000000000000),
    .A_WIDTH(32'b00000000000000000000000000000001),
    .B_SIGNED(32'b00000000000000000000000000000000),
    .B_WIDTH(32'b00000000000000000000000000000001),
    .Y_WIDTH(32'b00000000000000000000000000000001)
  ) \$procmux$943  (
    .A(\$procmux$1004_CMP ),
    .B(\$2\MeasureFSM_Start_o[0:0] ),
    .Y(MeasureFSM_Start_o)
  );
  \$and  #(
    .A_SIGNED(32'b00000000000000000000000000000000),
    .A_WIDTH(32'b00000000000000000000000000000001),
    .B_SIGNED(32'b00000000000000000000000000000000),
    .B_WIDTH(32'b00000000000000000000000000000001),
    .Y_WIDTH(32'b00000000000000000000000000000001)
  ) \$procmux$953  (
    .A(\$procmux$1129_CMP ),
    .B(\$2\SensorFSM_StoreNewValue[0:0] ),
    .Y(SensorFSM_StoreNewValue)
  );
  \$pmux  #(
    .S_WIDTH(32'b00000000000000000000000000000011),
    .WIDTH(32'b00000000000000000000000000000001)
  ) \$procmux$983  (
    .A(1'b0),
    .B({ Enable_i, 1'b1, \$2\SensorFSM_StoreNewValue[0:0]  }),
    .S({ \$procmux$1007_CMP , \$procmux$1004_CMP , \$procmux$1129_CMP  }),
    .Y(SensorFSM_TimerEnable)
  );
  \$pmux  #(
    .S_WIDTH(32'b00000000000000000000000000000011),
    .WIDTH(32'b00000000000000000000000000000001)
  ) \$procmux$998  (
    .A(1'b1),
    .B({ \$2\SensorFSM_TimerPreset[0:0] , 1'b0, \$3\SensorFSM_TimerPreset[0:0]  }),
    .S({ \$procmux$1007_CMP , \$procmux$1004_CMP , \$procmux$1129_CMP  }),
    .Y(SensorFSM_TimerPreset)
  );
  (* src = "../../verilog/sensorfsm.v:144" *)
  \$sub  #(
    .A_SIGNED(32'b00000000000000000000000000000000),
    .A_WIDTH(32'b00000000000000000000000000010000),
    .B_SIGNED(32'b00000000000000000000000000000000),
    .B_WIDTH(32'b00000000000000000000000000000001),
    .Y_WIDTH(32'b00000000000000000000000000010000)
  ) \$sub$../../verilog/sensorfsm.v:144$59  (
    .A(SensorFSM_Timer),
    .B(1'b1),
    .Y(\$sub$../../verilog/sensorfsm.v:144$59_Y )
  );
  (* src = "../../verilog/sensorfsm.v:170" *)
  \$sub  #(
    .A_SIGNED(32'b00000000000000000000000000000000),
    .A_WIDTH(32'b00000000000000000000000000010001),
    .B_SIGNED(32'b00000000000000000000000000000000),
    .B_WIDTH(32'b00000000000000000000000000010001),
    .Y_WIDTH(32'b00000000000000000000000000010001)
  ) \$sub$../../verilog/sensorfsm.v:170$64  (
    .A({ 1'b0, MeasureFSM_Byte1_i, MeasureFSM_Byte0_i }),
    .B({ 1'b0, Word0 }),
    .Y(DiffAB)
  );
  (* src = "../../verilog/sensorfsm.v:171" *)
  \$sub  #(
    .A_SIGNED(32'b00000000000000000000000000000000),
    .A_WIDTH(32'b00000000000000000000000000010000),
    .B_SIGNED(32'b00000000000000000000000000000000),
    .B_WIDTH(32'b00000000000000000000000000010000),
    .Y_WIDTH(32'b00000000000000000000000000010000)
  ) \$sub$../../verilog/sensorfsm.v:171$65  (
    .A(Word0),
    .B({ MeasureFSM_Byte1_i, MeasureFSM_Byte0_i }),
    .Y(DiffBA)
  );
  (* src = "../../verilog/sensorfsm.v:172" *)
  \$mux  #(
    .WIDTH(32'b00000000000000000000000000010000)
  ) \$ternary$../../verilog/sensorfsm.v:172$66  (
    .A(DiffAB[15:0]),
    .B(DiffBA),
    .S(DiffAB[16]),
    .Y(AbsDiffResult)
  );
  assign SensorValue = { MeasureFSM_Byte1_i, MeasureFSM_Byte0_i };
  assign SensorValue_o = Word0;
endmodule

(* src = "../../verilog/adt7310.v:1" *)
module ADT7310(Reset_n_i, Clk_i, Enable_i, CpuIntr_o, ADT7310CS_n_o, SPI_Data_i, SPI_Write_o, SPI_ReadNext_o, SPI_Data_o, SPI_FIFOFull_i, SPI_FIFOEmpty_i, SPI_Transmission_i, SPICounterPresetH_i, SPICounterPresetL_i, Threshold_i, PeriodCounterPreset_i, SensorValue_o, SPI_CPOL_o, SPI_CPHA_o, SPI_LSBFE_o);
  (* intersynth_conntype = "Bit" *)
  (* intersynth_port = "Outputs_o" *)
  (* src = "../../verilog/adt7310.v:11" *)
  output ADT7310CS_n_o;
  (* intersynth_port = "Clk_i" *)
  (* src = "../../verilog/adt7310.v:5" *)
  input Clk_i;
  (* intersynth_conntype = "Bit" *)
  (* intersynth_port = "ReconfModuleIRQs_s" *)
  (* src = "../../verilog/adt7310.v:9" *)
  output CpuIntr_o;
  (* intersynth_conntype = "Bit" *)
  (* intersynth_port = "ReconfModuleIn_s" *)
  (* src = "../../verilog/adt7310.v:7" *)
  input Enable_i;
  (* intersynth_conntype = "Word" *)
  (* intersynth_param = "PeriodCounterPreset_i" *)
  (* src = "../../verilog/adt7310.v:33" *)
  input [15:0] PeriodCounterPreset_i;
  (* intersynth_port = "Reset_n_i" *)
  (* src = "../../verilog/adt7310.v:3" *)
  input Reset_n_i;
  (* intersynth_conntype = "Word" *)
  (* intersynth_param = "SPICounterPresetH_i" *)
  (* src = "../../verilog/adt7310.v:27" *)
  input [15:0] SPICounterPresetH_i;
  (* intersynth_conntype = "Word" *)
  (* intersynth_param = "SPICounterPresetL_i" *)
  (* src = "../../verilog/adt7310.v:29" *)
  input [15:0] SPICounterPresetL_i;
  (* keep = 1 *)
  (* src = "../../verilog/adt7310.v:56" *)
  wire [7:0] SPIFSM_Byte0_s;
  (* keep = 1 *)
  (* src = "../../verilog/adt7310.v:58" *)
  wire [7:0] SPIFSM_Byte1_s;
  (* keep = 1 *)
  (* src = "../../verilog/adt7310.v:54" *)
  wire SPIFSM_Done_s;
  (* keep = 1 *)
  (* src = "../../verilog/adt7310.v:52" *)
  wire SPIFSM_Start_s;
  (* intersynth_conntype = "Bit" *)
  (* intersynth_port = "SPI_CPHA" *)
  (* src = "../../verilog/adt7310.v:39" *)
  output SPI_CPHA_o;
  (* intersynth_conntype = "Bit" *)
  (* intersynth_port = "SPI_CPOL" *)
  (* src = "../../verilog/adt7310.v:37" *)
  output SPI_CPOL_o;
  (* intersynth_conntype = "Byte" *)
  (* intersynth_port = "SPI_DataOut" *)
  (* src = "../../verilog/adt7310.v:13" *)
  input [7:0] SPI_Data_i;
  (* intersynth_conntype = "Byte" *)
  (* intersynth_port = "SPI_DataIn" *)
  (* src = "../../verilog/adt7310.v:19" *)
  output [7:0] SPI_Data_o;
  (* intersynth_conntype = "Bit" *)
  (* intersynth_port = "SPI_FIFOEmpty" *)
  (* src = "../../verilog/adt7310.v:23" *)
  input SPI_FIFOEmpty_i;
  (* intersynth_conntype = "Bit" *)
  (* intersynth_port = "SPI_FIFOFull" *)
  (* src = "../../verilog/adt7310.v:21" *)
  input SPI_FIFOFull_i;
  (* intersynth_conntype = "Bit" *)
  (* intersynth_port = "SPI_LSBFE" *)
  (* src = "../../verilog/adt7310.v:41" *)
  output SPI_LSBFE_o;
  (* intersynth_conntype = "Bit" *)
  (* intersynth_port = "SPI_ReadNext" *)
  (* src = "../../verilog/adt7310.v:17" *)
  output SPI_ReadNext_o;
  (* intersynth_conntype = "Bit" *)
  (* intersynth_port = "SPI_Transmission" *)
  (* src = "../../verilog/adt7310.v:25" *)
  input SPI_Transmission_i;
  (* intersynth_conntype = "Bit" *)
  (* intersynth_port = "SPI_Write" *)
  (* src = "../../verilog/adt7310.v:15" *)
  output SPI_Write_o;
  (* intersynth_conntype = "Word" *)
  (* intersynth_param = "SensorValue_o" *)
  (* src = "../../verilog/adt7310.v:35" *)
  output [15:0] SensorValue_o;
  (* intersynth_conntype = "Word" *)
  (* intersynth_param = "Threshold_i" *)
  (* src = "../../verilog/adt7310.v:31" *)
  input [15:0] Threshold_i;
  (* src = "../../verilog/adt7310.v:60" *)
  \$paramod\SPIFSM\SPPRWidth=4\SPRWidth=4\DataWidth=8  SPIFSM_1 (
    .ADT7310CS_n_o(ADT7310CS_n_o),
    .Byte0_o(SPIFSM_Byte0_s),
    .Byte1_o(SPIFSM_Byte1_s),
    .Clk_i(Clk_i),
    .Done_o(SPIFSM_Done_s),
    .ParamCounterPreset_i({ SPICounterPresetH_i, SPICounterPresetL_i }),
    .Reset_n_i(Reset_n_i),
    .SPI_Data_i(SPI_Data_i),
    .SPI_Data_o(SPI_Data_o),
    .SPI_FIFOEmpty_i(SPI_FIFOEmpty_i),
    .SPI_FIFOFull_i(SPI_FIFOFull_i),
    .SPI_ReadNext_o(SPI_ReadNext_o),
    .SPI_Transmission_i(SPI_Transmission_i),
    .SPI_Write_o(SPI_Write_o),
    .Start_i(SPIFSM_Start_s)
  );
  (* src = "../../verilog/adt7310.v:86" *)
  \$paramod\SensorFSM\DataWidth=8  SensorFSM_1 (
    .Clk_i(Clk_i),
    .CpuIntr_o(CpuIntr_o),
    .Enable_i(Enable_i),
    .MeasureFSM_Byte0_i(SPIFSM_Byte0_s),
    .MeasureFSM_Byte1_i(SPIFSM_Byte1_s),
    .MeasureFSM_Done_i(SPIFSM_Done_s),
    .MeasureFSM_Start_o(SPIFSM_Start_s),
    .ParamCounterPreset_i(PeriodCounterPreset_i),
    .ParamThreshold_i(Threshold_i),
    .Reset_n_i(Reset_n_i),
    .SensorValue_o(SensorValue_o)
  );
  assign SPI_CPHA_o = 1'b1;
  assign SPI_CPOL_o = 1'b1;
  assign SPI_LSBFE_o = 1'b0;
endmodule
