module SensorFSM (
  input Reset_n_i,
  input Clk_i,
  input In0_i,
  input In1_i,
  input In2_i,
  input In3_i,
  input In4_i,
  input In5_i,
  input In6_i,
  input In7_i,
  input In8_i,
  input In9_i,
  output Out0_o,
  output Out1_o,
  output Out2_o,
  output Out3_o,
  output Out4_o,
  output Out5_o,
  output Out6_o,
  output Out7_o,
  output Out8_o,
  output Out9_o,
  input CfgMode_i,
  input CfgClk_i,
  input CfgShift_i,
  input CfgDataIn_i,
  output CfgDataOut_o
);

  wire [9:0] Input_s;
  wire [9:0] Output_s;
  wire ScanEnable_s;
  wire ScanClk_s;
  wire ScanDataIn_s;
  wire ScanDataOut_s;


  TRFSM #(
    .InputWidth(10),
    .OutputWidth(10),
    .StateWidth(5),
    .UseResetRow(0),
    .NumRows0(5),
    .NumRows1(5),
    .NumRows2(5),
    .NumRows3(5),
    .NumRows4(5),
    .NumRows5(0),
    .NumRows6(0),
    .NumRows7(0),
    .NumRows8(0),
    .NumRows9(0)
  ) TRFSM_1 (
    .Reset_n_i(Reset_n_i),
    .Clk_i(Clk_i),
    .Input_i(Input_s),
    .Output_o(Output_s),
    .CfgMode_i(CfgMode_i),
    .CfgClk_i(CfgClk_i),
    .CfgShift_i(CfgShift_i),
    .CfgDataIn_i(CfgDataIn_i),
    .CfgDataOut_o(CfgDataOut_o),
    .ScanEnable_i(ScanEnable_s),
    .ScanClk_i(ScanClk_s),
    .ScanDataIn_i(ScanDataIn_s),
    .ScanDataOut_o(ScanDataOut_s)
  );
  assign Input_s = { In9_i, In8_i, In7_i, In6_i, In5_i, In4_i, In3_i, In2_i, In1_i, In0_i };
  assign Out0_o = Output_s[0];
  assign Out1_o = Output_s[1];
  assign Out2_o = Output_s[2];
  assign Out3_o = Output_s[3];
  assign Out4_o = Output_s[4];
  assign Out5_o = Output_s[5];
  assign Out6_o = Output_s[6];
  assign Out7_o = Output_s[7];
  assign Out8_o = Output_s[8];
  assign Out9_o = Output_s[9];
  assign ScanEnable_s = 1'b0;
  assign ScanClk_s = 1'b0;
  assign ScanDataIn_s = 1'b0;

endmodule
