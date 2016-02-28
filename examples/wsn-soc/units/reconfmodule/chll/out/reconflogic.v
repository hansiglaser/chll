// Automatically generated: write_netlist -reconfmodule -verilog -module reconflogic.v

module MyReconfigLogic (
  input Reset_n_i,
  input Clk_i,
  input AdcConvComplete_i,
  output AdcDoConvert_o,
  input[9:0] AdcValue_i,
  input I2C_Busy_i,
  output[7:0] I2C_DataIn_o,
  input[7:0] I2C_DataOut_i,
  output[15:0] I2C_Divider800_o,
  output I2C_ErrAckParam_o,
  input I2C_Error_i,
  output I2C_F100_400_n_o,
  input I2C_FIFOEmpty_i,
  input I2C_FIFOFull_i,
  output I2C_FIFOReadNext_o,
  output I2C_FIFOWrite_o,
  output[3:0] I2C_ReadCount_o,
  output I2C_ReceiveSend_n_o,
  output I2C_StartProcess_o,
  input[7:0] Inputs_i,
  output[7:0] Outputs_o,
  output[4:0] ReconfModuleIRQs_o,
  output SPI_CPHA_o,
  output SPI_CPOL_o,
  output[7:0] SPI_DataIn_o,
  input[7:0] SPI_DataOut_i,
  input SPI_FIFOEmpty_i,
  input SPI_FIFOFull_i,
  output SPI_LSBFE_o,
  output SPI_ReadNext_o,
  output[7:0] SPI_SPPR_SPR_o,
  input SPI_Transmission_i,
  output SPI_Write_o,
  input[7:0] ReconfModuleIn_i,
  output[7:0] ReconfModuleOut_o,
  input[7:0] I2C_Errors_i,
  input[13:0] PerAddr_i,
  input[15:0] PerDIn_i,
  input[1:0] PerWr_i,
  input PerEn_i,
  output[15:0] CfgIntfDOut_o,
  output[15:0] ParamIntfDOut_o
);

  wire [1281:0] BitData_s;
  wire [15:0] AdcValue_s;
  wire [7:0] I2C_ReadCount_s;
  wire [79:0] ParamIn_Word_s;
  wire [15:0] ParamIn_Word_0_s;
  wire [15:0] ParamIn_Word_1_s;
  wire [15:0] ParamIn_Word_2_s;
  wire [15:0] ParamIn_Word_3_s;
  wire [15:0] ParamIn_Word_4_s;
  wire [31:0] ParamOut_Word_s;
  wire [15:0] ParamOut_Word_0_s;
  wire [15:0] ParamOut_Word_1_s;
  wire [3:0] CfgClk_s;
  wire CfgMode_s;
  wire [3:0] CfgShift_s;
  wire CfgDataOut_s;
  wire [3:0] CfgDataIn_s;
  wire [8:0] CfgReconfSignals_s;
  wire [2:0] ParamWrAddr_s;
  wire [15:0] ParamWrData_s;
  wire ParamWr_s;
  wire [1:0] ParamRdAddr_s;
  wire [15:0] ParamRdData_s;

TODO: implement
  wire Params_s;
  wire [0:0] I2C_ErrAckParam_s;
  wire ParamI2C_Divider800Enable_s;
  wire ParamI2C_ErrAckParamEnable_s;
  wire ParamParamIn_Word_0Enable_s;
  wire ParamParamIn_Word_1Enable_s;
  wire ParamParamIn_Word_2Enable_s;
  wire ParamParamIn_Word_3Enable_s;
  wire ParamParamIn_Word_4Enable_s;


  // Configuration Interface
  CfgIntf #(
    .BaseAddr('h0180),
    .NumCfgs(4)
  ) CfgIntf_0 (
    .Reset_n_i(Reset_n_i),
    .Clk_i(Clk_i),
    .PerAddr_i(PerAddr_i),
    .PerDIn_i(PerDIn_i),
    .PerDOut_o(CfgIntfDOut_o),
    .PerWr_i(PerWr_i),
    .PerEn_i(PerEn_i),
    .CfgClk_o(CfgClk_s),
    .CfgMode_o(CfgMode_s),
    .CfgShift_o(CfgShift_s),
    .CfgDataOut_o(CfgDataOut_s),
    .CfgDataIn_i(CfgDataIn_s)
  );

  // Parameterization Interface: 7 write addresses, 3 read addresses
  ParamIntf #(
    .BaseAddr('h0188),
    .WrAddrWidth(3),
    .RdAddrWidth(2)
  ) ParamIntf_0 (
    .Reset_n_i(Reset_n_i),
    .Clk_i(Clk_i),
    .PerAddr_i(PerAddr_i),
    .PerDIn_i(PerDIn_i),
    .PerDOut_o(ParamIntfDOut_o),
    .PerWr_i(PerWr_i),
    .PerEn_i(PerEn_i),
    .ParamWrAddr_o(ParamWrAddr_s),
    .ParamWrData_o(ParamWrData_s),
    .ParamWr_o(ParamWr_s),
    .ParamRdAddr_o(ParamRdAddr_s),
    .ParamRdData_i(ParamRdData_s)
  );

  MyInterSynthModule MyInterSynthModule_0 (
    .bitdata(BitData_s),
    .AdcConvComplete_i(AdcConvComplete_i),
    .AdcDoConvert_o(AdcDoConvert_o),
    .AdcValue_i(AdcValue_s),
    .I2C_Busy_i(I2C_Busy_i),
    .I2C_DataIn_o(I2C_DataIn_o),
    .I2C_DataOut_i(I2C_DataOut_i),
    .I2C_Error_i(I2C_Error_i),
    .I2C_FIFOEmpty_i(I2C_FIFOEmpty_i),
    .I2C_FIFOFull_i(I2C_FIFOFull_i),
    .I2C_FIFOReadNext_o(I2C_FIFOReadNext_o),
    .I2C_FIFOWrite_o(I2C_FIFOWrite_o),
    .I2C_ReadCount_o(I2C_ReadCount_s),
    .I2C_ReceiveSend_n_o(I2C_ReceiveSend_n_o),
    .I2C_StartProcess_o(I2C_StartProcess_o),
    .Inputs_i_0(Inputs_i[0]),
    .Inputs_i_1(Inputs_i[1]),
    .Inputs_i_2(Inputs_i[2]),
    .Inputs_i_3(Inputs_i[3]),
    .Inputs_i_4(Inputs_i[4]),
    .Inputs_i_5(Inputs_i[5]),
    .Inputs_i_6(Inputs_i[6]),
    .Inputs_i_7(Inputs_i[7]),
    .Outputs_o_0(Outputs_o[0]),
    .Outputs_o_1(Outputs_o[1]),
    .Outputs_o_2(Outputs_o[2]),
    .Outputs_o_3(Outputs_o[3]),
    .Outputs_o_4(Outputs_o[4]),
    .Outputs_o_5(Outputs_o[5]),
    .Outputs_o_6(Outputs_o[6]),
    .Outputs_o_7(Outputs_o[7]),
    .ReconfModuleIRQs_o_0(ReconfModuleIRQs_o[0]),
    .ReconfModuleIRQs_o_1(ReconfModuleIRQs_o[1]),
    .ReconfModuleIRQs_o_2(ReconfModuleIRQs_o[2]),
    .ReconfModuleIRQs_o_3(ReconfModuleIRQs_o[3]),
    .ReconfModuleIRQs_o_4(ReconfModuleIRQs_o[4]),
    .SPI_CPHA_o(SPI_CPHA_o),
    .SPI_CPOL_o(SPI_CPOL_o),
    .SPI_DataIn_o(SPI_DataIn_o),
    .SPI_DataOut_i(SPI_DataOut_i),
    .SPI_FIFOEmpty_i(SPI_FIFOEmpty_i),
    .SPI_FIFOFull_i(SPI_FIFOFull_i),
    .SPI_LSBFE_o(SPI_LSBFE_o),
    .SPI_ReadNext_o(SPI_ReadNext_o),
    .SPI_Transmission_i(SPI_Transmission_i),
    .SPI_Write_o(SPI_Write_o),
    .ReconfModuleIn_i_0(ReconfModuleIn_i[0]),
    .ReconfModuleIn_i_1(ReconfModuleIn_i[1]),
    .ReconfModuleIn_i_2(ReconfModuleIn_i[2]),
    .ReconfModuleIn_i_3(ReconfModuleIn_i[3]),
    .ReconfModuleIn_i_4(ReconfModuleIn_i[4]),
    .ReconfModuleIn_i_5(ReconfModuleIn_i[5]),
    .ReconfModuleIn_i_6(ReconfModuleIn_i[6]),
    .ReconfModuleIn_i_7(ReconfModuleIn_i[7]),
    .ReconfModuleOut_o_0(ReconfModuleOut_o[0]),
    .ReconfModuleOut_o_1(ReconfModuleOut_o[1]),
    .ReconfModuleOut_o_2(ReconfModuleOut_o[2]),
    .ReconfModuleOut_o_3(ReconfModuleOut_o[3]),
    .ReconfModuleOut_o_4(ReconfModuleOut_o[4]),
    .ReconfModuleOut_o_5(ReconfModuleOut_o[5]),
    .ReconfModuleOut_o_6(ReconfModuleOut_o[6]),
    .ReconfModuleOut_o_7(ReconfModuleOut_o[7]),
    .Clk_i(Clk_i),
    .Reset_n_i(Reset_n_i),
    .ParamIn_Word_i(ParamIn_Word_s),
    .ParamOut_Word_o(ParamOut_Word_s),
    .CfgMode_i(CfgMode_s),
    .CfgDataIn_i(CfgDataOut_s),
    .CfgClk_TRFSM0_i(CfgClk_s[2]),
    .CfgShift_TRFSM0_i(CfgShift_s[2]),
    .CfgDataOut_TRFSM0_o(CfgDataIn_s[2]),
    .CfgClk_TRFSM1_i(CfgClk_s[3]),
    .CfgShift_TRFSM1_i(CfgShift_s[3]),
    .CfgDataOut_TRFSM1_o(CfgDataIn_s[3])
  );
  assign AdcValue_s = { 6'b000000, AdcValue_i };
  assign I2C_ReadCount_o = I2C_ReadCount_s[3:0];
  assign ParamIn_Word_s = { ParamIn_Word_4_s, ParamIn_Word_3_s, ParamIn_Word_2_s, ParamIn_Word_1_s, ParamIn_Word_0_s };
  assign ParamOut_Word_0_s = ParamOut_Word_s[15:0];
  assign ParamOut_Word_1_s = ParamOut_Word_s[31:16];

  ConfigRegister #(
    .Width(9)
  ) CfgRegReconfSignals (
    .Reset_n_i(Reset_n_i),
    .Output_o(CfgReconfSignals_s),
    .CfgMode_i(CfgMode_s),
    .CfgClk_i(CfgClk_s[0]),
    .CfgShift_i(CfgShift_s[0]),
    .CfgDataIn_i(CfgDataOut_s),
    .CfgDataOut_o(CfgDataIn_s[0])
  );

  ConfigRegister #(
    .Width(1282)
  ) CfgRegbitdata (
    .Reset_n_i(Reset_n_i),
    .Output_o(BitData_s),
    .CfgMode_i(CfgMode_s),
    .CfgClk_i(CfgClk_s[1]),
    .CfgShift_i(CfgShift_s[1]),
    .CfgDataIn_i(CfgDataOut_s),
    .CfgDataOut_o(CfgDataIn_s[1])
  );
  assign I2C_F100_400_n_o = CfgReconfSignals_s[0];
  assign SPI_SPPR_SPR_o = CfgReconfSignals_s[8:1];
  /* Param read address decoder
  Synthesis: Accept undefined behavior if ParamRdAddr_s >= NumParams and
    hope that the synthesis optimizes the MUX
  Simulation: ModelSim complains "Fatal: (vsim-3421) Value x is out of range
    0 to n.", even during param write cycles, because ParamRdAddr has the
    source as ParamWrAddr. Use the parameter "-noindexcheck" during
    compilation ("vcom"). Simulation works fine then, but ModelSim generates
    numerous "INTERNAL ERROR"s to stdout, which seem harmless. */ 
  assign ParamRdData_s = Params_s[to_integer(unsigned(ParamRdAddr_s))];

  ParamOutReg #(
    .Width(16)
  ) ParamOutReg_I2C_Divider800 (
    .Reset_n_i(Reset_n_i),
    .Clk_i(Clk_i),
    .Param_o(I2C_Divider800_o),
    .Enable_i(ParamI2C_Divider800Enable_s),
    .ParamWrData_i(ParamWrData_s)
  );

  ParamOutReg #(
    .Width(1)
  ) ParamOutReg_I2C_ErrAckParam (
    .Reset_n_i(Reset_n_i),
    .Clk_i(Clk_i),
    .Param_o(I2C_ErrAckParam_s),
    .Enable_i(ParamI2C_ErrAckParamEnable_s),
    .ParamWrData_i(ParamWrData_s[0:0])
  );

  ParamOutReg #(
    .Width(16)
  ) ParamOutReg_ParamIn_Word_0 (
    .Reset_n_i(Reset_n_i),
    .Clk_i(Clk_i),
    .Param_o(ParamIn_Word_0_s),
    .Enable_i(ParamParamIn_Word_0Enable_s),
    .ParamWrData_i(ParamWrData_s)
  );

  ParamOutReg #(
    .Width(16)
  ) ParamOutReg_ParamIn_Word_1 (
    .Reset_n_i(Reset_n_i),
    .Clk_i(Clk_i),
    .Param_o(ParamIn_Word_1_s),
    .Enable_i(ParamParamIn_Word_1Enable_s),
    .ParamWrData_i(ParamWrData_s)
  );

  ParamOutReg #(
    .Width(16)
  ) ParamOutReg_ParamIn_Word_2 (
    .Reset_n_i(Reset_n_i),
    .Clk_i(Clk_i),
    .Param_o(ParamIn_Word_2_s),
    .Enable_i(ParamParamIn_Word_2Enable_s),
    .ParamWrData_i(ParamWrData_s)
  );

  ParamOutReg #(
    .Width(16)
  ) ParamOutReg_ParamIn_Word_3 (
    .Reset_n_i(Reset_n_i),
    .Clk_i(Clk_i),
    .Param_o(ParamIn_Word_3_s),
    .Enable_i(ParamParamIn_Word_3Enable_s),
    .ParamWrData_i(ParamWrData_s)
  );

  ParamOutReg #(
    .Width(16)
  ) ParamOutReg_ParamIn_Word_4 (
    .Reset_n_i(Reset_n_i),
    .Clk_i(Clk_i),
    .Param_o(ParamIn_Word_4_s),
    .Enable_i(ParamParamIn_Word_4Enable_s),
    .ParamWrData_i(ParamWrData_s)
  );
  assign I2C_ErrAckParam_o = I2C_ErrAckParam_s[0];
  /* Address $00 */ 
  assign Params_s[0] = { 8'b00000000, I2C_Errors_i };
  /* Address $01 */ 
  assign Params_s[1] = ParamOut_Word_0_s;
  /* Address $02 */ 
  assign Params_s[2] = ParamOut_Word_1_s;
  /* Address $00 */ 
  assign ParamI2C_Divider800Enable_s = TODO: implement;
  /* Address $01 */ 
  assign ParamI2C_ErrAckParamEnable_s = TODO: implement;
  /* Address $02 */ 
  assign ParamParamIn_Word_0Enable_s = TODO: implement;
  /* Address $03 */ 
  assign ParamParamIn_Word_1Enable_s = TODO: implement;
  /* Address $04 */ 
  assign ParamParamIn_Word_2Enable_s = TODO: implement;
  /* Address $05 */ 
  assign ParamParamIn_Word_3Enable_s = TODO: implement;
  /* Address $06 */ 
  assign ParamParamIn_Word_4Enable_s = TODO: implement;

endmodule

