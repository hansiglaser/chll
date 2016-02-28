module ADT7310P32LS16L (
  (* intersynth_port="Reset_n_i" *) 
  input Reset_n_i,
  (* intersynth_port="Clk_i" *) 
  input Clk_i,
  (* intersynth_port="ReconfModuleIn_s", intersynth_conntype="Bit" *) 
  input Enable_i,
  (* intersynth_port="ReconfModuleIRQs_s", intersynth_conntype="Bit" *) 
  output CpuIntr_o,
  (* intersynth_port="Outputs_o", intersynth_conntype="Bit" *) 
  output ADT7310CS_n_o,
  (* intersynth_port="SPI_DataOut", intersynth_conntype="Byte" *) 
  input[7:0] SPI_Data_i,
  (* intersynth_port="SPI_Write", intersynth_conntype="Bit" *) 
  output SPI_Write_o,
  (* intersynth_port="SPI_ReadNext", intersynth_conntype="Bit" *) 
  output SPI_ReadNext_o,
  (* intersynth_port="SPI_DataIn", intersynth_conntype="Byte" *) 
  output[7:0] SPI_Data_o,
  (* intersynth_port="SPI_FIFOFull", intersynth_conntype="Bit" *) 
  input SPI_FIFOFull_i,
  (* intersynth_port="SPI_FIFOEmpty", intersynth_conntype="Bit" *) 
  input SPI_FIFOEmpty_i,
  (* intersynth_port="SPI_Transmission", intersynth_conntype="Bit" *) 
  input SPI_Transmission_i,
  (* intersynth_param="SPICounterPreset_i", intersynth_conntype="Word" *) 
  input[15:0] SPICounterPreset_i,
  (* intersynth_param="Threshold_i", intersynth_conntype="Word" *) 
  input[15:0] Threshold_i,
  (* intersynth_param="PeriodCounterPresetH_i", intersynth_conntype="Word" *)
  input[15:0] PeriodCounterPresetH_i,
  (* intersynth_param="PeriodCounterPresetL_i", intersynth_conntype="Word" *)
  input[15:0] PeriodCounterPresetL_i,
  (* intersynth_param="SensorValue_o", intersynth_conntype="Word" *) 
  output[15:0] SensorValue_o,
  (* intersynth_port="SPI_CPOL", intersynth_conntype="Bit" *) 
  output SPI_CPOL_o,
  (* intersynth_port="SPI_CPHA", intersynth_conntype="Bit" *) 
  output SPI_CPHA_o,
  (* intersynth_port="SPI_LSBFE", intersynth_conntype="Bit" *) 
  output SPI_LSBFE_o
);

  /* constant value for dynamic signal */ 
  assign SPI_CPOL_o = 1'b1;
  /* constant value for dynamic signal */ 
  assign SPI_CPHA_o = 1'b1;
  /* constant value for dynamic signal */ 
  assign SPI_LSBFE_o = 1'b0;

  (* keep *)
  wire SPIFSM_Start_s;
  (* keep *)
  wire SPIFSM_Done_s;
  (* keep *)
  wire [7:0] SPIFSM_Byte0_s;
  (* keep *)
  wire [7:0] SPIFSM_Byte1_s;

  SPIFSM #(
    .SPPRWidth (4),
    .SPRWidth  (4),
    .DataWidth (8)
  ) SPIFSM_1 (
    .Reset_n_i           (Reset_n_i),
    .Clk_i               (Clk_i),
    // FSM control
    .Start_i             (SPIFSM_Start_s),
    .Done_o              (SPIFSM_Done_s),
    .Byte0_o             (SPIFSM_Byte0_s),
    .Byte1_o             (SPIFSM_Byte1_s),
    // to/from SPI_Master
    .SPI_Transmission_i  (SPI_Transmission_i),
    .SPI_Write_o         (SPI_Write_o),
    .SPI_ReadNext_o      (SPI_ReadNext_o),
    .SPI_Data_o          (SPI_Data_o),
    .SPI_Data_i          (SPI_Data_i),
    .SPI_FIFOFull_i      (SPI_FIFOFull_i),
    .SPI_FIFOEmpty_i     (SPI_FIFOEmpty_i),
    // to ADT7310
    .ADT7310CS_n_o       (ADT7310CS_n_o),
    // parameters
    .ParamCounterPreset_i(SPICounterPreset_i)
  );

  SensorFSM #(
    .DataWidth           (8)
  ) SensorFSM_1 (
    .Reset_n_i           (Reset_n_i),
    .Clk_i               (Clk_i),
    .Enable_i            (Enable_i),
    .CpuIntr_o           (CpuIntr_o),
    .SensorValue_o       (SensorValue_o),
    .MeasureFSM_Start_o  (SPIFSM_Start_s),
    .MeasureFSM_Done_i   (SPIFSM_Done_s),
    .MeasureFSM_Byte0_i  (SPIFSM_Byte0_s),
    .MeasureFSM_Byte1_i  (SPIFSM_Byte1_s),
    // parameters
    .ParamThreshold_i    (Threshold_i),
    .ParamCounterPreset_i({PeriodCounterPresetH_i, PeriodCounterPresetL_i})
  );

endmodule
