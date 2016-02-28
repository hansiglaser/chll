module TMP421 (
  (* intersynth_port = "Reset_n_i" *)
  input Reset_n_i,
  (* intersynth_port = "Clk_i" *)
  input Clk_i,
  (* intersynth_port = "ReconfModuleIn_s", intersynth_conntype = "Bit" *)
  input Enable_i,
  (* intersynth_port = "ReconfModuleIRQs_s", intersynth_conntype = "Bit" *)
  output CpuIntr_o,
  (* intersynth_port = "I2C_ReceiveSend_n", intersynth_conntype = "Bit" *)
  output I2C_ReceiveSend_n_o,
  (* intersynth_port = "I2C_ReadCount", intersynth_conntype = "Byte" *)
  output[7:0] I2C_ReadCount_o,
  (* intersynth_port = "I2C_StartProcess", intersynth_conntype = "Bit" *)
  output I2C_StartProcess_o,
  (* intersynth_port = "I2C_Busy", intersynth_conntype = "Bit" *)
  input I2C_Busy_i,
  (* intersynth_port = "I2C_FIFOReadNext", intersynth_conntype = "Bit" *)
  output I2C_FIFOReadNext_o,
  (* intersynth_port = "I2C_FIFOWrite", intersynth_conntype = "Bit" *)
  output I2C_FIFOWrite_o,
  (* intersynth_port = "I2C_DataIn", intersynth_conntype = "Byte" *)
  output[7:0] I2C_Data_o,
  (* intersynth_port = "I2C_DataOut", intersynth_conntype = "Byte" *)
  input[7:0] I2C_Data_i,
  (* intersynth_port = "I2C_Error", intersynth_conntype = "Bit" *)
  input I2C_Error_i,
  (* intersynth_param = "PeriodCounterPresetH_i", intersynth_conntype = "Word" *)
  input[15:0] PeriodCounterPresetH_i,
  (* intersynth_param = "PeriodCounterPresetL_i", intersynth_conntype = "Word" *)
  input[15:0] PeriodCounterPresetL_i,
  (* intersynth_param = "SensorValueL_o", intersynth_conntype = "Word" *)
  output[15:0] SensorValueL_o,
  (* intersynth_param = "SensorValueR_o", intersynth_conntype = "Word" *)
  output[15:0] SensorValueR_o
);

  wire       I2CFSM_QueryLocal_s;
  wire       I2CFSM_QueryRemote_s;
  wire       I2CFSM_Done_s;
  wire       I2CFSM_Error_s;
  wire [7:0] I2CFSM_Byte0_s;
  wire [7:0] I2CFSM_Byte1_s;

  I2CFSM I2CFSM_1 (
    .Reset_n_i              (Reset_n_i),
    .Clk_i                  (Clk_i),
    // FSM control
    .QueryLocal_i           (I2CFSM_QueryLocal_s),
    .QueryRemote_i          (I2CFSM_QueryRemote_s),
    .Done_o                 (I2CFSM_Done_s),
    .Error_o                (I2CFSM_Error_s),
    .Byte0_o                (I2CFSM_Byte0_s),
    .Byte1_o                (I2CFSM_Byte1_s),
    // to/from I2C_Master 
    // I2C control
    .I2C_ReceiveSend_n_o    (I2C_ReceiveSend_n_o),
    .I2C_ReadCount_o        (I2C_ReadCount_o),
    .I2C_StartProcess_o     (I2C_StartProcess_o),
    .I2C_Busy_i             (I2C_Busy_i),
    // I2C FIFO
    .I2C_FIFOReadNext_o     (I2C_FIFOReadNext_o),
    .I2C_FIFOWrite_o        (I2C_FIFOWrite_o),
    .I2C_Data_o             (I2C_Data_o),
    .I2C_Data_i             (I2C_Data_i),
    // I2C error
    .I2C_Error_i            (I2C_Error_i)
  );

  SensorFSM #(
    .DataWidth               (8)
  ) SensorFSM_1 (
    .Reset_n_i               (Reset_n_i),
    .Clk_i                   (Clk_i),
    .Enable_i                (Enable_i),
    .CpuIntr_o               (CpuIntr_o),
    .SensorValueL_o          (SensorValueL_o),
    .SensorValueR_o          (SensorValueR_o),
    .MeasureFSM_QueryLocal_o (I2CFSM_QueryLocal_s),
    .MeasureFSM_QueryRemote_o(I2CFSM_QueryRemote_s),
    .MeasureFSM_Done_i       (I2CFSM_Done_s),
    .MeasureFSM_Error_i      (I2CFSM_Error_s),
    .MeasureFSM_Byte0_i      (I2CFSM_Byte0_s),
    .MeasureFSM_Byte1_i      (I2CFSM_Byte1_s),
    // parameters
    .ParamCounterPresetH_i  (PeriodCounterPresetH_i),
    .ParamCounterPresetL_i  (PeriodCounterPresetL_i)
  );

endmodule
