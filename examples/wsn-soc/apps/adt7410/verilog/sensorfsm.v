`timescale 1ns/1ps

module SensorFSM #(
  parameter DataWidth = 8
) (
  input                        Reset_n_i,
  input                        Clk_i,
  // top level
  input                        Enable_i,
  output reg                   CpuIntr_o,
  output     [2*DataWidth-1:0] SensorValue_o,
  // to/from Measure-FSM
  output reg                   MeasureFSM_Start_o,
  input                        MeasureFSM_Done_i,
  input                        MeasureFSM_Error_i,
  input     [DataWidth-1:0]    MeasureFSM_Byte0_i,
  input     [DataWidth-1:0]    MeasureFSM_Byte1_i,
  // parameters
  input [2*DataWidth-1:0]      ParamThreshold_i,
  input [2*DataWidth-1:0]      ParamCounterPreset_i
);

  // Sensor FSM
  localparam stDisabled   = 3'b000;
  localparam stIdle       = 3'b001;
  localparam stXfer       = 3'b010;
  localparam stNotify     = 3'b011;
  localparam stError      = 3'b100;
  reg  [2:0]             SensorFSM_State;
  reg  [2:0]             SensorFSM_NextState;
  wire                   SensorFSM_TimerOvfl;
  reg                    SensorFSM_TimerPreset;
  reg                    SensorFSM_TimerEnable;
  wire                   SensorFSM_DiffTooLarge;
  reg                    SensorFSM_StoreNewValue;
  /////////////////////////////////////////////////////////////////////////////
  // Word Arithmetic
  // interconnecting signals
  wire [2*DataWidth-1:0] SensorValue;
  reg  [2*DataWidth-1:0] Word0;
  wire [2*DataWidth-1:0] AbsDiffResult;

  /////////////////////////////////////////////////////////////////////////////
  // FSM //////////////////////////////////////////////////////////////////////
  /////////////////////////////////////////////////////////////////////////////

  always @(negedge Reset_n_i or posedge Clk_i)
  begin
    if (!Reset_n_i)
    begin
      SensorFSM_State <= stDisabled;
    end
    else
    begin
      SensorFSM_State <= SensorFSM_NextState;
    end  
  end

  always @(SensorFSM_State, Enable_i, SensorFSM_TimerOvfl, MeasureFSM_Done_i, MeasureFSM_Error_i, SensorFSM_DiffTooLarge)
  begin  // process SensorFSM_CombProc
    SensorFSM_NextState     = SensorFSM_State;
    // control signal default values
    SensorFSM_TimerPreset   = 1'b1;
    SensorFSM_TimerEnable   = 1'b0;
    MeasureFSM_Start_o      = 1'b0;
    SensorFSM_StoreNewValue = 1'b0;
    CpuIntr_o               = 1'b0;
    // next state and output logic
    case (SensorFSM_State)
      stDisabled: begin
        if (Enable_i == 1'b1)
        begin
          SensorFSM_NextState     = stIdle;
          SensorFSM_TimerPreset   = 1'b0;
          SensorFSM_TimerEnable   = 1'b1;  // start timer
        end
      end
      stIdle: begin
        SensorFSM_TimerPreset   = 1'b0;
        SensorFSM_TimerEnable   = 1'b1;  // timer running
        if (Enable_i == 1'b0)
        begin
          SensorFSM_NextState     = stDisabled;
        end
        else
        if (SensorFSM_TimerOvfl == 1'b1)
        begin
          SensorFSM_NextState     = stXfer;
          MeasureFSM_Start_o      = 1'b1;
        end
      end
      stXfer: begin
        if (MeasureFSM_Error_i == 1'b1)
        begin
          // on I2C Error go to state "stError" and notify the CPU
          SensorFSM_NextState     = stError;
          CpuIntr_o               = 1'b1;  // notify CPU
        end
        else if (MeasureFSM_Done_i == 1'b1)
        begin
          if (SensorFSM_DiffTooLarge == 1'b1)
          begin
            SensorFSM_NextState     = stNotify;
            SensorFSM_TimerPreset   = 1'b0;
            SensorFSM_TimerEnable   = 1'b1;  // timer running
            SensorFSM_StoreNewValue = 1'b1;  // store new value
          end
          else
          begin
            SensorFSM_NextState     = stIdle;
          end
        end
      end
      stNotify: begin
        SensorFSM_TimerPreset   = 1'b1;
        SensorFSM_TimerEnable   = 1'b0;  // preset timer
        SensorFSM_NextState     = stIdle;
        CpuIntr_o               = 1'b1;  // notify CPU
      end
      stError: begin
        // stay in this error state until the FSM is disabled
        if (Enable_i == 1'b0)
        begin
          SensorFSM_NextState     = stDisabled;
        end
      end
      default: begin
      end
    endcase
  end 

  /////////////////////////////////////////////////////////////////////////////
  // Word Arithmetic //////////////////////////////////////////////////////////
  /////////////////////////////////////////////////////////////////////////////

  reg [2*DataWidth-1:0] SensorFSM_Timer;
  
  always @(negedge Reset_n_i or posedge Clk_i)
  begin
    if (!Reset_n_i)
    begin
      SensorFSM_Timer <= 16'd0;
    end
    else
    begin
      if (SensorFSM_TimerPreset)
      begin
        SensorFSM_Timer <= ParamCounterPreset_i;
      end
      else if (SensorFSM_TimerEnable)
      begin
        SensorFSM_Timer <= SensorFSM_Timer - 1'b1;
      end
    end  
  end

  assign SensorFSM_TimerOvfl = (SensorFSM_Timer == 0) ? 1'b1 : 1'b0;

  assign SensorValue = {MeasureFSM_Byte1_i, MeasureFSM_Byte0_i};

  always @(negedge Reset_n_i or posedge Clk_i)
  begin
    if (!Reset_n_i)
    begin
      Word0 <= 16'd0;
    end
    else
    begin
      if (SensorFSM_StoreNewValue)
      begin
        Word0 <= SensorValue;
      end
    end  
  end

  wire [2*DataWidth   : 0] DiffAB;
  wire [2*DataWidth-1 : 0] DiffBA;
  assign DiffAB = {1'b0, SensorValue} - {1'b0, Word0};
  assign DiffBA =        Word0        -        SensorValue;
  assign AbsDiffResult = DiffAB[2*DataWidth] ? DiffBA : DiffAB[2*DataWidth-1 : 0];

  assign SensorFSM_DiffTooLarge = (AbsDiffResult > ParamThreshold_i) ? 1'b1 : 1'b0;

  assign SensorValue_o = Word0;

endmodule // SensorFSM
