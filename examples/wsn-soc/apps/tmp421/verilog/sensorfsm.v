`timescale 1ns/1ps

module SensorFSM #(
  parameter DataWidth = 8
) (
  input                        Reset_n_i,
  input                        Clk_i,
  // top level
  input                        Enable_i,
  output reg                   CpuIntr_o,
  output     [2*DataWidth-1:0] SensorValueL_o,
  output     [2*DataWidth-1:0] SensorValueR_o,
  // to/from Measure-FSM
  output reg                   MeasureFSM_QueryLocal_o,
  output reg                   MeasureFSM_QueryRemote_o,
  input                        MeasureFSM_Done_i,
  input                        MeasureFSM_Error_i,
  input     [DataWidth-1:0]    MeasureFSM_Byte0_i,
  input     [DataWidth-1:0]    MeasureFSM_Byte1_i,
  // parameters
  input [2*DataWidth-1:0]      ParamCounterPresetH_i,
  input [2*DataWidth-1:0]      ParamCounterPresetL_i
);

  // Sensor FSM
  localparam stDisabled    = 3'd0;
  localparam stIdle        = 3'd1;
  localparam stQueryLocal  = 3'd2;
  localparam stWait1       = 3'd3;
  localparam stQueryRemote = 3'd4;
  localparam stNotify      = 3'd5;
  localparam stError       = 3'd6;
  reg  [2:0]             SensorFSM_State;
  reg  [2:0]             SensorFSM_NextState;
  wire                   SensorFSM_TimerOvfl;
  reg                    SensorFSM_TimerPreset;
  reg                    SensorFSM_TimerEnable;
  reg                    SensorFSM_StoreLocal;
  reg                    SensorFSM_StoreRemote;
  /////////////////////////////////////////////////////////////////////////////
  // Word Arithmetic
  // interconnecting signals
  wire [2*DataWidth-1:0] SensorValue;
  reg  [2*DataWidth-1:0] WordL;
  reg  [2*DataWidth-1:0] WordR;

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

  always @(SensorFSM_State, Enable_i, SensorFSM_TimerOvfl, MeasureFSM_Done_i, MeasureFSM_Error_i)
  begin  // process SensorFSM_CombProc
    SensorFSM_NextState      = SensorFSM_State;
    // control signal default values
    SensorFSM_TimerPreset    = 1'b0;
    SensorFSM_TimerEnable    = 1'b0;
    MeasureFSM_QueryLocal_o  = 1'b0;
    MeasureFSM_QueryRemote_o = 1'b0;
    SensorFSM_StoreLocal     = 1'b0;
    SensorFSM_StoreRemote    = 1'b0;
    CpuIntr_o                = 1'b0;
    // next state and output logic
    case (SensorFSM_State)
      stDisabled: begin
        if (Enable_i == 1'b1)
        begin
          SensorFSM_NextState     = stIdle;
          SensorFSM_TimerPreset   = 1'b1;  // preset timer
          SensorFSM_TimerEnable   = 1'b0;
        end
      end
      stIdle: begin
        if (Enable_i == 1'b0)
        begin
          SensorFSM_NextState     = stDisabled;
        end
        else
        if (SensorFSM_TimerOvfl == 1'b1)
        begin
          SensorFSM_NextState     = stQueryLocal;
          MeasureFSM_QueryLocal_o = 1'b1;
        end
        else
        begin
          SensorFSM_TimerEnable   = 1'b1;  // timer running
        end
      end
      stQueryLocal: begin
        if (MeasureFSM_Error_i == 1'b1)
        begin
          // on I2C Error go to state "stError" and notify the CPU
          SensorFSM_NextState     = stError;
          CpuIntr_o               = 1'b1;  // notify CPU
        end
        else if (MeasureFSM_Done_i == 1'b1)
        begin
          SensorFSM_StoreLocal     = 1'b1;  // store new value
          SensorFSM_NextState      = stWait1;
        end
      end
      stWait1: begin
        // I2CFSM needs at least 1 cycle in between to go from stPause to stIdle
        MeasureFSM_QueryRemote_o = 1'b1;
        SensorFSM_NextState      = stQueryRemote;
      end
      stQueryRemote: begin
        if (MeasureFSM_Error_i == 1'b1)
        begin
          // on I2C Error go to state "stError" and notify the CPU
          SensorFSM_NextState     = stError;
          CpuIntr_o               = 1'b1;  // notify CPU
        end
        else if (MeasureFSM_Done_i == 1'b1)
        begin
          SensorFSM_StoreRemote   = 1'b1;  // store new value
          SensorFSM_NextState     = stNotify;
        end
      end
      stNotify: begin
        SensorFSM_TimerPreset   = 1'b1;  // preset timer
        CpuIntr_o               = 1'b1;  // notify CPU
        SensorFSM_NextState     = stIdle;
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

  reg [31:0] SensorFSM_Timer;
  
  always @(negedge Reset_n_i or posedge Clk_i)
  begin
    if (!Reset_n_i)
    begin
      SensorFSM_Timer <= 32'd0;
    end
    else
    begin
      if (SensorFSM_TimerPreset)
      begin
        SensorFSM_Timer <= {ParamCounterPresetH_i, ParamCounterPresetL_i};
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
      WordL <= 16'd0;
      WordR <= 16'd0;
    end
    else
    begin
      if (SensorFSM_StoreLocal)
      begin
        WordL <= SensorValue;
      end
      if (SensorFSM_StoreRemote)
      begin
        WordR <= SensorValue;
      end
    end  
  end

  assign SensorValueL_o = WordL;
  assign SensorValueR_o = WordR;

endmodule // SensorFSM
