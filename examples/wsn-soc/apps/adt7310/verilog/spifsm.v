`timescale 1ns/1ps

module SPIFSM #(
  parameter SPPRWidth = 4,
  parameter SPRWidth  = 4,
  parameter DataWidth = 8
) (
  input                  Reset_n_i,
  input                  Clk_i,
  // FSM control
  input                  Start_i,
  output reg             Done_o,
  output reg [DataWidth-1:0] Byte0_o,
  output reg [DataWidth-1:0] Byte1_o,
  // to/from SPI_Master
  input                  SPI_Transmission_i,
  output reg                 SPI_Write_o,
  output reg                 SPI_ReadNext_o,
  output reg [DataWidth-1:0] SPI_Data_o,
  input  [DataWidth-1:0] SPI_Data_i,
  input                  SPI_FIFOFull_i,
  input                  SPI_FIFOEmpty_i,
  // to ADT7310
  output reg             ADT7310CS_n_o,
  // parameters
  input [31:0]           ParamCounterPreset_i
);

  // SPI FSM
  localparam stIdle       = 4'b0000;
  localparam stWriteValue = 4'b0001;
  localparam stWaitSent   = 4'b0010;
  localparam stConsume1   = 4'b0011;
  localparam stWait       = 4'b0100;
  localparam stWriteDummy1= 4'b0101;
  localparam stWriteDummy2= 4'b0110;
  localparam stRead1      = 4'b0111;
  localparam stRead2      = 4'b1000;
  localparam stRead3      = 4'b1001;
  localparam stPause      = 4'b1010;
  reg  [3:0]             SPI_FSM_State;
  reg  [3:0]             SPI_FSM_NextState;
  wire                   SPI_FSM_TimerOvfl;
  reg                    SPI_FSM_TimerPreset;
  reg                    SPI_FSM_TimerEnable;
  reg                    SPI_FSM_Wr1;
  reg                    SPI_FSM_Wr0;

  /////////////////////////////////////////////////////////////////////////////
  // FSM //////////////////////////////////////////////////////////////////////
  /////////////////////////////////////////////////////////////////////////////

  always @(negedge Reset_n_i or posedge Clk_i)
  begin
    if (!Reset_n_i)
    begin
      SPI_FSM_State <= stIdle;
    end
    else
    begin
      SPI_FSM_State <= SPI_FSM_NextState;
    end  
  end

  always @(SPI_FSM_State, Start_i, SPI_Transmission_i, SPI_FSM_TimerOvfl)
  begin  // process SPI_FSM_CombProc
    SPI_FSM_NextState = SPI_FSM_State;
    // control signal default values
    ADT7310CS_n_o       = 1'b1;
    SPI_Data_o          = 8'bxxxxxxxx; // most time we don't care which value is set
    SPI_Write_o         = 1'b0;
    SPI_ReadNext_o      = 1'b0;
    SPI_FSM_TimerPreset = 1'b1;
    SPI_FSM_TimerEnable = 1'b0;
    SPI_FSM_Wr1         = 1'b0;
    SPI_FSM_Wr0         = 1'b0;
    Done_o              = 1'b1;
    // next state and output logic
    case (SPI_FSM_State)
      stIdle: begin
        if (Start_i == 1'b1)
        begin
          // single-shot measurement mode: write to 8-bit configuration
          // register (0x01): send 0x08 0x20 (one shot mode)
          SPI_FSM_NextState = stWriteValue;
          ADT7310CS_n_o     = 1'b0;
          SPI_Data_o        = 8'h08;
          SPI_Write_o       = 1'b1;
          Done_o            = 1'b0;
        end
      end
      stWriteValue: begin
        SPI_FSM_NextState   = stWaitSent;
        ADT7310CS_n_o       = 1'b0;
        // send 0x20
        SPI_Data_o          = 8'h20;
        SPI_Write_o         = 1'b1;
        Done_o              = 1'b0;
      end
      stWaitSent: begin
        // wait until SPI transmission has finished
        ADT7310CS_n_o       = 1'b0;
        Done_o              = 1'b0;
        if (SPI_Transmission_i == 1'b0)
        begin
          SPI_FSM_NextState   = stConsume1;
          SPI_ReadNext_o      = 1'b1;       // consume first received value
        end
      end
      stConsume1: begin
        SPI_FSM_NextState   = stWait;
        ADT7310CS_n_o       = 1'b0;
        Done_o              = 1'b0;
        SPI_ReadNext_o      = 1'b1;       // consume second received value
        SPI_FSM_TimerPreset = 1'b0;
        SPI_FSM_TimerEnable = 1'b1;  // start timer
      end
      stWait: begin
        // wait for 240ms
        ADT7310CS_n_o       = 1'b1;
        Done_o              = 1'b0;
        if (SPI_FSM_TimerOvfl == 1'b0)
        begin
          SPI_FSM_TimerPreset = 1'b0;
          SPI_FSM_TimerEnable = 1'b1;  // timer running
        end
        else
        begin
          // timer overflow -> continue: send read command and two dummy bytes
          ADT7310CS_n_o       = 1'b0;
          SPI_FSM_NextState   = stWriteDummy1;
          SPI_Data_o          = 8'h50;
          SPI_Write_o         = 1'b1;
        end
      end
      stWriteDummy1: begin
        SPI_FSM_NextState   = stWriteDummy2;
        ADT7310CS_n_o       = 1'b0;
        Done_o              = 1'b0;
          SPI_Data_o          = 8'hFF;
          SPI_Write_o         = 1'b1;
        end
      stWriteDummy2: begin
        SPI_FSM_NextState   = stRead1;
        ADT7310CS_n_o       = 1'b0;
        Done_o              = 1'b0;
        SPI_Data_o          = 8'hFF;
        SPI_Write_o         = 1'b1;
      end
      stRead1: begin
        ADT7310CS_n_o       = 1'b0;
        Done_o              = 1'b0;
        // wait until SPI transmission has finished
        if (SPI_Transmission_i == 1'b0) begin
          SPI_FSM_NextState = stRead2;
          // consume and ignore first byte
          SPI_ReadNext_o    = 1'b1;
        end
      end
      stRead2: begin
        Done_o              = 1'b0;
        // consume and store second byte
        SPI_ReadNext_o      = 1'b1;
        SPI_FSM_Wr1         = 1'b1;
        SPI_FSM_NextState   = stRead3;
      end
      stRead3: begin
        Done_o              = 1'b0;
        // consume and store third byte
        SPI_ReadNext_o      = 1'b1;
        SPI_FSM_Wr0         = 1'b1;
        SPI_FSM_NextState   = stPause;
      end
      stPause: begin
        SPI_FSM_NextState = stIdle;
      end
      default: begin
      end
    endcase
  end


  /////////////////////////////////////////////////////////////////////////////
  // Byte-wide Memory /////////////////////////////////////////////////////////
  /////////////////////////////////////////////////////////////////////////////

  always @(negedge Reset_n_i or posedge Clk_i)
  begin
    if (!Reset_n_i)
    begin
      Byte0_o <= 8'd0;
      Byte1_o <= 8'd0;
    end
    else
    begin
      if (SPI_FSM_Wr0)
      begin
        Byte0_o <= SPI_Data_i;
      end
      if (SPI_FSM_Wr1)
      begin
        Byte1_o <= SPI_Data_i;
      end
    end  
  end

  /////////////////////////////////////////////////////////////////////////////
  // Word Arithmetic //////////////////////////////////////////////////////////
  /////////////////////////////////////////////////////////////////////////////

  reg [31:0] SPI_FSM_Timer;
  
  always @(negedge Reset_n_i or posedge Clk_i)
  begin
    if (!Reset_n_i)
    begin
      SPI_FSM_Timer <= 32'd0;
    end
    else
    begin
      if (SPI_FSM_TimerPreset)
      begin
        SPI_FSM_Timer <= ParamCounterPreset_i;
      end
      else if (SPI_FSM_TimerEnable)
      begin
        SPI_FSM_Timer <= SPI_FSM_Timer - 1'b1;
      end
    end  
  end

  assign SPI_FSM_TimerOvfl = (SPI_FSM_Timer == 0) ? 1'b1 : 1'b0;

endmodule // SPIFSM
