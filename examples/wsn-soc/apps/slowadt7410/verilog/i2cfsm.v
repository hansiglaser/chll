`timescale 1ns/1ps

module I2CFSM (
  input                  Reset_n_i,
  input                  Clk_i,
  // FSM control
  input                  Start_i,
  output reg             Done_o,
  output reg             Error_o,
  output reg [7:0]       Byte0_o,
  output reg [7:0]       Byte1_o,
  // to/from I2C_Master
  // I2C control
  output reg             I2C_ReceiveSend_n_o,
  output reg [7:0]       I2C_ReadCount_o,
  output reg             I2C_StartProcess_o,
  input                  I2C_Busy_i,
  // I2C FIFO
  output reg             I2C_FIFOReadNext_o,
  output reg             I2C_FIFOWrite_o,
  output reg [7:0]       I2C_Data_o,
  input      [7:0]       I2C_Data_i,
  // I2C error
  input                  I2C_Error_i,
  // parameters
  input [15:0]           ParamCounterPresetH_i,
  input [15:0]           ParamCounterPresetL_i
);

  // ADT7410 I2C Temperature Sensor
  //  - ADT7410 is very similar to ADT7310
  //  - it uses different register addresses
  //  - instead of the "SPI Command Byte" it uses the normal I2C address byte
  //    including its R/W bit
  //  - One-Shot Measurement
  //     - initiate one-shot mode: write 0x20 to config register at 0x03
  //        - "10010aa0" "00000011" "00100000"
  //     - wait for 240ms
  //     - read temperature register at 0x00:
  //        - 1st transaction: write reg. address: "10010aa0" "00000000"
  //        - 2nd transaction: read two bytes:     "10010aa1" "smmmmmmm" "lllllfff"
  //     - Temperature = "smmmmmmmlllll", flags "fff" are unused
  //
  // I2C Master
  //  - write address + N bytes, see I2C Bus Controller Documentation, testcase 4
  //     - ReceiveSend_o = '0'
  //     - FIFOWrite_o = '1', Data_o = ..., N+1 cycles
  //     - FIFOWrite_o = '0', StartProcess_o = '1' for 1 cycles only, Busy_i
  //       goes high immediately  <-- really?
  //     - wait until Busy_i goes low again
  //  - read N bytes, see testcase 7
  //     - ReceiveSend_o = '1', ReadCount_o = N
  //     - FIFOWrite_o = '1', Data_o = address, 1 cycle
  //     - FIFOWrite_o = '0', StartProcess_o = '1' for 1 cycles only, Busy_i
  //       goes high immediately
  //     - wait until Busy_i goes low again
  //     - read bytes from FIFO via Data_i and FIFOReadNext_o
  //

  // I2C FSM
  localparam stIdle       = 4'b0000;
  localparam stWriteReq1  = 4'b0001;
  localparam stWriteReq2  = 4'b0010;
  localparam stStartReq   = 4'b0011;
  localparam stWaitReq    = 4'b0100;
  localparam stWait       = 4'b0101;
  localparam stWriteAddr1 = 4'b0110;
  localparam stStartAddr  = 4'b0111;
  localparam stWaitAddr   = 4'b1000;
  localparam stStartQuery = 4'b1001;
  localparam stWaitQuery  = 4'b1010;
  localparam stRead1      = 4'b1011;
  localparam stPause      = 4'b1100;
  reg  [3:0]             I2C_FSM_State;
  reg  [3:0]             I2C_FSM_NextState;
  wire                   I2C_FSM_TimerOvfl;
  reg                    I2C_FSM_TimerPreset;
  reg                    I2C_FSM_TimerEnable;
  reg                    I2C_FSM_Wr1;
  reg                    I2C_FSM_Wr0;

  /////////////////////////////////////////////////////////////////////////////
  // FSM //////////////////////////////////////////////////////////////////////
  /////////////////////////////////////////////////////////////////////////////

  always @(negedge Reset_n_i or posedge Clk_i)
  begin
    if (!Reset_n_i)
    begin
      I2C_FSM_State <= stIdle;
    end
    else
    begin
      I2C_FSM_State <= I2C_FSM_NextState;
    end  
  end

  always @(I2C_FSM_State, Start_i, I2C_Busy_i, I2C_Error_i, I2C_FSM_TimerOvfl)
  begin  // process I2C_FSM_CombProc
    I2C_FSM_NextState = I2C_FSM_State;
    // control signal default values
    Done_o              = 1'b0;
    Error_o             = 1'b0;
    // to I2C Master
    I2C_ReceiveSend_n_o = 1'b0;
    I2C_ReadCount_o     = 8'h00;
    I2C_StartProcess_o  = 1'b0;
    I2C_FIFOReadNext_o  = 1'b0; 
    I2C_FIFOWrite_o     = 1'b0; 
    I2C_Data_o          = 8'h00;
    // to other processes in this module
    I2C_FSM_TimerPreset = 1'b1;
    I2C_FSM_TimerEnable = 1'b0;
    I2C_FSM_Wr1         = 1'b0;
    I2C_FSM_Wr0         = 1'b0;
    // next state and output logic
    case (I2C_FSM_State)
      stIdle: begin
        if (Start_i == 1'b1)
        begin
          // single-shot measurement mode: write 0x20 to config register at 0x03
          // "10010aa0" "00000011" "00100000"
          I2C_FSM_NextState = stWriteReq1;
          I2C_Data_o        = 8'b10010000;
          I2C_FIFOWrite_o   = 1'b1;
        end
        else
        begin
          // nobody cares about the value, so simplify the MUX
          I2C_Data_o        = 8'b10010000;
        end
      end
      stWriteReq1: begin
        I2C_FSM_NextState   = stWriteReq2;
        I2C_Data_o          = 8'b00000011;
        I2C_FIFOWrite_o     = 1'b1;
      end
      stWriteReq2: begin
        I2C_FSM_NextState   = stStartReq;
        I2C_Data_o          = 8'h20;
        I2C_FIFOWrite_o     = 1'b1;
      end
      stStartReq: begin
        I2C_StartProcess_o  = 1'b1;
        I2C_FSM_NextState   = stWaitReq;
      end
      stWaitReq: begin
        // wait until I2C transmission has finished
        if (I2C_Error_i == 1'b1)
        begin
          I2C_FSM_NextState   = stIdle;
          Error_o             = 1'b1;
        end
        else if (I2C_Busy_i == 1'b0)
        begin
          I2C_FSM_NextState   = stWait;
          I2C_FSM_TimerPreset = 1'b0;
          I2C_FSM_TimerEnable = 1'b1;  // start timer
        end
      end
      stWait: begin
        // wait for 240ms
        if (I2C_FSM_TimerOvfl == 1'b0)
        begin
          I2C_FSM_TimerPreset = 1'b0;
          I2C_FSM_TimerEnable = 1'b1;  // timer running
          // nobody cares about the value, so simplify the MUX
          I2C_Data_o          = 8'b10010000;
        end
        else
        begin
          // timer overflow -> continue: write register address:  "10010aa0" "00000000"
          I2C_FSM_NextState   = stWriteAddr1;
          I2C_Data_o          = 8'b10010000;
          I2C_FIFOWrite_o     = 1'b1;
        end
      end
      stWriteAddr1: begin
        I2C_FSM_NextState   = stStartAddr;
        I2C_Data_o          = 8'b00000000;
        I2C_FIFOWrite_o     = 1'b1;
      end
      stStartAddr: begin
        // start sending register address
        I2C_StartProcess_o  = 1'b1;
        I2C_FSM_NextState   = stWaitAddr;
      end
      stWaitAddr: begin
        // wait until I2C transmission has finished
        if (I2C_Busy_i == 1'b0)
        begin
          I2C_FSM_NextState   = stStartQuery;
          I2C_Data_o          = 8'b10010001;   // read transfer: R/W = 1
          I2C_FIFOWrite_o     = 1'b1;
        end
        else
        begin
          // nobody cares about the value, so simplify the MUX
          I2C_Data_o          = 8'b10010001;   // read transfer: R/W = 1
        end
      end
      stStartQuery: begin
        // start sending read transfer
        I2C_ReceiveSend_n_o = 1'b1;
        I2C_ReadCount_o     = 8'h02;
        I2C_StartProcess_o  = 1'b1;
        I2C_FSM_NextState   = stWaitQuery;
      end
      stWaitQuery: begin
        I2C_ReceiveSend_n_o = 1'b1;
        I2C_ReadCount_o     = 8'h02;
        // wait until I2C transmission has finished
        if (I2C_Busy_i == 1'b0)
        begin
          I2C_FSM_NextState   = stRead1;
          // consume and store first byte
          I2C_FIFOReadNext_o  = 1'b1;
          I2C_FSM_Wr1         = 1'b1;
        end
      end
      stRead1: begin
        // consume and store second byte
        I2C_FIFOReadNext_o  = 1'b1;
        I2C_FSM_Wr0         = 1'b1;
        I2C_FSM_NextState   = stPause;
      end
      stPause: begin
        Done_o              = 1'b1;
        I2C_FSM_NextState   = stIdle;
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
      if (I2C_FSM_Wr0)
      begin
        Byte0_o <= I2C_Data_i;
      end
      if (I2C_FSM_Wr1)
      begin
        Byte1_o <= I2C_Data_i;
      end
    end  
  end

  /////////////////////////////////////////////////////////////////////////////
  // Word Arithmetic //////////////////////////////////////////////////////////
  /////////////////////////////////////////////////////////////////////////////

  reg [31:0] I2C_FSM_Timer;
  
  always @(negedge Reset_n_i or posedge Clk_i)
  begin
    if (!Reset_n_i)
    begin
      I2C_FSM_Timer <= 32'd0;
    end
    else
    begin
      if (I2C_FSM_TimerPreset)
      begin
        I2C_FSM_Timer <= {ParamCounterPresetH_i, ParamCounterPresetL_i};
      end
      else if (I2C_FSM_TimerEnable)
      begin
        I2C_FSM_Timer <= I2C_FSM_Timer - 1'b1;
      end
    end  
  end

  assign I2C_FSM_TimerOvfl = (I2C_FSM_Timer == 0) ? 1'b1 : 1'b0;

endmodule // I2CFSM
