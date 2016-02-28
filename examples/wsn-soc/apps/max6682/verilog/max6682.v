module MAX6682_SPI_FSM (
  input            Reset_n_i,
  input            Clk_i,
  input            SPI_FSM_Start,
  input            SPI_Transmission_i,
  output reg       MAX6682CS_n_o,
  output reg       SPI_Write_o,
  output reg       SPI_ReadNext_o,
  output reg       SPI_FSM_Done,
  input      [7:0] SPI_Data_i,
  output reg [7:0] Byte0,
  output reg [7:0] Byte1
);
  localparam stIdleSPI    = 3'b000;
  localparam stWrite1     = 3'b001;
  localparam stWrite2     = 3'b010;
  localparam stWait       = 3'b011;
  localparam stRead1      = 3'b100;
  localparam stRead2      = 3'b101;
  localparam stPause      = 3'b110;
  reg [2:0]              SPI_FSM_State;
  reg [2:0]              SPI_FSM_NextState;
  reg                    SPI_FSM_Wr1;
  reg                    SPI_FSM_Wr0;

  /////////////////////////////////////////////////////////////////////////////
  // SPI FSM //////////////////////////////////////////////////////////////////
  /////////////////////////////////////////////////////////////////////////////

  always @(negedge Reset_n_i or posedge Clk_i)
  begin
    if (!Reset_n_i)
    begin
      SPI_FSM_State <= stIdleSPI;
    end
    else
    begin // rising clock edge
      SPI_FSM_State <= SPI_FSM_NextState;
    end  
  end

  // Note: There is a possible infinite zero-delay loop between this always-
  // block and that of the SensorFSM with SPI_FSM_Start and SPI_FSM_Done.
  // Previously (and in the VHDL code) SPI_FSM_Done is set '1' as default
  // value and then set to '0' in nearly each case below, e.g. in stIdleSPI.
  // In the simulation, this changes the value of that signal each time this
  // always-block is executed, which then triggers the below always-block too.
  // Therefore this code was changed to set the default value '0' and only set
  // the signal '1' in stRead2 and stPause.
  always @(SPI_FSM_State, SPI_FSM_Start, SPI_Transmission_i)
  begin  // process SPI_FSM_CombProc
    SPI_FSM_NextState = SPI_FSM_State;
    // control signal default values
    MAX6682CS_n_o       = 1'b1;
    SPI_Write_o         = 1'b0;
    SPI_ReadNext_o      = 1'b0;
    SPI_FSM_Wr1         = 1'b0;
    SPI_FSM_Wr0         = 1'b0;
    SPI_FSM_Done        = 1'b0;
    // next state and output logic
    case (SPI_FSM_State)
      stIdleSPI: begin
        if (SPI_FSM_Start == 1'b1)
        begin
          SPI_FSM_NextState = stWrite1;
          MAX6682CS_n_o     = 1'b0;
          SPI_Write_o       = 1'b1;
        end
      end
      stWrite1: begin
        SPI_FSM_NextState   = stWrite2;
        MAX6682CS_n_o       = 1'b0;
        SPI_Write_o         = 1'b1;
      end
      stWrite2: begin
        SPI_FSM_NextState   = stWait;
        MAX6682CS_n_o       = 1'b0;
      end
      stWait: begin
        MAX6682CS_n_o       = 1'b0;
        // wait until SPI transmission has finished
        if (SPI_Transmission_i == 1'b0) begin
          SPI_FSM_NextState = stRead1;
          SPI_ReadNext_o    = 1'b1;
          SPI_FSM_Wr1       = 1'b1;
        end
      end
      stRead1: begin
        SPI_FSM_NextState   = stRead2;
        MAX6682CS_n_o       = 1'b0;
        SPI_ReadNext_o      = 1'b1;
        SPI_FSM_Wr0         = 1'b1;
      end
      stRead2: begin
        SPI_FSM_NextState = stPause;
        SPI_FSM_Done      = 1'b1;
      end
      stPause: begin
        SPI_FSM_NextState = stIdleSPI;
        SPI_FSM_Done      = 1'b1;
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
      Byte0 <= 8'd0;
      Byte1 <= 8'd0;
    end
    else
    begin
      if (SPI_FSM_Wr0)
      begin
        Byte0 <= SPI_Data_i;
      end
      if (SPI_FSM_Wr1)
      begin
        Byte1 <= SPI_Data_i;
      end
    end  
  end

endmodule

module MAX6682 (
  (* intersynth_port="Reset_n_i" *) 
  input Reset_n_i,
  (* intersynth_port="Clk_i" *) 
  input Clk_i,
  (* intersynth_port="ReconfModuleIn_s", intersynth_conntype="Bit" *) 
  input Enable_i,
  (* intersynth_port="ReconfModuleIRQs_s", intersynth_conntype="Bit" *) 
  output reg CpuIntr_o,
  (* intersynth_port="Outputs_o", intersynth_conntype="Bit" *) 
  output MAX6682CS_n_o,
  (* intersynth_port="SPI_DataOut", intersynth_conntype="Byte" *) 
  input[7:0] SPI_Data_i,
  (* intersynth_port="SPI_Write", intersynth_conntype="Bit" *) 
  output SPI_Write_o,
  (* intersynth_port="SPI_ReadNext", intersynth_conntype="Bit" *) 
  output SPI_ReadNext_o,
  (* intersynth_port="SPI_DataIn", intersynth_conntype="Byte" *) 
  output [7:0] SPI_Data_o,
  (* intersynth_port="SPI_FIFOFull", intersynth_conntype="Bit" *) 
  input SPI_FIFOFull_i,
  (* intersynth_port="SPI_FIFOEmpty", intersynth_conntype="Bit" *) 
  input SPI_FIFOEmpty_i,
  (* intersynth_port="SPI_Transmission", intersynth_conntype="Bit" *) 
  input SPI_Transmission_i,
  (* intersynth_param="PeriodCounterPresetH_i", intersynth_conntype="Word" *) 
  input[15:0] PeriodCounterPresetH_i,
  (* intersynth_param="PeriodCounterPresetL_i", intersynth_conntype="Word" *) 
  input[15:0] PeriodCounterPresetL_i,
  (* intersynth_param="SensorValue_o", intersynth_conntype="Word" *) 
  output[15:0] SensorValue_o,
  (* intersynth_param="Threshold_i", intersynth_conntype="Word" *) 
  input[15:0] Threshold_i,
  (* intersynth_port="SPI_CPOL", intersynth_conntype="Bit" *) 
  output SPI_CPOL_o,
  (* intersynth_port="SPI_CPHA", intersynth_conntype="Bit" *) 
  output SPI_CPHA_o,
  (* intersynth_port="SPI_LSBFE", intersynth_conntype="Bit" *) 
  output SPI_LSBFE_o
);

  /* constant value for dynamic signal */ 
  assign SPI_CPOL_o = 1'b0;
  /* constant value for dynamic signal */ 
  assign SPI_CPHA_o = 1'b0;
  /* constant value for dynamic signal */ 
  assign SPI_LSBFE_o = 1'b0;

  assign SPI_Data_o = 8'b00000000;
  reg        SPI_FSM_Start;
  wire       SPI_FSM_Done;
  wire [7:0] Byte0;
  wire [7:0] Byte1;

  MAX6682_SPI_FSM MAX6682_SPI_FSM_1 (
    .Reset_n_i          (Reset_n_i),
    .Clk_i              (Clk_i),
    .SPI_FSM_Start      (SPI_FSM_Start),
    .SPI_Transmission_i (SPI_Transmission_i),
    .MAX6682CS_n_o      (MAX6682CS_n_o),
    .SPI_Write_o        (SPI_Write_o),
    .SPI_ReadNext_o     (SPI_ReadNext_o),
    .SPI_FSM_Done       (SPI_FSM_Done),
    .SPI_Data_i         (SPI_Data_i),
    .Byte0              (Byte0),
    .Byte1              (Byte1)
  );

  /////////////////////////////////////////////////////////////////////////////
  // SensorFSM ////////////////////////////////////////////////////////////////
  /////////////////////////////////////////////////////////////////////////////

  // Sensor FSM
  localparam stDisabled   = 2'b00;
  localparam stIdle       = 2'b01;
  localparam stSPI_Xfer   = 2'b10;
  localparam stNotify     = 2'b11;
  reg  [1:0]             SensorFSM_State;
  reg  [1:0]             SensorFSM_NextState;
  wire                   SensorFSM_TimerOvfl;
  reg                    SensorFSM_TimerPreset;
  reg                    SensorFSM_TimerEnable;
  wire                   SensorFSM_DiffTooLarge;
  reg                    SensorFSM_StoreNewValue;

  always @(negedge Reset_n_i or posedge Clk_i)
  begin
    if (!Reset_n_i)
    begin
      SensorFSM_State <= stDisabled;
    end
    else
    begin // rising clock edge
      // state register
      SensorFSM_State <= SensorFSM_NextState;
    end  
  end

  always @(SensorFSM_State, Enable_i, SensorFSM_TimerOvfl, SPI_FSM_Done, SensorFSM_DiffTooLarge)
  begin  // process SensorFSM_CombProc
    SensorFSM_NextState     = SensorFSM_State;
    // control signal default values
    SensorFSM_TimerPreset   = 1'b1;
    SensorFSM_TimerEnable   = 1'b0;
    SPI_FSM_Start           = 1'b0;
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
          SensorFSM_NextState     = stSPI_Xfer;
          SPI_FSM_Start           = 1'b1;
        end
      end
      stSPI_Xfer: begin
        if (SPI_FSM_Done == 1'b1)
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
        SensorFSM_Timer <= {PeriodCounterPresetH_i, PeriodCounterPresetL_i};
      end
      else if (SensorFSM_TimerEnable)
      begin
        SensorFSM_Timer <= SensorFSM_Timer - 1'd1;
      end
    end  
  end

  assign SensorFSM_TimerOvfl = (SensorFSM_Timer == 0) ? 1'b1 : 1'b0;

  /////////////////////////////////////////////////////////////////////////////
  // Word Arithmetic
  // interconnecting signals
  wire [15:0] SensorValue;
  reg  [15:0] Word0;
  wire [15:0] AbsDiffResult;

  assign SensorValue = { 5'b00000, Byte1, Byte0[7:5] };

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

  wire [16:0] DiffAB;
  wire [15:0] DiffBA;
  assign DiffAB = {1'b0, SensorValue} - {1'b0, Word0};
  assign DiffBA =        Word0        -        SensorValue;
  assign AbsDiffResult = DiffAB[16] ? DiffBA : DiffAB[15:0];

  assign SensorFSM_DiffTooLarge = (AbsDiffResult > Threshold_i) ? 1'b1 : 1'b0;

  assign SensorValue_o = Word0;

endmodule
