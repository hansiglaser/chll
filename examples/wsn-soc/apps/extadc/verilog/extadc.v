module ExtADC (
  (* intersynth_port = "Reset_n_i" *)
  input Reset_n_i,
  (* intersynth_port = "Clk_i" *)
  input Clk_i,
  (* intersynth_port = "ReconfModuleIn_s", intersynth_conntype = "Bit" *)
  input Enable_i,
  (* intersynth_port = "ReconfModuleIRQs_s", intersynth_conntype = "Bit" *)
  output reg CpuIntr_o,
  (* intersynth_port = "Outputs_o", intersynth_conntype = "Bit" *)
  output reg SensorPower_o,
  (* intersynth_port = "Outputs_o", intersynth_conntype = "Bit" *)
  output reg SensorStart_o,
  (* intersynth_port = "Inputs_i", intersynth_conntype = "Bit" *)
  input SensorReady_i,
  (* intersynth_port = "AdcDoConvert_o", intersynth_conntype = "Bit" *)
  output reg AdcStart_o,
  (* intersynth_port = "AdcConvComplete_i", intersynth_conntype = "Bit" *)
  input AdcDone_i,
  (* intersynth_port = "AdcValue_i", intersynth_conntype = "Word" *)
  input[15:0] AdcValue_i,
  (* intersynth_param = "PeriodCounterPreset_i", intersynth_conntype = "Word" *)
  input[15:0] PeriodCounterPreset_i,
  (* intersynth_param = "SensorValue_o", intersynth_conntype = "Word" *)
  output [15:0] SensorValue_o,
  (* intersynth_param = "Threshold_i", intersynth_conntype = "Word" *)
  input[15:0] Threshold_i
);

  // Sensor FSM
  localparam stDisabled     = 3'b000;
  localparam stIdle         = 3'b001;
  localparam stSensorPower  = 3'b010;
  localparam stSensorSettle = 3'b011;
  localparam stMeasure      = 3'b100;
  localparam stNotify       = 3'b101;
  reg  [2:0]             State;
  reg  [2:0]             NextState;
  wire                   TimerOvfl;
  reg                    TimerPreset;
  reg                    TimerEnable;
  wire                   DiffTooLarge;
  reg                    StoreNewValue;

  always @(negedge Reset_n_i or posedge Clk_i)
  begin
    if (!Reset_n_i)
    begin
      State <= stDisabled;
    end
    else
    begin // rising clock edge
      // state register
      State <= NextState;
    end  
  end

  always @(State, Enable_i, TimerOvfl, SensorReady_i, AdcDone_i, DiffTooLarge)
  begin  // process CombProc
    NextState     = State;
    // control signal default values
    TimerPreset   = 1'b1;
    TimerEnable   = 1'b0;
    SensorPower_o = 1'b0;
    SensorStart_o = 1'b0;
    AdcStart_o    = 1'b0;
    StoreNewValue = 1'b0;
    CpuIntr_o     = 1'b0;
    // next state and output logic
    case (State)
      stDisabled: begin
        if (Enable_i == 1'b1)
        begin
          NextState     = stIdle;
          TimerPreset   = 1'b0;
          TimerEnable   = 1'b1;
        end
      end
      stIdle: begin
        if (Enable_i == 1'b0)
        begin
          NextState     = stDisabled;
        end
        else
        if (TimerOvfl == 1'b1)
        begin
          NextState     = stSensorPower;
          SensorPower_o = 1'b1;
        end
        else
        begin
          TimerPreset   = 1'b0;
          TimerEnable   = 1'b1;
        end
      end
      stSensorPower: begin
        SensorPower_o = 1'b1;
        SensorStart_o = 1'b1;
        NextState     = stSensorSettle;
      end
      stSensorSettle: begin
        SensorPower_o = 1'b1;
        SensorStart_o = 1'b1;
        if (SensorReady_i == 1'b1) 
        begin
          NextState     = stMeasure;
          AdcStart_o    = 1'b1;
        end
      end
      stMeasure: begin
        SensorPower_o = 1'b1;
        SensorStart_o = 1'b1;
        AdcStart_o    = 1'b1;
        if (AdcDone_i == 1'b1)
        begin
          if (DiffTooLarge == 1'b1)
          begin
            NextState     = stNotify;
            StoreNewValue = 1'b1;  // store new value
          end
          else
          begin
            NextState     = stIdle;
          end
        end
      end
      stNotify: begin
        NextState     = stIdle;
        CpuIntr_o     = 1'b1;  // notify CPU
      end
      default: begin
      end
    endcase
  end 

  /////////////////////////////////////////////////////////////////////////////
  // Word Arithmetic //////////////////////////////////////////////////////////
  /////////////////////////////////////////////////////////////////////////////

  reg [15:0] Timer;
  
  always @(negedge Reset_n_i or posedge Clk_i)
  begin
    if (!Reset_n_i)
    begin
      Timer <= 16'd0;
    end
    else
    begin
      if (TimerPreset)
      begin
        Timer <= PeriodCounterPreset_i;
      end
      else if (TimerEnable)
      begin
        Timer <= Timer - 1'b1;
      end
    end  
  end

  assign TimerOvfl = (Timer == 0) ? 1'b1 : 1'b0;

  /////////////////////////////////////////////////////////////////////////////
  // Word Arithmetic
  // interconnecting signals
  reg  [15:0] Word0;
  wire [15:0] AbsDiffResult;

  always @(negedge Reset_n_i or posedge Clk_i)
  begin
    if (!Reset_n_i)
    begin
      Word0 <= 16'd0;
    end
    else
    begin
      if (StoreNewValue)
      begin
        Word0 <= AdcValue_i;
      end
    end  
  end

  wire [16:0] DiffAB;
  wire [15:0] DiffBA;
  assign DiffAB = {1'b0, AdcValue_i} - {1'b0, Word0};
  assign DiffBA =        Word0       -        AdcValue_i;
  assign AbsDiffResult = DiffAB[16] ? DiffBA : DiffAB[15:0];

  assign DiffTooLarge = (AbsDiffResult > Threshold_i) ? 1'b1 : 1'b0;

  assign SensorValue_o = Word0;

endmodule
