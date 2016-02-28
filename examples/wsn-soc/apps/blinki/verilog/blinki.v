module blinki (
  (* intersynth_port = "Reset_n_i" *)
  input Reset_n_i,
  (* intersynth_port = "Clk_i" *)
  input Clk_i,
  (* intersynth_port = "Outputs_o", intersynth_conntype = "Bit" *)
  output reg LED_o,
  (* intersynth_param = "PeriodH_i", intersynth_conntype = "Word" *)
  input[15:0] PeriodH_i,
  (* intersynth_param = "PeriodL_i", intersynth_conntype = "Word" *)
  input[15:0] PeriodL_i
);

  // Sensor FSM
  localparam stStart   = 3'b000;
  localparam stOn      = 3'b001;
  localparam stOff     = 3'b010;
  reg  [2:0]             State;
  reg  [2:0]             NextState;
  reg                    TimerPreset;
  reg                    TimerEnable;
  wire                   TimerOvfl;

  always @(negedge Reset_n_i or posedge Clk_i)
  begin
    if (!Reset_n_i)
    begin
      State <= stStart;
    end
    else
    begin // rising clock edge
      // state register
      State <= NextState;
    end  
  end

  always @(State, TimerOvfl)
  begin  // process CombProc
    NextState     = State;
    // control signal default values
    TimerEnable   = 1'b1;
    TimerPreset   = 1'b0;
    // next state and output logic
    case (State)
      stStart: begin
        TimerPreset   = 1'b1;
        NextState     = stOn;
      end
      stOn: begin
        LED_o = 1'b1;
        if (TimerOvfl == 1'b1)
        begin
          NextState     = stOff;
          TimerPreset   = 1'b1;
        end
      end
      stOff: begin
        LED_o = 1'b0;
        if (TimerOvfl == 1'b1)
        begin
          NextState     = stOn;
          TimerPreset   = 1'b1;
        end
      end
      default: begin
      end
    endcase
  end 

  /////////////////////////////////////////////////////////////////////////////
  // Word Arithmetic //////////////////////////////////////////////////////////
  /////////////////////////////////////////////////////////////////////////////

  reg [31:0] Timer;
  
  always @(negedge Reset_n_i or posedge Clk_i)
  begin
    if (!Reset_n_i)
    begin
      Timer <= 32'd0;
    end
    else
    begin
      if (TimerPreset)
      begin
        Timer <= {PeriodH_i, PeriodL_i};
      end
      else if (TimerEnable)
      begin
        Timer <= Timer - 1'b1;
      end
    end  
  end

  assign TimerOvfl = (Timer == 0) ? 1'b1 : 1'b0;

endmodule
