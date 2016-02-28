attribute \src "../../verilog/sensorfsm.v:3"
module \SensorFSM
  attribute \src "../../verilog/sensorfsm.v:75"
  wire $0\CpuIntr_o[0:0]
  attribute \src "../../verilog/sensorfsm.v:75"
  wire $0\SPIFSM_Start_o[0:0]
  attribute \src "../../verilog/sensorfsm.v:75"
  wire width 2 $0\SensorFSM_NextState[1:0]
  attribute \src "../../verilog/sensorfsm.v:63"
  wire width 2 $0\SensorFSM_State[1:0]
  attribute \src "../../verilog/sensorfsm.v:75"
  wire $0\SensorFSM_StoreNewValue[0:0]
  attribute \src "../../verilog/sensorfsm.v:75"
  wire $0\SensorFSM_TimerEnable[0:0]
  attribute \src "../../verilog/sensorfsm.v:75"
  wire $0\SensorFSM_TimerPreset[0:0]
  attribute \src "../../verilog/sensorfsm.v:75"
  wire $1\CpuIntr_o[0:0]
  attribute \src "../../verilog/sensorfsm.v:75"
  wire $1\SPIFSM_Start_o[0:0]
  attribute \src "../../verilog/sensorfsm.v:75"
  wire width 2 $1\SensorFSM_NextState[1:0]
  attribute \src "../../verilog/sensorfsm.v:75"
  wire $1\SensorFSM_StoreNewValue[0:0]
  attribute \src "../../verilog/sensorfsm.v:75"
  wire $1\SensorFSM_TimerEnable[0:0]
  attribute \src "../../verilog/sensorfsm.v:75"
  wire $1\SensorFSM_TimerPreset[0:0]
  attribute \src "../../verilog/sensorfsm.v:75"
  wire $2\SPIFSM_Start_o[0:0]
  attribute \src "../../verilog/sensorfsm.v:75"
  wire width 2 $2\SensorFSM_NextState[1:0]
  attribute \src "../../verilog/sensorfsm.v:75"
  wire $2\SensorFSM_StoreNewValue[0:0]
  attribute \src "../../verilog/sensorfsm.v:75"
  wire $2\SensorFSM_TimerEnable[0:0]
  attribute \src "../../verilog/sensorfsm.v:75"
  wire $2\SensorFSM_TimerPreset[0:0]
  attribute \src "../../verilog/sensorfsm.v:75"
  wire $3\SPIFSM_Start_o[0:0]
  attribute \src "../../verilog/sensorfsm.v:75"
  wire width 2 $3\SensorFSM_NextState[1:0]
  attribute \src "../../verilog/sensorfsm.v:75"
  wire $3\SensorFSM_StoreNewValue[0:0]
  attribute \src "../../verilog/sensorfsm.v:75"
  wire $3\SensorFSM_TimerEnable[0:0]
  attribute \src "../../verilog/sensorfsm.v:75"
  wire $3\SensorFSM_TimerPreset[0:0]
  attribute \src "../../verilog/sensorfsm.v:75"
  wire width 2 $4\SensorFSM_NextState[1:0]
  attribute \src "../../verilog/sensorfsm.v:75"
  wire $4\SensorFSM_TimerEnable[0:0]
  attribute \src "../../verilog/sensorfsm.v:75"
  wire $4\SensorFSM_TimerPreset[0:0]
  attribute \src "../../verilog/sensorfsm.v:75"
  wire width 2 $5\SensorFSM_NextState[1:0]
  attribute \src "../../verilog/sensorfsm.v:75"
  wire width 2 $6\SensorFSM_NextState[1:0]
  attribute \src "../../verilog/sensorfsm.v:102"
  wire $eq$../../verilog/sensorfsm.v:102$7_Y
  attribute \src "../../verilog/sensorfsm.v:109"
  wire $eq$../../verilog/sensorfsm.v:109$8_Y
  attribute \src "../../verilog/sensorfsm.v:111"
  wire $eq$../../verilog/sensorfsm.v:111$9_Y
  attribute \src "../../verilog/sensorfsm.v:87"
  wire $eq$../../verilog/sensorfsm.v:87$5_Y
  attribute \src "../../verilog/sensorfsm.v:97"
  wire $eq$../../verilog/sensorfsm.v:97$6_Y
  attribute \src "../../verilog/sensorfsm.v:65"
  wire $logic_not$../../verilog/sensorfsm.v:65$3_Y
  attribute \src "../../verilog/sensorfsm.v:20"
  wire $not$../../verilog/sensorfsm.v:20$1_Y
  attribute \src "../../verilog/sensorfsm.v:42"
  wire width 16 \AbsDiffResult
  attribute \src "../../verilog/sensorfsm.v:51"
  wire \AddSubCmpCarry_s
  attribute \src "../../verilog/sensorfsm.v:50"
  wire width 16 \AddSubCmpD_s
  attribute \src "../../verilog/sensorfsm.v:53"
  wire \AddSubCmpOverflow_s
  attribute \src "../../verilog/sensorfsm.v:52"
  wire \AddSubCmpZero_s
  attribute \src "../../verilog/sensorfsm.v:7"
  wire input 2 \Clk_i
  attribute \src "../../verilog/sensorfsm.v:44"
  wire width 16 \CounterD_s
  attribute \src "../../verilog/sensorfsm.v:47"
  wire \CounterScanClk_s
  attribute \src "../../verilog/sensorfsm.v:48"
  wire \CounterScanDataIn_s
  attribute \src "../../verilog/sensorfsm.v:49"
  wire \CounterScanDataOut_s
  attribute \src "../../verilog/sensorfsm.v:46"
  wire \CounterScanEnable_s
  attribute \src "../../verilog/sensorfsm.v:45"
  wire \CounterZero_s
  attribute \src "../../verilog/sensorfsm.v:10"
  wire output 4 \CpuIntr_o
  attribute \src "../../verilog/sensorfsm.v:9"
  wire input 3 \Enable_i
  attribute \src "../../verilog/sensorfsm.v:38"
  wire width 16 \ParamCounterPreset
  attribute \src "../../verilog/sensorfsm.v:37"
  wire width 16 \ParamThreshold
  attribute \src "../../verilog/sensorfsm.v:6"
  wire input 1 \Reset_n_i
  attribute \src "../../verilog/sensorfsm.v:19"
  wire \Reset_s
  attribute \src "../../verilog/sensorfsm.v:15"
  wire width 8 input 8 \SPIFSM_Byte0_i
  attribute \src "../../verilog/sensorfsm.v:16"
  wire width 8 input 9 \SPIFSM_Byte1_i
  attribute \src "../../verilog/sensorfsm.v:14"
  wire input 7 \SPIFSM_Done_i
  attribute \src "../../verilog/sensorfsm.v:13"
  wire output 6 \SPIFSM_Start_o
  attribute \src "../../verilog/sensorfsm.v:32"
  wire \SensorFSM_DiffTooLarge
  attribute \src "../../verilog/sensorfsm.v:28"
  wire width 2 \SensorFSM_NextState
  attribute \src "../../verilog/sensorfsm.v:27"
  wire width 2 \SensorFSM_State
  attribute \src "../../verilog/sensorfsm.v:33"
  wire \SensorFSM_StoreNewValue
  attribute \src "../../verilog/sensorfsm.v:31"
  wire \SensorFSM_TimerEnable
  attribute \src "../../verilog/sensorfsm.v:29"
  wire \SensorFSM_TimerOvfl
  attribute \src "../../verilog/sensorfsm.v:30"
  wire \SensorFSM_TimerPreset
  attribute \src "../../verilog/sensorfsm.v:40"
  wire width 16 \SensorValue
  attribute \src "../../verilog/sensorfsm.v:11"
  wire width 16 output 5 \SensorValue_o
  attribute \src "../../verilog/sensorfsm.v:41"
  wire width 16 \Word0
  attribute \src "../../verilog/sensorfsm.v:55"
  wire \WordRegisterScanClk_s
  attribute \src "../../verilog/sensorfsm.v:56"
  wire \WordRegisterScanDataIn_s
  attribute \src "../../verilog/sensorfsm.v:57"
  wire \WordRegisterScanDataOut_s
  attribute \src "../../verilog/sensorfsm.v:54"
  wire \WordRegisterScanEnable_s
  attribute \src "../../verilog/sensorfsm.v:97"
  cell $not $eq$../../verilog/sensorfsm.v:97$6
    parameter \A_SIGNED 0
    parameter \A_WIDTH 1
    parameter \Y_WIDTH 1
    connect \A \Enable_i
    connect \Y $eq$../../verilog/sensorfsm.v:97$6_Y
  end
  attribute \src "../../verilog/sensorfsm.v:65"
  cell $logic_not $logic_not$../../verilog/sensorfsm.v:65$3
    parameter \A_SIGNED 0
    parameter \A_WIDTH 1
    parameter \Y_WIDTH 1
    connect \A \Reset_n_i
    connect \Y $logic_not$../../verilog/sensorfsm.v:65$3_Y
  end
  attribute \src "../../verilog/sensorfsm.v:20"
  cell $not $not$../../verilog/sensorfsm.v:20$1
    parameter \A_SIGNED 0
    parameter \A_WIDTH 1
    parameter \Y_WIDTH 1
    connect \A \Reset_n_i
    connect \Y $not$../../verilog/sensorfsm.v:20$1_Y
  end
  attribute \src "../../verilog/sensorfsm.v:189"
  cell \AbsDiff \AbsDiff_1
    parameter \Width 16
    connect \A_i \SensorValue
    connect \B_i \Word0
    connect \D_o \AbsDiffResult
  end
  attribute \src "../../verilog/sensorfsm.v:197"
  cell \AddSubCmp \AddSubCmp_1
    parameter \Width 16
    connect \A_i \ParamThreshold
    connect \AddOrSub_i 1'1
    connect \B_i \AbsDiffResult
    connect \Carry_i 1'0
    connect \Carry_o \AddSubCmpCarry_s
    connect \D_o \AddSubCmpD_s
    connect \Overflow_o \AddSubCmpOverflow_s
    connect \Sign_o \SensorFSM_DiffTooLarge
    connect \Zero_o \AddSubCmpZero_s
  end
  attribute \src "../../verilog/sensorfsm.v:149"
  cell \Counter \Counter_1
    parameter \Width 16
    connect \Clk_i \Clk_i
    connect \D_o \CounterD_s
    connect \Direction_i 1'1
    connect \Enable_i \SensorFSM_TimerEnable
    connect \Overflow_o \SensorFSM_TimerOvfl
    connect \PresetVal_i \ParamCounterPreset
    connect \Preset_i \SensorFSM_TimerPreset
    connect \ResetSig_i 1'0
    connect \Reset_i \Reset_s
    connect \ScanClk_i \CounterScanClk_s
    connect \ScanDataIn_i \CounterScanDataIn_s
    connect \ScanDataOut_o \CounterScanDataOut_s
    connect \ScanEnable_i \CounterScanEnable_s
    connect \Zero_o \CounterZero_s
  end
  attribute \src "../../verilog/sensorfsm.v:175"
  cell \WordRegister \WordRegister_1
    parameter \Width 16
    connect \Clk_i \Clk_i
    connect \D_i \SensorValue
    connect \Enable_i \SensorFSM_StoreNewValue
    connect \Q_o \Word0
    connect \Reset_i \Reset_s
    connect \ScanClk_i \WordRegisterScanClk_s
    connect \ScanDataIn_i \WordRegisterScanDataIn_s
    connect \ScanDataOut_o \WordRegisterScanDataOut_s
    connect \ScanEnable_i \WordRegisterScanEnable_s
  end
  process $proc$../../verilog/sensorfsm.v:63$2
    assign $0\SensorFSM_State[1:0] \SensorFSM_State
    switch $logic_not$../../verilog/sensorfsm.v:65$3_Y
      case 1'1
        assign $0\SensorFSM_State[1:0] 2'00
      case 
        assign $0\SensorFSM_State[1:0] \SensorFSM_NextState
    end
    sync negedge \Reset_n_i
      update \SensorFSM_State $0\SensorFSM_State[1:0]
    sync posedge \Clk_i
      update \SensorFSM_State $0\SensorFSM_State[1:0]
  end
  process $proc$../../verilog/sensorfsm.v:75$4
    assign { } { }
    assign { } { }
    assign { } { }
    assign { } { }
    assign { } { }
    assign { } { }
    assign { } { }
    assign { } { }
    assign { } { }
    assign { } { }
    assign { } { }
    assign { } { }
    assign $0\CpuIntr_o[0:0] $1\CpuIntr_o[0:0]
    assign $0\SPIFSM_Start_o[0:0] $1\SPIFSM_Start_o[0:0]
    assign $0\SensorFSM_NextState[1:0] $1\SensorFSM_NextState[1:0]
    assign $0\SensorFSM_StoreNewValue[0:0] $1\SensorFSM_StoreNewValue[0:0]
    assign $0\SensorFSM_TimerEnable[0:0] $1\SensorFSM_TimerEnable[0:0]
    assign $0\SensorFSM_TimerPreset[0:0] $1\SensorFSM_TimerPreset[0:0]
    switch \SensorFSM_State
      case 2'00
        assign $1\CpuIntr_o[0:0] 1'0
        assign $1\SPIFSM_Start_o[0:0] 1'0
        assign { } { }
        assign $1\SensorFSM_StoreNewValue[0:0] 1'0
        assign { } { }
        assign { } { }
        assign $1\SensorFSM_NextState[1:0] $2\SensorFSM_NextState[1:0]
        assign $1\SensorFSM_TimerEnable[0:0] $2\SensorFSM_TimerEnable[0:0]
        assign $1\SensorFSM_TimerPreset[0:0] $2\SensorFSM_TimerPreset[0:0]
        switch $eq$../../verilog/sensorfsm.v:87$5_Y
          case 1'1
            assign { } { }
            assign { } { }
            assign { } { }
            assign $2\SensorFSM_NextState[1:0] 2'01
            assign $2\SensorFSM_TimerPreset[0:0] 1'0
            assign $2\SensorFSM_TimerEnable[0:0] 1'1
          case 
            assign $2\SensorFSM_NextState[1:0] \SensorFSM_State
            assign $2\SensorFSM_TimerEnable[0:0] 1'0
            assign $2\SensorFSM_TimerPreset[0:0] 1'1
        end
      case 2'01
        assign $1\CpuIntr_o[0:0] 1'0
        assign { } { }
        assign { } { }
        assign $1\SensorFSM_StoreNewValue[0:0] 1'0
        assign { } { }
        assign { } { }
        assign $1\SensorFSM_TimerPreset[0:0] 1'0
        assign $1\SensorFSM_TimerEnable[0:0] 1'1
        assign $1\SPIFSM_Start_o[0:0] $2\SPIFSM_Start_o[0:0]
        assign $1\SensorFSM_NextState[1:0] $3\SensorFSM_NextState[1:0]
        switch $eq$../../verilog/sensorfsm.v:97$6_Y
          case 1'1
            assign $2\SPIFSM_Start_o[0:0] 1'0
            assign { } { }
            assign $3\SensorFSM_NextState[1:0] 2'00
          case 
            assign { } { }
            assign { } { }
            assign $2\SPIFSM_Start_o[0:0] $3\SPIFSM_Start_o[0:0]
            assign $3\SensorFSM_NextState[1:0] $4\SensorFSM_NextState[1:0]
            switch $eq$../../verilog/sensorfsm.v:102$7_Y
              case 1'1
                assign { } { }
                assign { } { }
                assign $4\SensorFSM_NextState[1:0] 2'10
                assign $3\SPIFSM_Start_o[0:0] 1'1
              case 
                assign $3\SPIFSM_Start_o[0:0] 1'0
                assign $4\SensorFSM_NextState[1:0] \SensorFSM_State
            end
        end
      case 2'10
        assign $1\CpuIntr_o[0:0] 1'0
        assign $1\SPIFSM_Start_o[0:0] 1'0
        assign { } { }
        assign { } { }
        assign { } { }
        assign { } { }
        assign $1\SensorFSM_NextState[1:0] $5\SensorFSM_NextState[1:0]
        assign $1\SensorFSM_StoreNewValue[0:0] $2\SensorFSM_StoreNewValue[0:0]
        assign $1\SensorFSM_TimerEnable[0:0] $3\SensorFSM_TimerEnable[0:0]
        assign $1\SensorFSM_TimerPreset[0:0] $3\SensorFSM_TimerPreset[0:0]
        switch $eq$../../verilog/sensorfsm.v:109$8_Y
          case 1'1
            assign { } { }
            assign { } { }
            assign { } { }
            assign { } { }
            assign $5\SensorFSM_NextState[1:0] $6\SensorFSM_NextState[1:0]
            assign $2\SensorFSM_StoreNewValue[0:0] $3\SensorFSM_StoreNewValue[0:0]
            assign $3\SensorFSM_TimerEnable[0:0] $4\SensorFSM_TimerEnable[0:0]
            assign $3\SensorFSM_TimerPreset[0:0] $4\SensorFSM_TimerPreset[0:0]
            switch $eq$../../verilog/sensorfsm.v:111$9_Y
              case 1'1
                assign { } { }
                assign { } { }
                assign { } { }
                assign { } { }
                assign $6\SensorFSM_NextState[1:0] 2'11
                assign $4\SensorFSM_TimerPreset[0:0] 1'0
                assign $4\SensorFSM_TimerEnable[0:0] 1'1
                assign $3\SensorFSM_StoreNewValue[0:0] 1'1
              case 
                assign { } { }
                assign $3\SensorFSM_StoreNewValue[0:0] 1'0
                assign $4\SensorFSM_TimerEnable[0:0] 1'0
                assign $4\SensorFSM_TimerPreset[0:0] 1'1
                assign $6\SensorFSM_NextState[1:0] 2'01
            end
          case 
            assign $5\SensorFSM_NextState[1:0] \SensorFSM_State
            assign $2\SensorFSM_StoreNewValue[0:0] 1'0
            assign $3\SensorFSM_TimerEnable[0:0] 1'0
            assign $3\SensorFSM_TimerPreset[0:0] 1'1
        end
      case 2'11
        assign { } { }
        assign $1\SPIFSM_Start_o[0:0] 1'0
        assign { } { }
        assign $1\SensorFSM_StoreNewValue[0:0] 1'0
        assign { } { }
        assign { } { }
        assign $1\SensorFSM_TimerPreset[0:0] 1'1
        assign $1\SensorFSM_TimerEnable[0:0] 1'0
        assign $1\SensorFSM_NextState[1:0] 2'01
        assign $1\CpuIntr_o[0:0] 1'1
      case 
        assign $1\CpuIntr_o[0:0] 1'0
        assign $1\SPIFSM_Start_o[0:0] 1'0
        assign $1\SensorFSM_NextState[1:0] \SensorFSM_State
        assign $1\SensorFSM_StoreNewValue[0:0] 1'0
        assign $1\SensorFSM_TimerEnable[0:0] 1'0
        assign $1\SensorFSM_TimerPreset[0:0] 1'1
    end
    sync always
      update \CpuIntr_o $0\CpuIntr_o[0:0]
      update \SPIFSM_Start_o $0\SPIFSM_Start_o[0:0]
      update \SensorFSM_NextState $0\SensorFSM_NextState[1:0]
      update \SensorFSM_StoreNewValue $0\SensorFSM_StoreNewValue[0:0]
      update \SensorFSM_TimerEnable $0\SensorFSM_TimerEnable[0:0]
      update \SensorFSM_TimerPreset $0\SensorFSM_TimerPreset[0:0]
  end
  connect \Reset_s $not$../../verilog/sensorfsm.v:20$1_Y
  connect \ParamThreshold 16'0000000000011110
  connect \ParamCounterPreset 16'0000000000001010
  connect \SensorValue { \SPIFSM_Byte1_i \SPIFSM_Byte0_i }
  connect \SensorValue_o \Word0
  connect $eq$../../verilog/sensorfsm.v:102$7_Y \SensorFSM_TimerOvfl
  connect $eq$../../verilog/sensorfsm.v:109$8_Y \SPIFSM_Done_i
  connect $eq$../../verilog/sensorfsm.v:111$9_Y \SensorFSM_DiffTooLarge
  connect $eq$../../verilog/sensorfsm.v:87$5_Y \Enable_i
end
