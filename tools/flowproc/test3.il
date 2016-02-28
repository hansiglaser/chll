attribute \src "../../verilog/sensorfsm.v:3"
module \SensorFSM
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
  attribute \fsm_encoding "auto"
  attribute \src "../../verilog/sensorfsm.v:27"
  cell $fsm $fsm$\SensorFSM_State$465
    parameter \ARST_POLARITY 1'0
    parameter \CLK_POLARITY 1'1
    parameter \CTRL_IN_WIDTH 4
    parameter \CTRL_OUT_WIDTH 5
    parameter \NAME "\SensorFSM_State"
    parameter \STATE_BITS 2
    parameter \STATE_NUM 4
    parameter \STATE_NUM_LOG2 3
    parameter \STATE_RST 0
    parameter \STATE_TABLE 8'11011000
    parameter \TRANS_NUM 9
    parameter \TRANS_TABLE 135'011----010110000100--1010001000101--100100101010---000000100001-11-01100110001-01-01001000001--0-00101000000---101000100000---000001000
    connect \ARST \Reset_n_i
    connect \CLK \Clk_i
    connect \CTRL_IN { \SensorFSM_TimerOvfl \SensorFSM_DiffTooLarge \SPIFSM_Done_i \Enable_i }
    connect \CTRL_OUT { \CpuIntr_o \SensorFSM_TimerPreset \SensorFSM_TimerEnable \SensorFSM_StoreNewValue \SPIFSM_Start_o }
  end
  attribute \src "../../verilog/sensorfsm.v:20"
  cell $not $not$../../verilog/sensorfsm.v:20$1
    parameter \A_SIGNED 0
    parameter \A_WIDTH 1
    parameter \Y_WIDTH 1
    connect \A \Reset_n_i
    connect \Y \Reset_s
  end
  attribute \src "../../verilog/sensorfsm.v:189"
  cell \AbsDiff \AbsDiff_1
    parameter \Width 16
    connect \A_i { \SPIFSM_Byte1_i \SPIFSM_Byte0_i }
    connect \B_i \SensorValue_o
    connect \D_o \AbsDiffResult
  end
  attribute \src "../../verilog/sensorfsm.v:197"
  cell \AddSubCmp \AddSubCmp_1
    parameter \Width 16
    connect \A_i 16'0000000000011110
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
    connect \PresetVal_i 16'0000000000001010
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
    connect \D_i { \SPIFSM_Byte1_i \SPIFSM_Byte0_i }
    connect \Enable_i \SensorFSM_StoreNewValue
    connect \Q_o \SensorValue_o
    connect \Reset_i \Reset_s
    connect \ScanClk_i \WordRegisterScanClk_s
    connect \ScanDataIn_i \WordRegisterScanDataIn_s
    connect \ScanDataOut_o \WordRegisterScanDataOut_s
    connect \ScanEnable_i \WordRegisterScanEnable_s
  end
  connect \SensorValue { \SPIFSM_Byte1_i \SPIFSM_Byte0_i }
  connect \Word0 \SensorValue_o
end
