attribute \src "../../verilog/sensorfsm.v:3"
module \SensorFSM
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
  wire $3\SensorFSM_TimerPreset[0:0]
  attribute \src "../../verilog/sensorfsm.v:75"
  wire width 2 $4\SensorFSM_NextState[1:0]
  attribute \src "../../verilog/sensorfsm.v:75"
  wire $4\SensorFSM_TimerPreset[0:0]
  attribute \src "../../verilog/sensorfsm.v:75"
  wire width 2 $5\SensorFSM_NextState[1:0]
  attribute \src "../../verilog/sensorfsm.v:75"
  wire width 2 $6\SensorFSM_NextState[1:0]
  wire $auto$opt_reduce.cc:123:opt_mux$463
  attribute \src "../../verilog/sensorfsm.v:97"
  wire $eq$../../verilog/sensorfsm.v:97$6_Y
  wire $procmux$101_CMP
  wire $procmux$107_CMP
  wire $procmux$111_CMP
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
  cell $reduce_or $auto$opt_reduce.cc:127:opt_mux$464
    parameter \A_WIDTH 3
    parameter \Y_WIDTH 1
    connect \A { $procmux$111_CMP $procmux$107_CMP $procmux$101_CMP }
    connect \Y $auto$opt_reduce.cc:123:opt_mux$463
  end
  attribute \src "../../verilog/sensorfsm.v:97"
  cell $not $eq$../../verilog/sensorfsm.v:97$6
    parameter \A_SIGNED 0
    parameter \A_WIDTH 1
    parameter \Y_WIDTH 1
    connect \A \Enable_i
    connect \Y $eq$../../verilog/sensorfsm.v:97$6_Y
  end
  attribute \src "../../verilog/sensorfsm.v:20"
  cell $not $not$../../verilog/sensorfsm.v:20$1
    parameter \A_SIGNED 0
    parameter \A_WIDTH 1
    parameter \Y_WIDTH 1
    connect \A \Reset_n_i
    connect \Y \Reset_s
  end
  cell $adff $procdff$462
    parameter \ARST_POLARITY 1'0
    parameter \ARST_VALUE 2'00
    parameter \CLK_POLARITY 1'1
    parameter \WIDTH 2
    connect \ARST \Reset_n_i
    connect \CLK \Clk_i
    connect \D \SensorFSM_NextState
    connect \Q \SensorFSM_State
  end
  cell $mux $procmux$100
    parameter \WIDTH 1
    connect \A 1'1
    connect \B 1'0
    connect \S $auto$opt_reduce.cc:123:opt_mux$463
    connect \Y \CpuIntr_o
  end
  cell $eq $procmux$101_CMP0
    parameter \A_SIGNED 0
    parameter \A_WIDTH 2
    parameter \B_SIGNED 0
    parameter \B_WIDTH 2
    parameter \Y_WIDTH 1
    connect \A \SensorFSM_State
    connect \B 2'10
    connect \Y $procmux$101_CMP
  end
  cell $eq $procmux$107_CMP0
    parameter \A_SIGNED 0
    parameter \A_WIDTH 2
    parameter \B_SIGNED 0
    parameter \B_WIDTH 2
    parameter \Y_WIDTH 1
    connect \A \SensorFSM_State
    connect \B 2'01
    connect \Y $procmux$107_CMP
  end
  cell $eq $procmux$111_CMP0
    parameter \A_SIGNED 0
    parameter \A_WIDTH 2
    parameter \B_SIGNED 0
    parameter \B_WIDTH 2
    parameter \Y_WIDTH 1
    connect \A \SensorFSM_State
    connect \B 2'00
    connect \Y $procmux$111_CMP
  end
  cell $mux $procmux$122
    parameter \WIDTH 1
    connect \A 1'0
    connect \B $2\SPIFSM_Start_o[0:0]
    connect \S $procmux$107_CMP
    connect \Y \SPIFSM_Start_o
  end
  cell $mux $procmux$133
    parameter \WIDTH 1
    connect \A 1'0
    connect \B $2\SensorFSM_StoreNewValue[0:0]
    connect \S $procmux$101_CMP
    connect \Y \SensorFSM_StoreNewValue
  end
  cell $pmux $procmux$150
    parameter \S_WIDTH 3
    parameter \WIDTH 2
    connect \A 2'01
    connect \B { $2\SensorFSM_NextState[1:0] $3\SensorFSM_NextState[1:0] $5\SensorFSM_NextState[1:0] }
    connect \S { $procmux$111_CMP $procmux$107_CMP $procmux$101_CMP }
    connect \Y \SensorFSM_NextState
  end
  cell $pmux $procmux$167
    parameter \S_WIDTH 3
    parameter \WIDTH 1
    connect \A 1'0
    connect \B { $2\SensorFSM_TimerEnable[0:0] 1'1 $2\SensorFSM_StoreNewValue[0:0] }
    connect \S { $procmux$111_CMP $procmux$107_CMP $procmux$101_CMP }
    connect \Y \SensorFSM_TimerEnable
  end
  cell $pmux $procmux$184
    parameter \S_WIDTH 3
    parameter \WIDTH 1
    connect \A 1'1
    connect \B { $2\SensorFSM_TimerPreset[0:0] 1'0 $3\SensorFSM_TimerPreset[0:0] }
    connect \S { $procmux$111_CMP $procmux$107_CMP $procmux$101_CMP }
    connect \Y \SensorFSM_TimerPreset
  end
  cell $mux $procmux$208
    parameter \WIDTH 2
    connect \A \SensorFSM_State
    connect \B 2'01
    connect \S \Enable_i
    connect \Y $2\SensorFSM_NextState[1:0]
  end
  cell $mux $procmux$224
    parameter \WIDTH 1
    connect \A 1'1
    connect \B 1'0
    connect \S \Enable_i
    connect \Y $2\SensorFSM_TimerPreset[0:0]
  end
  cell $mux $procmux$240
    parameter \WIDTH 1
    connect \A 1'0
    connect \B 1'1
    connect \S \Enable_i
    connect \Y $2\SensorFSM_TimerEnable[0:0]
  end
  cell $mux $procmux$253
    parameter \WIDTH 1
    connect \A $3\SPIFSM_Start_o[0:0]
    connect \B 1'0
    connect \S $eq$../../verilog/sensorfsm.v:97$6_Y
    connect \Y $2\SPIFSM_Start_o[0:0]
  end
  cell $mux $procmux$270
    parameter \WIDTH 2
    connect \A $4\SensorFSM_NextState[1:0]
    connect \B 2'00
    connect \S $eq$../../verilog/sensorfsm.v:97$6_Y
    connect \Y $3\SensorFSM_NextState[1:0]
  end
  cell $mux $procmux$285
    parameter \WIDTH 2
    connect \A \SensorFSM_State
    connect \B 2'10
    connect \S \SensorFSM_TimerOvfl
    connect \Y $4\SensorFSM_NextState[1:0]
  end
  cell $mux $procmux$303
    parameter \WIDTH 1
    connect \A 1'0
    connect \B 1'1
    connect \S \SensorFSM_TimerOvfl
    connect \Y $3\SPIFSM_Start_o[0:0]
  end
  cell $mux $procmux$318
    parameter \WIDTH 2
    connect \A \SensorFSM_State
    connect \B $6\SensorFSM_NextState[1:0]
    connect \S \SPIFSM_Done_i
    connect \Y $5\SensorFSM_NextState[1:0]
  end
  cell $mux $procmux$336
    parameter \WIDTH 1
    connect \A 1'0
    connect \B $3\SensorFSM_StoreNewValue[0:0]
    connect \S \SPIFSM_Done_i
    connect \Y $2\SensorFSM_StoreNewValue[0:0]
  end
  cell $mux $procmux$372
    parameter \WIDTH 1
    connect \A 1'1
    connect \B $4\SensorFSM_TimerPreset[0:0]
    connect \S \SPIFSM_Done_i
    connect \Y $3\SensorFSM_TimerPreset[0:0]
  end
  cell $mux $procmux$389
    parameter \WIDTH 2
    connect \A 2'01
    connect \B 2'11
    connect \S \SensorFSM_DiffTooLarge
    connect \Y $6\SensorFSM_NextState[1:0]
  end
  cell $mux $procmux$408
    parameter \WIDTH 1
    connect \A 1'1
    connect \B 1'0
    connect \S \SensorFSM_DiffTooLarge
    connect \Y $4\SensorFSM_TimerPreset[0:0]
  end
  cell $mux $procmux$427
    parameter \WIDTH 1
    connect \A 1'0
    connect \B 1'1
    connect \S \SensorFSM_DiffTooLarge
    connect \Y $3\SensorFSM_StoreNewValue[0:0]
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
