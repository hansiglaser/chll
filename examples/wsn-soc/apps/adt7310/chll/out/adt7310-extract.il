# Generated by Yosys 0.3.0+ (git sha1 3b52121)

attribute \src "../../verilog/adt7310.v:1"
attribute \top 1
module \ADT7310

  wire $techmap\SPIFSM_1.$auto$opt_reduce.cc:126:opt_mux$2459

  attribute \src "../../../../counter32/verilog/counter32_rv1.v:12"
  wire width 16 $techmap\SPIFSM_1.$extract$\Counter32_RV1_Timer$2533.DH_s

  attribute \src "../../../../counter32/verilog/counter32_rv1.v:13"
  wire width 16 $techmap\SPIFSM_1.$extract$\Counter32_RV1_Timer$2533.DL_s

  attribute \src "../../../../counter32/verilog/counter32_rv1.v:14"
  wire $techmap\SPIFSM_1.$extract$\Counter32_RV1_Timer$2533.Overflow_s

  wire $techmap\SPIFSM_1.$procmux$297_CMP

  wire $techmap\SPIFSM_1.$procmux$302_CMP

  attribute \src "../../../../addsubcmp/verilog/addsubcmp_greater.v:8"
  wire $techmap\SensorFSM_1.$extract$\AddSubCmp_Greater_Direct$2538.Carry_s

  attribute \src "../../../../addsubcmp/verilog/addsubcmp_greater.v:7"
  wire width 16 $techmap\SensorFSM_1.$extract$\AddSubCmp_Greater_Direct$2538.D_s

  attribute \src "../../../../addsubcmp/verilog/addsubcmp_greater.v:11"
  wire $techmap\SensorFSM_1.$extract$\AddSubCmp_Greater_Direct$2538.Overflow_s

  attribute \src "../../../../addsubcmp/verilog/addsubcmp_greater.v:10"
  wire $techmap\SensorFSM_1.$extract$\AddSubCmp_Greater_Direct$2538.Sign_s

  attribute \src "../../../../addsubcmp/verilog/addsubcmp_greater.v:9"
  wire $techmap\SensorFSM_1.$extract$\AddSubCmp_Greater_Direct$2538.Zero_s

  attribute \src "../../../../counter/verilog/counter_rv1.v:14"
  wire width 16 $techmap\SensorFSM_1.$extract$\Counter_RV1_Timer$2532.D_s

  attribute \src "../../../../counter/verilog/counter_rv1.v:15"
  wire $techmap\SensorFSM_1.$extract$\Counter_RV1_Timer$2532.Overflow_s

  attribute \intersynth_conntype "Bit"
  attribute \intersynth_port "Outputs_o"
  attribute \src "../../verilog/adt7310.v:11"
  wire output 5 \ADT7310CS_n_o

  attribute \intersynth_port "Clk_i"
  attribute \src "../../verilog/adt7310.v:5"
  wire input 2 \Clk_i

  attribute \intersynth_conntype "Bit"
  attribute \intersynth_port "ReconfModuleIRQs_s"
  attribute \src "../../verilog/adt7310.v:9"
  wire output 4 \CpuIntr_o

  attribute \intersynth_conntype "Bit"
  attribute \intersynth_port "ReconfModuleIn_s"
  attribute \src "../../verilog/adt7310.v:7"
  wire input 3 \Enable_i

  attribute \intersynth_conntype "Word"
  attribute \intersynth_param "PeriodCounterPreset_i"
  attribute \src "../../verilog/adt7310.v:33"
  wire width 16 input 16 \PeriodCounterPreset_i

  attribute \intersynth_port "Reset_n_i"
  attribute \src "../../verilog/adt7310.v:3"
  wire input 1 \Reset_n_i

  attribute \intersynth_conntype "Word"
  attribute \intersynth_param "SPICounterPresetH_i"
  attribute \src "../../verilog/adt7310.v:27"
  wire width 16 input 13 \SPICounterPresetH_i

  attribute \intersynth_conntype "Word"
  attribute \intersynth_param "SPICounterPresetL_i"
  attribute \src "../../verilog/adt7310.v:29"
  wire width 16 input 14 \SPICounterPresetL_i

  attribute \src "../../verilog/spifsm.v:45"
  wire \SPIFSM_1.SPI_FSM_TimerEnable

  attribute \src "../../verilog/spifsm.v:43"
  wire \SPIFSM_1.SPI_FSM_TimerOvfl

  attribute \src "../../verilog/spifsm.v:44"
  wire \SPIFSM_1.SPI_FSM_TimerPreset

  attribute \src "../../verilog/spifsm.v:47"
  wire \SPIFSM_1.SPI_FSM_Wr0

  attribute \src "../../verilog/spifsm.v:46"
  wire \SPIFSM_1.SPI_FSM_Wr1

  attribute \keep 1
  attribute \src "../../verilog/adt7310.v:56"
  wire width 8 \SPIFSM_Byte0_s

  attribute \keep 1
  attribute \src "../../verilog/adt7310.v:58"
  wire width 8 \SPIFSM_Byte1_s

  attribute \keep 1
  attribute \src "../../verilog/adt7310.v:54"
  wire \SPIFSM_Done_s

  attribute \keep 1
  attribute \src "../../verilog/adt7310.v:52"
  wire \SPIFSM_Start_s

  attribute \intersynth_conntype "Bit"
  attribute \intersynth_port "SPI_CPHA"
  attribute \src "../../verilog/adt7310.v:39"
  wire output 19 \SPI_CPHA_o

  attribute \intersynth_conntype "Bit"
  attribute \intersynth_port "SPI_CPOL"
  attribute \src "../../verilog/adt7310.v:37"
  wire output 18 \SPI_CPOL_o

  attribute \intersynth_conntype "Byte"
  attribute \intersynth_port "SPI_DataOut"
  attribute \src "../../verilog/adt7310.v:13"
  wire width 8 input 6 \SPI_Data_i

  attribute \intersynth_conntype "Byte"
  attribute \intersynth_port "SPI_DataIn"
  attribute \src "../../verilog/adt7310.v:19"
  wire width 8 output 9 \SPI_Data_o

  attribute \intersynth_conntype "Bit"
  attribute \intersynth_port "SPI_FIFOEmpty"
  attribute \src "../../verilog/adt7310.v:23"
  wire input 11 \SPI_FIFOEmpty_i

  attribute \intersynth_conntype "Bit"
  attribute \intersynth_port "SPI_FIFOFull"
  attribute \src "../../verilog/adt7310.v:21"
  wire input 10 \SPI_FIFOFull_i

  attribute \intersynth_conntype "Bit"
  attribute \intersynth_port "SPI_LSBFE"
  attribute \src "../../verilog/adt7310.v:41"
  wire output 20 \SPI_LSBFE_o

  attribute \intersynth_conntype "Bit"
  attribute \intersynth_port "SPI_ReadNext"
  attribute \src "../../verilog/adt7310.v:17"
  wire output 8 \SPI_ReadNext_o

  attribute \intersynth_conntype "Bit"
  attribute \intersynth_port "SPI_Transmission"
  attribute \src "../../verilog/adt7310.v:25"
  wire input 12 \SPI_Transmission_i

  attribute \intersynth_conntype "Bit"
  attribute \intersynth_port "SPI_Write"
  attribute \src "../../verilog/adt7310.v:15"
  wire output 7 \SPI_Write_o

  attribute \src "../../verilog/sensorfsm.v:39"
  wire width 16 \SensorFSM_1.AbsDiffResult

  attribute \src "../../verilog/sensorfsm.v:33"
  wire \SensorFSM_1.SensorFSM_StoreNewValue

  attribute \src "../../verilog/sensorfsm.v:31"
  wire \SensorFSM_1.SensorFSM_TimerEnable

  attribute \src "../../verilog/sensorfsm.v:29"
  wire \SensorFSM_1.SensorFSM_TimerOvfl

  attribute \src "../../verilog/sensorfsm.v:30"
  wire \SensorFSM_1.SensorFSM_TimerPreset

  attribute \src "../../verilog/sensorfsm.v:37"
  wire width 16 \SensorFSM_1.SensorValue

  attribute \intersynth_conntype "Word"
  attribute \intersynth_param "SensorValue_o"
  attribute \src "../../verilog/adt7310.v:35"
  wire width 16 output 17 \SensorValue_o

  attribute \intersynth_conntype "Word"
  attribute \intersynth_param "Threshold_i"
  attribute \src "../../verilog/adt7310.v:31"
  wire width 16 input 15 \Threshold_i

  cell \Byte2Word $extract$\Byte2Word$2543
    connect \H_i \SPIFSM_Byte1_s
    connect \L_i \SPIFSM_Byte0_s
    connect \Y_o \SensorFSM_1.SensorValue
  end

  cell \ByteMuxQuad $techmap\SPIFSM_1.$extract$\ByteMuxQuad$2539
    connect \A_i 8'00001000
    connect \B_i 8'11111111
    connect \C_i 8'01010000
    connect \D_i 8'00100000
    connect \SAB_i $techmap\SPIFSM_1.$auto$opt_reduce.cc:126:opt_mux$2459
    connect \SC_i $techmap\SPIFSM_1.$procmux$297_CMP
    connect \SD_i $techmap\SPIFSM_1.$procmux$302_CMP
    connect \Y_o \SPI_Data_o
  end

  cell \ByteRegister $techmap\SPIFSM_1.$extract$\ByteRegister$2536
    connect \Clk_i \Clk_i
    connect \D_i \SPI_Data_i
    connect \Enable_i \SPIFSM_1.SPI_FSM_Wr0
    connect \Q_o \SPIFSM_Byte0_s
    connect \Reset_n_i \Reset_n_i
  end

  cell \ByteRegister $techmap\SPIFSM_1.$extract$\ByteRegister$2537
    connect \Clk_i \Clk_i
    connect \D_i \SPI_Data_i
    connect \Enable_i \SPIFSM_1.SPI_FSM_Wr1
    connect \Q_o \SPIFSM_Byte1_s
    connect \Reset_n_i \Reset_n_i
  end

  attribute \src "../../../../counter32/verilog/counter32_rv1.v:19"
  cell \Counter32 $techmap\SPIFSM_1.$extract$\Counter32_RV1_Timer$2533.ThisCounter
    connect \Clk_i \Clk_i
    connect \DH_o $techmap\SPIFSM_1.$extract$\Counter32_RV1_Timer$2533.DH_s
    connect \DL_o $techmap\SPIFSM_1.$extract$\Counter32_RV1_Timer$2533.DL_s
    connect \Direction_i 1'1
    connect \Enable_i \SPIFSM_1.SPI_FSM_TimerEnable
    connect \Overflow_o $techmap\SPIFSM_1.$extract$\Counter32_RV1_Timer$2533.Overflow_s
    connect \PresetValH_i \SPICounterPresetH_i
    connect \PresetValL_i \SPICounterPresetL_i
    connect \Preset_i \SPIFSM_1.SPI_FSM_TimerPreset
    connect \ResetSig_i 1'0
    connect \Reset_n_i \Reset_n_i
    connect \Zero_o \SPIFSM_1.SPI_FSM_TimerOvfl
  end

  attribute \fsm_encoding "auto"
  attribute \src "../../verilog/spifsm.v:41"
  cell $fsm $techmap\SPIFSM_1.$fsm$\SPI_FSM_State$2481
    parameter \ARST_POLARITY 1'0
    parameter \CLK_POLARITY 1'1
    parameter \CTRL_IN_WIDTH 3
    parameter \CTRL_OUT_WIDTH 11
    parameter \NAME "\\SPI_FSM_State"
    parameter \STATE_BITS 4
    parameter \STATE_NUM 11
    parameter \STATE_NUM_LOG2 4
    parameter \STATE_RST 0
    parameter \STATE_TABLE 44'01110011010110010001011010100010010010000000
    parameter \TRANS_NUM 15
    parameter \TRANS_TABLE 330'1010-1-1010000010000001010-0-0001100010000001001---0010100001000001000---0101010010100000111---0100100110001000110---0011010010000100101---1010010010100000100---0000001110000000011-0-1001100010000000011-1-0011000010000000010--11000010010000010010--00010000101000010001---01111001100100000001--01100100100000000000--000000111000000
    connect \ARST \Reset_n_i
    connect \CLK \Clk_i
    connect \CTRL_IN { \SPIFSM_Start_s \SPI_Transmission_i \SPIFSM_1.SPI_FSM_TimerOvfl }
    connect \CTRL_OUT { \SPI_ReadNext_o \SPI_Write_o \SPIFSM_Done_s \ADT7310CS_n_o \SPIFSM_1.SPI_FSM_TimerPreset \SPIFSM_1.SPI_FSM_TimerEnable $techmap\SPIFSM_1.$auto$opt_reduce.cc:126:opt_mux$2459 \SPIFSM_1.SPI_FSM_Wr1 \SPIFSM_1.SPI_FSM_Wr0 $techmap\SPIFSM_1.$procmux$302_CMP $techmap\SPIFSM_1.$procmux$297_CMP }
  end

  cell \AbsDiff $techmap\SensorFSM_1.$extract$\AbsDiff$2534
    connect \A_i \SensorFSM_1.SensorValue
    connect \B_i \SensorValue_o
    connect \D_o \SensorFSM_1.AbsDiffResult
  end

  attribute \src "../../../../addsubcmp/verilog/addsubcmp_greater.v:13"
  cell \AddSubCmp $techmap\SensorFSM_1.$extract$\AddSubCmp_Greater_Direct$2538.ThisAddSubCmp
    connect \A_i \SensorFSM_1.AbsDiffResult
    connect \AddOrSub_i 1'1
    connect \B_i \Threshold_i
    connect \Carry_i 1'0
    connect \Carry_o $techmap\SensorFSM_1.$extract$\AddSubCmp_Greater_Direct$2538.Carry_s
    connect \D_o $techmap\SensorFSM_1.$extract$\AddSubCmp_Greater_Direct$2538.D_s
    connect \Overflow_o $techmap\SensorFSM_1.$extract$\AddSubCmp_Greater_Direct$2538.Overflow_s
    connect \Sign_o $techmap\SensorFSM_1.$extract$\AddSubCmp_Greater_Direct$2538.Sign_s
    connect \Zero_o $techmap\SensorFSM_1.$extract$\AddSubCmp_Greater_Direct$2538.Zero_s
  end

  attribute \src "../../../../counter/verilog/counter_rv1.v:20"
  cell \Counter $techmap\SensorFSM_1.$extract$\Counter_RV1_Timer$2532.ThisCounter
    connect \Clk_i \Clk_i
    connect \D_o $techmap\SensorFSM_1.$extract$\Counter_RV1_Timer$2532.D_s
    connect \Direction_i 1'1
    connect \Enable_i \SensorFSM_1.SensorFSM_TimerEnable
    connect \Overflow_o $techmap\SensorFSM_1.$extract$\Counter_RV1_Timer$2532.Overflow_s
    connect \PresetVal_i \PeriodCounterPreset_i
    connect \Preset_i \SensorFSM_1.SensorFSM_TimerPreset
    connect \ResetSig_i 1'0
    connect \Reset_n_i \Reset_n_i
    connect \Zero_o \SensorFSM_1.SensorFSM_TimerOvfl
  end

  cell \WordRegister $techmap\SensorFSM_1.$extract$\WordRegister$2535
    connect \Clk_i \Clk_i
    connect \D_i \SensorFSM_1.SensorValue
    connect \Enable_i \SensorFSM_1.SensorFSM_StoreNewValue
    connect \Q_o \SensorValue_o
    connect \Reset_n_i \Reset_n_i
  end

  attribute \fsm_encoding "auto"
  attribute \src "../../verilog/sensorfsm.v:27"
  cell $fsm $techmap\SensorFSM_1.$fsm$\SensorFSM_State$2494
    parameter \ARST_POLARITY 1'0
    parameter \CLK_POLARITY 1'1
    parameter \CTRL_IN_WIDTH 5
    parameter \CTRL_OUT_WIDTH 5
    parameter \NAME "\\SensorFSM_State"
    parameter \STATE_BITS 2
    parameter \STATE_NUM 4
    parameter \STATE_NUM_LOG2 3
    parameter \STATE_RST 0
    parameter \STATE_TABLE 8'11011000
    parameter \TRANS_NUM 10
    parameter \TRANS_TABLE 160'011-----01011000010--0-101000100010--1-100100101010----00000010000101-1-011001100011--1-0100100000100-1-01001000001---0-00101000000----101000100000----000001000
    connect \ARST \Reset_n_i
    connect \CLK \Clk_i
    connect \CTRL_IN { $techmap\SensorFSM_1.$extract$\AddSubCmp_Greater_Direct$2538.Zero_s $techmap\SensorFSM_1.$extract$\AddSubCmp_Greater_Direct$2538.Carry_s \SensorFSM_1.SensorFSM_TimerOvfl \SPIFSM_Done_s \Enable_i }
    connect \CTRL_OUT { \CpuIntr_o \SensorFSM_1.SensorFSM_TimerPreset \SensorFSM_1.SensorFSM_TimerEnable \SensorFSM_1.SensorFSM_StoreNewValue \SPIFSM_Start_s }
  end

  connect \SPI_CPHA_o 1'1
  connect \SPI_CPOL_o 1'1
  connect \SPI_LSBFE_o 1'0
end
