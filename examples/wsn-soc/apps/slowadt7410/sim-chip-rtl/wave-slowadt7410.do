onerror {resume}
quietly WaveActivateNextPane {} 0
add wave -noupdate /chip_tb/Reset_n_i
add wave -noupdate /chip_tb/Clk_i
add wave -noupdate /chip_tb/P1_b
add wave -noupdate /chip_tb/P2_b
add wave -noupdate /chip_tb/Inputs_i
add wave -noupdate /chip_tb/Outputs_o
add wave -noupdate /chip_tb/DUT/core_1/I2CSCL_o
add wave -noupdate /chip_tb/DUT/core_1/I2CSDA_i
add wave -noupdate /chip_tb/DUT/core_1/I2CSDA_o
add wave -noupdate -divider ADT7410
add wave -noupdate /chip_tb/adt7410_1/scl_i
add wave -noupdate /chip_tb/adt7410_1/sda_io
add wave -noupdate /chip_tb/CT_n_s
add wave -noupdate /chip_tb/INT_n_s
add wave -noupdate /chip_tb/Temp_s
add wave -noupdate /chip_tb/adt7410_1/Registers
add wave -noupdate -radix unsigned /chip_tb/adt7410_1/temp_reg
add wave -noupdate /chip_tb/adt7410_1/OpMode
add wave -noupdate /chip_tb/DUT/core_1/MyReconfigLogic_0/SlowADT7410_0/Enable_i
add wave -noupdate -radix unsigned /chip_tb/DUT/core_1/MyReconfigLogic_0/SlowADT7410_0/WaitCounterPresetH_i
add wave -noupdate -radix unsigned /chip_tb/DUT/core_1/MyReconfigLogic_0/SlowADT7410_0/WaitCounterPresetL_i
add wave -noupdate -radix unsigned /chip_tb/DUT/core_1/MyReconfigLogic_0/SlowADT7410_0/Threshold_i
add wave -noupdate -radix unsigned /chip_tb/DUT/core_1/MyReconfigLogic_0/SlowADT7410_0/PeriodCounterPresetH_i
add wave -noupdate -radix unsigned /chip_tb/DUT/core_1/MyReconfigLogic_0/SlowADT7410_0/PeriodCounterPresetL_i
add wave -noupdate -radix unsigned /chip_tb/DUT/core_1/MyReconfigLogic_0/SlowADT7410_0/SensorFSM_1/SensorValue
add wave -noupdate /chip_tb/DUT/core_1/MyReconfigLogic_0/SlowADT7410_0/I2CFSM_Byte0_s
add wave -noupdate /chip_tb/DUT/core_1/MyReconfigLogic_0/SlowADT7410_0/I2CFSM_Byte1_s
add wave -noupdate -radix unsigned /chip_tb/DUT/core_1/MyReconfigLogic_0/SlowADT7410_0/SensorValue_o
add wave -noupdate /chip_tb/DUT/core_1/MyReconfigLogic_0/SlowADT7410_0/I2CFSM_Done_s
add wave -noupdate /chip_tb/DUT/core_1/MyReconfigLogic_0/SlowADT7410_0/CpuIntr_o
add wave -noupdate /chip_tb/ExtNames_1/I2CFSM_Done
add wave -noupdate -radix decimal /chip_tb/SensorValue_o
add wave -noupdate /chip_tb/SensorValue_real
add wave -noupdate /chip_tb/CpuIntr_o
add wave -noupdate -divider OpenMSP430
add wave -noupdate -radix hexadecimal /chip_tb/DUT/core_1/openMSP430_0/execution_unit_0/pc
add wave -noupdate -radix unsigned /chip_tb/DUT/core_1/openMSP430_0/execution_unit_0/register_file_0/r15
add wave -noupdate -radix hexadecimal /chip_tb/DUT/core_1/openMSP430_0/pmem_addr
add wave -noupdate /chip_tb/DUT/core_1/openMSP430_0/pmem_cen
add wave -noupdate -radix hexadecimal /chip_tb/DUT/core_1/openMSP430_0/pmem_din
add wave -noupdate /chip_tb/DUT/core_1/openMSP430_0/pmem_wen
add wave -noupdate -radix hexadecimal /chip_tb/DUT/core_1/PMem_Addr_s
add wave -noupdate /chip_tb/DUT/core_1/PMem_En_n_s
add wave -noupdate -radix hexadecimal /chip_tb/DUT/core_1/PMem_DIn_s
add wave -noupdate /chip_tb/DUT/core_1/PMem_Wr_n_s
add wave -noupdate -radix hexadecimal /chip_tb/DUT/core_1/PMem_DOut_s
add wave -noupdate -radix hexadecimal /chip_tb/DUT/core_1/PMem_0/mem
add wave -noupdate -radix hexadecimal /chip_tb/DUT/core_1/openMSP430_0/dmem_addr
add wave -noupdate /chip_tb/DUT/core_1/openMSP430_0/dmem_cen
add wave -noupdate -radix hexadecimal /chip_tb/DUT/core_1/openMSP430_0/dmem_din
add wave -noupdate /chip_tb/DUT/core_1/openMSP430_0/dmem_wen
add wave -noupdate -radix hexadecimal /chip_tb/DUT/core_1/DMem_Addr_s
add wave -noupdate /chip_tb/DUT/core_1/DMem_En_n_s
add wave -noupdate -radix hexadecimal /chip_tb/DUT/core_1/DMem_DIn_s
add wave -noupdate /chip_tb/DUT/core_1/DMem_Wr_n_s
add wave -noupdate -radix hexadecimal /chip_tb/DUT/core_1/DMem_DOut_s
add wave -noupdate /chip_tb/DUT/core_1/openMSP430_0/per_addr
add wave -noupdate /chip_tb/DUT/core_1/openMSP430_0/per_din
add wave -noupdate /chip_tb/DUT/core_1/openMSP430_0/per_we
add wave -noupdate /chip_tb/DUT/core_1/openMSP430_0/per_en
add wave -noupdate /chip_tb/DUT/core_1/Per_Addr_s
add wave -noupdate /chip_tb/DUT/core_1/Per_DIn_s
add wave -noupdate /chip_tb/DUT/core_1/Per_DOut_s
add wave -noupdate /chip_tb/DUT/core_1/Per_Wr_s
add wave -noupdate /chip_tb/DUT/core_1/Per_En_s
add wave -noupdate /chip_tb/DUT/core_1/IRQ_s
add wave -noupdate /chip_tb/DUT/core_1/IRQ_Ack_s
add wave -noupdate /chip_tb/DUT/core_1/ReconfModuleIRQs_s
add wave -noupdate /chip_tb/DUT/core_1/ReconfModuleIn_s
add wave -noupdate /chip_tb/DUT/core_1/ReconfModuleOut_s
add wave -noupdate -divider CfgIntf
add wave -noupdate -radix hexadecimal /chip_tb/DUT/core_1/MyReconfigLogic_0/CfgIntf_0/PerAddr_i
add wave -noupdate -radix hexadecimal /chip_tb/DUT/core_1/MyReconfigLogic_0/CfgIntf_0/PerDIn_i
add wave -noupdate -radix hexadecimal /chip_tb/DUT/core_1/MyReconfigLogic_0/CfgIntf_0/PerDOut_o
add wave -noupdate /chip_tb/DUT/core_1/MyReconfigLogic_0/CfgIntf_0/PerWr_i
add wave -noupdate /chip_tb/DUT/core_1/MyReconfigLogic_0/CfgIntf_0/PerEn_i
add wave -noupdate /chip_tb/DUT/core_1/MyReconfigLogic_0/CfgIntf_0/ChainNum
add wave -noupdate -radix unsigned /chip_tb/DUT/core_1/MyReconfigLogic_0/CfgIntf_0/BitstreamSize
add wave -noupdate /chip_tb/DUT/core_1/MyReconfigLogic_0/CfgIntf_0/Bitstream
add wave -noupdate /chip_tb/DUT/core_1/MyReconfigLogic_0/CfgIntf_0/CfgMode_o
add wave -noupdate /chip_tb/DUT/core_1/MyReconfigLogic_0/CfgIntf_0/CfgShift_o
add wave -noupdate /chip_tb/DUT/core_1/MyReconfigLogic_0/CfgIntf_0/CfgDataOut_o
add wave -noupdate /chip_tb/DUT/core_1/MyReconfigLogic_0/CfgIntf_0/CfgDataIn_i
add wave -noupdate -divider ParamIntf
add wave -noupdate /chip_tb/DUT/core_1/MyReconfigLogic_0/ParamIntf_0/PerAddr_i
add wave -noupdate /chip_tb/DUT/core_1/MyReconfigLogic_0/ParamIntf_0/PerDIn_i
add wave -noupdate /chip_tb/DUT/core_1/MyReconfigLogic_0/ParamIntf_0/PerDOut_o
add wave -noupdate /chip_tb/DUT/core_1/MyReconfigLogic_0/ParamIntf_0/PerWr_i
add wave -noupdate /chip_tb/DUT/core_1/MyReconfigLogic_0/ParamIntf_0/PerEn_i
add wave -noupdate /chip_tb/DUT/core_1/MyReconfigLogic_0/ParamIntf_0/ParamWrAddr_o
add wave -noupdate /chip_tb/DUT/core_1/MyReconfigLogic_0/ParamIntf_0/ParamWrData_o
add wave -noupdate /chip_tb/DUT/core_1/MyReconfigLogic_0/ParamIntf_0/ParamWr_o
add wave -noupdate /chip_tb/DUT/core_1/MyReconfigLogic_0/ParamIntf_0/ParamRdAddr_o
add wave -noupdate /chip_tb/DUT/core_1/MyReconfigLogic_0/ParamIntf_0/ParamRdData_i
add wave -noupdate /chip_tb/DUT/core_1/MyReconfigLogic_0/Params_s
add wave -noupdate -divider InterSynth
add wave -noupdate -divider {I2C Master}
add wave -noupdate /chip_tb/DUT/core_1/i2c_master_1/F100_400_n_i
add wave -noupdate /chip_tb/DUT/core_1/i2c_master_1/Divider800_i
add wave -noupdate /chip_tb/DUT/core_1/i2c_master_1/StartProcess_i
add wave -noupdate /chip_tb/DUT/core_1/i2c_master_1/ReceiveSend_n_i
add wave -noupdate /chip_tb/DUT/core_1/i2c_master_1/Busy_o
add wave -noupdate /chip_tb/DUT/core_1/i2c_master_1/ReadCount_i
add wave -noupdate /chip_tb/DUT/core_1/i2c_master_1/FIFOReadNext_i
add wave -noupdate /chip_tb/DUT/core_1/i2c_master_1/FIFOWrite_i
add wave -noupdate /chip_tb/DUT/core_1/i2c_master_1/FIFOEmpty_o
add wave -noupdate /chip_tb/DUT/core_1/i2c_master_1/FIFOFull_o
add wave -noupdate /chip_tb/DUT/core_1/i2c_master_1/Data_i
add wave -noupdate /chip_tb/DUT/core_1/i2c_master_1/Data_o
add wave -noupdate /chip_tb/DUT/core_1/i2c_master_1/ErrAck_i
add wave -noupdate /chip_tb/DUT/core_1/i2c_master_1/ErrBusColl_o
add wave -noupdate /chip_tb/DUT/core_1/i2c_master_1/ErrFIFOFull_o
add wave -noupdate /chip_tb/DUT/core_1/i2c_master_1/ErrGotNAck_o
add wave -noupdate /chip_tb/DUT/core_1/i2c_master_1/ErrCoreBusy_o
add wave -noupdate /chip_tb/DUT/core_1/i2c_master_1/ErrFIFOEmpty_o
add wave -noupdate /chip_tb/DUT/core_1/i2c_master_1/ErrCoreStopped_o
add wave -noupdate /chip_tb/DUT/core_1/i2c_master_1/ErrDevNotPresent_o
add wave -noupdate /chip_tb/DUT/core_1/i2c_master_1/ErrReadCountZero_o
add wave -noupdate /chip_tb/DUT/core_1/i2c_master_1/SDA_i
add wave -noupdate /chip_tb/DUT/core_1/i2c_master_1/SDA_o
add wave -noupdate /chip_tb/DUT/core_1/i2c_master_1/SCL_o
TreeUpdate [SetDefaultTree]
configure wave -namecolwidth 248
configure wave -valuecolwidth 100
configure wave -justifyvalue left
configure wave -signalnamewidth 0
configure wave -snapdistance 10
configure wave -datasetprefix 0
configure wave -rowmargin 4
configure wave -childrowmargin 2
configure wave -gridoffset 0
configure wave -gridperiod 1
configure wave -griddelta 40
configure wave -timeline 0
configure wave -timelineunits ms
update
WaveRestoreZoom {0 ps} {2990999550 ns}
