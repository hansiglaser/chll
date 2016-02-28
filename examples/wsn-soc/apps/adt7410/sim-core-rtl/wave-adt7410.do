onerror {resume}
quietly WaveActivateNextPane {} 0
add wave -noupdate /core_tb/Reset_n_i
add wave -noupdate /core_tb/Clk_i
add wave -noupdate /core_tb/P1_DOut_o
add wave -noupdate /core_tb/P1_En_o
add wave -noupdate /core_tb/P1_DIn_i
add wave -noupdate /core_tb/P2_DOut_o
add wave -noupdate /core_tb/P2_En_o
add wave -noupdate /core_tb/P2_DIn_i
add wave -noupdate /core_tb/Inputs_i
add wave -noupdate /core_tb/Outputs_o
add wave -noupdate /core_tb/DUT/I2CSCL_o
add wave -noupdate /core_tb/DUT/I2CSDA_i
add wave -noupdate /core_tb/DUT/I2CSDA_o
add wave -noupdate -divider ADT7410
add wave -noupdate /core_tb/adt7410_1/scl_i
add wave -noupdate /core_tb/adt7410_1/sda_io
add wave -noupdate /core_tb/CT_n_s
add wave -noupdate /core_tb/INT_n_s
add wave -noupdate /core_tb/Temp_s
add wave -noupdate /core_tb/adt7410_1/Registers
add wave -noupdate -radix unsigned /core_tb/adt7410_1/temp_reg
add wave -noupdate /core_tb/adt7410_1/OpMode
add wave -noupdate -radix unsigned /core_tb/DUT/MyReconfigLogic_0/ADT7410_0/WaitCounterPreset_i
add wave -noupdate -radix unsigned /core_tb/DUT/MyReconfigLogic_0/ADT7410_0/Threshold_i
add wave -noupdate -radix unsigned /core_tb/DUT/MyReconfigLogic_0/ADT7410_0/PeriodCounterPreset_i
add wave -noupdate -radix unsigned /core_tb/DUT/MyReconfigLogic_0/ADT7410_0/SensorFSM_1/SensorValue
add wave -noupdate /core_tb/DUT/MyReconfigLogic_0/ADT7410_0/I2CFSM_Byte0_s
add wave -noupdate /core_tb/DUT/MyReconfigLogic_0/ADT7410_0/I2CFSM_Byte1_s
add wave -noupdate -radix unsigned /core_tb/DUT/MyReconfigLogic_0/ADT7410_0/SensorValue_o
add wave -noupdate /core_tb/DUT/MyReconfigLogic_0/ADT7410_0/I2CFSM_Done_s
add wave -noupdate /core_tb/DUT/MyReconfigLogic_0/ADT7410_0/CpuIntr_o
add wave -noupdate /core_tb/ExtNames_1/I2CFSM_Done
add wave -noupdate -radix decimal /core_tb/SensorValue_o
add wave -noupdate /core_tb/SensorValue_real
add wave -noupdate /core_tb/CpuIntr_o
add wave -noupdate -divider OpenMSP430
add wave -noupdate -radix hexadecimal /core_tb/DUT/openMSP430_0/execution_unit_0/pc
add wave -noupdate -radix unsigned /core_tb/DUT/openMSP430_0/execution_unit_0/register_file_0/r15
add wave -noupdate -radix hexadecimal /core_tb/DUT/openMSP430_0/pmem_addr
add wave -noupdate /core_tb/DUT/openMSP430_0/pmem_cen
add wave -noupdate -radix hexadecimal /core_tb/DUT/openMSP430_0/pmem_din
add wave -noupdate /core_tb/DUT/openMSP430_0/pmem_wen
add wave -noupdate -radix hexadecimal /core_tb/DUT/PMem_Addr_s
add wave -noupdate /core_tb/DUT/PMem_En_n_s
add wave -noupdate -radix hexadecimal /core_tb/DUT/PMem_DIn_s
add wave -noupdate /core_tb/DUT/PMem_Wr_n_s
add wave -noupdate -radix hexadecimal /core_tb/DUT/PMem_DOut_s
add wave -noupdate -radix hexadecimal /core_tb/DUT/PMem_0/mem
add wave -noupdate -radix hexadecimal /core_tb/DUT/openMSP430_0/dmem_addr
add wave -noupdate /core_tb/DUT/openMSP430_0/dmem_cen
add wave -noupdate -radix hexadecimal /core_tb/DUT/openMSP430_0/dmem_din
add wave -noupdate /core_tb/DUT/openMSP430_0/dmem_wen
add wave -noupdate -radix hexadecimal /core_tb/DUT/DMem_Addr_s
add wave -noupdate /core_tb/DUT/DMem_En_n_s
add wave -noupdate -radix hexadecimal /core_tb/DUT/DMem_DIn_s
add wave -noupdate /core_tb/DUT/DMem_Wr_n_s
add wave -noupdate -radix hexadecimal /core_tb/DUT/DMem_DOut_s
add wave -noupdate /core_tb/DUT/openMSP430_0/per_addr
add wave -noupdate /core_tb/DUT/openMSP430_0/per_din
add wave -noupdate /core_tb/DUT/openMSP430_0/per_we
add wave -noupdate /core_tb/DUT/openMSP430_0/per_en
add wave -noupdate /core_tb/DUT/Per_Addr_s
add wave -noupdate /core_tb/DUT/Per_DIn_s
add wave -noupdate /core_tb/DUT/Per_DOut_s
add wave -noupdate /core_tb/DUT/Per_Wr_s
add wave -noupdate /core_tb/DUT/Per_En_s
add wave -noupdate /core_tb/DUT/IRQ_s
add wave -noupdate /core_tb/DUT/IRQ_Ack_s
add wave -noupdate /core_tb/DUT/ReconfModuleIRQs_s
add wave -noupdate /core_tb/DUT/ReconfModuleIn_s
add wave -noupdate /core_tb/DUT/ReconfModuleOut_s
add wave -noupdate -divider CfgIntf
add wave -noupdate -radix hexadecimal /core_tb/DUT/MyReconfigLogic_0/CfgIntf_0/PerAddr_i
add wave -noupdate -radix hexadecimal /core_tb/DUT/MyReconfigLogic_0/CfgIntf_0/PerDIn_i
add wave -noupdate -radix hexadecimal /core_tb/DUT/MyReconfigLogic_0/CfgIntf_0/PerDOut_o
add wave -noupdate /core_tb/DUT/MyReconfigLogic_0/CfgIntf_0/PerWr_i
add wave -noupdate /core_tb/DUT/MyReconfigLogic_0/CfgIntf_0/PerEn_i
add wave -noupdate /core_tb/DUT/MyReconfigLogic_0/CfgIntf_0/ChainNum
add wave -noupdate -radix unsigned /core_tb/DUT/MyReconfigLogic_0/CfgIntf_0/BitstreamSize
add wave -noupdate /core_tb/DUT/MyReconfigLogic_0/CfgIntf_0/Bitstream
add wave -noupdate /core_tb/DUT/MyReconfigLogic_0/CfgIntf_0/CfgMode_o
add wave -noupdate /core_tb/DUT/MyReconfigLogic_0/CfgIntf_0/CfgShift_o
add wave -noupdate /core_tb/DUT/MyReconfigLogic_0/CfgIntf_0/CfgDataOut_o
add wave -noupdate /core_tb/DUT/MyReconfigLogic_0/CfgIntf_0/CfgDataIn_i
add wave -noupdate -divider ParamIntf
add wave -noupdate /core_tb/DUT/MyReconfigLogic_0/ParamIntf_0/PerAddr_i
add wave -noupdate /core_tb/DUT/MyReconfigLogic_0/ParamIntf_0/PerDIn_i
add wave -noupdate /core_tb/DUT/MyReconfigLogic_0/ParamIntf_0/PerDOut_o
add wave -noupdate /core_tb/DUT/MyReconfigLogic_0/ParamIntf_0/PerWr_i
add wave -noupdate /core_tb/DUT/MyReconfigLogic_0/ParamIntf_0/PerEn_i
add wave -noupdate /core_tb/DUT/MyReconfigLogic_0/ParamIntf_0/ParamWrAddr_o
add wave -noupdate /core_tb/DUT/MyReconfigLogic_0/ParamIntf_0/ParamWrData_o
add wave -noupdate /core_tb/DUT/MyReconfigLogic_0/ParamIntf_0/ParamWr_o
add wave -noupdate /core_tb/DUT/MyReconfigLogic_0/ParamIntf_0/ParamRdAddr_o
add wave -noupdate /core_tb/DUT/MyReconfigLogic_0/ParamIntf_0/ParamRdData_i
add wave -noupdate /core_tb/DUT/MyReconfigLogic_0/Params_s
add wave -noupdate -divider InterSynth
add wave -noupdate -divider {I2C Master}
add wave -noupdate /core_tb/DUT/i2c_master_1/F100_400_n_i
add wave -noupdate /core_tb/DUT/i2c_master_1/Divider800_i
add wave -noupdate /core_tb/DUT/i2c_master_1/StartProcess_i
add wave -noupdate /core_tb/DUT/i2c_master_1/ReceiveSend_n_i
add wave -noupdate /core_tb/DUT/i2c_master_1/Busy_o
add wave -noupdate /core_tb/DUT/i2c_master_1/ReadCount_i
add wave -noupdate /core_tb/DUT/i2c_master_1/FIFOReadNext_i
add wave -noupdate /core_tb/DUT/i2c_master_1/FIFOWrite_i
add wave -noupdate /core_tb/DUT/i2c_master_1/FIFOEmpty_o
add wave -noupdate /core_tb/DUT/i2c_master_1/FIFOFull_o
add wave -noupdate /core_tb/DUT/i2c_master_1/Data_i
add wave -noupdate /core_tb/DUT/i2c_master_1/Data_o
add wave -noupdate /core_tb/DUT/i2c_master_1/ErrAck_i
add wave -noupdate /core_tb/DUT/i2c_master_1/ErrBusColl_o
add wave -noupdate /core_tb/DUT/i2c_master_1/ErrFIFOFull_o
add wave -noupdate /core_tb/DUT/i2c_master_1/ErrGotNAck_o
add wave -noupdate /core_tb/DUT/i2c_master_1/ErrCoreBusy_o
add wave -noupdate /core_tb/DUT/i2c_master_1/ErrFIFOEmpty_o
add wave -noupdate /core_tb/DUT/i2c_master_1/ErrCoreStopped_o
add wave -noupdate /core_tb/DUT/i2c_master_1/ErrDevNotPresent_o
add wave -noupdate /core_tb/DUT/i2c_master_1/ErrReadCountZero_o
add wave -noupdate /core_tb/DUT/i2c_master_1/SDA_i
add wave -noupdate /core_tb/DUT/i2c_master_1/SDA_o
add wave -noupdate /core_tb/DUT/i2c_master_1/SCL_o
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
