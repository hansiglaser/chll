onerror {resume}
quietly WaveActivateNextPane {} 0
add wave -noupdate /chip_tb/Reset_n_i
add wave -noupdate /chip_tb/Clk_i
add wave -noupdate /chip_tb/P1_b
add wave -noupdate /chip_tb/P2_b
add wave -noupdate /chip_tb/Inputs_i
add wave -noupdate /chip_tb/Outputs_o
add wave -noupdate /chip_tb/SPIMISO_i
add wave -noupdate /chip_tb/SPIMOSI_o
add wave -noupdate /chip_tb/SPISCK_o
add wave -noupdate -divider ADT7310
add wave -noupdate /chip_tb/ADT7310CS_n_o
add wave -noupdate /chip_tb/CT_n_s
add wave -noupdate /chip_tb/INT_n_s
add wave -noupdate /chip_tb/Temp_s
add wave -noupdate /chip_tb/adt7310_1/Registers
add wave -noupdate /chip_tb/adt7310_1/TempReg
add wave -noupdate /chip_tb/adt7310_1/OpMode
add wave -noupdate /chip_tb/DUT/core_1/MyReconfigLogic_0/ADT7310_0/Enable_i
add wave -noupdate -radix unsigned /chip_tb/DUT/core_1/MyReconfigLogic_0/ADT7310_0/SPICounterPresetH_i
add wave -noupdate -radix unsigned /chip_tb/DUT/core_1/MyReconfigLogic_0/ADT7310_0/SPICounterPresetL_i
add wave -noupdate /chip_tb/DUT/core_1/MyReconfigLogic_0/ADT7310_0/SPI_Data_i
add wave -noupdate /chip_tb/DUT/core_1/MyReconfigLogic_0/ADT7310_0/SPI_Write_o
add wave -noupdate /chip_tb/DUT/core_1/MyReconfigLogic_0/ADT7310_0/SPI_ReadNext_o
add wave -noupdate /chip_tb/DUT/core_1/MyReconfigLogic_0/ADT7310_0/SPI_Data_o
add wave -noupdate /chip_tb/DUT/core_1/MyReconfigLogic_0/ADT7310_0/SPI_FIFOFull_i
add wave -noupdate /chip_tb/DUT/core_1/MyReconfigLogic_0/ADT7310_0/SPI_FIFOEmpty_i
add wave -noupdate /chip_tb/DUT/core_1/MyReconfigLogic_0/ADT7310_0/SPI_Transmission_i
add wave -noupdate /chip_tb/DUT/core_1/MyReconfigLogic_0/ADT7310_0/SensorFSM_1/MeasureFSM_Start_o
add wave -noupdate /chip_tb/DUT/core_1/MyReconfigLogic_0/ADT7310_0/SensorFSM_1/SensorFSM_State
add wave -noupdate /chip_tb/DUT/core_1/MyReconfigLogic_0/ADT7310_0/SensorFSM_1/SensorFSM_NextState
add wave -noupdate /chip_tb/DUT/core_1/MyReconfigLogic_0/ADT7310_0/SensorFSM_1/SensorFSM_TimerOvfl
add wave -noupdate /chip_tb/DUT/core_1/MyReconfigLogic_0/ADT7310_0/SensorFSM_1/SensorFSM_TimerPreset
add wave -noupdate /chip_tb/DUT/core_1/MyReconfigLogic_0/ADT7310_0/SensorFSM_1/SensorFSM_TimerEnable
add wave -noupdate /chip_tb/DUT/core_1/MyReconfigLogic_0/ADT7310_0/SensorFSM_1/SensorFSM_DiffTooLarge
add wave -noupdate /chip_tb/DUT/core_1/MyReconfigLogic_0/ADT7310_0/SensorFSM_1/SensorFSM_StoreNewValue
add wave -noupdate /chip_tb/DUT/core_1/MyReconfigLogic_0/ADT7310_0/SPIFSM_1/SPI_FSM_State
add wave -noupdate /chip_tb/DUT/core_1/MyReconfigLogic_0/ADT7310_0/SPIFSM_1/SPI_FSM_TimerOvfl
add wave -noupdate /chip_tb/DUT/core_1/MyReconfigLogic_0/ADT7310_0/SPIFSM_1/SPI_FSM_TimerPreset
add wave -noupdate /chip_tb/DUT/core_1/MyReconfigLogic_0/ADT7310_0/SPIFSM_1/SPI_FSM_TimerEnable
add wave -noupdate /chip_tb/DUT/core_1/MyReconfigLogic_0/ADT7310_0/SPIFSM_1/SPI_FSM_Wr1
add wave -noupdate /chip_tb/DUT/core_1/MyReconfigLogic_0/ADT7310_0/SPIFSM_1/SPI_FSM_Wr0
add wave -noupdate /chip_tb/DUT/core_1/MyReconfigLogic_0/ADT7310_0/SensorFSM_1/MeasureFSM_Done_i
add wave -noupdate -radix unsigned /chip_tb/DUT/core_1/MyReconfigLogic_0/ADT7310_0/Threshold_i
add wave -noupdate -radix unsigned /chip_tb/DUT/core_1/MyReconfigLogic_0/ADT7310_0/PeriodCounterPreset_i
add wave -noupdate /chip_tb/DUT/core_1/MyReconfigLogic_0/ADT7310_0/SPIFSM_Byte0_s
add wave -noupdate /chip_tb/DUT/core_1/MyReconfigLogic_0/ADT7310_0/SPIFSM_Byte1_s
add wave -noupdate -radix unsigned /chip_tb/DUT/core_1/MyReconfigLogic_0/ADT7310_0/SensorValue_o
add wave -noupdate /chip_tb/DUT/core_1/MyReconfigLogic_0/ADT7310_0/SPIFSM_Done_s
add wave -noupdate /chip_tb/DUT/core_1/MyReconfigLogic_0/ADT7310_0/CpuIntr_o
add wave -noupdate /chip_tb/ExtNames_1/SPIFSM_Done
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
add wave -noupdate -divider {SPI Master}
add wave -noupdate /chip_tb/DUT/core_1/spi_master_1/SPPR_i
add wave -noupdate /chip_tb/DUT/core_1/spi_master_1/SPR_i
add wave -noupdate /chip_tb/DUT/core_1/spi_master_1/Data_i
add wave -noupdate /chip_tb/DUT/core_1/spi_master_1/Data_o
add wave -noupdate /chip_tb/DUT/core_1/spi_master_1/Reset_n
add wave -noupdate /chip_tb/DUT/core_1/spi_master_1/Clk
add wave -noupdate /chip_tb/DUT/core_1/spi_master_1/CPOL_i
add wave -noupdate /chip_tb/DUT/core_1/spi_master_1/CPHA_i
add wave -noupdate /chip_tb/DUT/core_1/spi_master_1/LSBFE_i
add wave -noupdate /chip_tb/DUT/core_1/spi_master_1/MISO_i
add wave -noupdate /chip_tb/DUT/core_1/spi_master_1/Write_i
add wave -noupdate /chip_tb/DUT/core_1/spi_master_1/ReadNext_i
add wave -noupdate /chip_tb/DUT/core_1/spi_master_1/SCK_o
add wave -noupdate /chip_tb/DUT/core_1/spi_master_1/MOSI_o
add wave -noupdate /chip_tb/DUT/core_1/spi_master_1/Transmission_o
add wave -noupdate /chip_tb/DUT/core_1/spi_master_1/FIFOFull_o
add wave -noupdate /chip_tb/DUT/core_1/spi_master_1/FIFOEmpty_o
TreeUpdate [SetDefaultTree]
WaveRestoreCursors {{Cursor 1} {702032169162 ps} 0}
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
WaveRestoreZoom {0 ps} {870051 us}
