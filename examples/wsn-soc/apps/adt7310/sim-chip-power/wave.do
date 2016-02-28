onerror {resume}
quietly WaveActivateNextPane {} 0
add wave -noupdate /chip_power_tb/Reset_n_i
add wave -noupdate /chip_power_tb/Clk_i
add wave -noupdate /chip_power_tb/P1_b
add wave -noupdate /chip_power_tb/P2_b
add wave -noupdate /chip_power_tb/UartRxD_i
add wave -noupdate /chip_power_tb/UartTxD_o
add wave -noupdate /chip_power_tb/Inputs_i
add wave -noupdate /chip_power_tb/Outputs_o
add wave -noupdate /chip_power_tb/SPIMISO_i
add wave -noupdate /chip_power_tb/SPIMOSI_o
add wave -noupdate /chip_power_tb/SPISCK_o
add wave -noupdate -divider UART
add wave -noupdate /chip_power_tb/UartRx
add wave -noupdate /chip_power_tb/UartCh
add wave -noupdate /chip_power_tb/UartRxFifo
add wave -noupdate /chip_power_tb/UartRxFifoWr
add wave -noupdate /chip_power_tb/UartRxFifoRd
add wave -noupdate -divider Clock
add wave -noupdate /chip_power_tb/Clk_s
add wave -noupdate /chip_power_tb/RTC_s
add wave -noupdate /chip_power_tb/ClkEnable_s
add wave -noupdate /chip_power_tb/RTCEnable_s
add wave -noupdate /chip_power_tb/RTC_i
add wave -noupdate /chip_power_tb/ClkPeriode
add wave -noupdate /chip_power_tb/RTCPeriode
add wave -noupdate -divider {Impulse Generator}
add wave -noupdate /chip_power_tb/ImpulseGenState
add wave -noupdate /chip_power_tb/ImpulseGen_s
add wave -noupdate -divider Trigger
add wave -noupdate /chip_power_tb/TriggerEnable_s
add wave -noupdate /chip_power_tb/TriggerSPI_s
add wave -noupdate /chip_power_tb/TriggerManual_s
add wave -noupdate /chip_power_tb/Trigger_s
add wave -noupdate /chip_power_tb/Triggered_s
add wave -noupdate /chip_power_tb/ResetTriggered_s
add wave -noupdate -divider ADT7310
add wave -noupdate /chip_power_tb/ADT7310CS_n_s
add wave -noupdate /chip_power_tb/ADT7310SCK_s
add wave -noupdate /chip_power_tb/ADT7310MOSI_s
add wave -noupdate /chip_power_tb/ADT7310MISO_s
add wave -noupdate /chip_power_tb/CT_n_s
add wave -noupdate /chip_power_tb/INT_n_s
add wave -noupdate /chip_power_tb/Temp_s
add wave -noupdate /chip_power_tb/adt7310_1/Registers
add wave -noupdate /chip_power_tb/adt7310_1/TempReg
add wave -noupdate /chip_power_tb/adt7310_1/OpMode
add wave -noupdate -divider OpenMSP430
add wave -noupdate -radix hexadecimal /chip_power_tb/DUT/core_1/openMSP430_0/execution_unit_0/pc
add wave -noupdate -radix hexadecimal /chip_power_tb/DUT/core_1/openMSP430_0/pmem_addr
add wave -noupdate /chip_power_tb/DUT/core_1/openMSP430_0/pmem_cen
add wave -noupdate -radix hexadecimal /chip_power_tb/DUT/core_1/openMSP430_0/pmem_din
add wave -noupdate /chip_power_tb/DUT/core_1/openMSP430_0/pmem_wen
add wave -noupdate -radix hexadecimal /chip_power_tb/DUT/core_1/PMem_Addr_s
add wave -noupdate /chip_power_tb/DUT/core_1/PMem_En_n_s
add wave -noupdate -radix hexadecimal /chip_power_tb/DUT/core_1/PMem_DIn_s
add wave -noupdate /chip_power_tb/DUT/core_1/PMem_Wr_n_s
add wave -noupdate -radix hexadecimal /chip_power_tb/DUT/core_1/PMem_DOut_s
add wave -noupdate -radix hexadecimal /chip_power_tb/DUT/core_1/DMem_0/sram128x8_0/PLAN_MEM
add wave -noupdate -radix hexadecimal /chip_power_tb/DUT/core_1/DMem_0/sram128x8_1/PLAN_MEM
add wave -noupdate -radix hexadecimal /chip_power_tb/DUT/core_1/openMSP430_0/dmem_addr
add wave -noupdate /chip_power_tb/DUT/core_1/openMSP430_0/dmem_cen
add wave -noupdate -radix hexadecimal /chip_power_tb/DUT/core_1/openMSP430_0/dmem_din
add wave -noupdate /chip_power_tb/DUT/core_1/openMSP430_0/dmem_wen
add wave -noupdate -radix hexadecimal /chip_power_tb/DUT/core_1/DMem_Addr_s
add wave -noupdate /chip_power_tb/DUT/core_1/DMem_En_n_s
add wave -noupdate -radix hexadecimal /chip_power_tb/DUT/core_1/DMem_DIn_s
add wave -noupdate /chip_power_tb/DUT/core_1/DMem_Wr_n_s
add wave -noupdate -radix hexadecimal /chip_power_tb/DUT/core_1/DMem_DOut_s
add wave -noupdate /chip_power_tb/DUT/core_1/openMSP430_0/per_addr
add wave -noupdate /chip_power_tb/DUT/core_1/openMSP430_0/per_din
add wave -noupdate /chip_power_tb/DUT/core_1/openMSP430_0/per_we
add wave -noupdate /chip_power_tb/DUT/core_1/openMSP430_0/per_en
add wave -noupdate /chip_power_tb/DUT/core_1/Per_Addr_s
add wave -noupdate /chip_power_tb/DUT/core_1/Per_DIn_s
add wave -noupdate /chip_power_tb/DUT/core_1/Per_DOut_s
add wave -noupdate /chip_power_tb/DUT/core_1/Per_Wr_s
add wave -noupdate /chip_power_tb/DUT/core_1/Per_En_s
add wave -noupdate -divider CfgIntf
add wave -noupdate -radix hexadecimal /chip_power_tb/DUT/core_1/MyReconfigLogic_0/CfgIntf_0/PerAddr_i
add wave -noupdate -radix hexadecimal /chip_power_tb/DUT/core_1/MyReconfigLogic_0/CfgIntf_0/PerDIn_i
add wave -noupdate -radix hexadecimal /chip_power_tb/DUT/core_1/MyReconfigLogic_0/CfgIntf_0/PerDOut_o
add wave -noupdate /chip_power_tb/DUT/core_1/MyReconfigLogic_0/CfgIntf_0/PerWr_i
add wave -noupdate /chip_power_tb/DUT/core_1/MyReconfigLogic_0/CfgIntf_0/PerEn_i
add wave -noupdate /chip_power_tb/DUT/core_1/MyReconfigLogic_0/CfgIntf_0/ChainNum
add wave -noupdate /chip_power_tb/DUT/core_1/MyReconfigLogic_0/CfgIntf_0/ChunkCounter
add wave -noupdate -radix unsigned /chip_power_tb/DUT/core_1/MyReconfigLogic_0/CfgIntf_0/BitstreamSize
add wave -noupdate /chip_power_tb/DUT/core_1/MyReconfigLogic_0/CfgIntf_0/CfgClk_o
add wave -noupdate /chip_power_tb/DUT/core_1/MyReconfigLogic_0/CfgIntf_0/CfgMode_o
add wave -noupdate /chip_power_tb/DUT/core_1/MyReconfigLogic_0/CfgIntf_0/Gate_1_0/clk
add wave -noupdate /chip_power_tb/DUT/core_1/MyReconfigLogic_0/CfgIntf_0/Gate_1_0/enable
add wave -noupdate /chip_power_tb/DUT/core_1/MyReconfigLogic_0/CfgIntf_0/Gate_1_0/gclk
add wave -noupdate /chip_power_tb/DUT/core_1/MyReconfigLogic_0/CfgIntf_0/Gate_1_1/clk
add wave -noupdate /chip_power_tb/DUT/core_1/MyReconfigLogic_0/CfgIntf_0/Gate_1_1/enable
add wave -noupdate /chip_power_tb/DUT/core_1/MyReconfigLogic_0/CfgIntf_0/Gate_1_1/gclk
add wave -noupdate /chip_power_tb/DUT/core_1/MyReconfigLogic_0/CfgIntf_0/Gate_1_2/clk
add wave -noupdate /chip_power_tb/DUT/core_1/MyReconfigLogic_0/CfgIntf_0/Gate_1_2/enable
add wave -noupdate /chip_power_tb/DUT/core_1/MyReconfigLogic_0/CfgIntf_0/Gate_1_2/gclk
add wave -noupdate /chip_power_tb/DUT/core_1/MyReconfigLogic_0/CfgIntf_0/Gate_1_3/clk
add wave -noupdate /chip_power_tb/DUT/core_1/MyReconfigLogic_0/CfgIntf_0/Gate_1_3/enable
add wave -noupdate /chip_power_tb/DUT/core_1/MyReconfigLogic_0/CfgIntf_0/Gate_1_3/gclk
add wave -noupdate -expand /chip_power_tb/DUT/core_1/MyReconfigLogic_0/CfgIntf_0/CfgClk_o
add wave -noupdate /chip_power_tb/DUT/core_1/MyReconfigLogic_0/CfgIntf_0/CfgShift_o
add wave -noupdate /chip_power_tb/DUT/core_1/MyReconfigLogic_0/CfgIntf_0/CfgDataOut_o
add wave -noupdate /chip_power_tb/DUT/core_1/MyReconfigLogic_0/CfgIntf_0/CfgDataIn_i
add wave -noupdate /chip_power_tb/DUT/core_1/MyReconfigLogic_0/CfgRegReconfSignals/CfgMode_i
add wave -noupdate /chip_power_tb/DUT/core_1/MyReconfigLogic_0/CfgRegReconfSignals/CfgShift_i
add wave -noupdate -radix decimal /chip_power_tb/DUT/core_1/MyReconfigLogic_0/CfgRegReconfSignals/Output_o
add wave -noupdate /chip_power_tb/DUT/core_1/MyReconfigLogic_0/CfgRegbitdata/CfgMode_i
add wave -noupdate /chip_power_tb/DUT/core_1/MyReconfigLogic_0/CfgRegbitdata/CfgShift_i
add wave -noupdate -radix binary /chip_power_tb/DUT/core_1/MyReconfigLogic_0/CfgRegbitdata/Output_o
add wave -noupdate -divider ParamIntf
add wave -noupdate /chip_power_tb/DUT/core_1/MyReconfigLogic_0/ParamIntf_0/PerAddr_i
add wave -noupdate /chip_power_tb/DUT/core_1/MyReconfigLogic_0/ParamIntf_0/PerDIn_i
add wave -noupdate /chip_power_tb/DUT/core_1/MyReconfigLogic_0/ParamIntf_0/PerDOut_o
add wave -noupdate /chip_power_tb/DUT/core_1/MyReconfigLogic_0/ParamIntf_0/PerWr_i
add wave -noupdate /chip_power_tb/DUT/core_1/MyReconfigLogic_0/ParamIntf_0/PerEn_i
add wave -noupdate /chip_power_tb/DUT/core_1/MyReconfigLogic_0/ParamIntf_0/ParamWrAddr_o
add wave -noupdate /chip_power_tb/DUT/core_1/MyReconfigLogic_0/ParamIntf_0/ParamWrData_o
add wave -noupdate /chip_power_tb/DUT/core_1/MyReconfigLogic_0/ParamIntf_0/ParamWr_o
add wave -noupdate /chip_power_tb/DUT/core_1/MyReconfigLogic_0/ParamIntf_0/ParamRdData_i
add wave -noupdate -radix unsigned /chip_power_tb/DUT/core_1/MyReconfigLogic_0/ParamOutReg_ParamIn_Word_0/Param_o
add wave -noupdate -radix unsigned /chip_power_tb/DUT/core_1/MyReconfigLogic_0/ParamOutReg_ParamIn_Word_1/Param_o
add wave -noupdate -radix unsigned /chip_power_tb/DUT/core_1/MyReconfigLogic_0/ParamOutReg_ParamIn_Word_2/Param_o
add wave -noupdate -divider ReconfLogic
add wave -noupdate /chip_power_tb/DUT/core_1/MyReconfigLogic_0/ReconfModuleIn_i
add wave -noupdate /chip_power_tb/DUT/core_1/MyReconfigLogic_0/ReconfModuleIRQs_o
add wave -noupdate /chip_power_tb/DUT/core_1/MyReconfigLogic_0/ReconfModuleOut_o
add wave -noupdate -radix hexadecimal /chip_power_tb/DUT/core_1/MyReconfigLogic_0/SPI_DataIn_o
add wave -noupdate /chip_power_tb/DUT/core_1/MyReconfigLogic_0/SPI_Write_o
add wave -noupdate -radix hexadecimal /chip_power_tb/DUT/core_1/MyReconfigLogic_0/SPI_DataOut_i
add wave -noupdate /chip_power_tb/DUT/core_1/MyReconfigLogic_0/SPI_ReadNext_o
add wave -noupdate /chip_power_tb/DUT/core_1/MyReconfigLogic_0/Inputs_i
add wave -noupdate -expand /chip_power_tb/DUT/core_1/MyReconfigLogic_0/Outputs_o
add wave -noupdate -divider InterSynth
add wave -noupdate -radix unsigned /chip_power_tb/DUT/core_1/MyReconfigLogic_0/MyInterSynthModule_0/cell_64/DL_o
add wave -noupdate -radix unsigned /chip_power_tb/DUT/core_1/MyReconfigLogic_0/MyInterSynthModule_0/cell_64/DH_o
add wave -noupdate /chip_power_tb/DUT/core_1/MyReconfigLogic_0/MyInterSynthModule_0/cell_64/N138
add wave -noupdate -divider cell_113
add wave -noupdate -divider {SPI Master}
add wave -noupdate /chip_power_tb/DUT/core_1/spi_master_1/SPPR_i
add wave -noupdate /chip_power_tb/DUT/core_1/spi_master_1/SPR_i
add wave -noupdate /chip_power_tb/DUT/core_1/spi_master_1/Data_i
add wave -noupdate /chip_power_tb/DUT/core_1/spi_master_1/Data_o
add wave -noupdate /chip_power_tb/DUT/core_1/spi_master_1/Reset_n
add wave -noupdate /chip_power_tb/DUT/core_1/spi_master_1/Clk
add wave -noupdate /chip_power_tb/DUT/core_1/spi_master_1/CPOL_i
add wave -noupdate /chip_power_tb/DUT/core_1/spi_master_1/CPHA_i
add wave -noupdate /chip_power_tb/DUT/core_1/spi_master_1/LSBFE_i
add wave -noupdate /chip_power_tb/DUT/core_1/spi_master_1/Write_i
add wave -noupdate /chip_power_tb/DUT/core_1/spi_master_1/ReadNext_i
add wave -noupdate /chip_power_tb/DUT/core_1/spi_master_1/SCK_o
add wave -noupdate /chip_power_tb/DUT/core_1/spi_master_1/MOSI_o
add wave -noupdate /chip_power_tb/DUT/core_1/spi_master_1/MISO_i
add wave -noupdate /chip_power_tb/DUT/core_1/spi_master_1/Transmission_o
add wave -noupdate /chip_power_tb/DUT/core_1/spi_master_1/FIFOFull_o
add wave -noupdate /chip_power_tb/DUT/core_1/spi_master_1/FIFOEmpty_o
TreeUpdate [SetDefaultTree]
WaveRestoreCursors {{Cursor 1} {19538844403 ps} 0}
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
WaveRestoreZoom {9715516824 ps} {35383388208 ps}
