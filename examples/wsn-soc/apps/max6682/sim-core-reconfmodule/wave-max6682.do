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
add wave -noupdate /core_tb/SPIMISO_i
add wave -noupdate /core_tb/SPIMOSI_o
add wave -noupdate /core_tb/SPISCK_o
add wave -noupdate -divider MAX6682
add wave -noupdate /core_tb/MAX6682CS_n_o
add wave -noupdate -radix decimal /core_tb/MAX6682Value
add wave -noupdate /core_tb/ExtNames_1/Enable
add wave -noupdate /core_tb/ExtNames_1/SPIFSM_Done
add wave -noupdate -radix decimal /core_tb/SensorValue_o
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
add wave -noupdate /core_tb/DUT/MyReconfigLogic_0/CfgRegReconfSignals/CfgMode_i
add wave -noupdate /core_tb/DUT/MyReconfigLogic_0/CfgRegReconfSignals/CfgShift_i
add wave -noupdate -radix decimal /core_tb/DUT/MyReconfigLogic_0/CfgRegReconfSignals/Output_o
add wave -noupdate /core_tb/DUT/MyReconfigLogic_0/CfgRegbitdata/CfgMode_i
add wave -noupdate /core_tb/DUT/MyReconfigLogic_0/CfgRegbitdata/CfgShift_i
add wave -noupdate -radix binary /core_tb/DUT/MyReconfigLogic_0/CfgRegbitdata/Output_o
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
add wave -noupdate -radix unsigned /core_tb/DUT/MyReconfigLogic_0/ParamOutReg_ParamIn_Word_0/Param_o
add wave -noupdate -radix unsigned /core_tb/DUT/MyReconfigLogic_0/ParamOutReg_ParamIn_Word_1/Param_o
add wave -noupdate -radix unsigned /core_tb/DUT/MyReconfigLogic_0/ParamOutReg_ParamIn_Word_2/Param_o
add wave -noupdate -divider InterSynth
add wave -noupdate -radix unsigned /core_tb/DUT/MyReconfigLogic_0/MyInterSynthModule_0/cell_130/A_i
add wave -noupdate -radix unsigned /core_tb/DUT/MyReconfigLogic_0/MyInterSynthModule_0/cell_130/B_i
add wave -noupdate -radix unsigned /core_tb/DUT/MyReconfigLogic_0/MyInterSynthModule_0/cell_130/D_o
add wave -noupdate /core_tb/DUT/MyReconfigLogic_0/MyInterSynthModule_0/cell_134/AddOrSub_i
add wave -noupdate -radix unsigned /core_tb/DUT/MyReconfigLogic_0/MyInterSynthModule_0/cell_134/A_i
add wave -noupdate -radix unsigned /core_tb/DUT/MyReconfigLogic_0/MyInterSynthModule_0/cell_134/B_i
add wave -noupdate -radix unsigned /core_tb/DUT/MyReconfigLogic_0/MyInterSynthModule_0/cell_134/D_o
add wave -noupdate /core_tb/DUT/MyReconfigLogic_0/MyInterSynthModule_0/cell_134/Carry_i
add wave -noupdate /core_tb/DUT/MyReconfigLogic_0/MyInterSynthModule_0/cell_134/Carry_o
add wave -noupdate /core_tb/DUT/MyReconfigLogic_0/MyInterSynthModule_0/cell_134/Zero_o
add wave -noupdate /core_tb/DUT/MyReconfigLogic_0/MyInterSynthModule_0/cell_134/Sign_o
add wave -noupdate /core_tb/DUT/MyReconfigLogic_0/MyInterSynthModule_0/cell_134/Overflow_o
add wave -noupdate -divider {SPI Master}
add wave -noupdate /core_tb/DUT/spi_master_1/SPPR_i
add wave -noupdate /core_tb/DUT/spi_master_1/SPR_i
add wave -noupdate /core_tb/DUT/spi_master_1/Data_i
add wave -noupdate /core_tb/DUT/spi_master_1/Data_o
add wave -noupdate /core_tb/DUT/spi_master_1/Reset_n
add wave -noupdate /core_tb/DUT/spi_master_1/Clk
add wave -noupdate /core_tb/DUT/spi_master_1/CPOL_i
add wave -noupdate /core_tb/DUT/spi_master_1/CPHA_i
add wave -noupdate /core_tb/DUT/spi_master_1/LSBFE_i
add wave -noupdate /core_tb/DUT/spi_master_1/MISO_i
add wave -noupdate /core_tb/DUT/spi_master_1/Write_i
add wave -noupdate /core_tb/DUT/spi_master_1/ReadNext_i
add wave -noupdate /core_tb/DUT/spi_master_1/SCK_o
add wave -noupdate /core_tb/DUT/spi_master_1/MOSI_o
add wave -noupdate /core_tb/DUT/spi_master_1/Transmission_o
add wave -noupdate /core_tb/DUT/spi_master_1/FIFOFull_o
add wave -noupdate /core_tb/DUT/spi_master_1/FIFOEmpty_o
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
