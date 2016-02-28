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
add wave -noupdate /chip_tb/ExtNames_1/SPIFSM_Done
add wave -noupdate -radix unsigned /chip_tb/SensorValue_o
add wave -noupdate /chip_tb/SensorValue_real
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
add wave -noupdate -divider CfgIntf
add wave -noupdate -radix hexadecimal /chip_tb/DUT/core_1/MyReconfigLogic_0/CfgIntf_0/PerAddr_i
add wave -noupdate -radix hexadecimal /chip_tb/DUT/core_1/MyReconfigLogic_0/CfgIntf_0/PerDIn_i
add wave -noupdate -radix hexadecimal /chip_tb/DUT/core_1/MyReconfigLogic_0/CfgIntf_0/PerDOut_o
add wave -noupdate /chip_tb/DUT/core_1/MyReconfigLogic_0/CfgIntf_0/PerWr_i
add wave -noupdate /chip_tb/DUT/core_1/MyReconfigLogic_0/CfgIntf_0/PerEn_i
add wave -noupdate /chip_tb/DUT/core_1/MyReconfigLogic_0/CfgIntf_0/ChainNum
add wave -noupdate /chip_tb/DUT/core_1/MyReconfigLogic_0/CfgIntf_0/ChunkCounter
add wave -noupdate -radix unsigned /chip_tb/DUT/core_1/MyReconfigLogic_0/CfgIntf_0/BitstreamSize
add wave -noupdate /chip_tb/DUT/core_1/MyReconfigLogic_0/CfgIntf_0/CfgClk_o
add wave -noupdate /chip_tb/DUT/core_1/MyReconfigLogic_0/CfgIntf_0/CfgMode_o
add wave -noupdate /chip_tb/DUT/core_1/MyReconfigLogic_0/CfgIntf_0/Gate_1_0/clk
add wave -noupdate /chip_tb/DUT/core_1/MyReconfigLogic_0/CfgIntf_0/Gate_1_0/enable
add wave -noupdate /chip_tb/DUT/core_1/MyReconfigLogic_0/CfgIntf_0/Gate_1_0/gclk
add wave -noupdate /chip_tb/DUT/core_1/MyReconfigLogic_0/CfgIntf_0/Gate_1_1/clk
add wave -noupdate /chip_tb/DUT/core_1/MyReconfigLogic_0/CfgIntf_0/Gate_1_1/enable
add wave -noupdate /chip_tb/DUT/core_1/MyReconfigLogic_0/CfgIntf_0/Gate_1_1/gclk
add wave -noupdate /chip_tb/DUT/core_1/MyReconfigLogic_0/CfgIntf_0/Gate_1_2/clk
add wave -noupdate /chip_tb/DUT/core_1/MyReconfigLogic_0/CfgIntf_0/Gate_1_2/enable
add wave -noupdate /chip_tb/DUT/core_1/MyReconfigLogic_0/CfgIntf_0/Gate_1_2/gclk
add wave -noupdate /chip_tb/DUT/core_1/MyReconfigLogic_0/CfgIntf_0/Gate_1_3/clk
add wave -noupdate /chip_tb/DUT/core_1/MyReconfigLogic_0/CfgIntf_0/Gate_1_3/enable
add wave -noupdate /chip_tb/DUT/core_1/MyReconfigLogic_0/CfgIntf_0/Gate_1_3/gclk
add wave -noupdate -expand /chip_tb/DUT/core_1/MyReconfigLogic_0/CfgIntf_0/CfgClk_o
add wave -noupdate /chip_tb/DUT/core_1/MyReconfigLogic_0/CfgIntf_0/CfgShift_o
add wave -noupdate /chip_tb/DUT/core_1/MyReconfigLogic_0/CfgIntf_0/CfgDataOut_o
add wave -noupdate /chip_tb/DUT/core_1/MyReconfigLogic_0/CfgIntf_0/CfgDataIn_i
add wave -noupdate /chip_tb/DUT/core_1/MyReconfigLogic_0/CfgRegReconfSignals/CfgMode_i
add wave -noupdate /chip_tb/DUT/core_1/MyReconfigLogic_0/CfgRegReconfSignals/CfgShift_i
add wave -noupdate -radix decimal /chip_tb/DUT/core_1/MyReconfigLogic_0/CfgRegReconfSignals/Output_o
add wave -noupdate /chip_tb/DUT/core_1/MyReconfigLogic_0/CfgRegbitdata/CfgMode_i
add wave -noupdate /chip_tb/DUT/core_1/MyReconfigLogic_0/CfgRegbitdata/CfgShift_i
add wave -noupdate -radix binary /chip_tb/DUT/core_1/MyReconfigLogic_0/CfgRegbitdata/Output_o
add wave -noupdate -divider ParamIntf
add wave -noupdate /chip_tb/DUT/core_1/MyReconfigLogic_0/ParamIntf_0/PerAddr_i
add wave -noupdate /chip_tb/DUT/core_1/MyReconfigLogic_0/ParamIntf_0/PerDIn_i
add wave -noupdate /chip_tb/DUT/core_1/MyReconfigLogic_0/ParamIntf_0/PerDOut_o
add wave -noupdate /chip_tb/DUT/core_1/MyReconfigLogic_0/ParamIntf_0/PerWr_i
add wave -noupdate /chip_tb/DUT/core_1/MyReconfigLogic_0/ParamIntf_0/PerEn_i
add wave -noupdate /chip_tb/DUT/core_1/MyReconfigLogic_0/ParamIntf_0/ParamWrAddr_o
add wave -noupdate /chip_tb/DUT/core_1/MyReconfigLogic_0/ParamIntf_0/ParamWrData_o
add wave -noupdate /chip_tb/DUT/core_1/MyReconfigLogic_0/ParamIntf_0/ParamWr_o
add wave -noupdate /chip_tb/DUT/core_1/MyReconfigLogic_0/ParamIntf_0/ParamRdData_i
add wave -noupdate -radix unsigned /chip_tb/DUT/core_1/MyReconfigLogic_0/ParamOutReg_ParamIn_Word_0/Param_o
add wave -noupdate -radix unsigned /chip_tb/DUT/core_1/MyReconfigLogic_0/ParamOutReg_ParamIn_Word_1/Param_o
add wave -noupdate -radix unsigned /chip_tb/DUT/core_1/MyReconfigLogic_0/ParamOutReg_ParamIn_Word_2/Param_o
add wave -noupdate -divider InterSynth
add wave -noupdate /chip_tb/DUT/core_1/MyReconfigLogic_0/MyInterSynthModule_0/cell_112/Reset_n_i
add wave -noupdate /chip_tb/DUT/core_1/MyReconfigLogic_0/MyInterSynthModule_0/cell_112/Clk_i
add wave -noupdate /chip_tb/DUT/core_1/MyReconfigLogic_0/MyInterSynthModule_0/cell_112/ResetSig_i
add wave -noupdate /chip_tb/DUT/core_1/MyReconfigLogic_0/MyInterSynthModule_0/cell_112/Preset_i
add wave -noupdate /chip_tb/DUT/core_1/MyReconfigLogic_0/MyInterSynthModule_0/cell_112/Enable_i
add wave -noupdate /chip_tb/DUT/core_1/MyReconfigLogic_0/MyInterSynthModule_0/cell_112/Direction_i
add wave -noupdate -radix unsigned /chip_tb/DUT/core_1/MyReconfigLogic_0/MyInterSynthModule_0/cell_112/PresetVal_i
add wave -noupdate -radix unsigned /chip_tb/DUT/core_1/MyReconfigLogic_0/MyInterSynthModule_0/cell_112/D_o
add wave -noupdate /chip_tb/DUT/core_1/MyReconfigLogic_0/MyInterSynthModule_0/cell_112/Overflow_o
add wave -noupdate /chip_tb/DUT/core_1/MyReconfigLogic_0/MyInterSynthModule_0/cell_112/Zero_o
add wave -noupdate -divider cell_113
add wave -noupdate /chip_tb/DUT/core_1/MyReconfigLogic_0/MyInterSynthModule_0/cell_113/Reset_n_i
add wave -noupdate /chip_tb/DUT/core_1/MyReconfigLogic_0/MyInterSynthModule_0/cell_113/Clk_i
add wave -noupdate /chip_tb/DUT/core_1/MyReconfigLogic_0/MyInterSynthModule_0/cell_113/ResetSig_i
add wave -noupdate /chip_tb/DUT/core_1/MyReconfigLogic_0/MyInterSynthModule_0/cell_113/Preset_i
add wave -noupdate /chip_tb/DUT/core_1/MyReconfigLogic_0/MyInterSynthModule_0/cell_113/Enable_i
add wave -noupdate /chip_tb/DUT/core_1/MyReconfigLogic_0/MyInterSynthModule_0/cell_113/Direction_i
add wave -noupdate -radix unsigned /chip_tb/DUT/core_1/MyReconfigLogic_0/MyInterSynthModule_0/cell_113/PresetVal_i
add wave -noupdate -radix unsigned /chip_tb/DUT/core_1/MyReconfigLogic_0/MyInterSynthModule_0/cell_113/D_o
add wave -noupdate /chip_tb/DUT/core_1/MyReconfigLogic_0/MyInterSynthModule_0/cell_113/Overflow_o
add wave -noupdate /chip_tb/DUT/core_1/MyReconfigLogic_0/MyInterSynthModule_0/cell_113/Zero_o
add wave -noupdate /chip_tb/DUT/core_1/MyReconfigLogic_0/MyInterSynthModule_0/cell_125/Reset_n_i
add wave -noupdate /chip_tb/DUT/core_1/MyReconfigLogic_0/MyInterSynthModule_0/cell_125/Clk_i
add wave -noupdate /chip_tb/DUT/core_1/MyReconfigLogic_0/MyInterSynthModule_0/cell_125/In0_i
add wave -noupdate /chip_tb/DUT/core_1/MyReconfigLogic_0/MyInterSynthModule_0/cell_125/In1_i
add wave -noupdate /chip_tb/DUT/core_1/MyReconfigLogic_0/MyInterSynthModule_0/cell_125/In2_i
add wave -noupdate /chip_tb/DUT/core_1/MyReconfigLogic_0/MyInterSynthModule_0/cell_125/In3_i
add wave -noupdate /chip_tb/DUT/core_1/MyReconfigLogic_0/MyInterSynthModule_0/cell_125/In4_i
add wave -noupdate /chip_tb/DUT/core_1/MyReconfigLogic_0/MyInterSynthModule_0/cell_125/In5_i
add wave -noupdate /chip_tb/DUT/core_1/MyReconfigLogic_0/MyInterSynthModule_0/cell_125/In6_i
add wave -noupdate /chip_tb/DUT/core_1/MyReconfigLogic_0/MyInterSynthModule_0/cell_125/In7_i
add wave -noupdate /chip_tb/DUT/core_1/MyReconfigLogic_0/MyInterSynthModule_0/cell_125/Out0_o
add wave -noupdate /chip_tb/DUT/core_1/MyReconfigLogic_0/MyInterSynthModule_0/cell_125/Out1_o
add wave -noupdate /chip_tb/DUT/core_1/MyReconfigLogic_0/MyInterSynthModule_0/cell_125/Out2_o
add wave -noupdate /chip_tb/DUT/core_1/MyReconfigLogic_0/MyInterSynthModule_0/cell_125/Out3_o
add wave -noupdate /chip_tb/DUT/core_1/MyReconfigLogic_0/MyInterSynthModule_0/cell_125/Out4_o
add wave -noupdate /chip_tb/DUT/core_1/MyReconfigLogic_0/MyInterSynthModule_0/cell_125/Out5_o
add wave -noupdate /chip_tb/DUT/core_1/MyReconfigLogic_0/MyInterSynthModule_0/cell_125/Out6_o
add wave -noupdate /chip_tb/DUT/core_1/MyReconfigLogic_0/MyInterSynthModule_0/cell_125/Out7_o
add wave -noupdate /chip_tb/DUT/core_1/MyReconfigLogic_0/MyInterSynthModule_0/cell_125/Out8_o
add wave -noupdate /chip_tb/DUT/core_1/MyReconfigLogic_0/MyInterSynthModule_0/cell_125/Out9_o
add wave -noupdate /chip_tb/DUT/core_1/MyReconfigLogic_0/MyInterSynthModule_0/cell_125/Out10_o
add wave -noupdate /chip_tb/DUT/core_1/MyReconfigLogic_0/MyInterSynthModule_0/cell_125/Out11_o
add wave -noupdate /chip_tb/DUT/core_1/MyReconfigLogic_0/MyInterSynthModule_0/cell_125/Out12_o
add wave -noupdate /chip_tb/DUT/core_1/MyReconfigLogic_0/MyInterSynthModule_0/cell_125/Out13_o
add wave -noupdate /chip_tb/DUT/core_1/MyReconfigLogic_0/MyInterSynthModule_0/cell_125/Out14_o
add wave -noupdate /chip_tb/DUT/core_1/MyReconfigLogic_0/MyInterSynthModule_0/cell_125/CfgMode_i
add wave -noupdate /chip_tb/DUT/core_1/MyReconfigLogic_0/MyInterSynthModule_0/cell_125/CfgClk_i
add wave -noupdate /chip_tb/DUT/core_1/MyReconfigLogic_0/MyInterSynthModule_0/cell_125/CfgShift_i
add wave -noupdate /chip_tb/DUT/core_1/MyReconfigLogic_0/MyInterSynthModule_0/cell_125/CfgDataIn_i
add wave -noupdate /chip_tb/DUT/core_1/MyReconfigLogic_0/MyInterSynthModule_0/cell_125/CfgDataOut_o
add wave -noupdate {/chip_tb/DUT/core_1/MyReconfigLogic_0/MyInterSynthModule_0/cell_125/\<const0> }
add wave -noupdate /chip_tb/DUT/core_1/MyReconfigLogic_0/MyInterSynthModule_0/cell_125/Reset_n_i
add wave -noupdate /chip_tb/DUT/core_1/MyReconfigLogic_0/MyInterSynthModule_0/cell_125/Clk_i
add wave -noupdate /chip_tb/DUT/core_1/MyReconfigLogic_0/MyInterSynthModule_0/cell_125/In0_i
add wave -noupdate /chip_tb/DUT/core_1/MyReconfigLogic_0/MyInterSynthModule_0/cell_125/In1_i
add wave -noupdate /chip_tb/DUT/core_1/MyReconfigLogic_0/MyInterSynthModule_0/cell_125/In2_i
add wave -noupdate /chip_tb/DUT/core_1/MyReconfigLogic_0/MyInterSynthModule_0/cell_125/In3_i
add wave -noupdate /chip_tb/DUT/core_1/MyReconfigLogic_0/MyInterSynthModule_0/cell_125/In4_i
add wave -noupdate /chip_tb/DUT/core_1/MyReconfigLogic_0/MyInterSynthModule_0/cell_125/In5_i
add wave -noupdate /chip_tb/DUT/core_1/MyReconfigLogic_0/MyInterSynthModule_0/cell_125/In6_i
add wave -noupdate /chip_tb/DUT/core_1/MyReconfigLogic_0/MyInterSynthModule_0/cell_125/In7_i
add wave -noupdate /chip_tb/DUT/core_1/MyReconfigLogic_0/MyInterSynthModule_0/cell_125/TRFSM_1/NextState_i
add wave -noupdate /chip_tb/DUT/core_1/MyReconfigLogic_0/MyInterSynthModule_0/cell_125/TRFSM_1/NextState_o
add wave -noupdate /chip_tb/DUT/core_1/MyReconfigLogic_0/MyInterSynthModule_0/cell_125/Out0_o
add wave -noupdate /chip_tb/DUT/core_1/MyReconfigLogic_0/MyInterSynthModule_0/cell_125/Out1_o
add wave -noupdate /chip_tb/DUT/core_1/MyReconfigLogic_0/MyInterSynthModule_0/cell_125/Out2_o
add wave -noupdate /chip_tb/DUT/core_1/MyReconfigLogic_0/MyInterSynthModule_0/cell_125/Out3_o
add wave -noupdate /chip_tb/DUT/core_1/MyReconfigLogic_0/MyInterSynthModule_0/cell_125/Out4_o
add wave -noupdate /chip_tb/DUT/core_1/MyReconfigLogic_0/MyInterSynthModule_0/cell_125/Out5_o
add wave -noupdate /chip_tb/DUT/core_1/MyReconfigLogic_0/MyInterSynthModule_0/cell_125/Out6_o
add wave -noupdate /chip_tb/DUT/core_1/MyReconfigLogic_0/MyInterSynthModule_0/cell_125/Out7_o
add wave -noupdate /chip_tb/DUT/core_1/MyReconfigLogic_0/MyInterSynthModule_0/cell_125/Out8_o
add wave -noupdate /chip_tb/DUT/core_1/MyReconfigLogic_0/MyInterSynthModule_0/cell_125/Out9_o
add wave -noupdate /chip_tb/DUT/core_1/MyReconfigLogic_0/MyInterSynthModule_0/cell_125/Out10_o
add wave -noupdate /chip_tb/DUT/core_1/MyReconfigLogic_0/MyInterSynthModule_0/cell_125/Out11_o
add wave -noupdate /chip_tb/DUT/core_1/MyReconfigLogic_0/MyInterSynthModule_0/cell_125/Out12_o
add wave -noupdate /chip_tb/DUT/core_1/MyReconfigLogic_0/MyInterSynthModule_0/cell_125/Out13_o
add wave -noupdate /chip_tb/DUT/core_1/MyReconfigLogic_0/MyInterSynthModule_0/cell_125/Out14_o
add wave -noupdate /chip_tb/DUT/core_1/MyReconfigLogic_0/MyInterSynthModule_0/cell_125/CfgMode_i
add wave -noupdate /chip_tb/DUT/core_1/MyReconfigLogic_0/MyInterSynthModule_0/cell_125/CfgClk_i
add wave -noupdate /chip_tb/DUT/core_1/MyReconfigLogic_0/MyInterSynthModule_0/cell_125/CfgShift_i
add wave -noupdate /chip_tb/DUT/core_1/MyReconfigLogic_0/MyInterSynthModule_0/cell_125/CfgDataIn_i
add wave -noupdate /chip_tb/DUT/core_1/MyReconfigLogic_0/MyInterSynthModule_0/cell_125/CfgDataOut_o
add wave -noupdate /chip_tb/DUT/core_1/MyReconfigLogic_0/MyInterSynthModule_0/cell_125/TRFSM_1/StateRegister_1/State_o
add wave -noupdate /chip_tb/DUT/core_1/MyReconfigLogic_0/MyInterSynthModule_0/cell_125/TRFSM_1/StateRegister_1/NextState_i
add wave -noupdate /chip_tb/DUT/core_1/MyReconfigLogic_0/MyInterSynthModule_0/cell_124/Reset_n_i
add wave -noupdate /chip_tb/DUT/core_1/MyReconfigLogic_0/MyInterSynthModule_0/cell_124/Clk_i
add wave -noupdate /chip_tb/DUT/core_1/MyReconfigLogic_0/MyInterSynthModule_0/cell_124/In0_i
add wave -noupdate /chip_tb/DUT/core_1/MyReconfigLogic_0/MyInterSynthModule_0/cell_124/In1_i
add wave -noupdate /chip_tb/DUT/core_1/MyReconfigLogic_0/MyInterSynthModule_0/cell_124/In2_i
add wave -noupdate /chip_tb/DUT/core_1/MyReconfigLogic_0/MyInterSynthModule_0/cell_124/In3_i
add wave -noupdate /chip_tb/DUT/core_1/MyReconfigLogic_0/MyInterSynthModule_0/cell_124/In4_i
add wave -noupdate /chip_tb/DUT/core_1/MyReconfigLogic_0/MyInterSynthModule_0/cell_124/Out0_o
add wave -noupdate /chip_tb/DUT/core_1/MyReconfigLogic_0/MyInterSynthModule_0/cell_124/Out1_o
add wave -noupdate /chip_tb/DUT/core_1/MyReconfigLogic_0/MyInterSynthModule_0/cell_124/Out2_o
add wave -noupdate /chip_tb/DUT/core_1/MyReconfigLogic_0/MyInterSynthModule_0/cell_124/Out3_o
add wave -noupdate /chip_tb/DUT/core_1/MyReconfigLogic_0/MyInterSynthModule_0/cell_124/Out4_o
add wave -noupdate /chip_tb/DUT/core_1/MyReconfigLogic_0/MyInterSynthModule_0/cell_124/CfgMode_i
add wave -noupdate /chip_tb/DUT/core_1/MyReconfigLogic_0/MyInterSynthModule_0/cell_124/CfgClk_i
add wave -noupdate /chip_tb/DUT/core_1/MyReconfigLogic_0/MyInterSynthModule_0/cell_124/CfgShift_i
add wave -noupdate /chip_tb/DUT/core_1/MyReconfigLogic_0/MyInterSynthModule_0/cell_124/CfgDataIn_i
add wave -noupdate /chip_tb/DUT/core_1/MyReconfigLogic_0/MyInterSynthModule_0/cell_124/CfgDataOut_o
add wave -noupdate {/chip_tb/DUT/core_1/MyReconfigLogic_0/MyInterSynthModule_0/cell_124/\<const0> }
add wave -noupdate /chip_tb/DUT/core_1/MyReconfigLogic_0/MyInterSynthModule_0/cell_124/Reset_n_i
add wave -noupdate /chip_tb/DUT/core_1/MyReconfigLogic_0/MyInterSynthModule_0/cell_124/Clk_i
add wave -noupdate /chip_tb/DUT/core_1/MyReconfigLogic_0/MyInterSynthModule_0/cell_124/In0_i
add wave -noupdate /chip_tb/DUT/core_1/MyReconfigLogic_0/MyInterSynthModule_0/cell_124/In1_i
add wave -noupdate /chip_tb/DUT/core_1/MyReconfigLogic_0/MyInterSynthModule_0/cell_124/In2_i
add wave -noupdate /chip_tb/DUT/core_1/MyReconfigLogic_0/MyInterSynthModule_0/cell_124/In3_i
add wave -noupdate /chip_tb/DUT/core_1/MyReconfigLogic_0/MyInterSynthModule_0/cell_124/In4_i
add wave -noupdate /chip_tb/DUT/core_1/MyReconfigLogic_0/MyInterSynthModule_0/cell_124/Out0_o
add wave -noupdate /chip_tb/DUT/core_1/MyReconfigLogic_0/MyInterSynthModule_0/cell_124/Out1_o
add wave -noupdate /chip_tb/DUT/core_1/MyReconfigLogic_0/MyInterSynthModule_0/cell_124/Out2_o
add wave -noupdate /chip_tb/DUT/core_1/MyReconfigLogic_0/MyInterSynthModule_0/cell_124/Out3_o
add wave -noupdate /chip_tb/DUT/core_1/MyReconfigLogic_0/MyInterSynthModule_0/cell_124/Out4_o
add wave -noupdate /chip_tb/DUT/core_1/MyReconfigLogic_0/MyInterSynthModule_0/cell_124/CfgMode_i
add wave -noupdate /chip_tb/DUT/core_1/MyReconfigLogic_0/MyInterSynthModule_0/cell_124/CfgClk_i
add wave -noupdate /chip_tb/DUT/core_1/MyReconfigLogic_0/MyInterSynthModule_0/cell_124/CfgShift_i
add wave -noupdate /chip_tb/DUT/core_1/MyReconfigLogic_0/MyInterSynthModule_0/cell_124/CfgDataIn_i
add wave -noupdate /chip_tb/DUT/core_1/MyReconfigLogic_0/MyInterSynthModule_0/cell_124/CfgDataOut_o
add wave -noupdate /chip_tb/DUT/core_1/MyReconfigLogic_0/MyInterSynthModule_0/cell_124/TRFSM_1/StateRegister_1/State_o
add wave -noupdate /chip_tb/DUT/core_1/MyReconfigLogic_0/MyInterSynthModule_0/cell_124/TRFSM_1/StateRegister_1/NextState_i
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
WaveRestoreCursors {{Cursor 1} {138036423210 ps} 0}
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
WaveRestoreZoom {137963571937 ps} {138148608631 ps}
