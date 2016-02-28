
# list of all cells
# format: intersynth_cell <instance_name> <cell_type> <cell_num>
intersynth_cell cell_61 Counter 0
intersynth_cell cell_62 Counter 1
intersynth_cell cell_63 Counter32 0
intersynth_cell cell_64 Counter32 1
intersynth_cell cell_65 AbsDiff 0
intersynth_cell cell_66 WordRegister 0
intersynth_cell cell_67 WordRegister 1
intersynth_cell cell_68 WordRegister 2
intersynth_cell cell_69 ByteRegister 0
intersynth_cell cell_70 ByteRegister 1
intersynth_cell cell_71 AddSubCmp 0
intersynth_cell cell_72 AddSubCmp 1
intersynth_cell cell_73 ByteMuxQuad 0
intersynth_cell cell_74 ByteMuxDual 0
intersynth_cell cell_75 ByteMuxDual 1
intersynth_cell cell_76 Byte2Word 0
intersynth_cell cell_77 Byte2WordSel 0
intersynth_cell cell_78 WordMuxDual 0
intersynth_cell cell_79 WordMuxDual 1
intersynth_cell cell_80 TRFSM0 0
intersynth_cell cell_81 TRFSM1 0
intersynth_cell cell_82 CONST_Bit 0
intersynth_cell cell_83 CONST_Bit 1
intersynth_cell cell_84 CONST_Byte 0
intersynth_cell cell_85 CONST_Byte 1
intersynth_cell cell_86 CONST_Byte 2
intersynth_cell cell_87 CONST_Byte 3
intersynth_cell cell_88 CONST_Byte 4
intersynth_cell cell_89 CONST_Byte 5
intersynth_cell cell_90 CONST_Word 0

# all paths thru cells
# format: intersynth_path_cell <instance_name> <cell_type> {<input_names>} {<output_names>}
intersynth_path_cell cell_61 Counter {Direction_i Enable_i PresetVal_i Preset_i ResetSig_i} {D_o Overflow_o Zero_o}
intersynth_path_cell cell_62 Counter {Direction_i Enable_i PresetVal_i Preset_i ResetSig_i} {D_o Overflow_o Zero_o}
intersynth_path_cell cell_63 Counter32 {Direction_i Enable_i PresetValH_i PresetValL_i Preset_i ResetSig_i} {DH_o DL_o Overflow_o Zero_o}
intersynth_path_cell cell_64 Counter32 {Direction_i Enable_i PresetValH_i PresetValL_i Preset_i ResetSig_i} {DH_o DL_o Overflow_o Zero_o}
intersynth_path_cell cell_65 AbsDiff {A_i B_i} {D_o}
intersynth_path_cell cell_66 WordRegister {D_i Enable_i} {Q_o}
intersynth_path_cell cell_67 WordRegister {D_i Enable_i} {Q_o}
intersynth_path_cell cell_68 WordRegister {D_i Enable_i} {Q_o}
intersynth_path_cell cell_69 ByteRegister {D_i Enable_i} {Q_o}
intersynth_path_cell cell_70 ByteRegister {D_i Enable_i} {Q_o}
intersynth_path_cell cell_71 AddSubCmp {A_i AddOrSub_i B_i Carry_i} {Carry_o D_o Overflow_o Sign_o Zero_o}
intersynth_path_cell cell_72 AddSubCmp {A_i AddOrSub_i B_i Carry_i} {Carry_o D_o Overflow_o Sign_o Zero_o}
intersynth_path_cell cell_73 ByteMuxQuad {A_i B_i C_i D_i SAB_i SC_i SD_i} {Y_o}
intersynth_path_cell cell_74 ByteMuxDual {A_i B_i S_i} {Y_o}
intersynth_path_cell cell_75 ByteMuxDual {A_i B_i S_i} {Y_o}
intersynth_path_cell cell_76 Byte2Word {H_i L_i} {Y_o}
intersynth_path_cell cell_77 Byte2WordSel {H_i L_i} {Y_o}
intersynth_path_cell cell_78 WordMuxDual {A_i B_i S_i} {Y_o}
intersynth_path_cell cell_79 WordMuxDual {A_i B_i S_i} {Y_o}
intersynth_path_cell cell_80 TRFSM0 {In0_i In1_i In2_i In3_i In4_i In5_i} {Out0_o Out1_o Out2_o Out3_o Out4_o Out5_o Out6_o Out7_o Out8_o Out9_o}
intersynth_path_cell cell_81 TRFSM1 {In0_i In1_i In2_i In3_i In4_i In5_i In6_i In7_i In8_i In9_i} {Out0_o Out10_o Out11_o Out12_o Out13_o Out14_o Out1_o Out2_o Out3_o Out4_o Out5_o Out6_o Out7_o Out8_o Out9_o}
intersynth_path_cell cell_82 CONST_Bit {} {Value_o}
intersynth_path_cell cell_83 CONST_Bit {} {Value_o}
intersynth_path_cell cell_84 CONST_Byte {} {Value_o}
intersynth_path_cell cell_85 CONST_Byte {} {Value_o}
intersynth_path_cell cell_86 CONST_Byte {} {Value_o}
intersynth_path_cell cell_87 CONST_Byte {} {Value_o}
intersynth_path_cell cell_88 CONST_Byte {} {Value_o}
intersynth_path_cell cell_89 CONST_Byte {} {Value_o}
intersynth_path_cell cell_90 CONST_Word {} {Value_o}

# all paths thru interconnects
# format: intersynth_path_conn <connection_type> {<instance_outputs_pair>} {<input_ports>} {<instance_inputs_pair>} {<output_ports>}
intersynth_path_conn Bit {cell_61 {Overflow_o Zero_o} cell_62 {Overflow_o Zero_o} cell_63 {Overflow_o Zero_o} cell_64 {Overflow_o Zero_o} cell_65 {} cell_66 {} cell_67 {} cell_68 {} cell_69 {} cell_70 {} cell_71 {Carry_o Overflow_o Sign_o Zero_o} cell_72 {Carry_o Overflow_o Sign_o Zero_o} cell_73 {} cell_74 {} cell_75 {} cell_76 {} cell_77 {} cell_78 {} cell_79 {} cell_80 {Out0_o Out1_o Out2_o Out3_o Out4_o Out5_o Out6_o Out7_o Out8_o Out9_o} cell_81 {Out0_o Out10_o Out11_o Out12_o Out13_o Out14_o Out1_o Out2_o Out3_o Out4_o Out5_o Out6_o Out7_o Out8_o Out9_o} cell_82 {Value_o} cell_83 {Value_o} cell_84 {} cell_85 {} cell_86 {} cell_87 {} cell_88 {} cell_89 {} cell_90 {}} {AdcConvComplete_i I2C_Busy_i I2C_Error_i I2C_FIFOEmpty_i I2C_FIFOFull_i Inputs_i_0 Inputs_i_1 Inputs_i_2 Inputs_i_3 Inputs_i_4 Inputs_i_5 Inputs_i_6 Inputs_i_7 SPI_FIFOEmpty_i SPI_FIFOFull_i SPI_Transmission_i ReconfModuleIn_i_0 ReconfModuleIn_i_1 ReconfModuleIn_i_2 ReconfModuleIn_i_3 ReconfModuleIn_i_4 ReconfModuleIn_i_5 ReconfModuleIn_i_6 ReconfModuleIn_i_7} {cell_61 {Direction_i Enable_i Preset_i ResetSig_i} cell_62 {Direction_i Enable_i Preset_i ResetSig_i} cell_63 {Direction_i Enable_i Preset_i ResetSig_i} cell_64 {Direction_i Enable_i Preset_i ResetSig_i} cell_65 {} cell_66 {Enable_i} cell_67 {Enable_i} cell_68 {Enable_i} cell_69 {Enable_i} cell_70 {Enable_i} cell_71 {AddOrSub_i Carry_i} cell_72 {AddOrSub_i Carry_i} cell_73 {SAB_i SC_i SD_i} cell_74 {S_i} cell_75 {S_i} cell_76 {} cell_77 {} cell_78 {S_i} cell_79 {S_i} cell_80 {In0_i In1_i In2_i In3_i In4_i In5_i} cell_81 {In0_i In1_i In2_i In3_i In4_i In5_i In6_i In7_i In8_i In9_i} cell_82 {} cell_83 {} cell_84 {} cell_85 {} cell_86 {} cell_87 {} cell_88 {} cell_89 {} cell_90 {}} {AdcDoConvert_o I2C_FIFOReadNext_o I2C_FIFOWrite_o I2C_ReceiveSend_n_o I2C_StartProcess_o Outputs_o_0 Outputs_o_1 Outputs_o_2 Outputs_o_3 Outputs_o_4 Outputs_o_5 Outputs_o_6 Outputs_o_7 ReconfModuleIRQs_o_0 ReconfModuleIRQs_o_1 ReconfModuleIRQs_o_2 ReconfModuleIRQs_o_3 ReconfModuleIRQs_o_4 SPI_CPHA_o SPI_CPOL_o SPI_LSBFE_o SPI_ReadNext_o SPI_Write_o ReconfModuleOut_o_0 ReconfModuleOut_o_1 ReconfModuleOut_o_2 ReconfModuleOut_o_3 ReconfModuleOut_o_4 ReconfModuleOut_o_5 ReconfModuleOut_o_6 ReconfModuleOut_o_7}
intersynth_path_conn Byte {cell_61 {} cell_62 {} cell_63 {} cell_64 {} cell_65 {} cell_66 {} cell_67 {} cell_68 {} cell_69 {Q_o} cell_70 {Q_o} cell_71 {} cell_72 {} cell_73 {Y_o} cell_74 {Y_o} cell_75 {Y_o} cell_76 {} cell_77 {} cell_78 {} cell_79 {} cell_80 {} cell_81 {} cell_82 {} cell_83 {} cell_84 {Value_o} cell_85 {Value_o} cell_86 {Value_o} cell_87 {Value_o} cell_88 {Value_o} cell_89 {Value_o} cell_90 {}} {I2C_DataOut_i SPI_DataOut_i} {cell_61 {} cell_62 {} cell_63 {} cell_64 {} cell_65 {} cell_66 {} cell_67 {} cell_68 {} cell_69 {D_i} cell_70 {D_i} cell_71 {} cell_72 {} cell_73 {A_i B_i C_i D_i} cell_74 {A_i B_i} cell_75 {A_i B_i} cell_76 {H_i L_i} cell_77 {H_i L_i} cell_78 {} cell_79 {} cell_80 {} cell_81 {} cell_82 {} cell_83 {} cell_84 {} cell_85 {} cell_86 {} cell_87 {} cell_88 {} cell_89 {} cell_90 {}} {I2C_DataIn_o I2C_ReadCount_o SPI_DataIn_o}
intersynth_path_conn Word {cell_61 {D_o} cell_62 {D_o} cell_63 {DH_o DL_o} cell_64 {DH_o DL_o} cell_65 {D_o} cell_66 {Q_o} cell_67 {Q_o} cell_68 {Q_o} cell_69 {} cell_70 {} cell_71 {D_o} cell_72 {D_o} cell_73 {} cell_74 {} cell_75 {} cell_76 {Y_o} cell_77 {Y_o} cell_78 {Y_o} cell_79 {Y_o} cell_80 {} cell_81 {} cell_82 {} cell_83 {} cell_84 {} cell_85 {} cell_86 {} cell_87 {} cell_88 {} cell_89 {} cell_90 {Value_o}} {AdcValue_i ParamIn_Word_i} {cell_61 {PresetVal_i} cell_62 {PresetVal_i} cell_63 {PresetValH_i PresetValL_i} cell_64 {PresetValH_i PresetValL_i} cell_65 {A_i B_i} cell_66 {D_i} cell_67 {D_i} cell_68 {D_i} cell_69 {} cell_70 {} cell_71 {A_i B_i} cell_72 {A_i B_i} cell_73 {} cell_74 {} cell_75 {} cell_76 {} cell_77 {} cell_78 {A_i B_i} cell_79 {A_i B_i} cell_80 {} cell_81 {} cell_82 {} cell_83 {} cell_84 {} cell_85 {} cell_86 {} cell_87 {} cell_88 {} cell_89 {} cell_90 {}} {ParamOut_Word_o}

