create_clock -period 10.000 -name ClkIn -waveform {0.000 5.000} [get_ports Clk_i]
#create_generated_clock -name ClkDiv -source [get_ports Clk_i] -divide_by 10 [get_pins Clk_s_reg/Q]
create_clock -period 100.000 -name ClkDiv -waveform {0.000 50.000} [get_pins Clk_s_reg/Q]

set_input_delay -clock [get_clocks ClkDiv] -min -add_delay 10.000 [get_ports {AdcValue_i[*]}]
set_input_delay -clock [get_clocks ClkDiv] -max -add_delay 10.000 [get_ports {AdcValue_i[*]}]
set_input_delay -clock [get_clocks ClkDiv] -min -add_delay 10.000 [get_ports {Inputs_i[*]}]
set_input_delay -clock [get_clocks ClkDiv] -max -add_delay 10.000 [get_ports {Inputs_i[*]}]
set_input_delay -clock [get_clocks ClkDiv] -min -add_delay 10.000 [get_ports {P1_b[*]}]
set_input_delay -clock [get_clocks ClkDiv] -max -add_delay 10.000 [get_ports {P1_b[*]}]
set_input_delay -clock [get_clocks ClkDiv] -min -add_delay 10.000 [get_ports {P2_b[*]}]
set_input_delay -clock [get_clocks ClkDiv] -max -add_delay 10.000 [get_ports {P2_b[*]}]
set_input_delay -clock [get_clocks ClkDiv] -min -add_delay 10.000 [get_ports MISO_i]
set_input_delay -clock [get_clocks ClkDiv] -max -add_delay 10.000 [get_ports MISO_i]
set_input_delay -clock [get_clocks ClkDiv] -min -add_delay 10.000 [get_ports AdcConvComplete_i]
set_input_delay -clock [get_clocks ClkDiv] -max -add_delay 10.000 [get_ports AdcConvComplete_i]
set_input_delay -clock [get_clocks ClkDiv] -min -add_delay 10.000 [get_ports Dbg_En_i]
set_input_delay -clock [get_clocks ClkDiv] -max -add_delay 10.000 [get_ports Dbg_En_i]
set_input_delay -clock [get_clocks ClkDiv] -min -add_delay 10.000 [get_ports Dbg_SCL_i]
set_input_delay -clock [get_clocks ClkDiv] -max -add_delay 10.000 [get_ports Dbg_SCL_i]
set_input_delay -clock [get_clocks ClkDiv] -min -add_delay 10.000 [get_ports Dbg_SDA_b]
set_input_delay -clock [get_clocks ClkDiv] -max -add_delay 10.000 [get_ports Dbg_SDA_b]
set_input_delay -clock [get_clocks ClkDiv] -min -add_delay 10.000 [get_ports I2CSDA_b]
set_input_delay -clock [get_clocks ClkDiv] -max -add_delay 10.000 [get_ports I2CSDA_b]
#set_input_delay -clock [get_clocks ClkDiv] -min -add_delay 10.000 [get_ports OneWire_b]
#set_input_delay -clock [get_clocks ClkDiv] -max -add_delay 10.000 [get_ports OneWire_b]
#set_input_delay -clock [get_clocks ClkDiv] -min -add_delay 10.000 [get_ports PWM_i]
#set_input_delay -clock [get_clocks ClkDiv] -max -add_delay 10.000 [get_ports PWM_i]
set_input_delay -clock [get_clocks ClkDiv] -min -add_delay 10.000 [get_ports Reset_i]
set_input_delay -clock [get_clocks ClkDiv] -max -add_delay 10.000 [get_ports Reset_i]
#set_input_delay -clock [get_clocks ClkDiv] -min -add_delay 10.000 [get_ports SENT_i]
#set_input_delay -clock [get_clocks ClkDiv] -max -add_delay 10.000 [get_ports SENT_i]
#set_input_delay -clock [get_clocks ClkDiv] -min -add_delay 10.000 [get_ports SPC_b]
#set_input_delay -clock [get_clocks ClkDiv] -max -add_delay 10.000 [get_ports SPC_b]
set_input_delay -clock [get_clocks ClkDiv] -min -add_delay 10.000 [get_ports SPIMISO_i]
set_input_delay -clock [get_clocks ClkDiv] -max -add_delay 10.000 [get_ports SPIMISO_i]
set_input_delay -clock [get_clocks ClkDiv] -min -add_delay 10.000 [get_ports UartRxD_i]
set_input_delay -clock [get_clocks ClkDiv] -max -add_delay 10.000 [get_ports UartRxD_i]
set_output_delay -clock [get_clocks ClkDiv] -min -add_delay -10.000 [get_ports {Outputs_o[*]}]
set_output_delay -clock [get_clocks ClkDiv] -max -add_delay  10.000 [get_ports {Outputs_o[*]}]
set_output_delay -clock [get_clocks ClkDiv] -min -add_delay -10.000 [get_ports {P1_b[*]}]
set_output_delay -clock [get_clocks ClkDiv] -max -add_delay  10.000 [get_ports {P1_b[*]}]
set_output_delay -clock [get_clocks ClkDiv] -min -add_delay -10.000 [get_ports {P2_b[*]}]
set_output_delay -clock [get_clocks ClkDiv] -max -add_delay  10.000 [get_ports {P2_b[*]}]
set_output_delay -clock [get_clocks ClkDiv] -min -add_delay -10.000 [get_ports MOSI_o]
set_output_delay -clock [get_clocks ClkDiv] -max -add_delay  10.000 [get_ports MOSI_o]
set_output_delay -clock [get_clocks ClkDiv] -min -add_delay -10.000 [get_ports SCK_o]
set_output_delay -clock [get_clocks ClkDiv] -max -add_delay  10.000 [get_ports SCK_o]
set_output_delay -clock [get_clocks ClkDiv] -min -add_delay -10.000 [get_ports AdcDoConvert_o]
set_output_delay -clock [get_clocks ClkDiv] -max -add_delay  10.000 [get_ports AdcDoConvert_o]
set_output_delay -clock [get_clocks ClkDiv] -min -add_delay -10.000 [get_ports Dbg_SDA_b]
set_output_delay -clock [get_clocks ClkDiv] -max -add_delay  10.000 [get_ports Dbg_SDA_b]
set_output_delay -clock [get_clocks ClkDiv] -min -add_delay -10.000 [get_ports I2CSCL_b]
set_output_delay -clock [get_clocks ClkDiv] -max -add_delay  10.000 [get_ports I2CSCL_b]
set_output_delay -clock [get_clocks ClkDiv] -min -add_delay -10.000 [get_ports I2CSDA_b]
set_output_delay -clock [get_clocks ClkDiv] -max -add_delay  10.000 [get_ports I2CSDA_b]
#set_output_delay -clock [get_clocks ClkDiv] -min -add_delay -10.000 [get_ports OneWire_b]
#set_output_delay -clock [get_clocks ClkDiv] -max -add_delay  10.000 [get_ports OneWire_b]
#set_output_delay -clock [get_clocks ClkDiv] -min -add_delay -10.000 [get_ports SPC_b]
#set_output_delay -clock [get_clocks ClkDiv] -max -add_delay  10.000 [get_ports SPC_b]
set_output_delay -clock [get_clocks ClkDiv] -min -add_delay -10.000 [get_ports SPIMOSI_o]
set_output_delay -clock [get_clocks ClkDiv] -max -add_delay  10.000 [get_ports SPIMOSI_o]
set_output_delay -clock [get_clocks ClkDiv] -min -add_delay -10.000 [get_ports SPISCK_o]
set_output_delay -clock [get_clocks ClkDiv] -max -add_delay  10.000 [get_ports SPISCK_o]
set_output_delay -clock [get_clocks ClkDiv] -min -add_delay -10.000 [get_ports UartTxD_o]
set_output_delay -clock [get_clocks ClkDiv] -max -add_delay  10.000 [get_ports UartTxD_o]

# ignore paths from config registers
#set_false_path -from [get_pins -hierarchical {ValueShift_reg[*]/C}]
# ignore paths from CfgMode
#set_false_path -from [get_pins -hierarchical CfgMode_reg/C]
# ignore paths from Reset_i
#set_false_path -from [get_ports Reset_i]

#set_max_delay -through [get_nets {core_1/MyReconfigLogic_0/MyInterSynthModule_0/cell_114/A_i[0]}] -through [get_nets {core_1/MyReconfigLogic_0/MyInterSynthModule_0/cell_114/D_o[0]}] 1.000
#set_max_delay -through [get_nets {core_1/MyReconfigLogic_0/MyInterSynthModule_0/cell_114/A_i[0]}] -through [get_nets {core_1/MyReconfigLogic_0/MyInterSynthModule_0/cell_114/D_o[0]}] 1.000
#set_max_delay -through [get_nets {core_1/MyReconfigLogic_0/MyInterSynthModule_0/cell_114/A_i[0]}] -through [get_nets {core_1/MyReconfigLogic_0/MyInterSynthModule_0/cell_114/D_o[1]}] 1.000
#set_max_delay -through [get_nets {core_1/MyReconfigLogic_0/MyInterSynthModule_0/cell_114/A_i[0]}] -through [get_nets {core_1/MyReconfigLogic_0/MyInterSynthModule_0/cell_114/D_o[2]}] 1.000

#set_false_path -through [get_cells {core_1/MyReconfigLogic_0/MyInterSynthModule_0/cell_114}]
#set_false_path -through [get_cells {core_1/MyReconfigLogic_0/MyInterSynthModule_0/cell_118}]
#set_false_path -through [get_cells {core_1/MyReconfigLogic_0/MyInterSynthModule_0/cell_119}]
#set_false_path -through [get_cells {core_1/MyReconfigLogic_0/MyInterSynthModule_0/cell_120}]
#set_false_path -through [get_cells {core_1/MyReconfigLogic_0/MyInterSynthModule_0/cell_121}]
#set_false_path -through [get_cells {core_1/MyReconfigLogic_0/MyInterSynthModule_0/cell_122}]
#set_false_path -through [get_cells {core_1/MyReconfigLogic_0/MyInterSynthModule_0/cell_123}]
#set_false_path -through [get_cells {core_1/MyReconfigLogic_0/MyInterSynthModule_0/cell_124}]
#set_false_path -through [get_cells {core_1/MyReconfigLogic_0/MyInterSynthModule_0/cell_125}]
#set_false_path -through [get_cells {core_1/MyReconfigLogic_0/MyInterSynthModule_0/cell_126}]
#set_false_path -through [get_cells {core_1/MyReconfigLogic_0/MyInterSynthModule_0/cell_127}]

# set_case_analysis is ignored by synthesis and only used by implementation.
# Synthesis complains
# INFO: [Project 1-236] Implementation specific constraints were found while
# reading constraint file [.../constraints.xdc]. These constraints will be
# ignored for synthesis but will be used in implementation. Impacted constraints
# are listed in the file [.Xil/chip_propImpl.xdc].
#set_case_analysis 0 [get_pins ValueShift_reg*/Q -hierarchical]

#set INTERSYNTH_PATH "core_1/MyReconfigLogic_0/MyInterSynthModule_0/"
