onerror {resume}
quietly WaveActivateNextPane {} 0
add wave -noupdate /extintr_tb/ExtIntrOut_o
add wave -noupdate /extintr_tb/ExtIntrIn_i
add wave -noupdate -divider InterSynth
add wave -noupdate /extintr_tb/DUT/MyInterSynthModule_0/Inputs_i_0
add wave -noupdate /extintr_tb/DUT/MyInterSynthModule_0/Inputs_i_1
add wave -noupdate /extintr_tb/DUT/MyInterSynthModule_0/Inputs_i_2
add wave -noupdate /extintr_tb/DUT/MyInterSynthModule_0/Inputs_i_3
add wave -noupdate /extintr_tb/DUT/MyInterSynthModule_0/Inputs_i_4
add wave -noupdate /extintr_tb/DUT/MyInterSynthModule_0/Inputs_i_5
add wave -noupdate /extintr_tb/DUT/MyInterSynthModule_0/Inputs_i_6
add wave -noupdate /extintr_tb/DUT/MyInterSynthModule_0/Inputs_i_7
add wave -noupdate /extintr_tb/DUT/MyInterSynthModule_0/ReconfModuleIRQs_o_0
add wave -noupdate /extintr_tb/DUT/MyInterSynthModule_0/ReconfModuleIRQs_o_1
add wave -noupdate /extintr_tb/DUT/MyInterSynthModule_0/ReconfModuleIRQs_o_2
add wave -noupdate /extintr_tb/DUT/MyInterSynthModule_0/ReconfModuleIRQs_o_3
add wave -noupdate /extintr_tb/DUT/MyInterSynthModule_0/ReconfModuleIRQs_o_4
TreeUpdate [SetDefaultTree]
WaveRestoreCursors {{Cursor 1} {2540831 ps} 0}
configure wave -namecolwidth 186
configure wave -valuecolwidth 40
configure wave -justifyvalue left
configure wave -signalnamewidth 1
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
WaveRestoreZoom {0 ps} {5251050 ps}
