onerror {resume}
quietly WaveActivateNextPane {} 0
add wave -noupdate /counter32_tb/Reset_n_i
add wave -noupdate /counter32_tb/Clk_i
add wave -noupdate /counter32_tb/ResetSig_i
add wave -noupdate /counter32_tb/Preset_i
add wave -noupdate /counter32_tb/Enable_i
add wave -noupdate /counter32_tb/Direction_i
add wave -noupdate -radix unsigned /counter32_tb/PresetVal_i
add wave -noupdate -radix unsigned /counter32_tb/D_o
add wave -noupdate /counter32_tb/Overflow_o
add wave -noupdate /counter32_tb/Zero_o
TreeUpdate [SetDefaultTree]
WaveRestoreCursors {{Cursor 1} {64894 ns} 0}
configure wave -namecolwidth 230
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
WaveRestoreZoom {0 ns} {396721 ns}
