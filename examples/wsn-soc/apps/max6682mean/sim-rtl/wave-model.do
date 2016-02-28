onerror {resume}
quietly WaveActivateNextPane {} 0
add wave -noupdate /max6682_model_tb/ChipSelect_n_i
add wave -noupdate /max6682_model_tb/Clk
add wave -noupdate /max6682_model_tb/SCLK_i
add wave -noupdate /max6682_model_tb/SO_o
add wave -noupdate /max6682_model_tb/Value_i
TreeUpdate [SetDefaultTree]
WaveRestoreCursors {{Cursor 1} {14453 ns} 0}
configure wave -namecolwidth 258
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
WaveRestoreZoom {0 ns} {16905 ns}
