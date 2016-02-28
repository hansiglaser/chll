onerror {resume}
quietly WaveActivateNextPane {} 0
add wave -noupdate /extintr_tb/ExtIntrOut_o
add wave -noupdate /extintr_tb/ExtIntrIn_i
TreeUpdate [SetDefaultTree]
WaveRestoreCursors {{Cursor 1} {5140579 ps} 0}
configure wave -namecolwidth 150
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
WaveRestoreZoom {0 ps} {5251050 ps}
