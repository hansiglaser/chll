onerror {resume}
quietly WaveActivateNextPane {} 0
add wave -noupdate -radix unsigned /absdiff_tb/A_i
add wave -noupdate -radix unsigned /absdiff_tb/B_i
add wave -noupdate -radix unsigned /absdiff_tb/D_o
add wave -noupdate -radix unsigned /absdiff_tb/DUT/DiffAB
add wave -noupdate -radix unsigned /absdiff_tb/DUT/DiffBA
TreeUpdate [SetDefaultTree]
WaveRestoreCursors {{Cursor 1} {268691600000 ps} 0}
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
WaveRestoreZoom {533700563582 ps} {533701020922 ps}
