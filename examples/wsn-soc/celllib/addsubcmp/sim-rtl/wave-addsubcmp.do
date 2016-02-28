onerror {resume}
quietly WaveActivateNextPane {} 0
add wave -noupdate /addsubcmp_tb/AddOrSub_i
add wave -noupdate /addsubcmp_tb/A_i
add wave -noupdate /addsubcmp_tb/B_i
add wave -noupdate /addsubcmp_tb/D_o
add wave -noupdate /addsubcmp_tb/Carry_i
add wave -noupdate /addsubcmp_tb/Carry_o
add wave -noupdate /addsubcmp_tb/Zero_o
add wave -noupdate /addsubcmp_tb/Sign_o
add wave -noupdate /addsubcmp_tb/Overflow_o
add wave -noupdate /addsubcmp_tb/DUT/A
add wave -noupdate /addsubcmp_tb/DUT/B
add wave -noupdate /addsubcmp_tb/DUT/Result
add wave -noupdate /addsubcmp_tb/DUT/Carry
add wave -noupdate /addsubcmp_tb/DUT/D
TreeUpdate [SetDefaultTree]
WaveRestoreCursors {{Cursor 1} {44356 ns} 0}
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
WaveRestoreZoom {0 ns} {86027 ns}
