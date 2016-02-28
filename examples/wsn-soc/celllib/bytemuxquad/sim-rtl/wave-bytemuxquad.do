onerror {resume}
quietly WaveActivateNextPane {} 0
add wave -noupdate /bytemuxquad_tb/A_i
add wave -noupdate /bytemuxquad_tb/B_i
add wave -noupdate /bytemuxquad_tb/C_i
add wave -noupdate /bytemuxquad_tb/D_i
add wave -noupdate /bytemuxquad_tb/SAB_i
add wave -noupdate /bytemuxquad_tb/SC_i
add wave -noupdate /bytemuxquad_tb/SD_i
add wave -noupdate /bytemuxquad_tb/Y_o
TreeUpdate [SetDefaultTree]
WaveRestoreCursors {{Cursor 1} {9418547 ps} 0}
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
WaveRestoreZoom {8763712 ps} {9434967 ps}
