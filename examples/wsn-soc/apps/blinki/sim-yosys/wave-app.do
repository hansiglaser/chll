onerror {resume}
quietly WaveActivateNextPane {} 0
add wave -noupdate /blinki_tb/Reset_n_i
add wave -noupdate /blinki_tb/Clk_i
add wave -noupdate /blinki_tb/LED_o
add wave -noupdate /blinki_tb/DUT/State
add wave -noupdate /blinki_tb/DUT/NextState
add wave -noupdate -radix unsigned /blinki_tb/DUT/PeriodH_i
add wave -noupdate -radix unsigned /blinki_tb/DUT/PeriodL_i
add wave -noupdate /blinki_tb/DUT/TimerPreset
add wave -noupdate /blinki_tb/DUT/TimerOvfl
add wave -noupdate -radix unsigned /blinki_tb/DUT/Timer
TreeUpdate [SetDefaultTree]
WaveRestoreCursors {{Cursor 1} {1300833 ps} 0}
configure wave -namecolwidth 236
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
WaveRestoreZoom {0 ps} {10524150 ps}
