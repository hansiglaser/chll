onerror {resume}
quietly WaveActivateNextPane {} 0
add wave -noupdate /paramintf_tb/DUT/Reset_n_i
add wave -noupdate /paramintf_tb/DUT/Clk_i
add wave -noupdate -expand -group OpenMSP430 -radix hexadecimal /paramintf_tb/DUT/PerAddr_i
add wave -noupdate -expand -group OpenMSP430 -radix hexadecimal /paramintf_tb/DUT/PerDIn_i
add wave -noupdate -expand -group OpenMSP430 -radix hexadecimal /paramintf_tb/DUT/PerDOut_o
add wave -noupdate -expand -group OpenMSP430 /paramintf_tb/DUT/PerWr_i
add wave -noupdate -expand -group OpenMSP430 /paramintf_tb/DUT/PerEn_i
add wave -noupdate -expand -group Write -radix hexadecimal /paramintf_tb/DUT/ParamWrAddr_o
add wave -noupdate -expand -group Write -radix hexadecimal /paramintf_tb/DUT/ParamWrData_o
add wave -noupdate -expand -group Write /paramintf_tb/DUT/ParamWr_o
add wave -noupdate /paramintf_tb/ParamEnable_s
add wave -noupdate -expand -group Read -radix hexadecimal /paramintf_tb/DUT/ParamRdAddr_o
add wave -noupdate -expand -group Read -radix hexadecimal /paramintf_tb/DUT/ParamRdData_i
add wave -noupdate -expand -group internal /paramintf_tb/DUT/AutoIncrement
add wave -noupdate -expand -group internal -radix hexadecimal /paramintf_tb/DUT/Addr
add wave -noupdate -expand -group internal /paramintf_tb/DUT/DoIncrRd
add wave -noupdate -radix hexadecimal /paramintf_tb/ParamRdData
add wave -noupdate -radix hexadecimal -expand -subitemconfig {/paramintf_tb/Params(0) {-height 16 -radix hexadecimal} /paramintf_tb/Params(1) {-height 16 -radix hexadecimal} /paramintf_tb/Params(2) {-height 16 -radix hexadecimal} /paramintf_tb/Params(3) {-height 16 -radix hexadecimal} /paramintf_tb/Params(4) {-height 16 -radix hexadecimal} /paramintf_tb/Params(5) {-height 16 -radix hexadecimal} /paramintf_tb/Params(6) {-height 16 -radix hexadecimal} /paramintf_tb/Params(7) {-height 16 -radix hexadecimal} /paramintf_tb/Params(8) {-height 16 -radix hexadecimal} /paramintf_tb/Params(9) {-height 16 -radix hexadecimal} /paramintf_tb/Params(10) {-height 16 -radix hexadecimal} /paramintf_tb/Params(11) {-height 16 -radix hexadecimal} /paramintf_tb/Params(12) {-height 16 -radix hexadecimal} /paramintf_tb/Params(13) {-height 16 -radix hexadecimal} /paramintf_tb/Params(14) {-height 16 -radix hexadecimal} /paramintf_tb/Params(15) {-height 16 -radix hexadecimal}} /paramintf_tb/Params
TreeUpdate [SetDefaultTree]
WaveRestoreCursors {{Cursor 1} {753 ns} 0}
configure wave -namecolwidth 283
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
WaveRestoreZoom {0 ns} {1420 ns}
