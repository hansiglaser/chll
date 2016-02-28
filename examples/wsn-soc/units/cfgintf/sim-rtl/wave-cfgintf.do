onerror {resume}
quietly WaveActivateNextPane {} 0
add wave -noupdate /cfgintf_tb/Reset_n_i
add wave -noupdate /cfgintf_tb/Clk_i
add wave -noupdate -radix hexadecimal /cfgintf_tb/PerAddr_i
add wave -noupdate -radix hexadecimal /cfgintf_tb/PerDIn_i
add wave -noupdate -radix hexadecimal /cfgintf_tb/PerDOut_o
add wave -noupdate /cfgintf_tb/PerWr_i(1)
add wave -noupdate /cfgintf_tb/PerWr_i(0)
add wave -noupdate /cfgintf_tb/PerEn_i
add wave -noupdate /cfgintf_tb/CfgMode_o
add wave -noupdate /cfgintf_tb/CfgShift_o(2)
add wave -noupdate /cfgintf_tb/CfgShift_o(1)
add wave -noupdate /cfgintf_tb/CfgShift_o(0)
add wave -noupdate /cfgintf_tb/CfgDataOut_o
add wave -noupdate /cfgintf_tb/CfgDataIn_i
add wave -noupdate /cfgintf_tb/ConfigRegister
add wave -noupdate -divider CfgIntf
add wave -noupdate /cfgintf_tb/DUT/Busy
add wave -noupdate -radix unsigned /cfgintf_tb/DUT/ChainNum
add wave -noupdate -radix hexadecimal /cfgintf_tb/DUT/BitstreamSize
add wave -noupdate /cfgintf_tb/DUT/Bitstream
add wave -noupdate -radix unsigned /cfgintf_tb/DUT/ChunkCounter
TreeUpdate [SetDefaultTree]
WaveRestoreCursors {{Cursor 1} {297 ns} 0}
configure wave -namecolwidth 212
configure wave -valuecolwidth 122
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
WaveRestoreZoom {0 ns} {1630 ns}
