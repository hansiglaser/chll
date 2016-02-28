onerror {resume}
quietly WaveActivateNextPane {} 0
add wave -noupdate /simplespi_tb/Reset_n_i
add wave -noupdate /simplespi_tb/Clk_i
add wave -noupdate -divider {Peripheral Interface}
add wave -noupdate -radix hexadecimal /simplespi_tb/PerAddr_i
add wave -noupdate -radix hexadecimal /simplespi_tb/PerDIn_i
add wave -noupdate -radix hexadecimal /simplespi_tb/PerDOut_o
add wave -noupdate /simplespi_tb/PerEn_i
add wave -noupdate /simplespi_tb/PerWr_i
add wave -noupdate /simplespi_tb/Intr_o
add wave -noupdate -divider {SPI Signals}
add wave -noupdate /simplespi_tb/DUT/SCK_s
add wave -noupdate /simplespi_tb/SCK_o
add wave -noupdate /simplespi_tb/MOSI_o
add wave -noupdate /simplespi_tb/MISO_i
add wave -noupdate -divider Internal
add wave -noupdate -expand -group CSR /simplespi_tb/DUT/Busy
add wave -noupdate -expand -group CSR /simplespi_tb/DUT/IntEn
add wave -noupdate -expand -group CSR /simplespi_tb/DUT/BRDE
add wave -noupdate -expand -group CSR /simplespi_tb/DUT/BRDM
add wave -noupdate -expand -group CSR /simplespi_tb/DUT/CPHA
add wave -noupdate -expand -group CSR /simplespi_tb/DUT/CPOL
add wave -noupdate /simplespi_tb/DUT/DataRead
add wave -noupdate /simplespi_tb/DUT/DataWrite
add wave -noupdate -radix hexadecimal /simplespi_tb/DUT/Prescaler
add wave -noupdate -radix hexadecimal /simplespi_tb/DUT/PrescalerPrev
add wave -noupdate /simplespi_tb/DUT/PrescalerEdge
add wave -noupdate /simplespi_tb/DUT/PrescalerSel
add wave -noupdate -radix hexadecimal /simplespi_tb/DUT/BaudDivider
add wave -noupdate -radix hexadecimal /simplespi_tb/DUT/Xfer
add wave -noupdate /simplespi_tb/DUT/XferPhase
add wave -noupdate /simplespi_tb/DUT/SckToggle
TreeUpdate [SetDefaultTree]
WaveRestoreCursors {{Cursor 1} {44472 ns} 0} {{Cursor 2} {343478 ns} 0}
configure wave -namecolwidth 238
configure wave -valuecolwidth 134
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
WaveRestoreZoom {341353 ns} {346066 ns}
