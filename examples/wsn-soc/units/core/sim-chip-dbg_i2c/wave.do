onerror {resume}
quietly WaveActivateNextPane {} 0
add wave -noupdate /chip_tb/DUT/Reset_n_i
add wave -noupdate /chip_tb/DUT/Clk_i
add wave -noupdate /chip_tb/DUT/Cpu_En_i
add wave -noupdate /chip_tb/DUT/Dbg_En_i
add wave -noupdate /chip_tb/DUT/Dbg_SCL_i
add wave -noupdate /chip_tb/DUT/Dbg_SDA_b
add wave -noupdate /chip_tb/DUT/Dbg_SDA_In_i
add wave -noupdate /chip_tb/DUT/Dbg_SDA_Out_o
add wave -noupdate -radix hexadecimal /chip_tb/DUT/core_1/openMSP430_0/frontend_0/pc
add wave -noupdate /chip_tb/DUT/P1_b
add wave -noupdate /chip_tb/DUT/P2_b
add wave -noupdate -radix hexadecimal {/chip_tb/DUT/core_1/PMem_0/mem[0]}
add wave -noupdate -radix hexadecimal {/chip_tb/DUT/core_1/PMem_0/mem[1]}
add wave -noupdate -radix hexadecimal {/chip_tb/DUT/core_1/PMem_0/mem[2]}
add wave -noupdate -radix hexadecimal {/chip_tb/DUT/core_1/PMem_0/mem[3]}
add wave -noupdate -radix hexadecimal {/chip_tb/DUT/core_1/PMem_0/mem[4]}
add wave -noupdate -radix hexadecimal {/chip_tb/DUT/core_1/PMem_0/mem[5]}
add wave -noupdate -expand /chip_tb/P1_b
add wave -noupdate /chip_tb/DUT/core_1/openMSP430_0/dbg_0/dbg_i2c_0/dbg_state
add wave -noupdate /chip_tb/DUT/core_1/openMSP430_0/dbg_0/dbg_i2c_0/dbg_state_nxt
add wave -noupdate /chip_tb/DUT/core_1/openMSP430_0/dbg_0/dbg_i2c_0/i2c_state
add wave -noupdate /chip_tb/DUT/core_1/openMSP430_0/dbg_0/dbg_i2c_0/i2c_state_nxt
add wave -noupdate /chip_tb/DUT/core_1/openMSP430_0/dbg_0/dbg_i2c_0/scl_fe
add wave -noupdate /chip_tb/DUT/core_1/openMSP430_0/dbg_0/dbg_i2c_0/sda_in
add wave -noupdate /chip_tb/DUT/core_1/openMSP430_0/dbg_0/dbg_i2c_0/start_detect
add wave -noupdate /chip_tb/DUT/core_1/openMSP430_0/dbg_0/dbg_i2c_0/stop_detect
add wave -noupdate -divider {I2C Master}
add wave -noupdate /chip_tb/i2c_master_1/Divider800_i
add wave -noupdate /chip_tb/i2c_master_1/FIFOEmpty_o
add wave -noupdate /chip_tb/i2c_master_1/FIFOFull_o
add wave -noupdate /chip_tb/i2c_master_1/FIFOReadNext_i
add wave -noupdate /chip_tb/i2c_master_1/ReadCount_i
add wave -noupdate /chip_tb/i2c_master_1/StartProcess_i
add wave -noupdate /chip_tb/i2c_master_1/FIFOWrite_i
add wave -noupdate /chip_tb/i2c_master_1/Data_i
add wave -noupdate /chip_tb/i2c_master_1/Data_o
add wave -noupdate /chip_tb/i2c_master_1/Busy_o
add wave -noupdate /chip_tb/i2c_master_1/mycore/DoTransfer_i
add wave -noupdate /chip_tb/i2c_master_1/mycore/I2CClk_s
add wave -noupdate /chip_tb/i2c_master_1/mycore/NextBytePos_s
add wave -noupdate /chip_tb/i2c_master_1/mycore/BytePos_s
add wave -noupdate /chip_tb/i2c_master_1/mycore/Data_i
add wave -noupdate /chip_tb/i2c_master_1/mycore/NextData_s
add wave -noupdate /chip_tb/i2c_master_1/mycore/AckTx_i
add wave -noupdate /chip_tb/i2c_master_1/mycore/State
add wave -noupdate /chip_tb/i2c_master_1/mycore/NextState
add wave -noupdate /chip_tb/i2c_master_1/mycore/NextSCL_s
add wave -noupdate /chip_tb/i2c_master_1/mycore/SCL_o
add wave -noupdate /chip_tb/i2c_master_1/mycore/SCL_s
add wave -noupdate /chip_tb/i2c_master_1/SCL_o
add wave -noupdate /chip_tb/i2c_master_1/mycore/NextSDA_o_s
add wave -noupdate /chip_tb/i2c_master_1/mycore/SDA_o
add wave -noupdate /chip_tb/i2c_master_1/mycore/SDA_o_s
add wave -noupdate /chip_tb/i2c_master_1/SDA_i
add wave -noupdate /chip_tb/i2c_master_1/SDA_o
add wave -noupdate /chip_tb/i2c_master_1/ErrAck_i
add wave -noupdate /chip_tb/i2c_master_1/ErrBusColl_o
add wave -noupdate /chip_tb/i2c_master_1/ErrCoreBusy_o
add wave -noupdate /chip_tb/i2c_master_1/ErrCoreStopped_o
add wave -noupdate /chip_tb/i2c_master_1/ErrDevNotPresent_o
add wave -noupdate /chip_tb/i2c_master_1/ErrFIFOEmpty_o
add wave -noupdate /chip_tb/i2c_master_1/ErrFIFOFull_o
add wave -noupdate /chip_tb/i2c_master_1/ErrGotNAck_o
add wave -noupdate /chip_tb/i2c_master_1/ErrReadCountZero_o
TreeUpdate [SetDefaultTree]
WaveRestoreCursors {{Cursor 1} {124230000 ps} 0}
configure wave -namecolwidth 350
configure wave -valuecolwidth 133
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
WaveRestoreZoom {0 ps} {205656200 ps}
