onerror {resume}
quietly WaveActivateNextPane {} 0
add wave -noupdate /chip_tb/DUT/Reset_n_i
add wave -noupdate /chip_tb/DUT/Clk_i
add wave -noupdate /chip_tb/DUT/Cpu_En_i
add wave -noupdate /chip_tb/DUT/Dbg_En_i
add wave -noupdate -color Red /chip_tb/i2c_master_1/SCL_o
add wave -noupdate /chip_tb/i2c_master_1/SDA_i
add wave -noupdate -color Cyan /chip_tb/i2c_master_1/SDA_o
add wave -noupdate /chip_tb/DUT/Dbg_SCL_i
add wave -noupdate /chip_tb/DUT/Dbg_SDA_b
add wave -noupdate /chip_tb/DUT/core_1/openMSP430_0/dbg_0/dbg_i2c_0/dbg_i2c_scl
add wave -noupdate -color Magenta /chip_tb/DUT/core_1/openMSP430_0/dbg_0/dbg_i2c_0/scl
add wave -noupdate /chip_tb/DUT/core_1/openMSP430_0/dbg_0/dbg_i2c_0/dbg_i2c_sda_in
add wave -noupdate /chip_tb/DUT/core_1/openMSP430_0/dbg_0/dbg_i2c_0/sda_in
add wave -noupdate /chip_tb/DUT/core_1/openMSP430_0/dbg_0/dbg_i2c_0/sda_in_dly
add wave -noupdate -color Cyan /chip_tb/DUT/core_1/openMSP430_0/dbg_0/dbg_i2c_0/dbg_i2c_sda_out
add wave -noupdate /chip_tb/DUT/core_1/openMSP430_0/dbg_0/dbg_i2c_0/i2c_state_reg_2_/C
add wave -noupdate -radix hexadecimal /chip_tb/DUT/core_1/openMSP430_0/dbg_0/dbg_i2c_0/i2c_state
add wave -noupdate /chip_tb/DUT/P1_b
add wave -noupdate /chip_tb/DUT/P2_b
add wave -noupdate -radix hexadecimal /chip_tb/DUT/core_1/openMSP430_0/execution_unit_0/pc
add wave -noupdate /chip_tb/DUT/core_1/PMem_0/ram_addr
add wave -noupdate /chip_tb/DUT/core_1/PMem_0/ram_cen
add wave -noupdate /chip_tb/DUT/core_1/PMem_0/ram_clk
add wave -noupdate /chip_tb/DUT/core_1/PMem_0/ram_din
add wave -noupdate /chip_tb/DUT/core_1/PMem_0/ram_dout
add wave -noupdate /chip_tb/DUT/core_1/PMem_0/ram_wen
add wave -noupdate /chip_tb/DUT/core_1/PMem_0/ram_0/A
add wave -noupdate /chip_tb/DUT/core_1/PMem_0/ram_0/I
add wave -noupdate /chip_tb/DUT/core_1/PMem_0/ram_0/O
add wave -noupdate /chip_tb/DUT/core_1/PMem_0/ram_0/OEB
add wave -noupdate /chip_tb/DUT/core_1/PMem_0/ram_0/WEB
add wave -noupdate /chip_tb/DUT/core_1/PMem_0/ram_1/A
add wave -noupdate /chip_tb/DUT/core_1/PMem_0/ram_1/I
add wave -noupdate /chip_tb/DUT/core_1/PMem_0/ram_1/O
add wave -noupdate /chip_tb/DUT/core_1/PMem_0/ram_1/OEB
add wave -noupdate /chip_tb/DUT/core_1/PMem_0/ram_1/WEB
add wave -noupdate -radix hexadecimal {/chip_tb/DUT/core_1/PMem_0/ram_0/memory[0]}
add wave -noupdate -radix hexadecimal {/chip_tb/DUT/core_1/PMem_0/ram_0/memory[1]}
add wave -noupdate -radix hexadecimal {/chip_tb/DUT/core_1/PMem_0/ram_0/memory[2]}
add wave -noupdate -radix hexadecimal {/chip_tb/DUT/core_1/PMem_0/ram_0/memory[3]}
add wave -noupdate -radix hexadecimal {/chip_tb/DUT/core_1/PMem_0/ram_0/memory[4]}
add wave -noupdate -radix hexadecimal {/chip_tb/DUT/core_1/PMem_0/ram_0/memory[5]}
add wave -noupdate -radix hexadecimal {/chip_tb/DUT/core_1/PMem_0/ram_1/memory[0]}
add wave -noupdate -radix hexadecimal {/chip_tb/DUT/core_1/PMem_0/ram_1/memory[1]}
add wave -noupdate -radix hexadecimal {/chip_tb/DUT/core_1/PMem_0/ram_1/memory[2]}
add wave -noupdate -radix hexadecimal {/chip_tb/DUT/core_1/PMem_0/ram_1/memory[3]}
add wave -noupdate -radix hexadecimal {/chip_tb/DUT/core_1/PMem_0/ram_1/memory[4]}
add wave -noupdate -radix hexadecimal {/chip_tb/DUT/core_1/PMem_0/ram_1/memory[5]}
add wave -noupdate -radix hexadecimal {/chip_tb/DUT/core_1/PMem_0/ram_2/memory[0]}
add wave -noupdate -radix hexadecimal {/chip_tb/DUT/core_1/PMem_0/ram_2/memory[1]}
add wave -noupdate -radix hexadecimal {/chip_tb/DUT/core_1/PMem_0/ram_2/memory[2]}
add wave -noupdate -radix hexadecimal {/chip_tb/DUT/core_1/PMem_0/ram_2/memory[3]}
add wave -noupdate -radix hexadecimal {/chip_tb/DUT/core_1/PMem_0/ram_2/memory[4]}
add wave -noupdate -radix hexadecimal {/chip_tb/DUT/core_1/PMem_0/ram_2/memory[5]}
add wave -noupdate -radix hexadecimal {/chip_tb/DUT/core_1/PMem_0/ram_3/memory[0]}
add wave -noupdate -radix hexadecimal {/chip_tb/DUT/core_1/PMem_0/ram_3/memory[1]}
add wave -noupdate -radix hexadecimal {/chip_tb/DUT/core_1/PMem_0/ram_3/memory[2]}
add wave -noupdate -radix hexadecimal {/chip_tb/DUT/core_1/PMem_0/ram_3/memory[3]}
add wave -noupdate -radix hexadecimal {/chip_tb/DUT/core_1/PMem_0/ram_3/memory[4]}
add wave -noupdate -radix hexadecimal {/chip_tb/DUT/core_1/PMem_0/ram_3/memory[5]}
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
