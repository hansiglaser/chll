onerror {resume}
quietly WaveActivateNextPane {} 0
add wave -noupdate /chip_eval_tb/Reset_n_i
add wave -noupdate /chip_eval_tb/Clk_i
add wave -noupdate -divider OpenMSP430
add wave -noupdate /chip_eval_tb/Cpu_En_i
add wave -noupdate /chip_eval_tb/Dbg_En_i
add wave -noupdate /chip_eval_tb/Dbg_SCL_i
add wave -noupdate -expand -group Debug /chip_eval_tb/Dbg_SDA_b
add wave -noupdate -expand -group Debug -radix ascii /chip_eval_tb/DUT/core_1/msp_debug_0/e_state
add wave -noupdate -expand -group Debug -radix ascii /chip_eval_tb/DUT/core_1/msp_debug_0/i_state
add wave -noupdate -expand -group Debug -radix unsigned /chip_eval_tb/DUT/core_1/msp_debug_0/inst_cycle
add wave -noupdate -expand -group Debug -radix ascii /chip_eval_tb/DUT/core_1/msp_debug_0/inst_full
add wave -noupdate -expand -group Debug -radix hexadecimal /chip_eval_tb/DUT/core_1/msp_debug_0/inst_number
add wave -noupdate -expand -group Debug -radix hexadecimal /chip_eval_tb/DUT/core_1/msp_debug_0/inst_pc
add wave -noupdate -expand -group Debug -radix ascii /chip_eval_tb/DUT/core_1/msp_debug_0/inst_short
add wave -noupdate -expand -group Debug /chip_eval_tb/DUT/core_1/msp_debug_0/i_state_bin
add wave -noupdate -expand -group Debug /chip_eval_tb/DUT/core_1/msp_debug_0/e_state_bin
add wave -noupdate -expand -group Debug /chip_eval_tb/DUT/core_1/msp_debug_0/decode
add wave -noupdate -expand -group Debug -radix hexadecimal /chip_eval_tb/DUT/core_1/msp_debug_0/ir
add wave -noupdate -expand -group Debug /chip_eval_tb/DUT/core_1/msp_debug_0/irq_detect
add wave -noupdate -expand -group Debug /chip_eval_tb/DUT/core_1/msp_debug_0/irq_num
add wave -noupdate -expand -group Debug -radix hexadecimal /chip_eval_tb/DUT/core_1/msp_debug_0/pc
add wave -noupdate -expand -group Debug -radix hexadecimal /chip_eval_tb/DUT/core_1/msp_debug_0/opcode
add wave -noupdate -expand -group Debug /chip_eval_tb/DUT/core_1/msp_debug_0/irq
add wave -noupdate -expand -group Debug -radix ascii /chip_eval_tb/DUT/core_1/msp_debug_0/inst_type
add wave -noupdate -expand -group Debug -radix ascii /chip_eval_tb/DUT/core_1/msp_debug_0/inst_name
add wave -noupdate -expand -group Debug -radix ascii /chip_eval_tb/DUT/core_1/msp_debug_0/inst_bw
add wave -noupdate -expand -group Debug -radix ascii /chip_eval_tb/DUT/core_1/msp_debug_0/inst_src
add wave -noupdate -expand -group Debug /chip_eval_tb/DUT/core_1/msp_debug_0/src_reg
add wave -noupdate -expand -group Debug -radix ascii /chip_eval_tb/DUT/core_1/msp_debug_0/inst_dst
add wave -noupdate -expand -group Debug -radix ascii /chip_eval_tb/DUT/core_1/msp_debug_0/inst_as
add wave -noupdate -expand -group Debug -radix ascii /chip_eval_tb/DUT/core_1/msp_debug_0/inst_ad
add wave -noupdate -expand /chip_eval_tb/P1_b
add wave -noupdate -expand /chip_eval_tb/P2_b
add wave -noupdate /chip_eval_tb/UartRxD_i
add wave -noupdate /chip_eval_tb/UartTxD_o
add wave -noupdate /chip_eval_tb/SCK_o
add wave -noupdate /chip_eval_tb/MOSI_o
add wave -noupdate /chip_eval_tb/MISO_i
add wave -noupdate -expand -group TimerA /chip_eval_tb/DUT/core_1/timerA_0/irq_ta0
add wave -noupdate -expand -group TimerA /chip_eval_tb/DUT/core_1/timerA_0/irq_ta1
add wave -noupdate -expand -group TimerA /chip_eval_tb/DUT/core_1/timerA_0/per_dout
add wave -noupdate -expand -group TimerA /chip_eval_tb/DUT/core_1/timerA_0/ta_out0
add wave -noupdate -expand -group TimerA /chip_eval_tb/DUT/core_1/timerA_0/ta_out0_en
add wave -noupdate -expand -group TimerA /chip_eval_tb/DUT/core_1/timerA_0/ta_out1
add wave -noupdate -expand -group TimerA /chip_eval_tb/DUT/core_1/timerA_0/ta_out1_en
add wave -noupdate -expand -group TimerA /chip_eval_tb/DUT/core_1/timerA_0/ta_out2
add wave -noupdate -expand -group TimerA /chip_eval_tb/DUT/core_1/timerA_0/ta_out2_en
add wave -noupdate -expand -group TimerA /chip_eval_tb/DUT/core_1/timerA_0/aclk_en
add wave -noupdate -expand -group TimerA /chip_eval_tb/DUT/core_1/timerA_0/dbg_freeze
add wave -noupdate -expand -group TimerA /chip_eval_tb/DUT/core_1/timerA_0/inclk
add wave -noupdate -expand -group TimerA /chip_eval_tb/DUT/core_1/timerA_0/irq_ta0_acc
add wave -noupdate -expand -group TimerA /chip_eval_tb/DUT/core_1/timerA_0/mclk
add wave -noupdate -expand -group TimerA /chip_eval_tb/DUT/core_1/timerA_0/per_addr
add wave -noupdate -expand -group TimerA /chip_eval_tb/DUT/core_1/timerA_0/per_din
add wave -noupdate -expand -group TimerA /chip_eval_tb/DUT/core_1/timerA_0/per_en
add wave -noupdate -expand -group TimerA /chip_eval_tb/DUT/core_1/timerA_0/per_we
add wave -noupdate -expand -group TimerA /chip_eval_tb/DUT/core_1/timerA_0/puc_rst
add wave -noupdate -expand -group TimerA /chip_eval_tb/DUT/core_1/timerA_0/smclk_en
add wave -noupdate -expand -group TimerA /chip_eval_tb/DUT/core_1/timerA_0/ta_cci0a
add wave -noupdate -expand -group TimerA /chip_eval_tb/DUT/core_1/timerA_0/ta_cci0b
add wave -noupdate -expand -group TimerA /chip_eval_tb/DUT/core_1/timerA_0/ta_cci1a
add wave -noupdate -expand -group TimerA /chip_eval_tb/DUT/core_1/timerA_0/ta_cci1b
add wave -noupdate -expand -group TimerA /chip_eval_tb/DUT/core_1/timerA_0/ta_cci2a
add wave -noupdate -expand -group TimerA /chip_eval_tb/DUT/core_1/timerA_0/ta_cci2b
add wave -noupdate -expand -group TimerA /chip_eval_tb/DUT/core_1/timerA_0/taclk
add wave -noupdate -expand -group TimerA /chip_eval_tb/DUT/core_1/timerA_0/tactl
add wave -noupdate -expand -group TimerA -radix hexadecimal /chip_eval_tb/DUT/core_1/timerA_0/tar
add wave -noupdate -expand -group TimerA /chip_eval_tb/DUT/core_1/timerA_0/tacctl0
add wave -noupdate -expand -group TimerA -radix hexadecimal /chip_eval_tb/DUT/core_1/timerA_0/taccr0
add wave -noupdate -expand -group TimerA /chip_eval_tb/DUT/core_1/timerA_0/tacctl1
add wave -noupdate -expand -group TimerA -radix hexadecimal /chip_eval_tb/DUT/core_1/timerA_0/taccr1
add wave -noupdate -expand -group TimerA /chip_eval_tb/DUT/core_1/timerA_0/tacctl2
add wave -noupdate -expand -group TimerA -radix hexadecimal /chip_eval_tb/DUT/core_1/timerA_0/taccr2
add wave -noupdate -divider Reconf.Module
add wave -noupdate /chip_eval_tb/Inputs_i
add wave -noupdate /chip_eval_tb/Outputs_o
add wave -noupdate /chip_eval_tb/SPIMISO_i
add wave -noupdate /chip_eval_tb/SPIMOSI_o
add wave -noupdate /chip_eval_tb/SPISCK_o
add wave -noupdate /chip_eval_tb/I2CSCL_b
add wave -noupdate /chip_eval_tb/I2CSDA_b
add wave -noupdate /chip_eval_tb/AdcConvComplete_i
add wave -noupdate /chip_eval_tb/AdcDoConvert_o
add wave -noupdate /chip_eval_tb/AdcValue_i
add wave -noupdate -divider {ADT7310 Model}
add wave -noupdate /chip_eval_tb/ADT7310CS_n_o
add wave -noupdate /chip_eval_tb/CT_n_s
add wave -noupdate /chip_eval_tb/INT_n_s
add wave -noupdate /chip_eval_tb/Temp_s
add wave -noupdate -divider UART
add wave -noupdate /chip_eval_tb/TxData_i
add wave -noupdate /chip_eval_tb/TxWr_i
add wave -noupdate /chip_eval_tb/TxEmpty_o
add wave -noupdate /chip_eval_tb/TxFull_o
add wave -noupdate /chip_eval_tb/RxData_o
add wave -noupdate /chip_eval_tb/RxRd_i
add wave -noupdate /chip_eval_tb/RxFull_o
add wave -noupdate /chip_eval_tb/RxEmpty_o
add wave -noupdate /chip_eval_tb/BitsSelect_i
add wave -noupdate /chip_eval_tb/ParityOn_i
add wave -noupdate /chip_eval_tb/ParityEvenOdd_i
add wave -noupdate /chip_eval_tb/SpeedDivider_i
add wave -noupdate /chip_eval_tb/ErrorReset_i
add wave -noupdate /chip_eval_tb/RxParityErrorIndicator_o
add wave -noupdate /chip_eval_tb/RxStopBitErrorIndicator_o
add wave -noupdate /chip_eval_tb/RxBufferFullErrorIndicator_o
add wave -noupdate /chip_eval_tb/ScanEnable_i
add wave -noupdate /chip_eval_tb/ScanClk_i
add wave -noupdate /chip_eval_tb/ScanDataIn_i
add wave -noupdate /chip_eval_tb/ScanDataOut_o
add wave -noupdate -radix ascii /chip_eval_tb/UartRx
add wave -noupdate /chip_eval_tb/UartCh
add wave -noupdate -divider {Chip Internal}
add wave -noupdate /chip_eval_tb/DUT/SPIMOSI_o
add wave -noupdate /chip_eval_tb/DUT/SPISCK_o
add wave -noupdate /chip_eval_tb/DUT/I2CSCL_b
add wave -noupdate /chip_eval_tb/DUT/AdcDoConvert_o
add wave -noupdate /chip_eval_tb/DUT/P1_DIn_i_s
add wave -noupdate /chip_eval_tb/DUT/P1_DOut_o_s
add wave -noupdate /chip_eval_tb/DUT/P1_En_o_s
add wave -noupdate /chip_eval_tb/DUT/P1_En_o_s_n
add wave -noupdate /chip_eval_tb/DUT/P2_DIn_i_s
add wave -noupdate /chip_eval_tb/DUT/P2_DOut_o_s
add wave -noupdate /chip_eval_tb/DUT/P2_En_o_s
add wave -noupdate /chip_eval_tb/DUT/P2_En_o_s_n
add wave -noupdate /chip_eval_tb/DUT/SPIMISO_i_s
add wave -noupdate /chip_eval_tb/DUT/SPIMOSI_o_s
add wave -noupdate /chip_eval_tb/DUT/SPISCK_o_s
add wave -noupdate /chip_eval_tb/DUT/I2CSCL_o_s
add wave -noupdate /chip_eval_tb/DUT/I2CSDA_i_s
add wave -noupdate /chip_eval_tb/DUT/I2CSDA_o_s
add wave -noupdate /chip_eval_tb/DUT/I2CSDA_b
TreeUpdate [SetDefaultTree]
WaveRestoreCursors {{Cursor 1} {19051246500 ps} 0} {{Cursor 2} {5436252000 ps} 0}
configure wave -namecolwidth 291
configure wave -valuecolwidth 320
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
WaveRestoreZoom {0 ps} {52500 us}
