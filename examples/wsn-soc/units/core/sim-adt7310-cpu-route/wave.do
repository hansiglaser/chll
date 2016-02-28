onerror {resume}
quietly WaveActivateNextPane {} 0
add wave -noupdate Chip_ADT7310_tb/DUT/Reset_n_i
add wave -noupdate Chip_ADT7310_tb/DUT/Clk_i
add wave -noupdate -divider CPU
add wave -noupdate /chip_adt7310_tb/DUT/Cpu_En_i
add wave -noupdate /chip_adt7310_tb/DUT/Dbg_En_i
add wave -noupdate /chip_adt7310_tb/DUT/Dbg_SCL_i
add wave -noupdate /chip_adt7310_tb/DUT/Dbg_SDA_b
add wave -noupdate /chip_adt7310_tb/DUT/P1_b
add wave -noupdate /chip_adt7310_tb/DUT/P2_b
add wave -noupdate /chip_adt7310_tb/DUT/UartRxD_i
add wave -noupdate /chip_adt7310_tb/DUT/UartTxD_o
add wave -noupdate /chip_adt7310_tb/DUT/MISO_i
add wave -noupdate /chip_adt7310_tb/DUT/MOSI_o
add wave -noupdate /chip_adt7310_tb/DUT/SCK_o
add wave -noupdate -divider Reconf.Module
add wave -noupdate Chip_ADT7310_tb/DUT/Inputs_i
add wave -noupdate Chip_ADT7310_tb/DUT/Outputs_o
add wave -noupdate Chip_ADT7310_tb/DUT/SPIMISO_i
add wave -noupdate Chip_ADT7310_tb/DUT/SPIMOSI_o
add wave -noupdate Chip_ADT7310_tb/DUT/SPISCK_o
add wave -noupdate Chip_ADT7310_tb/DUT/I2CSCL_b
add wave -noupdate Chip_ADT7310_tb/DUT/I2CSDA_b
#add wave -noupdate Chip_ADT7310_tb/DUT/OneWire_i
#add wave -noupdate Chip_ADT7310_tb/DUT/OneWire_o
#add wave -noupdate Chip_ADT7310_tb/DUT/PWMInput_i
#add wave -noupdate Chip_ADT7310_tb/DUT/SENTInput_i
#add wave -noupdate Chip_ADT7310_tb/DUT/SPCInput_i
#add wave -noupdate Chip_ADT7310_tb/DUT/SPCTrigger_o
add wave -noupdate Chip_ADT7310_tb/DUT/AdcConvComplete_i
add wave -noupdate Chip_ADT7310_tb/DUT/AdcDoConvert_o
add wave -noupdate Chip_ADT7310_tb/DUT/AdcValue_i
add wave -noupdate -group {OpenMSP Memory Backbone} -radix hexadecimal Chip_ADT7310_tb/DUT/core_1/openMSP430_0/mem_backbone_0/dbg_mem_din
add wave -noupdate -group {OpenMSP Memory Backbone} -radix hexadecimal Chip_ADT7310_tb/DUT/core_1/openMSP430_0/mem_backbone_0/dmem_addr
add wave -noupdate -group {OpenMSP Memory Backbone} Chip_ADT7310_tb/DUT/core_1/openMSP430_0/mem_backbone_0/dmem_cen
add wave -noupdate -group {OpenMSP Memory Backbone} -radix hexadecimal Chip_ADT7310_tb/DUT/core_1/openMSP430_0/mem_backbone_0/dmem_din
add wave -noupdate -group {OpenMSP Memory Backbone} Chip_ADT7310_tb/DUT/core_1/openMSP430_0/mem_backbone_0/dmem_wen
add wave -noupdate -group {OpenMSP Memory Backbone} -radix hexadecimal Chip_ADT7310_tb/DUT/core_1/openMSP430_0/mem_backbone_0/eu_mdb_in
add wave -noupdate -group {OpenMSP Memory Backbone} -radix hexadecimal Chip_ADT7310_tb/DUT/core_1/openMSP430_0/mem_backbone_0/fe_mdb_in
add wave -noupdate -group {OpenMSP Memory Backbone} Chip_ADT7310_tb/DUT/core_1/openMSP430_0/mem_backbone_0/fe_pmem_wait
add wave -noupdate -group {OpenMSP Memory Backbone} -radix hexadecimal Chip_ADT7310_tb/DUT/core_1/openMSP430_0/mem_backbone_0/per_addr
add wave -noupdate -group {OpenMSP Memory Backbone} -radix hexadecimal Chip_ADT7310_tb/DUT/core_1/openMSP430_0/mem_backbone_0/per_din
add wave -noupdate -group {OpenMSP Memory Backbone} Chip_ADT7310_tb/DUT/core_1/openMSP430_0/mem_backbone_0/per_we
add wave -noupdate -group {OpenMSP Memory Backbone} Chip_ADT7310_tb/DUT/core_1/openMSP430_0/mem_backbone_0/per_en
add wave -noupdate -group {OpenMSP Memory Backbone} -radix hexadecimal Chip_ADT7310_tb/DUT/core_1/openMSP430_0/mem_backbone_0/pmem_addr
add wave -noupdate -group {OpenMSP Memory Backbone} Chip_ADT7310_tb/DUT/core_1/openMSP430_0/mem_backbone_0/pmem_cen
add wave -noupdate -group {OpenMSP Memory Backbone} -radix hexadecimal Chip_ADT7310_tb/DUT/core_1/openMSP430_0/mem_backbone_0/pmem_din
add wave -noupdate -group {OpenMSP Memory Backbone} Chip_ADT7310_tb/DUT/core_1/openMSP430_0/mem_backbone_0/pmem_wen
add wave -noupdate -group {OpenMSP Memory Backbone} Chip_ADT7310_tb/DUT/core_1/openMSP430_0/mem_backbone_0/dbg_halt_st
add wave -noupdate -group {OpenMSP Memory Backbone} -radix hexadecimal Chip_ADT7310_tb/DUT/core_1/openMSP430_0/mem_backbone_0/dbg_mem_addr
add wave -noupdate -group {OpenMSP Memory Backbone} -radix hexadecimal Chip_ADT7310_tb/DUT/core_1/openMSP430_0/mem_backbone_0/dbg_mem_dout
add wave -noupdate -group {OpenMSP Memory Backbone} Chip_ADT7310_tb/DUT/core_1/openMSP430_0/mem_backbone_0/dbg_mem_en
add wave -noupdate -group {OpenMSP Memory Backbone} Chip_ADT7310_tb/DUT/core_1/openMSP430_0/mem_backbone_0/dbg_mem_wr
add wave -noupdate -group {OpenMSP Memory Backbone} -radix hexadecimal Chip_ADT7310_tb/DUT/core_1/openMSP430_0/mem_backbone_0/dmem_dout
add wave -noupdate -group {OpenMSP Memory Backbone} -radix hexadecimal Chip_ADT7310_tb/DUT/core_1/openMSP430_0/mem_backbone_0/eu_mab
add wave -noupdate -group {OpenMSP Memory Backbone} Chip_ADT7310_tb/DUT/core_1/openMSP430_0/mem_backbone_0/eu_mb_wr
add wave -noupdate -group {OpenMSP Memory Backbone} -radix hexadecimal Chip_ADT7310_tb/DUT/core_1/openMSP430_0/mem_backbone_0/eu_mdb_out
add wave -noupdate -group {OpenMSP Memory Backbone} -radix hexadecimal Chip_ADT7310_tb/DUT/core_1/openMSP430_0/mem_backbone_0/fe_mab
add wave -noupdate -group {OpenMSP Memory Backbone} Chip_ADT7310_tb/DUT/core_1/openMSP430_0/mem_backbone_0/fe_mb_en
add wave -noupdate -group {OpenMSP Memory Backbone} Chip_ADT7310_tb/DUT/core_1/openMSP430_0/mem_backbone_0/mclk
add wave -noupdate -group {OpenMSP Memory Backbone} -radix hexadecimal Chip_ADT7310_tb/DUT/core_1/openMSP430_0/mem_backbone_0/per_dout
add wave -noupdate -group {OpenMSP Memory Backbone} -radix hexadecimal Chip_ADT7310_tb/DUT/core_1/openMSP430_0/mem_backbone_0/pmem_dout
add wave -noupdate -group {OpenMSP Memory Backbone} Chip_ADT7310_tb/DUT/core_1/openMSP430_0/mem_backbone_0/puc_rst
add wave -noupdate -group {OpenMSP Memory Backbone} Chip_ADT7310_tb/DUT/core_1/openMSP430_0/mem_backbone_0/scan_enable
add wave -noupdate -group {OpenMSP Memory Backbone} -radix hexadecimal Chip_ADT7310_tb/DUT/core_1/openMSP430_0/mem_backbone_0/per_dout_val
add wave -noupdate -group {OpenMSP Memory Backbone} Chip_ADT7310_tb/DUT/core_1/openMSP430_0/mem_backbone_0/pmem_dout_bckup_sel
add wave -noupdate -expand -group {OpenMSP430 Debug Utility} -radix hexadecimal Chip_ADT7310_tb/DUT/core_1/openMSP430_0/frontend_0/pc
add wave -noupdate -expand -group {OpenMSP430 Debug Utility} -radix hexadecimal Chip_ADT7310_tb/DUT/core_1/openMSP430_0/frontend_0/pc_nxt
add wave -noupdate -expand -group OpenMSP430 -group Reset Chip_ADT7310_tb/DUT/core_1/PUC_Reset_s
add wave -noupdate -expand -group OpenMSP430 -group Reset Chip_ADT7310_tb/DUT/core_1/Cpu_En_i
add wave -noupdate -expand -group OpenMSP430 -group Clocks Chip_ADT7310_tb/DUT/core_1/LFXT_Clk_i
add wave -noupdate -expand -group OpenMSP430 -group Clocks Chip_ADT7310_tb/DUT/core_1/MClk_s
add wave -noupdate -expand -group OpenMSP430 -group Clocks Chip_ADT7310_tb/DUT/core_1/SMClk_s
add wave -noupdate -expand -group OpenMSP430 -group {Program Memory} -radix hexadecimal Chip_ADT7310_tb/DUT/core_1/PMem_Addr_s
add wave -noupdate -expand -group OpenMSP430 -group {Program Memory} Chip_ADT7310_tb/DUT/core_1/PMem_En_n_s
add wave -noupdate -expand -group OpenMSP430 -group {Program Memory} -radix hexadecimal Chip_ADT7310_tb/DUT/core_1/PMem_DIn_s
add wave -noupdate -expand -group OpenMSP430 -group {Program Memory} Chip_ADT7310_tb/DUT/core_1/PMem_Wr_n_s
add wave -noupdate -expand -group OpenMSP430 -group {Program Memory} -radix hexadecimal Chip_ADT7310_tb/DUT/core_1/PMem_DOut_s
add wave -noupdate -expand -group OpenMSP430 -group {Data Memory} -radix hexadecimal Chip_ADT7310_tb/DUT/core_1/DMem_Addr_s
add wave -noupdate -expand -group OpenMSP430 -group {Data Memory} Chip_ADT7310_tb/DUT/core_1/DMem_En_n_s
add wave -noupdate -expand -group OpenMSP430 -group {Data Memory} -radix hexadecimal Chip_ADT7310_tb/DUT/core_1/DMem_DIn_s
add wave -noupdate -expand -group OpenMSP430 -group {Data Memory} Chip_ADT7310_tb/DUT/core_1/DMem_Wr_n_s
add wave -noupdate -expand -group OpenMSP430 -group {Data Memory} -radix hexadecimal Chip_ADT7310_tb/DUT/core_1/DMem_DOut_s
add wave -noupdate -expand -group OpenMSP430 -expand -group Peripherals -radix hexadecimal Chip_ADT7310_tb/DUT/core_1/Per_Addr_s
add wave -noupdate -expand -group OpenMSP430 -expand -group Peripherals -radix hexadecimal Chip_ADT7310_tb/DUT/core_1/Per_DIn_s
add wave -noupdate -expand -group OpenMSP430 -expand -group Peripherals -radix hexadecimal Chip_ADT7310_tb/DUT/core_1/Per_DOut_s
add wave -noupdate -expand -group OpenMSP430 -expand -group Peripherals Chip_ADT7310_tb/DUT/core_1/Per_Wr_s
add wave -noupdate -expand -group OpenMSP430 -expand -group Peripherals Chip_ADT7310_tb/DUT/core_1/Per_En_s
add wave -noupdate -expand -group OpenMSP430 -group Interrupts Chip_ADT7310_tb/DUT/core_1/IRQ_s
add wave -noupdate -expand -group OpenMSP430 -expand -group Interrupts /chip_adt7310_tb/DUT/core_1/openMSP430_0/irq
add wave -noupdate -expand -group OpenMSP430 -group Debug Chip_ADT7310_tb/DUT/core_1/Dbg_En_i
add wave -noupdate -expand -group OpenMSP430 -group Debug Chip_ADT7310_tb/DUT/core_1/Dbg_Freeze_s
#add wave -noupdate -expand -group OpenMSP430 -group Debug Chip_ADT7310_tb/DUT/core_1/Dbg_UART_RxD_s
#add wave -noupdate -expand -group OpenMSP430 -group Debug Chip_ADT7310_tb/DUT/core_1/Dbg_UART_TxD_s
add wave -noupdate -expand -group OpenMSP430 -group Debug Chip_ADT7310_tb/DUT/Dbg_SCL_i
add wave -noupdate -expand -group OpenMSP430 -group Debug Chip_ADT7310_tb/DUT/Dbg_SDA_b
add wave -noupdate -expand -group OpenMSP430 -group TimerA -radix hexadecimal Chip_ADT7310_tb/DUT/core_1/timerA_0/tar
add wave -noupdate -expand -group OpenMSP430 -group TimerA Chip_ADT7310_tb/DUT/core_1/timerA_0/tacctl0
add wave -noupdate -expand -group OpenMSP430 -group TimerA -radix hexadecimal Chip_ADT7310_tb/DUT/core_1/timerA_0/taccr0
add wave -noupdate -expand -group OpenMSP430 -group TimerA Chip_ADT7310_tb/DUT/core_1/timerA_0/tacctl1
add wave -noupdate -expand -group OpenMSP430 -group TimerA -radix hexadecimal Chip_ADT7310_tb/DUT/core_1/timerA_0/taccr1
add wave -noupdate -expand -group OpenMSP430 -group TimerA Chip_ADT7310_tb/DUT/core_1/timerA_0/tacctl2
add wave -noupdate -expand -group OpenMSP430 -group TimerA -radix hexadecimal Chip_ADT7310_tb/DUT/core_1/timerA_0/taccr2
add wave -noupdate Chip_ADT7310_tb/DUT/core_1/Gpio_DOut_s
add wave -noupdate Chip_ADT7310_tb/DUT/P1_b
add wave -noupdate Chip_ADT7310_tb/DUT/P2_b
add wave -noupdate Chip_ADT7310_tb/DUT/core_1/TimerA_DOut_s
add wave -noupdate Chip_ADT7310_tb/DUT/core_1/TimerA_Out0_s
add wave -noupdate Chip_ADT7310_tb/DUT/core_1/TimerA_Out0_En_s
add wave -noupdate Chip_ADT7310_tb/DUT/core_1/TimerA_Out1_s
add wave -noupdate Chip_ADT7310_tb/DUT/core_1/TimerA_Out1_En_s
add wave -noupdate Chip_ADT7310_tb/DUT/core_1/TimerA_Out2_s
add wave -noupdate Chip_ADT7310_tb/DUT/core_1/TimerA_Out2_En_s
add wave -noupdate -divider UART
add wave -noupdate /Chip_ADT7310_tb/UartRxD_i
add wave -noupdate /Chip_ADT7310_tb/UartTxD_o
add wave -noupdate -radix ascii /Chip_ADT7310_tb/Uart_1/RxData_o
add wave -noupdate -radix ascii /Chip_ADT7310_tb/UartRx_Proc/Ch
add wave -noupdate /Chip_ADT7310_tb/Uart_1/RxRd_i
add wave -noupdate /Chip_ADT7310_tb/Uart_1/RxEmpty_o
add wave -noupdate /Chip_ADT7310_tb/Uart_1/RxFull_o
add wave -noupdate -radix ascii /Chip_ADT7310_tb/Uart_1/TxData_i
add wave -noupdate /Chip_ADT7310_tb/Uart_1/TxWr_i
add wave -noupdate /Chip_ADT7310_tb/Uart_1/TxEmpty_o
add wave -noupdate /Chip_ADT7310_tb/Uart_1/TxFull_o
add wave -noupdate -radix ascii /Chip_ADT7310_tb/Uart_1/TXMOD/TXFIFO/DualPortRam/DataB_o
add wave -noupdate -radix ascii /Chip_ADT7310_tb/Uart_1/RXMOD/RXFIFO/DualPortRam/DataBuffer
add wave -noupdate -divider SPI
add wave -noupdate {/chip_adt7310_tb/DUT/P1_b[0]}
add wave -noupdate Chip_ADT7310_tb/DUT/SCK_o
add wave -noupdate Chip_ADT7310_tb/DUT/MOSI_o
add wave -noupdate Chip_ADT7310_tb/DUT/MISO_i
add wave -noupdate -divider ADT7310
add wave -noupdate /Chip_ADT7310_tb/adt7310_1/SCLK_i
add wave -noupdate /Chip_ADT7310_tb/adt7310_1/DOUT_o
add wave -noupdate /Chip_ADT7310_tb/adt7310_1/DIN_i
add wave -noupdate /Chip_ADT7310_tb/adt7310_1/CS_n_i
add wave -noupdate /Chip_ADT7310_tb/adt7310_1/CT_n_o
add wave -noupdate /Chip_ADT7310_tb/adt7310_1/INT_n_o
add wave -noupdate /Chip_ADT7310_tb/adt7310_1/Temp_i
TreeUpdate [SetDefaultTree]
WaveRestoreCursors {{Cursor 2} {1901256700 ps} 0}
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
WaveRestoreZoom {0 ps} {4456931500 ps}
