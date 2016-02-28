onerror {resume}
quietly WaveActivateNextPane {} 0
add wave -noupdate /chip_tb/DUT/core_1/DBG_I2C_ADDR
add wave -noupdate /chip_tb/DUT/core_1/DBG_I2C_BROADCAST
add wave -noupdate /chip_tb/DUT/core_1/SPI_DataWidth
add wave -noupdate /chip_tb/DUT/core_1/SPI_SPPRWidth
add wave -noupdate /chip_tb/DUT/core_1/SPI_SPRWidth
add wave -noupdate /chip_tb/DUT/core_1/SPI_FIFOReadWidth
add wave -noupdate /chip_tb/DUT/core_1/SPI_FIFOWriteWidth
add wave -noupdate /chip_tb/DUT/core_1/I2C_ReadCountWidth
add wave -noupdate /chip_tb/DUT/core_1/I2C_FIFOAddressWidth
add wave -noupdate /chip_tb/DUT/core_1/I2C_DividerWidth
#add wave -noupdate /chip_tb/DUT/core_1/OneWire_ROMIDArraySize
#add wave -noupdate /chip_tb/DUT/core_1/OneWire_ROMIDIndexSize
#add wave -noupdate /chip_tb/DUT/core_1/OneWire_ROMIDByteIndexSize
#add wave -noupdate /chip_tb/DUT/core_1/OneWire_SearchCommand
#add wave -noupdate /chip_tb/DUT/core_1/OneWire_CondSearchCommand
#add wave -noupdate /chip_tb/DUT/core_1/OneWire_MatchCommand
#add wave -noupdate /chip_tb/DUT/core_1/OneWire_ReadCommand
#add wave -noupdate /chip_tb/DUT/core_1/OneWire_SkipCommand
#add wave -noupdate /chip_tb/DUT/core_1/OneWire_OverdriveSkipCommand
#add wave -noupdate /chip_tb/DUT/core_1/OneWire_OverdriveMatchCommand
#add wave -noupdate /chip_tb/DUT/core_1/OneWire_ConditionalReadCommand
#add wave -noupdate /chip_tb/DUT/core_1/OneWire_ResumeCommand
#add wave -noupdate /chip_tb/DUT/core_1/OneWire_TimerWidth
#add wave -noupdate /chip_tb/DUT/core_1/PWM_Resolution
#add wave -noupdate /chip_tb/DUT/core_1/PWM_CounterWidth
#add wave -noupdate /chip_tb/DUT/core_1/SENT_MaxDatNibble
#add wave -noupdate /chip_tb/DUT/core_1/SENT_CountWidth
#add wave -noupdate /chip_tb/DUT/core_1/SPC_MaxDatNibble
#add wave -noupdate /chip_tb/DUT/core_1/SPC_CountWidth
#add wave -noupdate /chip_tb/DUT/core_1/SPC_TimeoutWidth
#add wave -noupdate /chip_tb/DUT/core_1/SPC_UseTimeout
add wave -noupdate /chip_tb/DUT/core_1/Reset_n_i
add wave -noupdate /chip_tb/DUT/core_1/Clk_i
add wave -noupdate /chip_tb/DUT/core_1/Inputs_i
add wave -noupdate /chip_tb/DUT/core_1/Outputs_o
add wave -noupdate /chip_tb/DUT/core_1/SPIMISO_i
add wave -noupdate /chip_tb/DUT/core_1/SPIMOSI_o
add wave -noupdate /chip_tb/DUT/core_1/SPISCK_o
add wave -noupdate /chip_tb/DUT/core_1/I2CSCL_o
add wave -noupdate /chip_tb/DUT/core_1/I2CSDA_i
add wave -noupdate /chip_tb/DUT/core_1/I2CSDA_o
#add wave -noupdate /chip_tb/DUT/core_1/OneWire_i
#add wave -noupdate /chip_tb/DUT/core_1/OneWire_o
#add wave -noupdate /chip_tb/DUT/core_1/PWMInput_i
#add wave -noupdate /chip_tb/DUT/core_1/SENTInput_i
#add wave -noupdate /chip_tb/DUT/core_1/SPCInput_i
#add wave -noupdate /chip_tb/DUT/core_1/SPCTrigger_o
add wave -noupdate /chip_tb/DUT/core_1/AdcConvComplete_i
add wave -noupdate /chip_tb/DUT/core_1/AdcDoConvert_o
add wave -noupdate /chip_tb/DUT/core_1/AdcValue_i
add wave -noupdate -group {OpenMSP Memory Backbone} -radix hexadecimal /chip_tb/DUT/core_1/openMSP430_0/mem_backbone_0/PMEM_OFFSET
add wave -noupdate -group {OpenMSP Memory Backbone} -radix hexadecimal /chip_tb/DUT/core_1/openMSP430_0/mem_backbone_0/dbg_mem_din
add wave -noupdate -group {OpenMSP Memory Backbone} -radix hexadecimal /chip_tb/DUT/core_1/openMSP430_0/mem_backbone_0/dmem_addr
add wave -noupdate -group {OpenMSP Memory Backbone} /chip_tb/DUT/core_1/openMSP430_0/mem_backbone_0/dmem_cen
add wave -noupdate -group {OpenMSP Memory Backbone} -radix hexadecimal /chip_tb/DUT/core_1/openMSP430_0/mem_backbone_0/dmem_din
add wave -noupdate -group {OpenMSP Memory Backbone} /chip_tb/DUT/core_1/openMSP430_0/mem_backbone_0/dmem_wen
add wave -noupdate -group {OpenMSP Memory Backbone} -radix hexadecimal /chip_tb/DUT/core_1/openMSP430_0/mem_backbone_0/eu_mdb_in
add wave -noupdate -group {OpenMSP Memory Backbone} -radix hexadecimal /chip_tb/DUT/core_1/openMSP430_0/mem_backbone_0/fe_mdb_in
add wave -noupdate -group {OpenMSP Memory Backbone} /chip_tb/DUT/core_1/openMSP430_0/mem_backbone_0/fe_pmem_wait
add wave -noupdate -group {OpenMSP Memory Backbone} -radix hexadecimal /chip_tb/DUT/core_1/openMSP430_0/mem_backbone_0/per_addr
add wave -noupdate -group {OpenMSP Memory Backbone} -radix hexadecimal /chip_tb/DUT/core_1/openMSP430_0/mem_backbone_0/per_din
add wave -noupdate -group {OpenMSP Memory Backbone} /chip_tb/DUT/core_1/openMSP430_0/mem_backbone_0/per_we
add wave -noupdate -group {OpenMSP Memory Backbone} /chip_tb/DUT/core_1/openMSP430_0/mem_backbone_0/per_en
add wave -noupdate -group {OpenMSP Memory Backbone} -radix hexadecimal /chip_tb/DUT/core_1/openMSP430_0/mem_backbone_0/pmem_addr
add wave -noupdate -group {OpenMSP Memory Backbone} /chip_tb/DUT/core_1/openMSP430_0/mem_backbone_0/pmem_cen
add wave -noupdate -group {OpenMSP Memory Backbone} -radix hexadecimal /chip_tb/DUT/core_1/openMSP430_0/mem_backbone_0/pmem_din
add wave -noupdate -group {OpenMSP Memory Backbone} /chip_tb/DUT/core_1/openMSP430_0/mem_backbone_0/pmem_wen
add wave -noupdate -group {OpenMSP Memory Backbone} /chip_tb/DUT/core_1/openMSP430_0/mem_backbone_0/dbg_halt_st
add wave -noupdate -group {OpenMSP Memory Backbone} -radix hexadecimal /chip_tb/DUT/core_1/openMSP430_0/mem_backbone_0/dbg_mem_addr
add wave -noupdate -group {OpenMSP Memory Backbone} -radix hexadecimal /chip_tb/DUT/core_1/openMSP430_0/mem_backbone_0/dbg_mem_dout
add wave -noupdate -group {OpenMSP Memory Backbone} /chip_tb/DUT/core_1/openMSP430_0/mem_backbone_0/dbg_mem_en
add wave -noupdate -group {OpenMSP Memory Backbone} /chip_tb/DUT/core_1/openMSP430_0/mem_backbone_0/dbg_mem_wr
add wave -noupdate -group {OpenMSP Memory Backbone} -radix hexadecimal /chip_tb/DUT/core_1/openMSP430_0/mem_backbone_0/dmem_dout
add wave -noupdate -group {OpenMSP Memory Backbone} -radix hexadecimal /chip_tb/DUT/core_1/openMSP430_0/mem_backbone_0/eu_mab
add wave -noupdate -group {OpenMSP Memory Backbone} /chip_tb/DUT/core_1/openMSP430_0/mem_backbone_0/eu_mb_en
add wave -noupdate -group {OpenMSP Memory Backbone} /chip_tb/DUT/core_1/openMSP430_0/mem_backbone_0/eu_mb_wr
add wave -noupdate -group {OpenMSP Memory Backbone} -radix hexadecimal /chip_tb/DUT/core_1/openMSP430_0/mem_backbone_0/eu_mdb_out
add wave -noupdate -group {OpenMSP Memory Backbone} -radix hexadecimal /chip_tb/DUT/core_1/openMSP430_0/mem_backbone_0/fe_mab
add wave -noupdate -group {OpenMSP Memory Backbone} /chip_tb/DUT/core_1/openMSP430_0/mem_backbone_0/fe_mb_en
add wave -noupdate -group {OpenMSP Memory Backbone} /chip_tb/DUT/core_1/openMSP430_0/mem_backbone_0/mclk
add wave -noupdate -group {OpenMSP Memory Backbone} -radix hexadecimal /chip_tb/DUT/core_1/openMSP430_0/mem_backbone_0/per_dout
add wave -noupdate -group {OpenMSP Memory Backbone} -radix hexadecimal /chip_tb/DUT/core_1/openMSP430_0/mem_backbone_0/pmem_dout
add wave -noupdate -group {OpenMSP Memory Backbone} /chip_tb/DUT/core_1/openMSP430_0/mem_backbone_0/puc_rst
add wave -noupdate -group {OpenMSP Memory Backbone} /chip_tb/DUT/core_1/openMSP430_0/mem_backbone_0/scan_enable
add wave -noupdate -group {OpenMSP Memory Backbone} /chip_tb/DUT/core_1/openMSP430_0/mem_backbone_0/eu_dmem_cen
add wave -noupdate -group {OpenMSP Memory Backbone} -radix hexadecimal /chip_tb/DUT/core_1/openMSP430_0/mem_backbone_0/eu_dmem_addr
add wave -noupdate -group {OpenMSP Memory Backbone} /chip_tb/DUT/core_1/openMSP430_0/mem_backbone_0/dbg_dmem_cen
add wave -noupdate -group {OpenMSP Memory Backbone} -radix hexadecimal /chip_tb/DUT/core_1/openMSP430_0/mem_backbone_0/dbg_dmem_addr
add wave -noupdate -group {OpenMSP Memory Backbone} /chip_tb/DUT/core_1/openMSP430_0/mem_backbone_0/eu_pmem_cen
add wave -noupdate -group {OpenMSP Memory Backbone} -radix hexadecimal /chip_tb/DUT/core_1/openMSP430_0/mem_backbone_0/eu_pmem_addr
add wave -noupdate -group {OpenMSP Memory Backbone} /chip_tb/DUT/core_1/openMSP430_0/mem_backbone_0/fe_pmem_cen
add wave -noupdate -group {OpenMSP Memory Backbone} -radix hexadecimal /chip_tb/DUT/core_1/openMSP430_0/mem_backbone_0/fe_pmem_addr
add wave -noupdate -group {OpenMSP Memory Backbone} /chip_tb/DUT/core_1/openMSP430_0/mem_backbone_0/dbg_pmem_cen
add wave -noupdate -group {OpenMSP Memory Backbone} -radix hexadecimal /chip_tb/DUT/core_1/openMSP430_0/mem_backbone_0/dbg_pmem_addr
add wave -noupdate -group {OpenMSP Memory Backbone} /chip_tb/DUT/core_1/openMSP430_0/mem_backbone_0/dbg_per_en
add wave -noupdate -group {OpenMSP Memory Backbone} /chip_tb/DUT/core_1/openMSP430_0/mem_backbone_0/eu_per_en
add wave -noupdate -group {OpenMSP Memory Backbone} -radix hexadecimal /chip_tb/DUT/core_1/openMSP430_0/mem_backbone_0/per_addr_mux
add wave -noupdate -group {OpenMSP Memory Backbone} -radix hexadecimal /chip_tb/DUT/core_1/openMSP430_0/mem_backbone_0/per_addr_ful
add wave -noupdate -group {OpenMSP Memory Backbone} -radix hexadecimal /chip_tb/DUT/core_1/openMSP430_0/mem_backbone_0/per_dout_val
add wave -noupdate -group {OpenMSP Memory Backbone} /chip_tb/DUT/core_1/openMSP430_0/mem_backbone_0/fe_pmem_cen_dly
add wave -noupdate -group {OpenMSP Memory Backbone} /chip_tb/DUT/core_1/openMSP430_0/mem_backbone_0/fe_pmem_save
add wave -noupdate -group {OpenMSP Memory Backbone} /chip_tb/DUT/core_1/openMSP430_0/mem_backbone_0/fe_pmem_restore
add wave -noupdate -group {OpenMSP Memory Backbone} /chip_tb/DUT/core_1/openMSP430_0/mem_backbone_0/mclk_bckup
add wave -noupdate -group {OpenMSP Memory Backbone} /chip_tb/DUT/core_1/openMSP430_0/mem_backbone_0/pmem_dout_bckup
add wave -noupdate -group {OpenMSP Memory Backbone} /chip_tb/DUT/core_1/openMSP430_0/mem_backbone_0/pmem_dout_bckup_sel
add wave -noupdate -group {OpenMSP Memory Backbone} /chip_tb/DUT/core_1/openMSP430_0/mem_backbone_0/eu_mdb_in_sel
add wave -noupdate -group {OpenMSP Memory Backbone} /chip_tb/DUT/core_1/openMSP430_0/mem_backbone_0/dbg_mem_din_sel
add wave -noupdate -expand -group {OpenMSP430 Debug Utility} -radix ascii /chip_tb/DUT/core_1/i_state
add wave -noupdate -expand -group {OpenMSP430 Debug Utility} -radix ascii /chip_tb/DUT/core_1/e_state
add wave -noupdate -expand -group {OpenMSP430 Debug Utility} -radix unsigned /chip_tb/DUT/core_1/inst_cycle
add wave -noupdate -expand -group {OpenMSP430 Debug Utility} -radix ascii /chip_tb/DUT/core_1/inst_full
add wave -noupdate -expand -group {OpenMSP430 Debug Utility} -radix unsigned /chip_tb/DUT/core_1/inst_number
add wave -noupdate -expand -group {OpenMSP430 Debug Utility} -radix hexadecimal /chip_tb/DUT/core_1/inst_pc
add wave -noupdate -expand -group {OpenMSP430 Debug Utility} -radix ascii /chip_tb/DUT/core_1/inst_short
add wave -noupdate -expand -group {OpenMSP430 Debug Utility} -radix hexadecimal /chip_tb/DUT/core_1/openMSP430_0/frontend_0/pc
add wave -noupdate -expand -group {OpenMSP430 Debug Utility} -radix hexadecimal /chip_tb/DUT/core_1/openMSP430_0/frontend_0/pc_nxt
add wave -noupdate -expand -group {OpenMSP430 Debug Utility} /chip_tb/DUT/core_1/openMSP430_0/frontend_0/ir
add wave -noupdate -expand -group {OpenMSP430 Debug Utility} /chip_tb/DUT/core_1/openMSP430_0/clock_module_0/dbg_rst_nxt
add wave -noupdate -expand -group OpenMSP430 -expand -group Reset /chip_tb/DUT/core_1/PUC_Reset_s
add wave -noupdate -expand -group OpenMSP430 -expand -group Reset /chip_tb/DUT/core_1/CPU_Enable_s
add wave -noupdate -expand -group OpenMSP430 -expand -group Clocks /chip_tb/DUT/core_1/DCO_Enable_s
add wave -noupdate -expand -group OpenMSP430 -expand -group Clocks /chip_tb/DUT/core_1/DCO_Wakeup_s
add wave -noupdate -expand -group OpenMSP430 -expand -group Clocks /chip_tb/DUT/core_1/LFXT_Enable_s
add wave -noupdate -expand -group OpenMSP430 -expand -group Clocks /chip_tb/DUT/core_1/LFXT_Wakeup_s
add wave -noupdate -expand -group OpenMSP430 -expand -group Clocks /chip_tb/DUT/core_1/LFXT_Clk_i
add wave -noupdate -expand -group OpenMSP430 -expand -group Clocks /chip_tb/DUT/core_1/MClk_s
add wave -noupdate -expand -group OpenMSP430 -expand -group Clocks /chip_tb/DUT/core_1/AClk_s
add wave -noupdate -expand -group OpenMSP430 -expand -group Clocks /chip_tb/DUT/core_1/AClk_En_s
add wave -noupdate -expand -group OpenMSP430 -expand -group Clocks /chip_tb/DUT/core_1/SMClk_s
add wave -noupdate -expand -group OpenMSP430 -expand -group Clocks /chip_tb/DUT/core_1/SMClk_En_s
add wave -noupdate -expand -group OpenMSP430 /chip_tb/DUT/core_1/Wakeup_s
add wave -noupdate -expand -group OpenMSP430 -expand -group {Program Memory} -radix hexadecimal /chip_tb/DUT/core_1/PMem_Addr_s
add wave -noupdate -expand -group OpenMSP430 -expand -group {Program Memory} /chip_tb/DUT/core_1/PMem_En_n_s
add wave -noupdate -expand -group OpenMSP430 -expand -group {Program Memory} -radix hexadecimal /chip_tb/DUT/core_1/PMem_DIn_s
add wave -noupdate -expand -group OpenMSP430 -expand -group {Program Memory} /chip_tb/DUT/core_1/PMem_Wr_n_s
add wave -noupdate -expand -group OpenMSP430 -expand -group {Program Memory} -radix hexadecimal /chip_tb/DUT/core_1/PMem_DOut_s
add wave -noupdate -expand -group OpenMSP430 -expand -group {Data Memory} -radix hexadecimal /chip_tb/DUT/core_1/DMem_Addr_s
add wave -noupdate -expand -group OpenMSP430 -expand -group {Data Memory} /chip_tb/DUT/core_1/DMem_En_n_s
add wave -noupdate -expand -group OpenMSP430 -expand -group {Data Memory} -radix hexadecimal /chip_tb/DUT/core_1/DMem_DIn_s
add wave -noupdate -expand -group OpenMSP430 -expand -group {Data Memory} /chip_tb/DUT/core_1/DMem_Wr_n_s
add wave -noupdate -expand -group OpenMSP430 -expand -group {Data Memory} -radix hexadecimal /chip_tb/DUT/core_1/DMem_DOut_s
add wave -noupdate -expand -group OpenMSP430 -expand -group Peripherals -radix hexadecimal /chip_tb/DUT/core_1/Per_Addr_s
add wave -noupdate -expand -group OpenMSP430 -expand -group Peripherals -radix hexadecimal /chip_tb/DUT/core_1/Per_DIn_s
add wave -noupdate -expand -group OpenMSP430 -expand -group Peripherals -radix hexadecimal /chip_tb/DUT/core_1/Per_DOut_s
add wave -noupdate -expand -group OpenMSP430 -expand -group Peripherals /chip_tb/DUT/core_1/Per_Wr_s
add wave -noupdate -expand -group OpenMSP430 -expand -group Peripherals /chip_tb/DUT/core_1/Per_En_s
add wave -noupdate -expand -group OpenMSP430 -group Interrupts /chip_tb/DUT/core_1/IRQ_s
add wave -noupdate -expand -group OpenMSP430 -group Interrupts /chip_tb/DUT/core_1/IRQ_Ack_s
add wave -noupdate -expand -group OpenMSP430 -group Interrupts /chip_tb/DUT/core_1/NMI_s
add wave -noupdate -expand -group OpenMSP430 -group Debug /chip_tb/DUT/core_1/Dbg_En_i
add wave -noupdate -expand -group OpenMSP430 -group Debug /chip_tb/DUT/core_1/Dbg_Freeze_s
#add wave -noupdate -expand -group OpenMSP430 -group Debug /chip_tb/DUT/core_1/Dbg_UART_RxD_i
#add wave -noupdate -expand -group OpenMSP430 -group Debug /chip_tb/DUT/core_1/Dbg_UART_TxD_o
#add wave -noupdate -expand -group OpenMSP430 -group Debug /chip_tb/DUT/core_1/Dbg_SCL_s
#add wave -noupdate -expand -group OpenMSP430 -group Debug /chip_tb/DUT/core_1/Dbg_SDA_Out_s
#add wave -noupdate -expand -group OpenMSP430 -group Debug /chip_tb/DUT/core_1/Dbg_SDA_In_s
add wave -noupdate -expand -group OpenMSP430 -group Debug /chip_tb/DUT/core_1/Dbg_UART_RxD_s
add wave -noupdate -expand -group OpenMSP430 -group Debug /chip_tb/DUT/core_1/Dbg_UART_TxD_s
add wave -noupdate -expand -group OpenMSP430 -group Debug /chip_tb/DUT/Dbg_SCL_i
add wave -noupdate -expand -group OpenMSP430 -group Debug /chip_tb/DUT/Dbg_SDA_b
add wave -noupdate -expand -group OpenMSP430 /chip_tb/DUT/core_1/Scan_Enable_s
add wave -noupdate -expand -group OpenMSP430 /chip_tb/DUT/core_1/Scan_Mode_s
add wave -noupdate /chip_tb/DUT/core_1/Gpio_DOut_s
add wave -noupdate /chip_tb/DUT/core_1/Gpio_IRQ1_s
add wave -noupdate /chip_tb/DUT/core_1/Gpio_IRQ2_s
add wave -noupdate -expand /chip_tb/DUT/P1_b
add wave -noupdate /chip_tb/DUT/core_1/P1_DOut_s
add wave -noupdate /chip_tb/DUT/core_1/P1_En_s
add wave -noupdate /chip_tb/DUT/core_1/P1_Sel_s
add wave -noupdate /chip_tb/DUT/core_1/P1_DIn_s
add wave -noupdate /chip_tb/DUT/P1_b
add wave -noupdate /chip_tb/DUT/core_1/P2_DOut_s
add wave -noupdate /chip_tb/DUT/core_1/P2_En_s
add wave -noupdate /chip_tb/DUT/core_1/P2_Sel_s
add wave -noupdate /chip_tb/DUT/core_1/P2_DIn_s
add wave -noupdate /chip_tb/DUT/core_1/P3_DOut_s
add wave -noupdate /chip_tb/DUT/core_1/P3_En_s
add wave -noupdate /chip_tb/DUT/core_1/P3_Sel_s
add wave -noupdate /chip_tb/DUT/core_1/P3_DIn_s
add wave -noupdate /chip_tb/DUT/core_1/P4_DOut_s
add wave -noupdate /chip_tb/DUT/core_1/P4_En_s
add wave -noupdate /chip_tb/DUT/core_1/P4_Sel_s
add wave -noupdate /chip_tb/DUT/core_1/P4_DIn_s
add wave -noupdate /chip_tb/DUT/core_1/P5_DOut_s
add wave -noupdate /chip_tb/DUT/core_1/P5_En_s
add wave -noupdate /chip_tb/DUT/core_1/P5_Sel_s
add wave -noupdate /chip_tb/DUT/core_1/P5_DIn_s
add wave -noupdate /chip_tb/DUT/core_1/P6_DOut_s
add wave -noupdate /chip_tb/DUT/core_1/P6_En_s
add wave -noupdate /chip_tb/DUT/core_1/P6_Sel_s
add wave -noupdate /chip_tb/DUT/core_1/P6_DIn_s
add wave -noupdate /chip_tb/DUT/core_1/TimerA_DOut_s
add wave -noupdate /chip_tb/DUT/core_1/TimerA_IRQ1_s
add wave -noupdate /chip_tb/DUT/core_1/TimerA_IRQ2_s
add wave -noupdate /chip_tb/DUT/core_1/INClk_s
add wave -noupdate /chip_tb/DUT/core_1/TAClk_s
add wave -noupdate /chip_tb/DUT/core_1/TimerA_CCI0A_s
add wave -noupdate /chip_tb/DUT/core_1/TimerA_CCI0B_s
add wave -noupdate /chip_tb/DUT/core_1/TimerA_Out0_s
add wave -noupdate /chip_tb/DUT/core_1/TimerA_Out0_En_s
add wave -noupdate /chip_tb/DUT/core_1/TimerA_CCI1A_s
add wave -noupdate /chip_tb/DUT/core_1/TimerA_CCI1B_s
add wave -noupdate /chip_tb/DUT/core_1/TimerA_Out1_s
add wave -noupdate /chip_tb/DUT/core_1/TimerA_Out1_En_s
add wave -noupdate /chip_tb/DUT/core_1/TimerA_CCI2A_s
add wave -noupdate /chip_tb/DUT/core_1/TimerA_CCI2B_s
add wave -noupdate /chip_tb/DUT/core_1/TimerA_Out2_s
add wave -noupdate /chip_tb/DUT/core_1/TimerA_Out2_En_s
add wave -noupdate /chip_tb/DUT/core_1/UartRxD_i
add wave -noupdate /chip_tb/DUT/core_1/UartTxD_o
add wave -noupdate -group {SPI Master} /chip_tb/DUT/core_1/SPI_CPOL
add wave -noupdate -group {SPI Master} /chip_tb/DUT/core_1/SPI_CPHA
add wave -noupdate -group {SPI Master} /chip_tb/DUT/core_1/SPI_LSBFE
add wave -noupdate -group {SPI Master} /chip_tb/DUT/core_1/SPI_SPPR_SPR
add wave -noupdate -group {SPI Master} /chip_tb/DUT/core_1/SPI_Transmission
add wave -noupdate -group {SPI Master} /chip_tb/DUT/core_1/SPI_Write
add wave -noupdate -group {SPI Master} /chip_tb/DUT/core_1/SPI_ReadNext
add wave -noupdate -group {SPI Master} /chip_tb/DUT/core_1/SPI_DataIn
add wave -noupdate -group {SPI Master} /chip_tb/DUT/core_1/SPI_DataOut
add wave -noupdate -group {SPI Master} /chip_tb/DUT/core_1/SPI_FIFOFull
add wave -noupdate -group {SPI Master} /chip_tb/DUT/core_1/SPI_FIFOEmpty
add wave -noupdate -group {SPI Master} /chip_tb/DUT/core_1/SPI_ScanEnable
add wave -noupdate -group {SPI Master} /chip_tb/DUT/core_1/SPI_ScanClk
add wave -noupdate -group {SPI Master} /chip_tb/DUT/core_1/SPI_ScanDataIn
add wave -noupdate -group {SPI Master} /chip_tb/DUT/core_1/SPI_ScanDataOut
add wave -noupdate -group {I2C Master} /chip_tb/DUT/core_1/I2C_F100_400_n
add wave -noupdate -group {I2C Master} /chip_tb/DUT/core_1/I2C_Divider800
add wave -noupdate -group {I2C Master} /chip_tb/DUT/core_1/I2C_StartProcess
add wave -noupdate -group {I2C Master} /chip_tb/DUT/core_1/I2C_ReceiveSend_n
add wave -noupdate -group {I2C Master} /chip_tb/DUT/core_1/I2C_Busy
add wave -noupdate -group {I2C Master} /chip_tb/DUT/core_1/I2C_ReadCount
add wave -noupdate -group {I2C Master} /chip_tb/DUT/core_1/I2C_FIFOReadNext
add wave -noupdate -group {I2C Master} /chip_tb/DUT/core_1/I2C_FIFOWrite
add wave -noupdate -group {I2C Master} /chip_tb/DUT/core_1/I2C_FIFOEmpty
add wave -noupdate -group {I2C Master} /chip_tb/DUT/core_1/I2C_FIFOFull
add wave -noupdate -group {I2C Master} /chip_tb/DUT/core_1/I2C_DataIn
add wave -noupdate -group {I2C Master} /chip_tb/DUT/core_1/I2C_DataOut
add wave -noupdate -group {I2C Master} /chip_tb/DUT/core_1/I2C_ErrAck
add wave -noupdate -group {I2C Master} /chip_tb/DUT/core_1/I2C_ErrBusColl
add wave -noupdate -group {I2C Master} /chip_tb/DUT/core_1/I2C_ErrFIFOFull
add wave -noupdate -group {I2C Master} /chip_tb/DUT/core_1/I2C_ErrGotNAck
add wave -noupdate -group {I2C Master} /chip_tb/DUT/core_1/I2C_ErrCoreBusy
add wave -noupdate -group {I2C Master} /chip_tb/DUT/core_1/I2C_ErrFIFOEmpty
add wave -noupdate -group {I2C Master} /chip_tb/DUT/core_1/I2C_ErrCoreStopped
add wave -noupdate -group {I2C Master} /chip_tb/DUT/core_1/I2C_ErrDevNotPresent
add wave -noupdate -group {I2C Master} /chip_tb/DUT/core_1/I2C_ErrReadCountZero
add wave -noupdate -group {I2C Master} /chip_tb/DUT/core_1/I2C_ScanEnable
add wave -noupdate -group {I2C Master} /chip_tb/DUT/core_1/I2C_ScanClk
add wave -noupdate -group {I2C Master} /chip_tb/DUT/core_1/I2C_ScanDataIn
add wave -noupdate -group {I2C Master} /chip_tb/DUT/core_1/I2C_ScanDataOut
#add wave -noupdate -group {OneWire Master} /chip_tb/DUT/core_1/OneWire_OWReset
#add wave -noupdate -group {OneWire Master} /chip_tb/DUT/core_1/OneWire_DeactivateOverdriveMode
#add wave -noupdate -group {OneWire Master} /chip_tb/DUT/core_1/OneWire_SearchROM
#add wave -noupdate -group {OneWire Master} /chip_tb/DUT/core_1/OneWire_ReadROM
#add wave -noupdate -group {OneWire Master} /chip_tb/DUT/core_1/OneWire_MatchROM
#add wave -noupdate -group {OneWire Master} /chip_tb/DUT/core_1/OneWire_SkipROM
#add wave -noupdate -group {OneWire Master} /chip_tb/DUT/core_1/OneWire_CondSearchROM
#add wave -noupdate -group {OneWire Master} /chip_tb/DUT/core_1/OneWire_OverdriveSkipROM
#add wave -noupdate -group {OneWire Master} /chip_tb/DUT/core_1/OneWire_OverdriveMatchROM
#add wave -noupdate -group {OneWire Master} /chip_tb/DUT/core_1/OneWire_CondReadROM
#add wave -noupdate -group {OneWire Master} /chip_tb/DUT/core_1/OneWire_ResumeROM
#add wave -noupdate -group {OneWire Master} /chip_tb/DUT/core_1/OneWire_WriteByte
#add wave -noupdate -group {OneWire Master} /chip_tb/DUT/core_1/OneWire_ReadByte
#add wave -noupdate -group {OneWire Master} /chip_tb/DUT/core_1/OneWire_GetROMID
#add wave -noupdate -group {OneWire Master} /chip_tb/DUT/core_1/OneWire_DataIn
#add wave -noupdate -group {OneWire Master} /chip_tb/DUT/core_1/OneWire_DataOut
#add wave -noupdate -group {OneWire Master} /chip_tb/DUT/core_1/OneWire_ROMIDsInArray
#add wave -noupdate -group {OneWire Master} /chip_tb/DUT/core_1/OneWire_Noslaves
#add wave -noupdate -group {OneWire Master} /chip_tb/DUT/core_1/OneWire_ROMIDArrayToSmall
#add wave -noupdate -group {OneWire Master} /chip_tb/DUT/core_1/OneWire_PDROut
#add wave -noupdate -group {OneWire Master} /chip_tb/DUT/core_1/OneWire_Ready
#add wave -noupdate -group {OneWire Master} /chip_tb/DUT/core_1/OneWire_ResetLowTime
#add wave -noupdate -group {OneWire Master} /chip_tb/DUT/core_1/OneWire_ResetTime
#add wave -noupdate -group {OneWire Master} /chip_tb/DUT/core_1/OneWire_ResetWaitForDetectionDuration
#add wave -noupdate -group {OneWire Master} /chip_tb/DUT/core_1/OneWire_ResetPrecenceIntervalDuration
#add wave -noupdate -group {OneWire Master} /chip_tb/DUT/core_1/OneWire_WRSlotHighDataTime
#add wave -noupdate -group {OneWire Master} /chip_tb/DUT/core_1/OneWire_RDSlotSampleTime
#add wave -noupdate -group {OneWire Master} /chip_tb/DUT/core_1/OneWire_SlotLowDataTime
#add wave -noupdate -group {OneWire Master} /chip_tb/DUT/core_1/OneWire_SlotDuration
#add wave -noupdate -group {OneWire Master} /chip_tb/DUT/core_1/OneWire_RDSlotInitTime
#add wave -noupdate -group {OneWire Master} /chip_tb/DUT/core_1/OneWire_ODResetLowTime
#add wave -noupdate -group {OneWire Master} /chip_tb/DUT/core_1/OneWire_ODResetTime
#add wave -noupdate -group {OneWire Master} /chip_tb/DUT/core_1/OneWire_ODResetWaitForDetectionDuration
#add wave -noupdate -group {OneWire Master} /chip_tb/DUT/core_1/OneWire_ODResetPrecenceIntervalDuration
#add wave -noupdate -group {OneWire Master} /chip_tb/DUT/core_1/OneWire_ODWRSlotHighDataTime
#add wave -noupdate -group {OneWire Master} /chip_tb/DUT/core_1/OneWire_ODRDSlotSampleTime
#add wave -noupdate -group {OneWire Master} /chip_tb/DUT/core_1/OneWire_ODSlotLowDataTime
#add wave -noupdate -group {OneWire Master} /chip_tb/DUT/core_1/OneWire_ODSlotDuration
#add wave -noupdate -group {OneWire Master} /chip_tb/DUT/core_1/OneWire_ODRDSlotInitTime
#add wave -noupdate -group {OneWire Master} /chip_tb/DUT/core_1/OneWire_ScanEnable
#add wave -noupdate -group {OneWire Master} /chip_tb/DUT/core_1/OneWire_ScanClk
#add wave -noupdate -group {OneWire Master} /chip_tb/DUT/core_1/OneWire_ScanDataIn
#add wave -noupdate -group {OneWire Master} /chip_tb/DUT/core_1/OneWire_ScanDataOut
#add wave -noupdate -group PWM /chip_tb/DUT/core_1/PWM_Polarity
#add wave -noupdate -group PWM /chip_tb/DUT/core_1/PWM_Value
#add wave -noupdate -group PWM /chip_tb/DUT/core_1/PWM_NewValue
#add wave -noupdate -group PWM /chip_tb/DUT/core_1/PWM_ScanEnable
#add wave -noupdate -group PWM /chip_tb/DUT/core_1/PWM_ScanClk
#add wave -noupdate -group PWM /chip_tb/DUT/core_1/PWM_ScanDataIn
#add wave -noupdate -group PWM /chip_tb/DUT/core_1/PWM_ScanDataOut
#add wave -noupdate -group SENT /chip_tb/DUT/core_1/SENT_Chipselect
#add wave -noupdate -group SENT /chip_tb/DUT/core_1/SENT_NumDatNibble
#add wave -noupdate -group SENT /chip_tb/DUT/core_1/SENT_MinSync
#add wave -noupdate -group SENT /chip_tb/DUT/core_1/SENT_OutWide
#add wave -noupdate -group SENT /chip_tb/DUT/core_1/SENT_NewData
#add wave -noupdate -group SENT /chip_tb/DUT/core_1/SENT_CrcOk
#add wave -noupdate -group SENT /chip_tb/DUT/core_1/SENT_ScanEnable
#add wave -noupdate -group SENT /chip_tb/DUT/core_1/SENT_ScanClk
#add wave -noupdate -group SENT /chip_tb/DUT/core_1/SENT_ScanDataIn
#add wave -noupdate -group SENT /chip_tb/DUT/core_1/SENT_ScanDataOut
#add wave -noupdate -group SPC /chip_tb/DUT/core_1/SPC_Start
#add wave -noupdate -group SPC /chip_tb/DUT/core_1/SPC_NumDatNibble
#add wave -noupdate -group SPC /chip_tb/DUT/core_1/SPC_LengthTrigger
#add wave -noupdate -group SPC /chip_tb/DUT/core_1/SPC_LengthTimeout
#add wave -noupdate -group SPC /chip_tb/DUT/core_1/SPC_MinSync
#add wave -noupdate -group SPC /chip_tb/DUT/core_1/SPC_OutWide
#add wave -noupdate -group SPC /chip_tb/DUT/core_1/SPC_NewData
#add wave -noupdate -group SPC /chip_tb/DUT/core_1/SPC_CrcOk
#add wave -noupdate -group SPC /chip_tb/DUT/core_1/SPC_SPCReady
#add wave -noupdate -group SPC /chip_tb/DUT/core_1/SPC_ScanEnable
#add wave -noupdate -group SPC /chip_tb/DUT/core_1/SPC_ScanClk
#add wave -noupdate -group SPC /chip_tb/DUT/core_1/SPC_ScanDataIn
#add wave -noupdate -group SPC /chip_tb/DUT/core_1/SPC_ScanDataOut
TreeUpdate [SetDefaultTree]
WaveRestoreCursors {{Cursor 1} {24220800 ps} 0}
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
WaveRestoreZoom {0 ps} {52554600 ps}
