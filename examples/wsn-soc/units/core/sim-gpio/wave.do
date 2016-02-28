onerror {resume}
quietly WaveActivateNextPane {} 0
add wave -noupdate /core_tb/DUT/DBG_I2C_ADDR
add wave -noupdate /core_tb/DUT/DBG_I2C_BROADCAST
add wave -noupdate /core_tb/DUT/SPI_DataWidth
add wave -noupdate /core_tb/DUT/SPI_SPPRWidth
add wave -noupdate /core_tb/DUT/SPI_SPRWidth
add wave -noupdate /core_tb/DUT/SPI_FIFOReadWidth
add wave -noupdate /core_tb/DUT/SPI_FIFOWriteWidth
add wave -noupdate /core_tb/DUT/I2C_ReadCountWidth
add wave -noupdate /core_tb/DUT/I2C_FIFOAddressWidth
add wave -noupdate /core_tb/DUT/I2C_DividerWidth
#add wave -noupdate /core_tb/DUT/OneWire_ROMIDArraySize
#add wave -noupdate /core_tb/DUT/OneWire_ROMIDIndexSize
#add wave -noupdate /core_tb/DUT/OneWire_ROMIDByteIndexSize
#add wave -noupdate /core_tb/DUT/OneWire_SearchCommand
#add wave -noupdate /core_tb/DUT/OneWire_CondSearchCommand
#add wave -noupdate /core_tb/DUT/OneWire_MatchCommand
#add wave -noupdate /core_tb/DUT/OneWire_ReadCommand
#add wave -noupdate /core_tb/DUT/OneWire_SkipCommand
#add wave -noupdate /core_tb/DUT/OneWire_OverdriveSkipCommand
#add wave -noupdate /core_tb/DUT/OneWire_OverdriveMatchCommand
#add wave -noupdate /core_tb/DUT/OneWire_ConditionalReadCommand
#add wave -noupdate /core_tb/DUT/OneWire_ResumeCommand
#add wave -noupdate /core_tb/DUT/OneWire_TimerWidth
#add wave -noupdate /core_tb/DUT/PWM_Resolution
#add wave -noupdate /core_tb/DUT/PWM_CounterWidth
#add wave -noupdate /core_tb/DUT/SENT_MaxDatNibble
#add wave -noupdate /core_tb/DUT/SENT_CountWidth
#add wave -noupdate /core_tb/DUT/SPC_MaxDatNibble
#add wave -noupdate /core_tb/DUT/SPC_CountWidth
#add wave -noupdate /core_tb/DUT/SPC_TimeoutWidth
#add wave -noupdate /core_tb/DUT/SPC_UseTimeout
add wave -noupdate /core_tb/DUT/Reset_n_i
add wave -noupdate /core_tb/DUT/Clk_i
add wave -noupdate /core_tb/DUT/Inputs_i
add wave -noupdate /core_tb/DUT/Outputs_o
add wave -noupdate /core_tb/DUT/SPIMISO_i
add wave -noupdate /core_tb/DUT/SPIMOSI_o
add wave -noupdate /core_tb/DUT/SPISCK_o
add wave -noupdate /core_tb/DUT/I2CSCL_o
add wave -noupdate /core_tb/DUT/I2CSDA_i
add wave -noupdate /core_tb/DUT/I2CSDA_o
#add wave -noupdate /core_tb/DUT/OneWire_i
#add wave -noupdate /core_tb/DUT/OneWire_o
#add wave -noupdate /core_tb/DUT/PWMInput_i
#add wave -noupdate /core_tb/DUT/SENTInput_i
#add wave -noupdate /core_tb/DUT/SPCInput_i
#add wave -noupdate /core_tb/DUT/SPCTrigger_o
add wave -noupdate /core_tb/DUT/AdcConvComplete_i
add wave -noupdate /core_tb/DUT/AdcDoConvert_o
add wave -noupdate /core_tb/DUT/AdcValue_i
add wave -noupdate -group {OpenMSP Memory Backbone} -radix hexadecimal /core_tb/DUT/openMSP430_0/mem_backbone_0/PMEM_OFFSET
add wave -noupdate -group {OpenMSP Memory Backbone} -radix hexadecimal /core_tb/DUT/openMSP430_0/mem_backbone_0/dbg_mem_din
add wave -noupdate -group {OpenMSP Memory Backbone} -radix hexadecimal /core_tb/DUT/openMSP430_0/mem_backbone_0/dmem_addr
add wave -noupdate -group {OpenMSP Memory Backbone} /core_tb/DUT/openMSP430_0/mem_backbone_0/dmem_cen
add wave -noupdate -group {OpenMSP Memory Backbone} -radix hexadecimal /core_tb/DUT/openMSP430_0/mem_backbone_0/dmem_din
add wave -noupdate -group {OpenMSP Memory Backbone} /core_tb/DUT/openMSP430_0/mem_backbone_0/dmem_wen
add wave -noupdate -group {OpenMSP Memory Backbone} -radix hexadecimal /core_tb/DUT/openMSP430_0/mem_backbone_0/eu_mdb_in
add wave -noupdate -group {OpenMSP Memory Backbone} -radix hexadecimal /core_tb/DUT/openMSP430_0/mem_backbone_0/fe_mdb_in
add wave -noupdate -group {OpenMSP Memory Backbone} /core_tb/DUT/openMSP430_0/mem_backbone_0/fe_pmem_wait
add wave -noupdate -group {OpenMSP Memory Backbone} -radix hexadecimal /core_tb/DUT/openMSP430_0/mem_backbone_0/per_addr
add wave -noupdate -group {OpenMSP Memory Backbone} -radix hexadecimal /core_tb/DUT/openMSP430_0/mem_backbone_0/per_din
add wave -noupdate -group {OpenMSP Memory Backbone} /core_tb/DUT/openMSP430_0/mem_backbone_0/per_we
add wave -noupdate -group {OpenMSP Memory Backbone} /core_tb/DUT/openMSP430_0/mem_backbone_0/per_en
add wave -noupdate -group {OpenMSP Memory Backbone} -radix hexadecimal /core_tb/DUT/openMSP430_0/mem_backbone_0/pmem_addr
add wave -noupdate -group {OpenMSP Memory Backbone} /core_tb/DUT/openMSP430_0/mem_backbone_0/pmem_cen
add wave -noupdate -group {OpenMSP Memory Backbone} -radix hexadecimal /core_tb/DUT/openMSP430_0/mem_backbone_0/pmem_din
add wave -noupdate -group {OpenMSP Memory Backbone} /core_tb/DUT/openMSP430_0/mem_backbone_0/pmem_wen
add wave -noupdate -group {OpenMSP Memory Backbone} /core_tb/DUT/openMSP430_0/mem_backbone_0/dbg_halt_st
add wave -noupdate -group {OpenMSP Memory Backbone} -radix hexadecimal /core_tb/DUT/openMSP430_0/mem_backbone_0/dbg_mem_addr
add wave -noupdate -group {OpenMSP Memory Backbone} -radix hexadecimal /core_tb/DUT/openMSP430_0/mem_backbone_0/dbg_mem_dout
add wave -noupdate -group {OpenMSP Memory Backbone} /core_tb/DUT/openMSP430_0/mem_backbone_0/dbg_mem_en
add wave -noupdate -group {OpenMSP Memory Backbone} /core_tb/DUT/openMSP430_0/mem_backbone_0/dbg_mem_wr
add wave -noupdate -group {OpenMSP Memory Backbone} -radix hexadecimal /core_tb/DUT/openMSP430_0/mem_backbone_0/dmem_dout
add wave -noupdate -group {OpenMSP Memory Backbone} -radix hexadecimal /core_tb/DUT/openMSP430_0/mem_backbone_0/eu_mab
add wave -noupdate -group {OpenMSP Memory Backbone} /core_tb/DUT/openMSP430_0/mem_backbone_0/eu_mb_en
add wave -noupdate -group {OpenMSP Memory Backbone} /core_tb/DUT/openMSP430_0/mem_backbone_0/eu_mb_wr
add wave -noupdate -group {OpenMSP Memory Backbone} -radix hexadecimal /core_tb/DUT/openMSP430_0/mem_backbone_0/eu_mdb_out
add wave -noupdate -group {OpenMSP Memory Backbone} -radix hexadecimal /core_tb/DUT/openMSP430_0/mem_backbone_0/fe_mab
add wave -noupdate -group {OpenMSP Memory Backbone} /core_tb/DUT/openMSP430_0/mem_backbone_0/fe_mb_en
add wave -noupdate -group {OpenMSP Memory Backbone} /core_tb/DUT/openMSP430_0/mem_backbone_0/mclk
add wave -noupdate -group {OpenMSP Memory Backbone} -radix hexadecimal /core_tb/DUT/openMSP430_0/mem_backbone_0/per_dout
add wave -noupdate -group {OpenMSP Memory Backbone} -radix hexadecimal /core_tb/DUT/openMSP430_0/mem_backbone_0/pmem_dout
add wave -noupdate -group {OpenMSP Memory Backbone} /core_tb/DUT/openMSP430_0/mem_backbone_0/puc_rst
add wave -noupdate -group {OpenMSP Memory Backbone} /core_tb/DUT/openMSP430_0/mem_backbone_0/scan_enable
add wave -noupdate -group {OpenMSP Memory Backbone} /core_tb/DUT/openMSP430_0/mem_backbone_0/eu_dmem_cen
add wave -noupdate -group {OpenMSP Memory Backbone} -radix hexadecimal /core_tb/DUT/openMSP430_0/mem_backbone_0/eu_dmem_addr
add wave -noupdate -group {OpenMSP Memory Backbone} /core_tb/DUT/openMSP430_0/mem_backbone_0/dbg_dmem_cen
add wave -noupdate -group {OpenMSP Memory Backbone} -radix hexadecimal /core_tb/DUT/openMSP430_0/mem_backbone_0/dbg_dmem_addr
add wave -noupdate -group {OpenMSP Memory Backbone} /core_tb/DUT/openMSP430_0/mem_backbone_0/eu_pmem_cen
add wave -noupdate -group {OpenMSP Memory Backbone} -radix hexadecimal /core_tb/DUT/openMSP430_0/mem_backbone_0/eu_pmem_addr
add wave -noupdate -group {OpenMSP Memory Backbone} /core_tb/DUT/openMSP430_0/mem_backbone_0/fe_pmem_cen
add wave -noupdate -group {OpenMSP Memory Backbone} -radix hexadecimal /core_tb/DUT/openMSP430_0/mem_backbone_0/fe_pmem_addr
add wave -noupdate -group {OpenMSP Memory Backbone} /core_tb/DUT/openMSP430_0/mem_backbone_0/dbg_pmem_cen
add wave -noupdate -group {OpenMSP Memory Backbone} -radix hexadecimal /core_tb/DUT/openMSP430_0/mem_backbone_0/dbg_pmem_addr
add wave -noupdate -group {OpenMSP Memory Backbone} /core_tb/DUT/openMSP430_0/mem_backbone_0/dbg_per_en
add wave -noupdate -group {OpenMSP Memory Backbone} /core_tb/DUT/openMSP430_0/mem_backbone_0/eu_per_en
add wave -noupdate -group {OpenMSP Memory Backbone} -radix hexadecimal /core_tb/DUT/openMSP430_0/mem_backbone_0/per_addr_mux
add wave -noupdate -group {OpenMSP Memory Backbone} -radix hexadecimal /core_tb/DUT/openMSP430_0/mem_backbone_0/per_addr_ful
add wave -noupdate -group {OpenMSP Memory Backbone} -radix hexadecimal /core_tb/DUT/openMSP430_0/mem_backbone_0/per_dout_val
add wave -noupdate -group {OpenMSP Memory Backbone} /core_tb/DUT/openMSP430_0/mem_backbone_0/fe_pmem_cen_dly
add wave -noupdate -group {OpenMSP Memory Backbone} /core_tb/DUT/openMSP430_0/mem_backbone_0/fe_pmem_save
add wave -noupdate -group {OpenMSP Memory Backbone} /core_tb/DUT/openMSP430_0/mem_backbone_0/fe_pmem_restore
add wave -noupdate -group {OpenMSP Memory Backbone} /core_tb/DUT/openMSP430_0/mem_backbone_0/mclk_bckup
add wave -noupdate -group {OpenMSP Memory Backbone} /core_tb/DUT/openMSP430_0/mem_backbone_0/pmem_dout_bckup
add wave -noupdate -group {OpenMSP Memory Backbone} /core_tb/DUT/openMSP430_0/mem_backbone_0/pmem_dout_bckup_sel
add wave -noupdate -group {OpenMSP Memory Backbone} /core_tb/DUT/openMSP430_0/mem_backbone_0/eu_mdb_in_sel
add wave -noupdate -group {OpenMSP Memory Backbone} /core_tb/DUT/openMSP430_0/mem_backbone_0/dbg_mem_din_sel
add wave -noupdate -expand -group {OpenMSP430 Debug Utility} -radix ascii /core_tb/DUT/i_state
add wave -noupdate -expand -group {OpenMSP430 Debug Utility} -radix ascii /core_tb/DUT/e_state
add wave -noupdate -expand -group {OpenMSP430 Debug Utility} -radix unsigned /core_tb/DUT/inst_cycle
add wave -noupdate -expand -group {OpenMSP430 Debug Utility} -radix ascii /core_tb/DUT/inst_full
add wave -noupdate -expand -group {OpenMSP430 Debug Utility} -radix unsigned /core_tb/DUT/inst_number
add wave -noupdate -expand -group {OpenMSP430 Debug Utility} -radix hexadecimal /core_tb/DUT/inst_pc
add wave -noupdate -expand -group {OpenMSP430 Debug Utility} -radix ascii /core_tb/DUT/inst_short
add wave -noupdate -expand -group {OpenMSP430 Debug Utility} -radix hexadecimal /core_tb/DUT/openMSP430_0/frontend_0/pc
add wave -noupdate -expand -group {OpenMSP430 Debug Utility} -radix hexadecimal /core_tb/DUT/openMSP430_0/frontend_0/pc_nxt
add wave -noupdate -expand -group {OpenMSP430 Debug Utility} /core_tb/DUT/openMSP430_0/frontend_0/ir
add wave -noupdate -expand -group {OpenMSP430 Debug Utility} /core_tb/DUT/openMSP430_0/clock_module_0/dbg_rst_nxt
add wave -noupdate -expand -group OpenMSP430 -expand -group Reset /core_tb/DUT/PUC_Reset_s
add wave -noupdate -expand -group OpenMSP430 -expand -group Reset /core_tb/DUT/CPU_Enable_s
add wave -noupdate -expand -group OpenMSP430 -expand -group Clocks /core_tb/DUT/DCO_Enable_s
add wave -noupdate -expand -group OpenMSP430 -expand -group Clocks /core_tb/DUT/DCO_Wakeup_s
add wave -noupdate -expand -group OpenMSP430 -expand -group Clocks /core_tb/DUT/LFXT_Enable_s
add wave -noupdate -expand -group OpenMSP430 -expand -group Clocks /core_tb/DUT/LFXT_Wakeup_s
add wave -noupdate -expand -group OpenMSP430 -expand -group Clocks /core_tb/DUT/LFXT_Clk_i
add wave -noupdate -expand -group OpenMSP430 -expand -group Clocks /core_tb/DUT/MClk_s
add wave -noupdate -expand -group OpenMSP430 -expand -group Clocks /core_tb/DUT/AClk_s
add wave -noupdate -expand -group OpenMSP430 -expand -group Clocks /core_tb/DUT/AClk_En_s
add wave -noupdate -expand -group OpenMSP430 -expand -group Clocks /core_tb/DUT/SMClk_s
add wave -noupdate -expand -group OpenMSP430 -expand -group Clocks /core_tb/DUT/SMClk_En_s
add wave -noupdate -expand -group OpenMSP430 /core_tb/DUT/Wakeup_s
add wave -noupdate -expand -group OpenMSP430 -expand -group {Program Memory} -radix hexadecimal /core_tb/DUT/PMem_Addr_s
add wave -noupdate -expand -group OpenMSP430 -expand -group {Program Memory} /core_tb/DUT/PMem_En_n_s
add wave -noupdate -expand -group OpenMSP430 -expand -group {Program Memory} -radix hexadecimal /core_tb/DUT/PMem_DIn_s
add wave -noupdate -expand -group OpenMSP430 -expand -group {Program Memory} /core_tb/DUT/PMem_Wr_n_s
add wave -noupdate -expand -group OpenMSP430 -expand -group {Program Memory} -radix hexadecimal /core_tb/DUT/PMem_DOut_s
add wave -noupdate -expand -group OpenMSP430 -expand -group {Data Memory} -radix hexadecimal /core_tb/DUT/DMem_Addr_s
add wave -noupdate -expand -group OpenMSP430 -expand -group {Data Memory} /core_tb/DUT/DMem_En_n_s
add wave -noupdate -expand -group OpenMSP430 -expand -group {Data Memory} -radix hexadecimal /core_tb/DUT/DMem_DIn_s
add wave -noupdate -expand -group OpenMSP430 -expand -group {Data Memory} /core_tb/DUT/DMem_Wr_n_s
add wave -noupdate -expand -group OpenMSP430 -expand -group {Data Memory} -radix hexadecimal /core_tb/DUT/DMem_DOut_s
add wave -noupdate -expand -group OpenMSP430 -expand -group Peripherals -radix hexadecimal /core_tb/DUT/Per_Addr_s
add wave -noupdate -expand -group OpenMSP430 -expand -group Peripherals -radix hexadecimal /core_tb/DUT/Per_DIn_s
add wave -noupdate -expand -group OpenMSP430 -expand -group Peripherals -radix hexadecimal /core_tb/DUT/Per_DOut_s
add wave -noupdate -expand -group OpenMSP430 -expand -group Peripherals /core_tb/DUT/Per_Wr_s
add wave -noupdate -expand -group OpenMSP430 -expand -group Peripherals /core_tb/DUT/Per_En_s
add wave -noupdate -expand -group OpenMSP430 -group Interrupts /core_tb/DUT/IRQ_s
add wave -noupdate -expand -group OpenMSP430 -group Interrupts /core_tb/DUT/IRQ_Ack_s
add wave -noupdate -expand -group OpenMSP430 -group Interrupts /core_tb/DUT/NMI_s
add wave -noupdate -expand -group OpenMSP430 -group Debug /core_tb/DUT/Dbg_En_i
add wave -noupdate -expand -group OpenMSP430 -group Debug /core_tb/DUT/Dbg_Freeze_s
#add wave -noupdate -expand -group OpenMSP430 -group Debug /core_tb/DUT/Dbg_UART_RxD_i
#add wave -noupdate -expand -group OpenMSP430 -group Debug /core_tb/DUT/Dbg_UART_TxD_o
#add wave -noupdate -expand -group OpenMSP430 -group Debug /core_tb/DUT/Dbg_SCL_s
#add wave -noupdate -expand -group OpenMSP430 -group Debug /core_tb/DUT/Dbg_SDA_Out_s
#add wave -noupdate -expand -group OpenMSP430 -group Debug /core_tb/DUT/Dbg_SDA_In_s
add wave -noupdate -expand -group OpenMSP430 -group Debug /core_tb/DUT/Dbg_UART_RxD_s
add wave -noupdate -expand -group OpenMSP430 -group Debug /core_tb/DUT/Dbg_UART_TxD_s
add wave -noupdate -expand -group OpenMSP430 -group Debug /core_tb/DUT/Dbg_SCL_i
add wave -noupdate -expand -group OpenMSP430 -group Debug /core_tb/DUT/Dbg_SDA_Out_o
add wave -noupdate -expand -group OpenMSP430 -group Debug /core_tb/DUT/Dbg_SDA_In_i
add wave -noupdate -expand -group OpenMSP430 /core_tb/DUT/Scan_Enable_s
add wave -noupdate -expand -group OpenMSP430 /core_tb/DUT/Scan_Mode_s
add wave -noupdate /core_tb/DUT/Gpio_DOut_s
add wave -noupdate /core_tb/DUT/Gpio_IRQ1_s
add wave -noupdate /core_tb/DUT/Gpio_IRQ2_s
add wave -noupdate -expand /core_tb/DUT/P1_DOut_s
add wave -noupdate /core_tb/DUT/P1_En_s
add wave -noupdate /core_tb/DUT/P1_Sel_s
add wave -noupdate /core_tb/DUT/P1_DIn_s
add wave -noupdate /core_tb/DUT/P2_DOut_s
add wave -noupdate /core_tb/DUT/P2_En_s
add wave -noupdate /core_tb/DUT/P2_Sel_s
add wave -noupdate /core_tb/DUT/P2_DIn_s
add wave -noupdate /core_tb/DUT/P3_DOut_s
add wave -noupdate /core_tb/DUT/P3_En_s
add wave -noupdate /core_tb/DUT/P3_Sel_s
add wave -noupdate /core_tb/DUT/P3_DIn_s
add wave -noupdate /core_tb/DUT/P4_DOut_s
add wave -noupdate /core_tb/DUT/P4_En_s
add wave -noupdate /core_tb/DUT/P4_Sel_s
add wave -noupdate /core_tb/DUT/P4_DIn_s
add wave -noupdate /core_tb/DUT/P5_DOut_s
add wave -noupdate /core_tb/DUT/P5_En_s
add wave -noupdate /core_tb/DUT/P5_Sel_s
add wave -noupdate /core_tb/DUT/P5_DIn_s
add wave -noupdate /core_tb/DUT/P6_DOut_s
add wave -noupdate /core_tb/DUT/P6_En_s
add wave -noupdate /core_tb/DUT/P6_Sel_s
add wave -noupdate /core_tb/DUT/P6_DIn_s
add wave -noupdate /core_tb/DUT/TimerA_DOut_s
add wave -noupdate /core_tb/DUT/TimerA_IRQ1_s
add wave -noupdate /core_tb/DUT/TimerA_IRQ2_s
add wave -noupdate /core_tb/DUT/INClk_s
add wave -noupdate /core_tb/DUT/TAClk_s
add wave -noupdate /core_tb/DUT/TimerA_CCI0A_s
add wave -noupdate /core_tb/DUT/TimerA_CCI0B_s
add wave -noupdate /core_tb/DUT/TimerA_Out0_s
add wave -noupdate /core_tb/DUT/TimerA_Out0_En_s
add wave -noupdate /core_tb/DUT/TimerA_CCI1A_s
add wave -noupdate /core_tb/DUT/TimerA_CCI1B_s
add wave -noupdate /core_tb/DUT/TimerA_Out1_s
add wave -noupdate /core_tb/DUT/TimerA_Out1_En_s
add wave -noupdate /core_tb/DUT/TimerA_CCI2A_s
add wave -noupdate /core_tb/DUT/TimerA_CCI2B_s
add wave -noupdate /core_tb/DUT/TimerA_Out2_s
add wave -noupdate /core_tb/DUT/TimerA_Out2_En_s
add wave -noupdate /core_tb/DUT/UartRxD_i
add wave -noupdate /core_tb/DUT/UartTxD_o
add wave -noupdate -group {SPI Master} /core_tb/DUT/SPI_CPOL
add wave -noupdate -group {SPI Master} /core_tb/DUT/SPI_CPHA
add wave -noupdate -group {SPI Master} /core_tb/DUT/SPI_LSBFE
add wave -noupdate -group {SPI Master} /core_tb/DUT/SPI_SPPR_SPR
add wave -noupdate -group {SPI Master} /core_tb/DUT/SPI_Transmission
add wave -noupdate -group {SPI Master} /core_tb/DUT/SPI_Write
add wave -noupdate -group {SPI Master} /core_tb/DUT/SPI_ReadNext
add wave -noupdate -group {SPI Master} /core_tb/DUT/SPI_DataIn
add wave -noupdate -group {SPI Master} /core_tb/DUT/SPI_DataOut
add wave -noupdate -group {SPI Master} /core_tb/DUT/SPI_FIFOFull
add wave -noupdate -group {SPI Master} /core_tb/DUT/SPI_FIFOEmpty
add wave -noupdate -group {SPI Master} /core_tb/DUT/SPI_ScanEnable
add wave -noupdate -group {SPI Master} /core_tb/DUT/SPI_ScanClk
add wave -noupdate -group {SPI Master} /core_tb/DUT/SPI_ScanDataIn
add wave -noupdate -group {SPI Master} /core_tb/DUT/SPI_ScanDataOut
add wave -noupdate -group {I2C Master} /core_tb/DUT/I2C_F100_400_n
add wave -noupdate -group {I2C Master} /core_tb/DUT/I2C_Divider800
add wave -noupdate -group {I2C Master} /core_tb/DUT/I2C_StartProcess
add wave -noupdate -group {I2C Master} /core_tb/DUT/I2C_ReceiveSend_n
add wave -noupdate -group {I2C Master} /core_tb/DUT/I2C_Busy
add wave -noupdate -group {I2C Master} /core_tb/DUT/I2C_ReadCount
add wave -noupdate -group {I2C Master} /core_tb/DUT/I2C_FIFOReadNext
add wave -noupdate -group {I2C Master} /core_tb/DUT/I2C_FIFOWrite
add wave -noupdate -group {I2C Master} /core_tb/DUT/I2C_FIFOEmpty
add wave -noupdate -group {I2C Master} /core_tb/DUT/I2C_FIFOFull
add wave -noupdate -group {I2C Master} /core_tb/DUT/I2C_DataIn
add wave -noupdate -group {I2C Master} /core_tb/DUT/I2C_DataOut
add wave -noupdate -group {I2C Master} /core_tb/DUT/I2C_ErrAck
add wave -noupdate -group {I2C Master} /core_tb/DUT/I2C_ErrBusColl
add wave -noupdate -group {I2C Master} /core_tb/DUT/I2C_ErrFIFOFull
add wave -noupdate -group {I2C Master} /core_tb/DUT/I2C_ErrGotNAck
add wave -noupdate -group {I2C Master} /core_tb/DUT/I2C_ErrCoreBusy
add wave -noupdate -group {I2C Master} /core_tb/DUT/I2C_ErrFIFOEmpty
add wave -noupdate -group {I2C Master} /core_tb/DUT/I2C_ErrCoreStopped
add wave -noupdate -group {I2C Master} /core_tb/DUT/I2C_ErrDevNotPresent
add wave -noupdate -group {I2C Master} /core_tb/DUT/I2C_ErrReadCountZero
add wave -noupdate -group {I2C Master} /core_tb/DUT/I2C_ScanEnable
add wave -noupdate -group {I2C Master} /core_tb/DUT/I2C_ScanClk
add wave -noupdate -group {I2C Master} /core_tb/DUT/I2C_ScanDataIn
add wave -noupdate -group {I2C Master} /core_tb/DUT/I2C_ScanDataOut
#add wave -noupdate -group {OneWire Master} /core_tb/DUT/OneWire_OWReset
#add wave -noupdate -group {OneWire Master} /core_tb/DUT/OneWire_DeactivateOverdriveMode
#add wave -noupdate -group {OneWire Master} /core_tb/DUT/OneWire_SearchROM
#add wave -noupdate -group {OneWire Master} /core_tb/DUT/OneWire_ReadROM
#add wave -noupdate -group {OneWire Master} /core_tb/DUT/OneWire_MatchROM
#add wave -noupdate -group {OneWire Master} /core_tb/DUT/OneWire_SkipROM
#add wave -noupdate -group {OneWire Master} /core_tb/DUT/OneWire_CondSearchROM
#add wave -noupdate -group {OneWire Master} /core_tb/DUT/OneWire_OverdriveSkipROM
#add wave -noupdate -group {OneWire Master} /core_tb/DUT/OneWire_OverdriveMatchROM
#add wave -noupdate -group {OneWire Master} /core_tb/DUT/OneWire_CondReadROM
#add wave -noupdate -group {OneWire Master} /core_tb/DUT/OneWire_ResumeROM
#add wave -noupdate -group {OneWire Master} /core_tb/DUT/OneWire_WriteByte
#add wave -noupdate -group {OneWire Master} /core_tb/DUT/OneWire_ReadByte
#add wave -noupdate -group {OneWire Master} /core_tb/DUT/OneWire_GetROMID
#add wave -noupdate -group {OneWire Master} /core_tb/DUT/OneWire_DataIn
#add wave -noupdate -group {OneWire Master} /core_tb/DUT/OneWire_DataOut
#add wave -noupdate -group {OneWire Master} /core_tb/DUT/OneWire_ROMIDsInArray
#add wave -noupdate -group {OneWire Master} /core_tb/DUT/OneWire_Noslaves
#add wave -noupdate -group {OneWire Master} /core_tb/DUT/OneWire_ROMIDArrayToSmall
#add wave -noupdate -group {OneWire Master} /core_tb/DUT/OneWire_PDROut
#add wave -noupdate -group {OneWire Master} /core_tb/DUT/OneWire_Ready
#add wave -noupdate -group {OneWire Master} /core_tb/DUT/OneWire_ResetLowTime
#add wave -noupdate -group {OneWire Master} /core_tb/DUT/OneWire_ResetTime
#add wave -noupdate -group {OneWire Master} /core_tb/DUT/OneWire_ResetWaitForDetectionDuration
#add wave -noupdate -group {OneWire Master} /core_tb/DUT/OneWire_ResetPrecenceIntervalDuration
#add wave -noupdate -group {OneWire Master} /core_tb/DUT/OneWire_WRSlotHighDataTime
#add wave -noupdate -group {OneWire Master} /core_tb/DUT/OneWire_RDSlotSampleTime
#add wave -noupdate -group {OneWire Master} /core_tb/DUT/OneWire_SlotLowDataTime
#add wave -noupdate -group {OneWire Master} /core_tb/DUT/OneWire_SlotDuration
#add wave -noupdate -group {OneWire Master} /core_tb/DUT/OneWire_RDSlotInitTime
#add wave -noupdate -group {OneWire Master} /core_tb/DUT/OneWire_ODResetLowTime
#add wave -noupdate -group {OneWire Master} /core_tb/DUT/OneWire_ODResetTime
#add wave -noupdate -group {OneWire Master} /core_tb/DUT/OneWire_ODResetWaitForDetectionDuration
#add wave -noupdate -group {OneWire Master} /core_tb/DUT/OneWire_ODResetPrecenceIntervalDuration
#add wave -noupdate -group {OneWire Master} /core_tb/DUT/OneWire_ODWRSlotHighDataTime
#add wave -noupdate -group {OneWire Master} /core_tb/DUT/OneWire_ODRDSlotSampleTime
#add wave -noupdate -group {OneWire Master} /core_tb/DUT/OneWire_ODSlotLowDataTime
#add wave -noupdate -group {OneWire Master} /core_tb/DUT/OneWire_ODSlotDuration
#add wave -noupdate -group {OneWire Master} /core_tb/DUT/OneWire_ODRDSlotInitTime
#add wave -noupdate -group {OneWire Master} /core_tb/DUT/OneWire_ScanEnable
#add wave -noupdate -group {OneWire Master} /core_tb/DUT/OneWire_ScanClk
#add wave -noupdate -group {OneWire Master} /core_tb/DUT/OneWire_ScanDataIn
#add wave -noupdate -group {OneWire Master} /core_tb/DUT/OneWire_ScanDataOut
#add wave -noupdate -group PWM /core_tb/DUT/PWM_Polarity
#add wave -noupdate -group PWM /core_tb/DUT/PWM_Value
#add wave -noupdate -group PWM /core_tb/DUT/PWM_NewValue
#add wave -noupdate -group PWM /core_tb/DUT/PWM_ScanEnable
#add wave -noupdate -group PWM /core_tb/DUT/PWM_ScanClk
#add wave -noupdate -group PWM /core_tb/DUT/PWM_ScanDataIn
#add wave -noupdate -group PWM /core_tb/DUT/PWM_ScanDataOut
#add wave -noupdate -group SENT /core_tb/DUT/SENT_Chipselect
#add wave -noupdate -group SENT /core_tb/DUT/SENT_NumDatNibble
#add wave -noupdate -group SENT /core_tb/DUT/SENT_MinSync
#add wave -noupdate -group SENT /core_tb/DUT/SENT_OutWide
#add wave -noupdate -group SENT /core_tb/DUT/SENT_NewData
#add wave -noupdate -group SENT /core_tb/DUT/SENT_CrcOk
#add wave -noupdate -group SENT /core_tb/DUT/SENT_ScanEnable
#add wave -noupdate -group SENT /core_tb/DUT/SENT_ScanClk
#add wave -noupdate -group SENT /core_tb/DUT/SENT_ScanDataIn
#add wave -noupdate -group SENT /core_tb/DUT/SENT_ScanDataOut
#add wave -noupdate -group SPC /core_tb/DUT/SPC_Start
#add wave -noupdate -group SPC /core_tb/DUT/SPC_NumDatNibble
#add wave -noupdate -group SPC /core_tb/DUT/SPC_LengthTrigger
#add wave -noupdate -group SPC /core_tb/DUT/SPC_LengthTimeout
#add wave -noupdate -group SPC /core_tb/DUT/SPC_MinSync
#add wave -noupdate -group SPC /core_tb/DUT/SPC_OutWide
#add wave -noupdate -group SPC /core_tb/DUT/SPC_NewData
#add wave -noupdate -group SPC /core_tb/DUT/SPC_CrcOk
#add wave -noupdate -group SPC /core_tb/DUT/SPC_SPCReady
#add wave -noupdate -group SPC /core_tb/DUT/SPC_ScanEnable
#add wave -noupdate -group SPC /core_tb/DUT/SPC_ScanClk
#add wave -noupdate -group SPC /core_tb/DUT/SPC_ScanDataIn
#add wave -noupdate -group SPC /core_tb/DUT/SPC_ScanDataOut
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
