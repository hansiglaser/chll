onerror {resume}
quietly WaveActivateNextPane {} 0
add wave -noupdate /Core_ADT7310_tb/DUT/Reset_n_i
add wave -noupdate /Core_ADT7310_tb/DUT/Clk_i
add wave -noupdate /Core_ADT7310_tb/DUT/Inputs_i
add wave -noupdate /Core_ADT7310_tb/DUT/Outputs_o
add wave -noupdate /Core_ADT7310_tb/DUT/SPIMISO_i
add wave -noupdate /Core_ADT7310_tb/DUT/SPIMOSI_o
add wave -noupdate /Core_ADT7310_tb/DUT/SPISCK_o
add wave -noupdate /Core_ADT7310_tb/DUT/I2CSCL_o
add wave -noupdate /Core_ADT7310_tb/DUT/I2CSDA_i
add wave -noupdate /Core_ADT7310_tb/DUT/I2CSDA_o
#add wave -noupdate /Core_ADT7310_tb/DUT/OneWire_i
#add wave -noupdate /Core_ADT7310_tb/DUT/OneWire_o
#add wave -noupdate /Core_ADT7310_tb/DUT/PWMInput_i
#add wave -noupdate /Core_ADT7310_tb/DUT/SENTInput_i
#add wave -noupdate /Core_ADT7310_tb/DUT/SPCInput_i
#add wave -noupdate /Core_ADT7310_tb/DUT/SPCTrigger_o
add wave -noupdate /Core_ADT7310_tb/DUT/AdcConvComplete_i
add wave -noupdate /Core_ADT7310_tb/DUT/AdcDoConvert_o
add wave -noupdate /Core_ADT7310_tb/DUT/AdcValue_i
add wave -noupdate -group {OpenMSP Memory Backbone} -radix hexadecimal /Core_ADT7310_tb/DUT/openMSP430_0/mem_backbone_0/PMEM_OFFSET
add wave -noupdate -group {OpenMSP Memory Backbone} -radix hexadecimal /Core_ADT7310_tb/DUT/openMSP430_0/mem_backbone_0/dbg_mem_din
add wave -noupdate -group {OpenMSP Memory Backbone} -radix hexadecimal /Core_ADT7310_tb/DUT/openMSP430_0/mem_backbone_0/dmem_addr
add wave -noupdate -group {OpenMSP Memory Backbone} /Core_ADT7310_tb/DUT/openMSP430_0/mem_backbone_0/dmem_cen
add wave -noupdate -group {OpenMSP Memory Backbone} -radix hexadecimal /Core_ADT7310_tb/DUT/openMSP430_0/mem_backbone_0/dmem_din
add wave -noupdate -group {OpenMSP Memory Backbone} /Core_ADT7310_tb/DUT/openMSP430_0/mem_backbone_0/dmem_wen
add wave -noupdate -group {OpenMSP Memory Backbone} -radix hexadecimal /Core_ADT7310_tb/DUT/openMSP430_0/mem_backbone_0/eu_mdb_in
add wave -noupdate -group {OpenMSP Memory Backbone} -radix hexadecimal /Core_ADT7310_tb/DUT/openMSP430_0/mem_backbone_0/fe_mdb_in
add wave -noupdate -group {OpenMSP Memory Backbone} /Core_ADT7310_tb/DUT/openMSP430_0/mem_backbone_0/fe_pmem_wait
add wave -noupdate -group {OpenMSP Memory Backbone} -radix hexadecimal /Core_ADT7310_tb/DUT/openMSP430_0/mem_backbone_0/per_addr
add wave -noupdate -group {OpenMSP Memory Backbone} -radix hexadecimal /Core_ADT7310_tb/DUT/openMSP430_0/mem_backbone_0/per_din
add wave -noupdate -group {OpenMSP Memory Backbone} /Core_ADT7310_tb/DUT/openMSP430_0/mem_backbone_0/per_we
add wave -noupdate -group {OpenMSP Memory Backbone} /Core_ADT7310_tb/DUT/openMSP430_0/mem_backbone_0/per_en
add wave -noupdate -group {OpenMSP Memory Backbone} -radix hexadecimal /Core_ADT7310_tb/DUT/openMSP430_0/mem_backbone_0/pmem_addr
add wave -noupdate -group {OpenMSP Memory Backbone} /Core_ADT7310_tb/DUT/openMSP430_0/mem_backbone_0/pmem_cen
add wave -noupdate -group {OpenMSP Memory Backbone} -radix hexadecimal /Core_ADT7310_tb/DUT/openMSP430_0/mem_backbone_0/pmem_din
add wave -noupdate -group {OpenMSP Memory Backbone} /Core_ADT7310_tb/DUT/openMSP430_0/mem_backbone_0/pmem_wen
add wave -noupdate -group {OpenMSP Memory Backbone} /Core_ADT7310_tb/DUT/openMSP430_0/mem_backbone_0/dbg_halt_st
add wave -noupdate -group {OpenMSP Memory Backbone} -radix hexadecimal /Core_ADT7310_tb/DUT/openMSP430_0/mem_backbone_0/dbg_mem_addr
add wave -noupdate -group {OpenMSP Memory Backbone} -radix hexadecimal /Core_ADT7310_tb/DUT/openMSP430_0/mem_backbone_0/dbg_mem_dout
add wave -noupdate -group {OpenMSP Memory Backbone} /Core_ADT7310_tb/DUT/openMSP430_0/mem_backbone_0/dbg_mem_en
add wave -noupdate -group {OpenMSP Memory Backbone} /Core_ADT7310_tb/DUT/openMSP430_0/mem_backbone_0/dbg_mem_wr
add wave -noupdate -group {OpenMSP Memory Backbone} -radix hexadecimal /Core_ADT7310_tb/DUT/openMSP430_0/mem_backbone_0/dmem_dout
add wave -noupdate -group {OpenMSP Memory Backbone} -radix hexadecimal /Core_ADT7310_tb/DUT/openMSP430_0/mem_backbone_0/eu_mab
add wave -noupdate -group {OpenMSP Memory Backbone} /Core_ADT7310_tb/DUT/openMSP430_0/mem_backbone_0/eu_mb_en
add wave -noupdate -group {OpenMSP Memory Backbone} /Core_ADT7310_tb/DUT/openMSP430_0/mem_backbone_0/eu_mb_wr
add wave -noupdate -group {OpenMSP Memory Backbone} -radix hexadecimal /Core_ADT7310_tb/DUT/openMSP430_0/mem_backbone_0/eu_mdb_out
add wave -noupdate -group {OpenMSP Memory Backbone} -radix hexadecimal /Core_ADT7310_tb/DUT/openMSP430_0/mem_backbone_0/fe_mab
add wave -noupdate -group {OpenMSP Memory Backbone} /Core_ADT7310_tb/DUT/openMSP430_0/mem_backbone_0/fe_mb_en
add wave -noupdate -group {OpenMSP Memory Backbone} /Core_ADT7310_tb/DUT/openMSP430_0/mem_backbone_0/mclk
add wave -noupdate -group {OpenMSP Memory Backbone} -radix hexadecimal /Core_ADT7310_tb/DUT/openMSP430_0/mem_backbone_0/per_dout
add wave -noupdate -group {OpenMSP Memory Backbone} -radix hexadecimal /Core_ADT7310_tb/DUT/openMSP430_0/mem_backbone_0/pmem_dout
add wave -noupdate -group {OpenMSP Memory Backbone} /Core_ADT7310_tb/DUT/openMSP430_0/mem_backbone_0/puc_rst
add wave -noupdate -group {OpenMSP Memory Backbone} /Core_ADT7310_tb/DUT/openMSP430_0/mem_backbone_0/scan_enable
add wave -noupdate -group {OpenMSP Memory Backbone} /Core_ADT7310_tb/DUT/openMSP430_0/mem_backbone_0/eu_dmem_cen
add wave -noupdate -group {OpenMSP Memory Backbone} -radix hexadecimal /Core_ADT7310_tb/DUT/openMSP430_0/mem_backbone_0/eu_dmem_addr
add wave -noupdate -group {OpenMSP Memory Backbone} /Core_ADT7310_tb/DUT/openMSP430_0/mem_backbone_0/dbg_dmem_cen
add wave -noupdate -group {OpenMSP Memory Backbone} -radix hexadecimal /Core_ADT7310_tb/DUT/openMSP430_0/mem_backbone_0/dbg_dmem_addr
add wave -noupdate -group {OpenMSP Memory Backbone} /Core_ADT7310_tb/DUT/openMSP430_0/mem_backbone_0/eu_pmem_cen
add wave -noupdate -group {OpenMSP Memory Backbone} -radix hexadecimal /Core_ADT7310_tb/DUT/openMSP430_0/mem_backbone_0/eu_pmem_addr
add wave -noupdate -group {OpenMSP Memory Backbone} /Core_ADT7310_tb/DUT/openMSP430_0/mem_backbone_0/fe_pmem_cen
add wave -noupdate -group {OpenMSP Memory Backbone} -radix hexadecimal /Core_ADT7310_tb/DUT/openMSP430_0/mem_backbone_0/fe_pmem_addr
add wave -noupdate -group {OpenMSP Memory Backbone} /Core_ADT7310_tb/DUT/openMSP430_0/mem_backbone_0/dbg_pmem_cen
add wave -noupdate -group {OpenMSP Memory Backbone} -radix hexadecimal /Core_ADT7310_tb/DUT/openMSP430_0/mem_backbone_0/dbg_pmem_addr
add wave -noupdate -group {OpenMSP Memory Backbone} /Core_ADT7310_tb/DUT/openMSP430_0/mem_backbone_0/dbg_per_en
add wave -noupdate -group {OpenMSP Memory Backbone} /Core_ADT7310_tb/DUT/openMSP430_0/mem_backbone_0/eu_per_en
add wave -noupdate -group {OpenMSP Memory Backbone} -radix hexadecimal /Core_ADT7310_tb/DUT/openMSP430_0/mem_backbone_0/per_addr_mux
add wave -noupdate -group {OpenMSP Memory Backbone} -radix hexadecimal /Core_ADT7310_tb/DUT/openMSP430_0/mem_backbone_0/per_addr_ful
add wave -noupdate -group {OpenMSP Memory Backbone} -radix hexadecimal /Core_ADT7310_tb/DUT/openMSP430_0/mem_backbone_0/per_dout_val
add wave -noupdate -group {OpenMSP Memory Backbone} /Core_ADT7310_tb/DUT/openMSP430_0/mem_backbone_0/fe_pmem_cen_dly
add wave -noupdate -group {OpenMSP Memory Backbone} /Core_ADT7310_tb/DUT/openMSP430_0/mem_backbone_0/fe_pmem_save
add wave -noupdate -group {OpenMSP Memory Backbone} /Core_ADT7310_tb/DUT/openMSP430_0/mem_backbone_0/fe_pmem_restore
add wave -noupdate -group {OpenMSP Memory Backbone} /Core_ADT7310_tb/DUT/openMSP430_0/mem_backbone_0/mclk_bckup
add wave -noupdate -group {OpenMSP Memory Backbone} /Core_ADT7310_tb/DUT/openMSP430_0/mem_backbone_0/pmem_dout_bckup
add wave -noupdate -group {OpenMSP Memory Backbone} /Core_ADT7310_tb/DUT/openMSP430_0/mem_backbone_0/pmem_dout_bckup_sel
add wave -noupdate -group {OpenMSP Memory Backbone} /Core_ADT7310_tb/DUT/openMSP430_0/mem_backbone_0/eu_mdb_in_sel
add wave -noupdate -group {OpenMSP Memory Backbone} /Core_ADT7310_tb/DUT/openMSP430_0/mem_backbone_0/dbg_mem_din_sel
add wave -noupdate -expand -group {OpenMSP430 Debug Utility} -radix ascii /Core_ADT7310_tb/DUT/i_state
add wave -noupdate -expand -group {OpenMSP430 Debug Utility} -radix ascii /Core_ADT7310_tb/DUT/e_state
add wave -noupdate -expand -group {OpenMSP430 Debug Utility} -radix unsigned /Core_ADT7310_tb/DUT/inst_cycle
add wave -noupdate -expand -group {OpenMSP430 Debug Utility} -radix ascii /Core_ADT7310_tb/DUT/inst_full
add wave -noupdate -expand -group {OpenMSP430 Debug Utility} -radix unsigned /Core_ADT7310_tb/DUT/inst_number
add wave -noupdate -expand -group {OpenMSP430 Debug Utility} -radix hexadecimal /Core_ADT7310_tb/DUT/inst_pc
add wave -noupdate -expand -group {OpenMSP430 Debug Utility} -radix ascii /Core_ADT7310_tb/DUT/inst_short
add wave -noupdate -expand -group {OpenMSP430 Debug Utility} -radix hexadecimal /Core_ADT7310_tb/DUT/openMSP430_0/frontend_0/pc
add wave -noupdate -expand -group {OpenMSP430 Debug Utility} -radix hexadecimal /Core_ADT7310_tb/DUT/openMSP430_0/frontend_0/pc_nxt
add wave -noupdate -expand -group {OpenMSP430 Debug Utility} /Core_ADT7310_tb/DUT/openMSP430_0/frontend_0/ir
add wave -noupdate -expand -group {OpenMSP430 Debug Utility} /Core_ADT7310_tb/DUT/openMSP430_0/clock_module_0/dbg_rst_nxt
add wave -noupdate -expand -group {OpenMSP430 Debug Utility} -radix hexadecimal /Core_ADT7310_tb/DUT/openMSP430_0/execution_unit_0/register_file_0/r0
add wave -noupdate -expand -group {OpenMSP430 Debug Utility} -radix hexadecimal /Core_ADT7310_tb/DUT/openMSP430_0/execution_unit_0/register_file_0/r1
add wave -noupdate -expand -group {OpenMSP430 Debug Utility} -radix hexadecimal /Core_ADT7310_tb/DUT/openMSP430_0/execution_unit_0/register_file_0/r2
add wave -noupdate -expand -group {OpenMSP430 Debug Utility} -radix hexadecimal /Core_ADT7310_tb/DUT/openMSP430_0/execution_unit_0/register_file_0/r3
add wave -noupdate -expand -group {OpenMSP430 Debug Utility} -radix hexadecimal /Core_ADT7310_tb/DUT/openMSP430_0/execution_unit_0/register_file_0/r4
add wave -noupdate -expand -group {OpenMSP430 Debug Utility} -radix hexadecimal /Core_ADT7310_tb/DUT/openMSP430_0/execution_unit_0/register_file_0/r5
add wave -noupdate -expand -group {OpenMSP430 Debug Utility} -radix hexadecimal /Core_ADT7310_tb/DUT/openMSP430_0/execution_unit_0/register_file_0/r6
add wave -noupdate -expand -group {OpenMSP430 Debug Utility} -radix hexadecimal /Core_ADT7310_tb/DUT/openMSP430_0/execution_unit_0/register_file_0/r7
add wave -noupdate -expand -group {OpenMSP430 Debug Utility} -radix hexadecimal /Core_ADT7310_tb/DUT/openMSP430_0/execution_unit_0/register_file_0/r8
add wave -noupdate -expand -group {OpenMSP430 Debug Utility} -radix hexadecimal /Core_ADT7310_tb/DUT/openMSP430_0/execution_unit_0/register_file_0/r9
add wave -noupdate -expand -group {OpenMSP430 Debug Utility} -radix hexadecimal /Core_ADT7310_tb/DUT/openMSP430_0/execution_unit_0/register_file_0/r10
add wave -noupdate -expand -group {OpenMSP430 Debug Utility} -radix hexadecimal /Core_ADT7310_tb/DUT/openMSP430_0/execution_unit_0/register_file_0/r11
add wave -noupdate -expand -group {OpenMSP430 Debug Utility} -radix hexadecimal /Core_ADT7310_tb/DUT/openMSP430_0/execution_unit_0/register_file_0/r12
add wave -noupdate -expand -group {OpenMSP430 Debug Utility} -radix hexadecimal /Core_ADT7310_tb/DUT/openMSP430_0/execution_unit_0/register_file_0/r13
add wave -noupdate -expand -group {OpenMSP430 Debug Utility} -radix ascii /Core_ADT7310_tb/DUT/openMSP430_0/execution_unit_0/register_file_0/r14
add wave -noupdate -expand -group {OpenMSP430 Debug Utility} -radix ascii /Core_ADT7310_tb/DUT/openMSP430_0/execution_unit_0/register_file_0/r15
add wave -noupdate -expand -group OpenMSP430 -group Reset /Core_ADT7310_tb/DUT/PUC_Reset_s
add wave -noupdate -expand -group OpenMSP430 -group Reset /Core_ADT7310_tb/DUT/CPU_Enable_s
add wave -noupdate -expand -group OpenMSP430 -group Clocks /Core_ADT7310_tb/DUT/DCO_Enable_s
add wave -noupdate -expand -group OpenMSP430 -group Clocks /Core_ADT7310_tb/DUT/DCO_Wakeup_s
add wave -noupdate -expand -group OpenMSP430 -group Clocks /Core_ADT7310_tb/DUT/LFXT_Enable_s
add wave -noupdate -expand -group OpenMSP430 -group Clocks /Core_ADT7310_tb/DUT/LFXT_Wakeup_s
add wave -noupdate -expand -group OpenMSP430 -group Clocks /Core_ADT7310_tb/DUT/LFXT_Clk_i
add wave -noupdate -expand -group OpenMSP430 -group Clocks /Core_ADT7310_tb/DUT/MClk_s
add wave -noupdate -expand -group OpenMSP430 -group Clocks /Core_ADT7310_tb/DUT/AClk_s
add wave -noupdate -expand -group OpenMSP430 -group Clocks /Core_ADT7310_tb/DUT/AClk_En_s
add wave -noupdate -expand -group OpenMSP430 -group Clocks /Core_ADT7310_tb/DUT/SMClk_s
add wave -noupdate -expand -group OpenMSP430 -group Clocks /Core_ADT7310_tb/DUT/SMClk_En_s
add wave -noupdate -expand -group OpenMSP430 /Core_ADT7310_tb/DUT/Wakeup_s
add wave -noupdate -expand -group OpenMSP430 -group {Program Memory} -radix hexadecimal /Core_ADT7310_tb/DUT/PMem_Addr_s
add wave -noupdate -expand -group OpenMSP430 -group {Program Memory} /Core_ADT7310_tb/DUT/PMem_En_n_s
add wave -noupdate -expand -group OpenMSP430 -group {Program Memory} -radix hexadecimal /Core_ADT7310_tb/DUT/PMem_DIn_s
add wave -noupdate -expand -group OpenMSP430 -group {Program Memory} /Core_ADT7310_tb/DUT/PMem_Wr_n_s
add wave -noupdate -expand -group OpenMSP430 -group {Program Memory} -radix hexadecimal /Core_ADT7310_tb/DUT/PMem_DOut_s
add wave -noupdate -expand -group OpenMSP430 -group {Data Memory} -radix hexadecimal /Core_ADT7310_tb/DUT/DMem_Addr_s
add wave -noupdate -expand -group OpenMSP430 -group {Data Memory} /Core_ADT7310_tb/DUT/DMem_En_n_s
add wave -noupdate -expand -group OpenMSP430 -group {Data Memory} -radix hexadecimal /Core_ADT7310_tb/DUT/DMem_DIn_s
add wave -noupdate -expand -group OpenMSP430 -group {Data Memory} /Core_ADT7310_tb/DUT/DMem_Wr_n_s
add wave -noupdate -expand -group OpenMSP430 -group {Data Memory} -radix hexadecimal /Core_ADT7310_tb/DUT/DMem_DOut_s
add wave -noupdate -expand -group OpenMSP430 -expand -group Peripherals -radix hexadecimal /Core_ADT7310_tb/DUT/Per_Addr_s
add wave -noupdate -expand -group OpenMSP430 -expand -group Peripherals -radix hexadecimal /Core_ADT7310_tb/DUT/Per_DIn_s
add wave -noupdate -expand -group OpenMSP430 -expand -group Peripherals -radix hexadecimal /Core_ADT7310_tb/DUT/Per_DOut_s
add wave -noupdate -expand -group OpenMSP430 -expand -group Peripherals /Core_ADT7310_tb/DUT/Per_Wr_s
add wave -noupdate -expand -group OpenMSP430 -expand -group Peripherals /Core_ADT7310_tb/DUT/Per_En_s
add wave -noupdate -expand -group OpenMSP430 -group Interrupts /Core_ADT7310_tb/DUT/IRQ_s
add wave -noupdate -expand -group OpenMSP430 -group Interrupts /Core_ADT7310_tb/DUT/IRQ_Ack_s
add wave -noupdate -expand -group OpenMSP430 -group Interrupts /Core_ADT7310_tb/DUT/NMI_s
add wave -noupdate -expand -group OpenMSP430 -group Debug /Core_ADT7310_tb/DUT/Dbg_En_i
add wave -noupdate -expand -group OpenMSP430 -group Debug /Core_ADT7310_tb/DUT/Dbg_Freeze_s
add wave -noupdate -expand -group OpenMSP430 -group Debug /Core_ADT7310_tb/DUT/Dbg_UART_RxD_s
add wave -noupdate -expand -group OpenMSP430 -group Debug /Core_ADT7310_tb/DUT/Dbg_UART_TxD_s
add wave -noupdate -expand -group OpenMSP430 -group Debug /Core_ADT7310_tb/DUT/Dbg_SCL_i
add wave -noupdate -expand -group OpenMSP430 -group Debug /Core_ADT7310_tb/DUT/Dbg_SDA_Out_o
add wave -noupdate -expand -group OpenMSP430 -group Debug /Core_ADT7310_tb/DUT/Dbg_SDA_In_i
add wave -noupdate -expand -group OpenMSP430 /Core_ADT7310_tb/DUT/Scan_Enable_s
add wave -noupdate -expand -group OpenMSP430 /Core_ADT7310_tb/DUT/Scan_Mode_s
add wave -noupdate -expand -group OpenMSP430 -group TimerA -radix hexadecimal /Core_ADT7310_tb/DUT/timerA_0/tar
add wave -noupdate -expand -group OpenMSP430 -group TimerA /Core_ADT7310_tb/DUT/timerA_0/tacctl0
add wave -noupdate -expand -group OpenMSP430 -group TimerA -radix hexadecimal /Core_ADT7310_tb/DUT/timerA_0/taccr0
add wave -noupdate -expand -group OpenMSP430 -group TimerA /Core_ADT7310_tb/DUT/timerA_0/tacctl1
add wave -noupdate -expand -group OpenMSP430 -group TimerA -radix hexadecimal /Core_ADT7310_tb/DUT/timerA_0/taccr1
add wave -noupdate -expand -group OpenMSP430 -group TimerA /Core_ADT7310_tb/DUT/timerA_0/tacctl2
add wave -noupdate -expand -group OpenMSP430 -group TimerA -radix hexadecimal /Core_ADT7310_tb/DUT/timerA_0/taccr2
add wave -noupdate /Core_ADT7310_tb/DUT/Gpio_DOut_s
add wave -noupdate /Core_ADT7310_tb/DUT/Gpio_IRQ1_s
add wave -noupdate /Core_ADT7310_tb/DUT/Gpio_IRQ2_s
add wave -noupdate /Core_ADT7310_tb/DUT/P1_DOut_s
add wave -noupdate /Core_ADT7310_tb/DUT/P1_En_s
add wave -noupdate /Core_ADT7310_tb/DUT/P1_Sel_s
add wave -noupdate /Core_ADT7310_tb/DUT/P1_DIn_s
add wave -noupdate /Core_ADT7310_tb/DUT/P2_DOut_s
add wave -noupdate /Core_ADT7310_tb/DUT/P2_En_s
add wave -noupdate /Core_ADT7310_tb/DUT/P2_Sel_s
add wave -noupdate /Core_ADT7310_tb/DUT/P2_DIn_s
add wave -noupdate /Core_ADT7310_tb/DUT/P3_DOut_s
add wave -noupdate /Core_ADT7310_tb/DUT/P3_En_s
add wave -noupdate /Core_ADT7310_tb/DUT/P3_Sel_s
add wave -noupdate /Core_ADT7310_tb/DUT/P3_DIn_s
add wave -noupdate /Core_ADT7310_tb/DUT/P4_DOut_s
add wave -noupdate /Core_ADT7310_tb/DUT/P4_En_s
add wave -noupdate /Core_ADT7310_tb/DUT/P4_Sel_s
add wave -noupdate /Core_ADT7310_tb/DUT/P4_DIn_s
add wave -noupdate /Core_ADT7310_tb/DUT/P5_DOut_s
add wave -noupdate /Core_ADT7310_tb/DUT/P5_En_s
add wave -noupdate /Core_ADT7310_tb/DUT/P5_Sel_s
add wave -noupdate /Core_ADT7310_tb/DUT/P5_DIn_s
add wave -noupdate /Core_ADT7310_tb/DUT/P6_DOut_s
add wave -noupdate /Core_ADT7310_tb/DUT/P6_En_s
add wave -noupdate /Core_ADT7310_tb/DUT/P6_Sel_s
add wave -noupdate /Core_ADT7310_tb/DUT/P6_DIn_s
add wave -noupdate /Core_ADT7310_tb/DUT/TimerA_DOut_s
add wave -noupdate /Core_ADT7310_tb/DUT/TimerA_IRQ1_s
add wave -noupdate /Core_ADT7310_tb/DUT/TimerA_IRQ2_s
add wave -noupdate /Core_ADT7310_tb/DUT/INClk_s
add wave -noupdate /Core_ADT7310_tb/DUT/TAClk_s
add wave -noupdate /Core_ADT7310_tb/DUT/TimerA_CCI0A_s
add wave -noupdate /Core_ADT7310_tb/DUT/TimerA_CCI0B_s
add wave -noupdate /Core_ADT7310_tb/DUT/TimerA_Out0_s
add wave -noupdate /Core_ADT7310_tb/DUT/TimerA_Out0_En_s
add wave -noupdate /Core_ADT7310_tb/DUT/TimerA_CCI1A_s
add wave -noupdate /Core_ADT7310_tb/DUT/TimerA_CCI1B_s
add wave -noupdate /Core_ADT7310_tb/DUT/TimerA_Out1_s
add wave -noupdate /Core_ADT7310_tb/DUT/TimerA_Out1_En_s
add wave -noupdate /Core_ADT7310_tb/DUT/TimerA_CCI2A_s
add wave -noupdate /Core_ADT7310_tb/DUT/TimerA_CCI2B_s
add wave -noupdate /Core_ADT7310_tb/DUT/TimerA_Out2_s
add wave -noupdate /Core_ADT7310_tb/DUT/TimerA_Out2_En_s
add wave -noupdate -divider UART
add wave -noupdate /Core_ADT7310_tb/UartRxD_i
add wave -noupdate /Core_ADT7310_tb/UartTxD_o
add wave -noupdate -radix ascii /Core_ADT7310_tb/Uart_1/RxData_o
add wave -noupdate -radix ascii /Core_ADT7310_tb/UartRx_Proc/Ch
add wave -noupdate /Core_ADT7310_tb/Uart_1/RxRd_i
add wave -noupdate /Core_ADT7310_tb/Uart_1/RxEmpty_o
add wave -noupdate /Core_ADT7310_tb/Uart_1/RxFull_o
add wave -noupdate -radix ascii /Core_ADT7310_tb/Uart_1/TxData_i
add wave -noupdate /Core_ADT7310_tb/Uart_1/TxWr_i
add wave -noupdate /Core_ADT7310_tb/Uart_1/TxEmpty_o
add wave -noupdate /Core_ADT7310_tb/Uart_1/TxFull_o
add wave -noupdate -radix ascii /Core_ADT7310_tb/Uart_1/TXMOD/TXFIFO/DualPortRam/DataB_o
add wave -noupdate -radix ascii /Core_ADT7310_tb/Uart_1/RXMOD/RXFIFO/DualPortRam/DataBuffer
add wave -noupdate -divider SPI
add wave -noupdate /Core_ADT7310_tb/DUT/SCK_o
add wave -noupdate /Core_ADT7310_tb/DUT/MOSI_o
add wave -noupdate /Core_ADT7310_tb/DUT/MISO_i
add wave -noupdate -divider ADT7310
add wave -noupdate /Core_ADT7310_tb/adt7310_1/SCLK_i
add wave -noupdate /Core_ADT7310_tb/adt7310_1/DOUT_o
add wave -noupdate /Core_ADT7310_tb/adt7310_1/DIN_i
add wave -noupdate /Core_ADT7310_tb/adt7310_1/CS_n_i
add wave -noupdate /Core_ADT7310_tb/adt7310_1/CT_n_o
add wave -noupdate /Core_ADT7310_tb/adt7310_1/INT_n_o
add wave -noupdate /Core_ADT7310_tb/adt7310_1/Temp_i
add wave -noupdate -divider Peripherals
add wave -noupdate -group {SPI Master} /Core_ADT7310_tb/DUT/SPI_CPOL
add wave -noupdate -group {SPI Master} /Core_ADT7310_tb/DUT/SPI_CPHA
add wave -noupdate -group {SPI Master} /Core_ADT7310_tb/DUT/SPI_LSBFE
add wave -noupdate -group {SPI Master} /Core_ADT7310_tb/DUT/SPI_SPPR_SPR
add wave -noupdate -group {SPI Master} /Core_ADT7310_tb/DUT/SPI_Transmission
add wave -noupdate -group {SPI Master} /Core_ADT7310_tb/DUT/SPI_Write
add wave -noupdate -group {SPI Master} /Core_ADT7310_tb/DUT/SPI_ReadNext
add wave -noupdate -group {SPI Master} /Core_ADT7310_tb/DUT/SPI_DataIn
add wave -noupdate -group {SPI Master} /Core_ADT7310_tb/DUT/SPI_DataOut
add wave -noupdate -group {SPI Master} /Core_ADT7310_tb/DUT/SPI_FIFOFull
add wave -noupdate -group {SPI Master} /Core_ADT7310_tb/DUT/SPI_FIFOEmpty
add wave -noupdate -group {SPI Master} /Core_ADT7310_tb/DUT/SPI_ScanEnable
add wave -noupdate -group {SPI Master} /Core_ADT7310_tb/DUT/SPI_ScanClk
add wave -noupdate -group {SPI Master} /Core_ADT7310_tb/DUT/SPI_ScanDataIn
add wave -noupdate -group {SPI Master} /Core_ADT7310_tb/DUT/SPI_ScanDataOut
add wave -noupdate -group {I2C Master} /Core_ADT7310_tb/DUT/I2C_F100_400_n
add wave -noupdate -group {I2C Master} /Core_ADT7310_tb/DUT/I2C_Divider800
add wave -noupdate -group {I2C Master} /Core_ADT7310_tb/DUT/I2C_StartProcess
add wave -noupdate -group {I2C Master} /Core_ADT7310_tb/DUT/I2C_ReceiveSend_n
add wave -noupdate -group {I2C Master} /Core_ADT7310_tb/DUT/I2C_Busy
add wave -noupdate -group {I2C Master} /Core_ADT7310_tb/DUT/I2C_ReadCount
add wave -noupdate -group {I2C Master} /Core_ADT7310_tb/DUT/I2C_FIFOReadNext
add wave -noupdate -group {I2C Master} /Core_ADT7310_tb/DUT/I2C_FIFOWrite
add wave -noupdate -group {I2C Master} /Core_ADT7310_tb/DUT/I2C_FIFOEmpty
add wave -noupdate -group {I2C Master} /Core_ADT7310_tb/DUT/I2C_FIFOFull
add wave -noupdate -group {I2C Master} /Core_ADT7310_tb/DUT/I2C_DataIn
add wave -noupdate -group {I2C Master} /Core_ADT7310_tb/DUT/I2C_DataOut
add wave -noupdate -group {I2C Master} /Core_ADT7310_tb/DUT/I2C_ErrAck
add wave -noupdate -group {I2C Master} /Core_ADT7310_tb/DUT/I2C_ErrBusColl
add wave -noupdate -group {I2C Master} /Core_ADT7310_tb/DUT/I2C_ErrFIFOFull
add wave -noupdate -group {I2C Master} /Core_ADT7310_tb/DUT/I2C_ErrGotNAck
add wave -noupdate -group {I2C Master} /Core_ADT7310_tb/DUT/I2C_ErrCoreBusy
add wave -noupdate -group {I2C Master} /Core_ADT7310_tb/DUT/I2C_ErrFIFOEmpty
add wave -noupdate -group {I2C Master} /Core_ADT7310_tb/DUT/I2C_ErrCoreStopped
add wave -noupdate -group {I2C Master} /Core_ADT7310_tb/DUT/I2C_ErrDevNotPresent
add wave -noupdate -group {I2C Master} /Core_ADT7310_tb/DUT/I2C_ErrReadCountZero
add wave -noupdate -group {I2C Master} /Core_ADT7310_tb/DUT/I2C_ScanEnable
add wave -noupdate -group {I2C Master} /Core_ADT7310_tb/DUT/I2C_ScanClk
add wave -noupdate -group {I2C Master} /Core_ADT7310_tb/DUT/I2C_ScanDataIn
add wave -noupdate -group {I2C Master} /Core_ADT7310_tb/DUT/I2C_ScanDataOut
#add wave -noupdate -group {OneWire Master} /Core_ADT7310_tb/DUT/OneWire_OWReset
#add wave -noupdate -group {OneWire Master} /Core_ADT7310_tb/DUT/OneWire_DeactivateOverdriveMode
#add wave -noupdate -group {OneWire Master} /Core_ADT7310_tb/DUT/OneWire_SearchROM
#add wave -noupdate -group {OneWire Master} /Core_ADT7310_tb/DUT/OneWire_ReadROM
#add wave -noupdate -group {OneWire Master} /Core_ADT7310_tb/DUT/OneWire_MatchROM
#add wave -noupdate -group {OneWire Master} /Core_ADT7310_tb/DUT/OneWire_SkipROM
#add wave -noupdate -group {OneWire Master} /Core_ADT7310_tb/DUT/OneWire_CondSearchROM
#add wave -noupdate -group {OneWire Master} /Core_ADT7310_tb/DUT/OneWire_OverdriveSkipROM
#add wave -noupdate -group {OneWire Master} /Core_ADT7310_tb/DUT/OneWire_OverdriveMatchROM
#add wave -noupdate -group {OneWire Master} /Core_ADT7310_tb/DUT/OneWire_CondReadROM
#add wave -noupdate -group {OneWire Master} /Core_ADT7310_tb/DUT/OneWire_ResumeROM
#add wave -noupdate -group {OneWire Master} /Core_ADT7310_tb/DUT/OneWire_WriteByte
#add wave -noupdate -group {OneWire Master} /Core_ADT7310_tb/DUT/OneWire_ReadByte
#add wave -noupdate -group {OneWire Master} /Core_ADT7310_tb/DUT/OneWire_GetROMID
#add wave -noupdate -group {OneWire Master} /Core_ADT7310_tb/DUT/OneWire_DataIn
#add wave -noupdate -group {OneWire Master} /Core_ADT7310_tb/DUT/OneWire_DataOut
#add wave -noupdate -group {OneWire Master} /Core_ADT7310_tb/DUT/OneWire_ROMIDsInArray
#add wave -noupdate -group {OneWire Master} /Core_ADT7310_tb/DUT/OneWire_Noslaves
#add wave -noupdate -group {OneWire Master} /Core_ADT7310_tb/DUT/OneWire_ROMIDArrayToSmall
#add wave -noupdate -group {OneWire Master} /Core_ADT7310_tb/DUT/OneWire_PDROut
#add wave -noupdate -group {OneWire Master} /Core_ADT7310_tb/DUT/OneWire_Ready
#add wave -noupdate -group {OneWire Master} /Core_ADT7310_tb/DUT/OneWire_ResetLowTime
#add wave -noupdate -group {OneWire Master} /Core_ADT7310_tb/DUT/OneWire_ResetTime
#add wave -noupdate -group {OneWire Master} /Core_ADT7310_tb/DUT/OneWire_ResetWaitForDetectionDuration
#add wave -noupdate -group {OneWire Master} /Core_ADT7310_tb/DUT/OneWire_ResetPrecenceIntervalDuration
#add wave -noupdate -group {OneWire Master} /Core_ADT7310_tb/DUT/OneWire_WRSlotHighDataTime
#add wave -noupdate -group {OneWire Master} /Core_ADT7310_tb/DUT/OneWire_RDSlotSampleTime
#add wave -noupdate -group {OneWire Master} /Core_ADT7310_tb/DUT/OneWire_SlotLowDataTime
#add wave -noupdate -group {OneWire Master} /Core_ADT7310_tb/DUT/OneWire_SlotDuration
#add wave -noupdate -group {OneWire Master} /Core_ADT7310_tb/DUT/OneWire_RDSlotInitTime
#add wave -noupdate -group {OneWire Master} /Core_ADT7310_tb/DUT/OneWire_ODResetLowTime
#add wave -noupdate -group {OneWire Master} /Core_ADT7310_tb/DUT/OneWire_ODResetTime
#add wave -noupdate -group {OneWire Master} /Core_ADT7310_tb/DUT/OneWire_ODResetWaitForDetectionDuration
#add wave -noupdate -group {OneWire Master} /Core_ADT7310_tb/DUT/OneWire_ODResetPrecenceIntervalDuration
#add wave -noupdate -group {OneWire Master} /Core_ADT7310_tb/DUT/OneWire_ODWRSlotHighDataTime
#add wave -noupdate -group {OneWire Master} /Core_ADT7310_tb/DUT/OneWire_ODRDSlotSampleTime
#add wave -noupdate -group {OneWire Master} /Core_ADT7310_tb/DUT/OneWire_ODSlotLowDataTime
#add wave -noupdate -group {OneWire Master} /Core_ADT7310_tb/DUT/OneWire_ODSlotDuration
#add wave -noupdate -group {OneWire Master} /Core_ADT7310_tb/DUT/OneWire_ODRDSlotInitTime
#add wave -noupdate -group {OneWire Master} /Core_ADT7310_tb/DUT/OneWire_ScanEnable
#add wave -noupdate -group {OneWire Master} /Core_ADT7310_tb/DUT/OneWire_ScanClk
#add wave -noupdate -group {OneWire Master} /Core_ADT7310_tb/DUT/OneWire_ScanDataIn
#add wave -noupdate -group {OneWire Master} /Core_ADT7310_tb/DUT/OneWire_ScanDataOut
#add wave -noupdate -group PWM /Core_ADT7310_tb/DUT/PWM_Polarity
#add wave -noupdate -group PWM /Core_ADT7310_tb/DUT/PWM_Value
#add wave -noupdate -group PWM /Core_ADT7310_tb/DUT/PWM_NewValue
#add wave -noupdate -group PWM /Core_ADT7310_tb/DUT/PWM_ScanEnable
#add wave -noupdate -group PWM /Core_ADT7310_tb/DUT/PWM_ScanClk
#add wave -noupdate -group PWM /Core_ADT7310_tb/DUT/PWM_ScanDataIn
#add wave -noupdate -group PWM /Core_ADT7310_tb/DUT/PWM_ScanDataOut
#add wave -noupdate -group SENT /Core_ADT7310_tb/DUT/SENT_Chipselect
#add wave -noupdate -group SENT /Core_ADT7310_tb/DUT/SENT_NumDatNibble
#add wave -noupdate -group SENT /Core_ADT7310_tb/DUT/SENT_MinSync
#add wave -noupdate -group SENT /Core_ADT7310_tb/DUT/SENT_OutWide
#add wave -noupdate -group SENT /Core_ADT7310_tb/DUT/SENT_NewData
#add wave -noupdate -group SENT /Core_ADT7310_tb/DUT/SENT_CrcOk
#add wave -noupdate -group SENT /Core_ADT7310_tb/DUT/SENT_ScanEnable
#add wave -noupdate -group SENT /Core_ADT7310_tb/DUT/SENT_ScanClk
#add wave -noupdate -group SENT /Core_ADT7310_tb/DUT/SENT_ScanDataIn
#add wave -noupdate -group SENT /Core_ADT7310_tb/DUT/SENT_ScanDataOut
#add wave -noupdate -group SPC /Core_ADT7310_tb/DUT/SPC_Start
#add wave -noupdate -group SPC /Core_ADT7310_tb/DUT/SPC_NumDatNibble
#add wave -noupdate -group SPC /Core_ADT7310_tb/DUT/SPC_LengthTrigger
#add wave -noupdate -group SPC /Core_ADT7310_tb/DUT/SPC_LengthTimeout
#add wave -noupdate -group SPC /Core_ADT7310_tb/DUT/SPC_MinSync
#add wave -noupdate -group SPC /Core_ADT7310_tb/DUT/SPC_OutWide
#add wave -noupdate -group SPC /Core_ADT7310_tb/DUT/SPC_NewData
#add wave -noupdate -group SPC /Core_ADT7310_tb/DUT/SPC_CrcOk
#add wave -noupdate -group SPC /Core_ADT7310_tb/DUT/SPC_SPCReady
#add wave -noupdate -group SPC /Core_ADT7310_tb/DUT/SPC_ScanEnable
#add wave -noupdate -group SPC /Core_ADT7310_tb/DUT/SPC_ScanClk
#add wave -noupdate -group SPC /Core_ADT7310_tb/DUT/SPC_ScanDataIn
#add wave -noupdate -group SPC /Core_ADT7310_tb/DUT/SPC_ScanDataOut
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
