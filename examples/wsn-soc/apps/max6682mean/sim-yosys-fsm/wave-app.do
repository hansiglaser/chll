onerror {resume}
quietly WaveActivateNextPane {} 0
add wave -noupdate /max6682mean_tb/Reset_n_i
add wave -noupdate /max6682mean_tb/Clk_i
add wave -noupdate /max6682mean_tb/Enable_i
add wave -noupdate /max6682mean_tb/CpuIntr_o
add wave -noupdate -radix unsigned /max6682mean_tb/DUT/SensorValue_o
add wave -noupdate /max6682mean_tb/MAX6682CS_n_o
add wave -noupdate /max6682mean_tb/SPI_CPOL_o
add wave -noupdate /max6682mean_tb/SPI_CPHA_o
add wave -noupdate /max6682mean_tb/SPI_LSBFE_o
add wave -noupdate /max6682mean_tb/SPI_SPPR_SPR_o
add wave -noupdate /max6682mean_tb/SPI_Data_o
add wave -noupdate /max6682mean_tb/SPI_Data_i
add wave -noupdate /max6682mean_tb/SPI_FIFOEmpty_i
add wave -noupdate /max6682mean_tb/SPI_FIFOFull_i
add wave -noupdate /max6682mean_tb/SPI_SCK_s
add wave -noupdate /max6682mean_tb/SPI_MISO_s
add wave -noupdate /max6682mean_tb/SPI_MOSI_s
add wave -noupdate /max6682mean_tb/SPI_ReadNext_o
add wave -noupdate /max6682mean_tb/SPI_Transmission_i
add wave -noupdate /max6682mean_tb/SPI_Write_o
add wave -noupdate -radix unsigned /max6682mean_tb/MAX6682Value
add wave -noupdate -divider {SPI FSM}
add wave -noupdate -radix unsigned /max6682mean_tb/DUT/PeriodCounterPresetH_i
add wave -noupdate -radix unsigned /max6682mean_tb/DUT/PeriodCounterPresetL_i
add wave -noupdate /max6682mean_tb/DUT/SPI_FSM_Start
add wave -noupdate [find nets {/max6682mean_tb/DUT/SPI_FSM_1/*fsm*SPI_FSM_State*/next_state} ]
add wave -noupdate [find nets {/max6682mean_tb/DUT/SPI_FSM_1/*fsm*SPI_FSM_State*/state} ]
add wave -noupdate /max6682mean_tb/DUT/SPI_FSM_1/SPI_FSM_Wr0
add wave -noupdate /max6682mean_tb/DUT/SPI_FSM_1/SPI_FSM_Wr1
add wave -noupdate /max6682mean_tb/DUT/SPI_FSM_Done
add wave -noupdate -divider SensorFSM
add wave -noupdate [find nets {/max6682mean_tb/DUT/*fsm*SensorFSM_State* /next_state} ]
add wave -noupdate [find nets {/max6682mean_tb/DUT/*fsm*SensorFSM_State* /state} ]
add wave -noupdate /max6682mean_tb/DUT/SensorFSM_StoreNewValue
add wave -noupdate -radix unsigned /max6682mean_tb/DUT/SensorFSM_Timer
add wave -noupdate /max6682mean_tb/DUT/SensorFSM_TimerEnable
add wave -noupdate /max6682mean_tb/DUT/SensorFSM_TimerOvfl
add wave -noupdate /max6682mean_tb/DUT/SensorFSM_TimerPreset
add wave -noupdate -radix hexadecimal /max6682mean_tb/DUT/Byte0
add wave -noupdate -radix hexadecimal /max6682mean_tb/DUT/Byte1
add wave -noupdate -radix unsigned /max6682mean_tb/DUT/SensorValue
add wave -noupdate /max6682mean_tb/DUT/SensorFSM_AddValue
add wave -noupdate /max6682mean_tb/DUT/SensorFSM_StoreValue
add wave -noupdate -radix unsigned /max6682mean_tb/DUT/Accumulator
add wave -noupdate /max6682mean_tb/DUT/SensorFSM_StoreNewValue
add wave -noupdate -radix unsigned /max6682mean_tb/DUT/LastValue
add wave -noupdate -radix decimal /max6682mean_tb/DUT/DiffAB
add wave -noupdate -radix decimal /max6682mean_tb/DUT/DiffBA
add wave -noupdate -radix unsigned /max6682mean_tb/DUT/AbsDiffResult
add wave -noupdate -radix unsigned /max6682mean_tb/DUT/Threshold_i
add wave -noupdate /max6682mean_tb/DUT/SensorFSM_DiffTooLarge
add wave -noupdate -divider {SPI Master}
add wave -noupdate /max6682mean_tb/spi_master_1/EnFrqDivider
add wave -noupdate /max6682mean_tb/spi_master_1/EnSample
add wave -noupdate /max6682mean_tb/spi_master_1/EnShift
add wave -noupdate /max6682mean_tb/spi_master_1/LoadShifter
add wave -noupdate /max6682mean_tb/spi_master_1/ReadFIFOData
add wave -noupdate /max6682mean_tb/spi_master_1/ReadFIFOFull
add wave -noupdate /max6682mean_tb/spi_master_1/ReadFIFOLoad
TreeUpdate [SetDefaultTree]
WaveRestoreCursors {{Cursor 1} {2510 ns} 0} {{Cursor 2} {1487 ns} 0}
configure wave -namecolwidth 358
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
configure wave -timelineunits ns
update
WaveRestoreZoom {0 ns} {2754 ns}
