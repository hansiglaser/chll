onerror {resume}
quietly WaveActivateNextPane {} 0
add wave -noupdate /max6682_tb/Reset_n_i
add wave -noupdate /max6682_tb/Clk_i
add wave -noupdate /max6682_tb/Enable_i
add wave -noupdate /max6682_tb/CpuIntr_o
add wave -noupdate -radix unsigned /max6682_tb/DUT/SensorValue_o
add wave -noupdate /max6682_tb/MAX6682CS_n_o
add wave -noupdate /max6682_tb/SPI_CPOL_o
add wave -noupdate /max6682_tb/SPI_CPHA_o
add wave -noupdate /max6682_tb/SPI_LSBFE_o
add wave -noupdate /max6682_tb/SPI_SPPR_SPR
add wave -noupdate /max6682_tb/SPI_Data_o
add wave -noupdate /max6682_tb/SPI_Data_i
add wave -noupdate /max6682_tb/SPI_FIFOEmpty_i
add wave -noupdate /max6682_tb/SPI_FIFOFull_i
add wave -noupdate /max6682_tb/SPI_SCK_s
add wave -noupdate /max6682_tb/SPI_MISO_s
add wave -noupdate /max6682_tb/SPI_MOSI_s
add wave -noupdate /max6682_tb/SPI_ReadNext_o
add wave -noupdate /max6682_tb/SPI_Transmission_i
add wave -noupdate /max6682_tb/SPI_Write_o
add wave -noupdate -radix unsigned /max6682_tb/MAX6682Value
add wave -noupdate -divider {SPI FSM}
add wave -noupdate -radix unsigned /max6682_tb/DUT/PeriodCounterPresetH_i
add wave -noupdate -radix unsigned /max6682_tb/DUT/PeriodCounterPresetL_i
add wave -noupdate {/max6682_tb/DUT/\MAX6682_SPI_FSM_1.SPI_FSM_Done }
add wave -noupdate /max6682_tb/DUT/TRFSM0_1/TRFSM_1/NextState_s
add wave -noupdate /max6682_tb/DUT/TRFSM0_1/TRFSM_1/State_s
add wave -noupdate {/max6682_tb/DUT/\MAX6682_SPI_FSM_1.SPI_FSM_Start }
add wave -noupdate {/max6682_tb/DUT/\MAX6682_SPI_FSM_1.SPI_FSM_Wr0 }
add wave -noupdate {/max6682_tb/DUT/\MAX6682_SPI_FSM_1.SPI_FSM_Wr1 }
add wave -noupdate -divider SensorFSM
add wave -noupdate -radix unsigned /max6682_tb/DUT/Threshold_i
add wave -noupdate [find nets {/max6682_tb/DUT/*AddSubCmp* /Carry_o} ]
add wave -noupdate [find nets {/max6682_tb/DUT/*AddSubCmp* /Zero_o} ]
add wave -noupdate /max6682_tb/DUT/TRFSM1_1/TRFSM_1/NextState_s
add wave -noupdate /max6682_tb/DUT/TRFSM1_1/TRFSM_1/State_s
add wave -noupdate /max6682_tb/DUT/SensorFSM_StoreNewValue
add wave -noupdate -radix unsigned [find nets {/max6682_tb/DUT/*Timer*/Value} ]
add wave -noupdate /max6682_tb/DUT/SensorFSM_TimerEnable
add wave -noupdate /max6682_tb/DUT/SensorFSM_TimerOvfl
add wave -noupdate /max6682_tb/DUT/SensorFSM_TimerPreset
add wave -noupdate -radix unsigned /max6682_tb/DUT/SensorValue
add wave -noupdate /max6682_tb/DUT/Byte0
add wave -noupdate /max6682_tb/DUT/Byte1
add wave -noupdate -radix unsigned /max6682_tb/DUT/SensorValue_o
add wave -noupdate -divider {SPI Master}
add wave -noupdate /max6682_tb/spi_master_1/EnFrqDivider
add wave -noupdate /max6682_tb/spi_master_1/EnSample
add wave -noupdate /max6682_tb/spi_master_1/EnShift
add wave -noupdate /max6682_tb/spi_master_1/LoadShifter
add wave -noupdate /max6682_tb/spi_master_1/ReadFIFOData
add wave -noupdate /max6682_tb/spi_master_1/ReadFIFOFull
add wave -noupdate /max6682_tb/spi_master_1/ReadFIFOLoad
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
