onerror {resume}
quietly WaveActivateNextPane {} 0
add wave -noupdate /tmp421_tb/Reset_n_i
add wave -noupdate /tmp421_tb/Clk_i
add wave -noupdate /tmp421_tb/Enable_i
add wave -noupdate /tmp421_tb/CpuIntr_o
add wave -noupdate /tmp421_tb/DUT/SensorFSM_1/TRFSM_1/State_s
add wave -noupdate /tmp421_tb/DUT/SensorFSM_1/TRFSM_1/NextState_s
add wave -noupdate /tmp421_tb/DUT/SensorFSM_1/SensorFSM_TimerOvfl
add wave -noupdate /tmp421_tb/DUT/SensorFSM_1/SensorFSM_TimerPreset
add wave -noupdate /tmp421_tb/DUT/SensorFSM_1/SensorFSM_TimerEnable
add wave -noupdate /tmp421_tb/DUT/SensorFSM_1/SensorFSM_StoreLocal
add wave -noupdate /tmp421_tb/DUT/SensorFSM_1/SensorFSM_StoreRemote
add wave -noupdate /tmp421_tb/DUT/SensorFSM_1/SensorValue
add wave -noupdate /tmp421_tb/DUT/SensorFSM_1/WordL
add wave -noupdate /tmp421_tb/DUT/SensorFSM_1/WordR
add wave -noupdate /tmp421_tb/DUT/SensorFSM_1/SensorFSM_Timer
add wave -noupdate /tmp421_tb/DUT/I2CFSM_QueryLocal_s
add wave -noupdate /tmp421_tb/DUT/I2CFSM_QueryRemote_s
add wave -noupdate /tmp421_tb/DUT/I2CFSM_Done_s
add wave -noupdate /tmp421_tb/DUT/I2CFSM_Error_s
add wave -noupdate /tmp421_tb/DUT/I2CFSM_Byte0_s
add wave -noupdate /tmp421_tb/DUT/I2CFSM_Byte1_s
add wave -noupdate /tmp421_tb/DUT/I2CFSM_1/I2C_FSM_Wr1
add wave -noupdate /tmp421_tb/DUT/I2CFSM_1/I2C_FSM_Wr0
add wave -noupdate /tmp421_tb/I2C_ReceiveSend_n_o
add wave -noupdate /tmp421_tb/I2C_ReadCount_o
add wave -noupdate /tmp421_tb/I2C_StartProcess_o
add wave -noupdate /tmp421_tb/I2C_Busy_i
add wave -noupdate /tmp421_tb/I2C_FIFOReadNext_o
add wave -noupdate /tmp421_tb/I2C_FIFOWrite_o
add wave -noupdate /tmp421_tb/I2C_Data_o
add wave -noupdate /tmp421_tb/I2C_Data_i
add wave -noupdate /tmp421_tb/I2C_Error_i
add wave -noupdate /tmp421_tb/I2C_F100_400_n_o
add wave -noupdate /tmp421_tb/I2C_Divider800_o
add wave -noupdate /tmp421_tb/DUT/I2CFSM_1/TRFSM_1/State_s
add wave -noupdate /tmp421_tb/DUT/I2CFSM_1/TRFSM_1/NextState_s
add wave -noupdate -radix unsigned /tmp421_tb/PeriodCounterPresetH_i
add wave -noupdate -radix unsigned /tmp421_tb/PeriodCounterPresetL_i
add wave -noupdate /tmp421_tb/LocalTemp_s
add wave -noupdate /tmp421_tb/RemoteTemp_s
add wave -noupdate /tmp421_tb/LocalTempBin_s
add wave -noupdate /tmp421_tb/RemoteTempBin_s
add wave -noupdate /tmp421_tb/SensorValueL_o
add wave -noupdate /tmp421_tb/SensorValueR_o
add wave -noupdate /tmp421_tb/SensorValueL_real
add wave -noupdate /tmp421_tb/SensorValueR_real
add wave -noupdate /tmp421_tb/I2CFSM_Done_i
add wave -noupdate /tmp421_tb/I2CFSM_Done_d
add wave -noupdate /tmp421_tb/I2CFSM_Done_a
add wave -noupdate /tmp421_tb/I2C_SDA_i
add wave -noupdate /tmp421_tb/I2C_SDA_o
add wave -noupdate /tmp421_tb/I2C_SDA_s
add wave -noupdate /tmp421_tb/I2C_SCL_o
add wave -noupdate /tmp421_tb/I2C_FIFOEmpty_s
add wave -noupdate /tmp421_tb/I2C_FIFOFull_s
add wave -noupdate /tmp421_tb/I2C_ErrBusColl_s
add wave -noupdate /tmp421_tb/I2C_ErrCoreBusy_s
add wave -noupdate /tmp421_tb/I2C_ErrCoreStopped_s
add wave -noupdate /tmp421_tb/I2C_ErrDevNotPresent_s
add wave -noupdate /tmp421_tb/I2C_ErrFIFOEmpty_s
add wave -noupdate /tmp421_tb/I2C_ErrFIFOFull_s
add wave -noupdate /tmp421_tb/I2C_ErrGotNAck_s
add wave -noupdate /tmp421_tb/I2C_ErrReadCountZero_s
TreeUpdate [SetDefaultTree]
WaveRestoreCursors {{Cursor 1} {949789845 ps} 0}
quietly wave cursor active 1
configure wave -namecolwidth 326
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
WaveRestoreZoom {0 ps} {3439390500 ps}
