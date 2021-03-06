onerror {resume}
quietly WaveActivateNextPane {} 0
add wave -noupdate /slowadt7410_tb/Reset_n_i
add wave -noupdate /slowadt7410_tb/Clk_i
add wave -noupdate /slowadt7410_tb/Enable_i
add wave -noupdate /slowadt7410_tb/CpuIntr_o
add wave -noupdate -radix unsigned /slowadt7410_tb/SensorValue_o
add wave -noupdate [find nets {/slowadt7410_tb/DUT/*I2CFSM*Start*} ]
add wave -noupdate [find nets {/slowadt7410_tb/DUT/*I2CFSM*Done*} ]
add wave -noupdate [find nets {/slowadt7410_tb/DUT/*I2CFSM*Byte0*} ]
add wave -noupdate [find nets {/slowadt7410_tb/DUT/*I2CFSM*Byte1*} ]
add wave -noupdate /slowadt7410_tb/I2C_ReceiveSend_n_o
add wave -noupdate /slowadt7410_tb/I2C_ReadCount_o
add wave -noupdate /slowadt7410_tb/I2C_StartProcess_o
add wave -noupdate /slowadt7410_tb/I2C_Busy_i
add wave -noupdate /slowadt7410_tb/I2C_FIFOReadNext_o
add wave -noupdate /slowadt7410_tb/I2C_FIFOWrite_o
add wave -noupdate /slowadt7410_tb/I2C_Data_o
add wave -noupdate /slowadt7410_tb/I2C_Data_i
add wave -noupdate /slowadt7410_tb/I2C_ErrBusColl_s
add wave -noupdate /slowadt7410_tb/I2C_ErrCoreBusy_s
add wave -noupdate /slowadt7410_tb/I2C_ErrCoreStopped_s
add wave -noupdate /slowadt7410_tb/I2C_ErrDevNotPresent_s
add wave -noupdate /slowadt7410_tb/I2C_ErrFIFOEmpty_s
add wave -noupdate /slowadt7410_tb/I2C_ErrFIFOFull_s
add wave -noupdate /slowadt7410_tb/I2C_ErrGotNAck_s
add wave -noupdate /slowadt7410_tb/I2C_ErrReadCountZero_s
add wave -noupdate /slowadt7410_tb/I2C_SDA_o
add wave -noupdate /slowadt7410_tb/I2C_SDA_i
add wave -noupdate /slowadt7410_tb/I2C_SDA_s
add wave -noupdate /slowadt7410_tb/I2C_SCL_o
add wave -noupdate /slowadt7410_tb/CT_n_s
add wave -noupdate /slowadt7410_tb/INT_n_s
add wave -noupdate /slowadt7410_tb/Temp_s
add wave -noupdate -divider SensorFSM
add wave -noupdate /slowadt7410_tb/DUT/Enable_i
add wave -noupdate [find nets {/slowadt7410_tb/DUT/*SensorFSM_1*SensorFSM_TimerOvfl*} ]
add wave -noupdate [find nets {/slowadt7410_tb/DUT/*I2CFSM*Done*} ]
add wave -noupdate /slowadt7410_tb/DUT/CpuIntr_o
add wave -noupdate [find nets {/slowadt7410_tb/DUT/*SensorFSM_1*SensorFSM_StoreNewValue*} ]
add wave -noupdate [find nets {/slowadt7410_tb/DUT/*SensorFSM_1*SensorFSM_TimerEnable*} ]
add wave -noupdate [find nets {/slowadt7410_tb/DUT/*I2CFSM*Start*} ]
add wave -noupdate [find nets {/slowadt7410_tb/DUT/*SensorFSM_1*SensorFSM_TimerPreset*} ]
add wave -noupdate [find nets {/slowadt7410_tb/DUT/*I2CFSM*Byte0*} ]
add wave -noupdate [find nets {/slowadt7410_tb/DUT/*I2CFSM*Byte1*} ]
add wave -noupdate -radix decimal /slowadt7410_tb/DUT/Threshold_i
add wave -noupdate -radix decimal /slowadt7410_tb/DUT/PeriodCounterPresetH_i
add wave -noupdate -radix decimal /slowadt7410_tb/DUT/PeriodCounterPresetL_i
add wave -noupdate /slowadt7410_tb/DUT/TRFSM0_1/TRFSM_1/NextState_s
add wave -noupdate /slowadt7410_tb/DUT/TRFSM0_1/TRFSM_1/State_s
add wave -noupdate -radix decimal [find nets {/slowadt7410_tb/DUT/*SensorFSM_1*SensorValue*} ]
add wave -noupdate -radix decimal [find nets {/slowadt7410_tb/DUT/*SensorFSM_1*AbsDiffResult*} ]
add wave -noupdate [find nets {/slowadt7410_tb/DUT/*AddSubCmp*/Carry_o} ]
add wave -noupdate -divider I2C-FSM
add wave -noupdate /slowadt7410_tb/DUT/TRFSM1_1/TRFSM_1/NextState_s
add wave -noupdate /slowadt7410_tb/DUT/TRFSM1_1/TRFSM_1/State_s
add wave -noupdate [find nets {/slowadt7410_tb/DUT/*I2CFSM_1*I2C_FSM_TimerOvfl*} ]
add wave -noupdate [find nets {/slowadt7410_tb/DUT/*I2CFSM_1*I2C_FSM_TimerPreset*} ]
add wave -noupdate [find nets {/slowadt7410_tb/DUT/*I2CFSM_1*I2C_FSM_TimerEnable*} ]
add wave -noupdate [find nets {/slowadt7410_tb/DUT/*I2CFSM_1*I2C_FSM_Wr1*} ]
add wave -noupdate [find nets {/slowadt7410_tb/DUT/*I2CFSM_1*I2C_FSM_Wr0*} ]
add wave -noupdate [find nets {/slowadt7410_tb/DUT/*I2CFSM_1*Timer*D*} ]
add wave -noupdate -divider {ADT7410 Sensor}
add wave -noupdate /slowadt7410_tb/adt7410_1/scl_i
add wave -noupdate /slowadt7410_tb/adt7410_1/sda_io
add wave -noupdate /slowadt7410_tb/adt7410_1/i2c_addr_i
add wave -noupdate /slowadt7410_tb/adt7410_1/int_o
add wave -noupdate /slowadt7410_tb/adt7410_1/ct_o
add wave -noupdate /slowadt7410_tb/adt7410_1/temp_i
add wave -noupdate -divider {I2C Master}
add wave -noupdate /slowadt7410_tb/i2c_master_1/Reset_i
add wave -noupdate /slowadt7410_tb/i2c_master_1/Clk_i
add wave -noupdate /slowadt7410_tb/i2c_master_1/F100_400_n_i
add wave -noupdate /slowadt7410_tb/i2c_master_1/Divider800_i
add wave -noupdate /slowadt7410_tb/i2c_master_1/StartProcess_i
add wave -noupdate /slowadt7410_tb/i2c_master_1/ReceiveSend_n_i
add wave -noupdate /slowadt7410_tb/i2c_master_1/Busy_o
add wave -noupdate /slowadt7410_tb/i2c_master_1/ReadCount_i
add wave -noupdate /slowadt7410_tb/i2c_master_1/FIFOReadNext_i
add wave -noupdate /slowadt7410_tb/i2c_master_1/FIFOWrite_i
add wave -noupdate /slowadt7410_tb/i2c_master_1/FIFOEmpty_o
add wave -noupdate /slowadt7410_tb/i2c_master_1/FIFOFull_o
add wave -noupdate /slowadt7410_tb/i2c_master_1/Data_i
add wave -noupdate /slowadt7410_tb/i2c_master_1/Data_o
add wave -noupdate /slowadt7410_tb/i2c_master_1/ErrAck_i
add wave -noupdate /slowadt7410_tb/i2c_master_1/ErrBusColl_o
add wave -noupdate /slowadt7410_tb/i2c_master_1/ErrFIFOFull_o
add wave -noupdate /slowadt7410_tb/i2c_master_1/ErrGotNAck_o
add wave -noupdate /slowadt7410_tb/i2c_master_1/ErrCoreBusy_o
add wave -noupdate /slowadt7410_tb/i2c_master_1/ErrFIFOEmpty_o
add wave -noupdate /slowadt7410_tb/i2c_master_1/ErrCoreStopped_o
add wave -noupdate /slowadt7410_tb/i2c_master_1/ErrDevNotPresent_o
add wave -noupdate /slowadt7410_tb/i2c_master_1/ErrReadCountZero_o
add wave -noupdate /slowadt7410_tb/i2c_master_1/SDA_i
add wave -noupdate /slowadt7410_tb/i2c_master_1/SDA_o
add wave -noupdate /slowadt7410_tb/i2c_master_1/SCL_o
add wave -noupdate /slowadt7410_tb/i2c_master_1/ScanEnable_i
add wave -noupdate /slowadt7410_tb/i2c_master_1/ScanClk_i
add wave -noupdate /slowadt7410_tb/i2c_master_1/ScanDataIn_i
add wave -noupdate /slowadt7410_tb/i2c_master_1/ScanDataOut_o
add wave -noupdate /slowadt7410_tb/i2c_master_1/TCFIFOWrite_s
add wave -noupdate /slowadt7410_tb/i2c_master_1/TCFIFOReadNext_s
add wave -noupdate /slowadt7410_tb/i2c_master_1/FIFOReadNext_s
add wave -noupdate /slowadt7410_tb/i2c_master_1/FIFOWrite_s
add wave -noupdate /slowadt7410_tb/i2c_master_1/FIFOFull_s
add wave -noupdate /slowadt7410_tb/i2c_master_1/FIFOEmpty_s
add wave -noupdate /slowadt7410_tb/i2c_master_1/CoreDoTransfer_s
add wave -noupdate /slowadt7410_tb/i2c_master_1/CoreReadWrite_n_s
add wave -noupdate /slowadt7410_tb/i2c_master_1/CoreAckTx_s
add wave -noupdate /slowadt7410_tb/i2c_master_1/CoreAckRx_s
add wave -noupdate /slowadt7410_tb/i2c_master_1/CoreBusy_s
add wave -noupdate /slowadt7410_tb/i2c_master_1/CoreByteReady_s
add wave -noupdate /slowadt7410_tb/i2c_master_1/CoreBusErr_s
add wave -noupdate /slowadt7410_tb/i2c_master_1/Data_s
add wave -noupdate /slowadt7410_tb/i2c_master_1/CoreData_o_s
add wave -noupdate /slowadt7410_tb/i2c_master_1/CoreData_i_s
add wave -noupdate /slowadt7410_tb/i2c_master_1/mycore/DividerWidth_g
add wave -noupdate /slowadt7410_tb/i2c_master_1/mycore/Reset_i
add wave -noupdate /slowadt7410_tb/i2c_master_1/mycore/Clk_i
add wave -noupdate /slowadt7410_tb/i2c_master_1/mycore/Data_i
add wave -noupdate /slowadt7410_tb/i2c_master_1/mycore/Data_o
add wave -noupdate /slowadt7410_tb/i2c_master_1/mycore/DoTransfer_i
add wave -noupdate /slowadt7410_tb/i2c_master_1/mycore/ReadWrite_n_i
add wave -noupdate /slowadt7410_tb/i2c_master_1/mycore/AckTx_i
add wave -noupdate /slowadt7410_tb/i2c_master_1/mycore/AckRx_o
add wave -noupdate /slowadt7410_tb/i2c_master_1/mycore/Busy_o
add wave -noupdate /slowadt7410_tb/i2c_master_1/mycore/ByteReady_o
add wave -noupdate /slowadt7410_tb/i2c_master_1/mycore/BusErr_o
add wave -noupdate /slowadt7410_tb/i2c_master_1/mycore/SDA_o
add wave -noupdate /slowadt7410_tb/i2c_master_1/mycore/SDA_i
add wave -noupdate /slowadt7410_tb/i2c_master_1/mycore/SCL_o
add wave -noupdate /slowadt7410_tb/i2c_master_1/mycore/F100_400_n_i
add wave -noupdate /slowadt7410_tb/i2c_master_1/mycore/Divider800_i
add wave -noupdate /slowadt7410_tb/i2c_master_1/mycore/BytePos_s
add wave -noupdate /slowadt7410_tb/i2c_master_1/mycore/NextBytePos_s
add wave -noupdate /slowadt7410_tb/i2c_master_1/mycore/I2CClk_s
add wave -noupdate /slowadt7410_tb/i2c_master_1/mycore/SCL_s
add wave -noupdate /slowadt7410_tb/i2c_master_1/mycore/SDA_i_s
add wave -noupdate /slowadt7410_tb/i2c_master_1/mycore/SDA_o_s
add wave -noupdate /slowadt7410_tb/i2c_master_1/mycore/NextSDA_o_s
add wave -noupdate /slowadt7410_tb/i2c_master_1/mycore/NextSCL_s
add wave -noupdate /slowadt7410_tb/i2c_master_1/mycore/State
add wave -noupdate /slowadt7410_tb/i2c_master_1/mycore/NextState
add wave -noupdate /slowadt7410_tb/i2c_master_1/mycore/Data_s
add wave -noupdate /slowadt7410_tb/i2c_master_1/mycore/NextData_s
add wave -noupdate /slowadt7410_tb/i2c_master_1/mycontroller/ReadCountWidth_g
add wave -noupdate /slowadt7410_tb/i2c_master_1/mycontroller/Reset_i
add wave -noupdate /slowadt7410_tb/i2c_master_1/mycontroller/Clk_i
add wave -noupdate /slowadt7410_tb/i2c_master_1/mycontroller/ReadCount_i
add wave -noupdate /slowadt7410_tb/i2c_master_1/mycontroller/StartProcess_i
add wave -noupdate /slowadt7410_tb/i2c_master_1/mycontroller/ReceiveSend_n_i
add wave -noupdate /slowadt7410_tb/i2c_master_1/mycontroller/Busy_o
add wave -noupdate /slowadt7410_tb/i2c_master_1/mycontroller/FiFoReadNext_o
add wave -noupdate /slowadt7410_tb/i2c_master_1/mycontroller/FiFoWrite_o
add wave -noupdate /slowadt7410_tb/i2c_master_1/mycontroller/FiFoEmpty_i
add wave -noupdate /slowadt7410_tb/i2c_master_1/mycontroller/FiFoFull_i
add wave -noupdate /slowadt7410_tb/i2c_master_1/mycontroller/CoreDoTransfer_o
add wave -noupdate /slowadt7410_tb/i2c_master_1/mycontroller/CoreReadWrite_n_o
add wave -noupdate /slowadt7410_tb/i2c_master_1/mycontroller/CoreAckTx_o
add wave -noupdate /slowadt7410_tb/i2c_master_1/mycontroller/CoreAckRx_i
add wave -noupdate /slowadt7410_tb/i2c_master_1/mycontroller/CoreBusy_i
add wave -noupdate /slowadt7410_tb/i2c_master_1/mycontroller/CoreByteReady_i
add wave -noupdate /slowadt7410_tb/i2c_master_1/mycontroller/CoreBusErr_i
add wave -noupdate /slowadt7410_tb/i2c_master_1/mycontroller/ErrAck_i
add wave -noupdate /slowadt7410_tb/i2c_master_1/mycontroller/ErrBusColl_o
add wave -noupdate /slowadt7410_tb/i2c_master_1/mycontroller/ErrFiFoFull_o
add wave -noupdate /slowadt7410_tb/i2c_master_1/mycontroller/ErrGotNAck_o
add wave -noupdate /slowadt7410_tb/i2c_master_1/mycontroller/ErrCoreBusy_o
add wave -noupdate /slowadt7410_tb/i2c_master_1/mycontroller/ErrFiFoEmpty_o
add wave -noupdate /slowadt7410_tb/i2c_master_1/mycontroller/ErrCoreStopped_o
add wave -noupdate /slowadt7410_tb/i2c_master_1/mycontroller/ErrDevNotPresent_o
add wave -noupdate /slowadt7410_tb/i2c_master_1/mycontroller/ErrReadCountZero_o
add wave -noupdate /slowadt7410_tb/i2c_master_1/mycontroller/ByteCount_s
add wave -noupdate /slowadt7410_tb/i2c_master_1/mycontroller/NextByteCount_s
add wave -noupdate /slowadt7410_tb/i2c_master_1/mycontroller/State
add wave -noupdate /slowadt7410_tb/i2c_master_1/mycontroller/NextState
add wave -noupdate /slowadt7410_tb/i2c_master_1/mycontroller/ErrBusColl_s
add wave -noupdate /slowadt7410_tb/i2c_master_1/mycontroller/NextErrBusColl_s
add wave -noupdate /slowadt7410_tb/i2c_master_1/mycontroller/ErrFiFoFull_s
add wave -noupdate /slowadt7410_tb/i2c_master_1/mycontroller/NextErrFiFoFull_s
add wave -noupdate /slowadt7410_tb/i2c_master_1/mycontroller/ErrGotNAck_s
add wave -noupdate /slowadt7410_tb/i2c_master_1/mycontroller/NextErrGotNAck_s
add wave -noupdate /slowadt7410_tb/i2c_master_1/mycontroller/ErrCoreBusy_s
add wave -noupdate /slowadt7410_tb/i2c_master_1/mycontroller/NextErrCoreBusy_s
add wave -noupdate /slowadt7410_tb/i2c_master_1/mycontroller/ErrFiFoEmpty_s
add wave -noupdate /slowadt7410_tb/i2c_master_1/mycontroller/NextErrFiFoEmpty_s
add wave -noupdate /slowadt7410_tb/i2c_master_1/mycontroller/ErrCoreStopped_s
add wave -noupdate /slowadt7410_tb/i2c_master_1/mycontroller/NextErrCoreStopped_s
add wave -noupdate /slowadt7410_tb/i2c_master_1/mycontroller/ErrDevNotPresent_s
add wave -noupdate /slowadt7410_tb/i2c_master_1/mycontroller/NextErrDevNotPresent_s
add wave -noupdate /slowadt7410_tb/i2c_master_1/mycontroller/ErrReadCountZero_s
add wave -noupdate /slowadt7410_tb/i2c_master_1/mycontroller/NextErrReadCountZero_s
TreeUpdate [SetDefaultTree]
WaveRestoreCursors {{Cursor 1} {662544046154 ps} 0}
configure wave -namecolwidth 321
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
configure wave -timelineunits ms
update
WaveRestoreZoom {0 ps} {1370261550 ns}
