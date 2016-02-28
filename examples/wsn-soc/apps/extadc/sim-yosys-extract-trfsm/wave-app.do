onerror {resume}
quietly WaveActivateNextPane {} 0
add wave -noupdate /extadc_tb/DUT/Reset_n_i
add wave -noupdate /extadc_tb/DUT/Clk_i
add wave -noupdate /extadc_tb/DUT/FSM_1/TRFSM_1/NextState_s
add wave -noupdate /extadc_tb/DUT/FSM_1/TRFSM_1/State_s
add wave -noupdate /extadc_tb/DUT/Enable_i
add wave -noupdate /extadc_tb/DUT/SensorPower_o
add wave -noupdate /extadc_tb/DUT/SensorStart_o
add wave -noupdate /extadc_tb/DUT/SensorReady_i
add wave -noupdate /extadc_tb/DUT/AdcStart_o
add wave -noupdate /extadc_tb/DUT/AdcDone_i
add wave -noupdate -radix unsigned /extadc_tb/DUT/AdcValue_i
add wave -noupdate -radix unsigned /extadc_tb/DUT/PeriodCounterPreset_i
add wave -noupdate -radix unsigned [find nets {/extadc_tb/DUT/*extract*Timer* /D_o} ]
add wave -noupdate /extadc_tb/DUT/TimerEnable
add wave -noupdate /extadc_tb/DUT/TimerOvfl
add wave -noupdate /extadc_tb/DUT/TimerPreset
add wave -noupdate -radix unsigned /extadc_tb/DUT/Threshold_i
add wave -noupdate -radix decimal [find nets {/extadc_tb/DUT/*extract*AbsDiff* /DiffAB} ]
add wave -noupdate -radix decimal [find nets {/extadc_tb/DUT/*extract*AbsDiff* /DiffBA} ]
add wave -noupdate [find nets {/extadc_tb/DUT/*AddSubCmp* /Carry_o} ]
add wave -noupdate [find nets {/extadc_tb/DUT/*AddSubCmp* /Zero_o} ]
add wave -noupdate /extadc_tb/DUT/StoreNewValue
add wave -noupdate -radix unsigned [find nets {/extadc_tb/DUT/*extract*WordRegister* /Q_o} ]
add wave -noupdate -radix unsigned /extadc_tb/DUT/SensorValue_o
add wave -noupdate /extadc_tb/DUT/CpuIntr_o
TreeUpdate [SetDefaultTree]
WaveRestoreCursors {{Cursor 1} {1744416 ps} 0}
configure wave -namecolwidth 244
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
WaveRestoreZoom {0 ps} {1809150 ps}
