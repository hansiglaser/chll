#!modelsim
#
# Apply TR-FSM bitstreams
#
# Auto-generated by ../scripts/insert-trfsm-replace.tcl
#

# I2CFSM
set TRFSMBase "/adt7410_tb/DUT/I2CFSM_1/TRFSM_1"
do adt7410-extract-i2cfsm-bitstream-modelsim.do
# SensorFSM
set TRFSMBase "/adt7410_tb/DUT/SensorFSM_1/TRFSM_1"
do adt7410-extract-sensorfsm-bitstream-modelsim.do
