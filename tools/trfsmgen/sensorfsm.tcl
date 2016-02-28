#!trfsmgen
#
# $ ./trfsmgen -b -f sensorfsm.tcl
#
# create FSM definition
set fsm [create_fsm_definition "SensorFSM"]
# add inputs
add_input  $fsm "Enable"
add_input  $fsm "TimerOvfl"
add_input  $fsm "I2CReady"
add_input  $fsm "DiffTooLarge"
# add outputs
add_output $fsm "TimerPreset"
add_output $fsm "TimerEn"
add_output $fsm "I2CStart"
add_output $fsm "StoreNewValue"
add_output $fsm "AddOrSub"
add_output $fsm "CpuIntr"
# add states
add_state  $fsm "Off"
add_state  $fsm "Sleep"
add_state  $fsm "SensorQuery"
# add transitions
add_transition $fsm "Off"         "Off"          "000010" "Enable" 0
add_transition $fsm "Off"         "Sleep"        "100010" "Enable" 1
add_transition $fsm "Sleep"       "Off"          "000010" "Enable" 0
add_transition $fsm "Sleep"       "Sleep"        "010010" "Enable" 1 "TimerOvfl" 0
add_transition $fsm "Sleep"       "SensorQuery"  "111010" "Enable" 1 "TimerOvfl" 1
add_transition $fsm "SensorQuery" "SensorQuery"  "010010" "I2CReady" 0
add_transition $fsm "SensorQuery" "Sleep"        "010010" "I2CReady" 1 "DiffTooLarge" 0
add_transition $fsm "SensorQuery" "Sleep"        "010111" "I2CReady" 1 "DiffTooLarge" 1
# check FSM consistency
check_fsm $fsm
# show FSM
print_fsm $fsm

# create TR-FSM with 10 input and 10 output signals, 5 bit state vector, 8 TRs
# with 0, 1, 2 and 3 inputs, 6 TRs with 4 input and 2 TRs with 5 inputs
set trfsm [create_trfsm 10 10 5 8 8 8 8 6 2]
# assign FSM definition to TR-FSM
set_fsm_definition $trfsm $fsm
# map inputs
map_input  $trfsm "Enable"        9
map_input  $trfsm "TimerOvfl"     8
map_input  $trfsm "I2CReady"      7
map_input  $trfsm "DiffTooLarge"  6
# map outputs
map_output $trfsm "TimerPreset"   9
map_output $trfsm "TimerEn"       8
map_output $trfsm "I2CStart"      7
map_output $trfsm "StoreNewValue" 6
map_output $trfsm "AddOrSub"      5
map_output $trfsm "CpuIntr"       4
# generate the bitstream
set bs [generate_bitstream $trfsm]
print_bitstream $bs
print_bitstream $bs -trfsm
write_bitstream $bs -format vhdl    "SensorFSM" "/tmp/bitstream.vhd"
write_bitstream $bs -format verilog "SensorFSM" "/tmp/bitstream.v"
write_bitstream $bs -format c       "SensorFSM" "/tmp/bitstream.h"
write_bitstream $bs -format text    "SensorFSM" "/tmp/bitstream.txt"

