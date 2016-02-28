#!trfsmgen
#
# $ ./trfsmgen -b -f test-s27.tcl
#
set s27 [read_kiss s27.kiss2]
print_fsm $s27
set trfsm [create_trfsm 4 1 4 1 1 17 15 5]
set_fsm_definition $trfsm $s27
map_input $trfsm Input0 0
map_input $trfsm Input1 1
map_input $trfsm Input2 2
map_input $trfsm Input3 3
map_output $trfsm Output0 0
set bs [generate_bitstream $trfsm]
print_bitstream $bs -trfsm
