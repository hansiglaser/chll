#!yosys
#
# $ yosys -s test-write_fsm-yosys.tcl
#
read_ilang s27.il
#fsm_info
fsm_export -o s27-yosys.kiss -origenc
fsm_map
opt
#show
write_verilog s27.v
