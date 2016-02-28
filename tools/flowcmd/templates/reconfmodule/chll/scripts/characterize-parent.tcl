#!yosys
#
# Characterize parent module which instantiates the reconfigurable module
#
# This file is executed through FlowCmd.
#
# Required Environment Variables:
#   UNIT_BASE     base directory of the reconfmodule unit 
#   STUB_FILE     destination file name for the stub nets list of the parent module
#   PARENT_ILANG  destination file name for the netlist in ILang format of the parent module
#
# Required Yosys Plugins:
#   stubnets.so
#   ports.so
#

# remove the following two lines
puts "ERROR: You have to edit this file [info script] before executing."
exit 1

# Configure
set PARENT_MODULE "<parent module>"

# get data from outside this script
set UNIT_BASE "$env(UNIT_BASE)"
set STUB_FILE "$env(STUB_FILE)"
set PARENT_ILANG "$env(PARENT_ILANG)"

# setup variables
set VLOG_DIR $UNIT_BASE/../core/verilog

# setup TCL commands to be equal to the yosys commands (except "proc" -> "procs")
yosys -import

# read HDL
#$# read_verilog -DReconfModuleNone $VLOG_DIR/core.v

# build parametric modules as needed
hierarchy

# Replace processes by MUXes and D-FFs
procs

# Optimize
opt

# Determine unused ports in the top module
stubnets -o "$STUB_FILE" "$PARENT_MODULE"

# write netlist
write_ilang "$PARENT_ILANG"

