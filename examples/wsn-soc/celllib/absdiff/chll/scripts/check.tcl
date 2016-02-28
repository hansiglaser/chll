#!flowproc
#
# Check synthesized cell netlist
#

# get data from outside this script
set CELL_BASE      "$env(CELL_BASE)"
set CELL_NAME      "$env(CELL_NAME)"
set CELL_NAME_LC   "$env(CELL_NAME_LC)"
set CELL_ILANG_OUT "$env(CELL_ILANG_OUT)"
set RECONF_BASE    "$env(RECONF_BASE)"

# setup whole reconf.module
source $RECONF_BASE/chll/scripts/setup-reconf-module.tcl

# setup cell
set SCRIPT_BASE [file dirname [info script]]
source $SCRIPT_BASE/setup.tcl

# print information to the screen
cell_show

puts "## Reading and checking synthesized netlist $CELL_ILANG_OUT"
cell_read_netlist $CELL_ILANG_OUT

puts "## Done."
