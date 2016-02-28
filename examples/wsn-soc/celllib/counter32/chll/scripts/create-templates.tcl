#!flowproc
#
# Create template for this cell
#

# get data from outside this script
set CHIP_BASE    "$env(CHIP_BASE)"
set CELL_BASE    "$env(CELL_BASE)"
set CELL_NAME    "$env(CELL_NAME)"
set CELL_NAME_LC "$env(CELL_NAME_LC)"
set RECONF_BASE  "$env(RECONF_BASE)"

# functions
source $CHIP_BASE/chll/functions.tcl

# setup whole reconf.module
source $RECONF_BASE/chll/scripts/setup-reconf-module.tcl

# setup cell
set SCRIPT_BASE [file dirname [info script]]
source $SCRIPT_BASE/setup.tcl

# print information to the screen
list_cells
cell_show

puts "## Creating HDL templates"
# write HDL templates for the cell
cell_write_template -vhdl    -o $CELL_BASE/vhdl/$CELL_NAME_LC.vhd.tpl
cell_write_template -verilog -o $CELL_BASE/verilog/$CELL_NAME_LC.v.tpl

cell_write_template -vhdl    -testbench -o $CELL_BASE/tb/${CELL_NAME_LC}_tb.vhd.tpl
cell_write_template -verilog -testbench -o $CELL_BASE/tb/${CELL_NAME_LC}_tb.v.tpl

# service: copy templates if the user didn't yet work on them
CopyTemplate $CELL_BASE/vhdl/$CELL_NAME_LC.vhd.tpl
CopyTemplate $CELL_BASE/verilog/$CELL_NAME_LC.v.tpl 
CopyTemplate $CELL_BASE/tb/${CELL_NAME_LC}_tb.vhd.tpl 
CopyTemplate $CELL_BASE/tb/${CELL_NAME_LC}_tb.v.tpl 

puts "## Done."
