#!flowproc
#
# Use list of unused signals in parent module to setup the list of signals
# used by the reconfigurable module.
#
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

# read ports of the parent module
read_parent "$PARENT_ILANG" "$PARENT_MODULE"

# read file provided by special yosys pass to extract unused signals
read_unused_signals "$STUB_FILE"

# amend list of signals
### del_reconf_signals -name "ENQ$"
### add_reconf_signal "Clk_i"
### add_reconf_signal "Reset_n_i"
### add_reconf_signal -cell "I2C_Master" -output "SDA_o"   ;# fork output of submodule as our input
