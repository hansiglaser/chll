#!trfsmgen
#
# Replace Yosys $fsm cells with TR-FSMs in all example applications
#

# get data from outside this script
set APP_NAME      "$env(APP_NAME)"
set APP_NAME_LC   "$env(APP_NAME_LC)"
set APP_ILANG_OUT "$env(APP_ILANG_OUT)"
set APP_OUT_DIR   "$env(APP_OUT_DIR)"
set APP_OUT_BASE  "$env(APP_OUT_BASE)"
set APP_VLOG_OUT  "$env(APP_VLOG_OUT)"

#set MODE          "$env(MODE)"
#set SCRIPT_BASE   [file dirname [info script]]

set RECONF_BASE   "$env(RECONF_BASE)"
#set CELLLIB_BASE  "$env(CELLLIB_BASE)"

# setup TR-FSM instances
source "$RECONF_BASE/chll/scripts/setup-trfsms.tcl"

puts ""
print_trfsm $TRFSMs
puts ""

# set environment variables used by TCL scripts
set Local         false

puts "##############################################################################################"
puts "## Inserting Reconf.Module TR-FSMs"

array unset AllFSMs

# Read netlist, find $fsm cells
set SCRIPT_BASE [file dirname [info script]]
source "$SCRIPT_BASE/insert-trfsm-read.tcl"

puts ""
print_fsms AllFSMs
puts ""

exit

# Map and replace $fsm to TR-FSMs
source "$SCRIPT_BASE/insert-trfsm-replace.tcl"

# show all FSM instances
puts ""
puts "##############################################################################################"
puts "## FSMs ######################################################################################"
puts "##############################################################################################"
puts ""
print_fsms AllFSMs
puts ""
puts "##############################################################################################"

puts "## Done."
