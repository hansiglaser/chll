#!trfsmgen
#
# Check synthesized application netlists: FSM specific
#

# get data from outside this script
set APP_NAME      "$env(APP_NAME)"
set APP_ILANG_OUT "$env(APP_ILANG_OUT)"

set Local false

set SCRIPT_BASE   [file dirname [info script]]
source "$SCRIPT_BASE/insert-trfsm-read.tcl"

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
