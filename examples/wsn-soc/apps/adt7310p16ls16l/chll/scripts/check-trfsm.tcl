#!trfsmgen
#
# Check synthesized application netlist: FSM specific
#

# get data from outside this script
set APP_BASE      "$env(APP_BASE)"
set APP_NAME      "$env(APP_NAME)"
set APP_NAME_LC   "$env(APP_NAME_LC)"
set APP_ILANG_OUT "$env(APP_ILANG_OUT)"
if { [info exists env(EXTRACTED) ] } {
  set EXTRACTED     "$env(EXTRACTED)"
} else {
  set EXTRACTED false
}

set Local false
# read netlist and extract FSMs
source "$APP_BASE/chll/scripts/insert-trfsm-read.tcl"

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
