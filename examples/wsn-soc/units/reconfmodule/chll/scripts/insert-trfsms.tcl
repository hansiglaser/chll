#!trfsmgen
#
# Replace Yosys $fsm cells with TR-FSMs in all example applications
#

# get data from outside this script
set RECONF_BASE   "$env(RECONF_BASE)"
set APPS_BASE     "$env(APPS_BASE)"
set UNIT_OUT_BASE "$env(UNIT_OUT_BASE)"

# setup TR-FSM instances
source "$RECONF_BASE/chll/scripts/setup-trfsms.tcl"
# write TR-FSM information scripts and wrappers to files
source "$RECONF_BASE/chll/scripts/write-trfsms.tcl"

puts ""
print_trfsm $TRFSMs
puts ""

# setup example applications
source "$RECONF_BASE/chll/scripts/setup-exapps.tcl"

foreach App $ExApps {
  set AppLC       [string tolower "$App"]
  set AppBase     "${APPS_BASE}/$AppLC"
  set ScriptBase  "$AppBase/chll/scripts/"
  set OutDir      "$AppBase/chll/out"
  set ILangFile   "$OutDir/${AppLC}-extract.il"
  set OutBase     "${AppLC}-extract-intersynth-"
  set VLogOutFile "$OutDir/${AppLC}-extract-intersynth.v"

  # set environment variables used by TCL script
  set APP_NAME      "$App"
  set APP_NAME_LC   "$AppLC"
  set APP_ILANG_OUT "$ILangFile"
  set APP_OUT_DIR   "$OutDir"
  set APP_OUT_BASE  "$OutBase"
  set APP_VLOG_OUT  "$VLogOutFile"
  set Local         false

  puts ""
  puts "##############################################################################################"
  puts "## Inserting Reconf.Module TR-FSMs into Ex.App. $App"

  # Read netlist, find $fsm cells
  source "$ScriptBase/insert-trfsm-read.tcl"

  puts ""
  print_fsms $AllFSMs($App)
  puts ""

  # Map and replace $fsm to TR-FSMs
  source "$ScriptBase/insert-trfsm-replace.tcl"

  # setup new TR-FSM instances and wrappers for next example application
  source "$RECONF_BASE/chll/scripts/setup-trfsms.tcl"
}

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
