#!trfsmgen
#
# Check synthesized application netlists: FSM specific
#

# get data from outside this script
set RECONF_BASE   "$env(RECONF_BASE)"
set APPS_BASE     "$env(APPS_BASE)"

# setup example applications
source $RECONF_BASE/chll/scripts/setup-exapps.tcl

foreach App $ExApps {
  set AppLC       [string tolower "$App"]
  set AppBase     "${APPS_BASE}/$AppLC"
  set ScriptBase  "$AppBase/chll/scripts"
  set OutDir      "$AppBase/chll/out"
  set ILangFile   "$OutDir/${AppLC}-extract.il"

  # set environment variables used by TCL script
  set APP_NAME      "$App"
  set APP_ILANG_OUT "$ILangFile"
  set Local         false

  source "$ScriptBase/insert-trfsm-read.tcl"
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
