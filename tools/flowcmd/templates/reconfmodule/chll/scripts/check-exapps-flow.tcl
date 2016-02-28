#!trfsmgen
#
# Check synthesized application netlists
#

# get data from outside this script
set RECONF_BASE   "$env(RECONF_BASE)"
set CELLLIB_BASE  "$env(CELLLIB_BASE)"
set CELLLIB_FILE  "$env(CELLLIB_FILE)"
set CELLLIB_MAPS  "$env(CELLLIB_MAPS)"
set APPS_BASE     "$env(APPS_BASE)"

# setup whole reconf.module
source $RECONF_BASE/chll/scripts/setup-reconf-module.tcl
# setup cell library
source $RECONF_BASE/chll/scripts/setup-celllib.tcl
# setup example applications
source $RECONF_BASE/chll/scripts/setup-exapps.tcl

foreach App $ExApps {
  set AppLC       [string tolower "$App"]
  set AppBase     "${APPS_BASE}/$AppLC"
  set Script      "$AppBase/chll/scripts/setup.tcl"
  set OutDir      "$AppBase/chll/out"
  set ILangFile   "$OutDir/${AppLC}-extract.il"

  # set environment variables used by TCL script
  set APP_NAME      "$App"

  # setup application
  source $Script

  # Read and check synthesized netlist
  set IgnoreList [list "\$fsm"]
  app_read_netlist -extracted -ignore $IgnoreList $ILangFile
}

puts ""
list_apps
#app_show -all

# show all cell instances
puts ""
puts "##############################################################################################"
puts "## Resources #################################################################################"
puts "##############################################################################################"
puts ""
app_print_usage -all
puts ""
puts "##############################################################################################"

puts "## Done."
