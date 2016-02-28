#!flowproc
#
# Check synthesized application netlist
#

# get data from outside this script
set APP_BASE      "$env(APP_BASE)"
set APP_NAME      "$env(APP_NAME)"
set APP_NAME_LC   "$env(APP_NAME_LC)"
set APP_ILANG_OUT "$env(APP_ILANG_OUT)"
set RECONF_BASE   "$env(RECONF_BASE)"
if { [info exists env(EXTRACTED) ] } {
  set CELLLIB_BASE  "$env(CELLLIB_BASE)"
  set CELLLIB_FILE  "$env(CELLLIB_FILE)"
  set CELLLIB_MAPS  "$env(CELLLIB_MAPS)"
  set EXTRACTED     "$env(EXTRACTED)"
} else {
  set EXTRACTED false
}

# Add Yosys cells to ignore if you are not yet done building the full cell
# library. The "$fsm" cell will be added below if this script is run in
# "extract" mode.
set IgnoreList [list ]

# setup whole reconf.module
source $RECONF_BASE/chll/scripts/setup-reconf-module.tcl

if { $EXTRACTED == "true" } {
  # setup cell library
  source $RECONF_BASE/chll/scripts/setup-celllib.tcl
}

# setup application
set SCRIPT_BASE [file dirname [info script]]
source $SCRIPT_BASE/setup.tcl

# print information to the screen
app_show

puts "## Reading and checking synthesized netlist $APP_ILANG_OUT"
if { $EXTRACTED != "true" } {
  app_read_netlist $APP_ILANG_OUT
} else {
  puts "## Running in 'extract' mode"
  lappend IgnoreList "\$fsm"
  app_read_netlist -extracted -ignore $IgnoreList $APP_ILANG_OUT
  puts ""
  app_print_usage
  puts ""
}

puts "## Done."
