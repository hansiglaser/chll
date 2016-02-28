#!flowproc
#
# Create template for this application
#

# get data from outside this script
set CHIP_BASE   "$env(CHIP_BASE)"
set APP_BASE    "$env(APP_BASE)"
set APP_NAME    "$env(APP_NAME)"
set APP_NAME_LC "$env(APP_NAME_LC)"
set RECONF_BASE "$env(RECONF_BASE)"

# functions
source $CHIP_BASE/chll/functions.tcl

# setup whole reconf.module
source $RECONF_BASE/chll/scripts/setup-reconf-module.tcl

# setup application
set SCRIPT_BASE [file dirname [info script]]
source $SCRIPT_BASE/setup.tcl

# print information to the screen
list_apps
app_show

puts "## Creating HDL templates"
# write HDL templates for the application
app_write_template -vhdl    -o $APP_BASE/vhdl/$APP_NAME_LC.vhd.tpl
app_write_template -verilog -o $APP_BASE/verilog/$APP_NAME_LC.v.tpl

app_write_template -vhdl    -testbench -o $APP_BASE/tb/${APP_NAME_LC}_tb.vhd.tpl
app_write_template -verilog -testbench -o $APP_BASE/tb/${APP_NAME_LC}_tb.v.tpl

# service: copy templates if the user didn't yet work on them
CopyTemplate $APP_BASE/vhdl/$APP_NAME_LC.vhd.tpl
CopyTemplate $APP_BASE/verilog/$APP_NAME_LC.v.tpl 
CopyTemplate $APP_BASE/tb/${APP_NAME_LC}_tb.vhd.tpl 
CopyTemplate $APP_BASE/tb/${APP_NAME_LC}_tb.v.tpl 

puts "## Create netlist wrapping application $APP_NAME as MyReconfLogic"
create_reconf_module_netlist -wrapapp -appinstname "${APP_NAME}_0"

puts "## Writing netlists to files"
write_netlist -wrapapp -verilog -module       "$APP_BASE/chll/out/reconflogic-wrap$APP_NAME_LC.v"
write_netlist -wrapapp -vhdl    -architecture "$APP_BASE/chll/out/reconflogic-wrap$APP_NAME_LC-a.vhd"
write_netlist -wrapapp -vhdl    -module       "$APP_BASE/chll/out/reconflogic-wrap$APP_NAME_LC.vhd"
write_netlist -wrapapp -vhdl    -instance     "$APP_BASE/chll/out/reconflogic-wrap$APP_NAME_LC-instance.vhd"
puts "## Writing firmware files"
app_write_firmware -wrapapp -driver_header "$APP_BASE/chll/out/$APP_NAME_LC-wrapapp-driver.h"
app_write_firmware -wrapapp -driver_source "$APP_BASE/chll/out/$APP_NAME_LC-wrapapp-driver.c"

puts "## Done."
