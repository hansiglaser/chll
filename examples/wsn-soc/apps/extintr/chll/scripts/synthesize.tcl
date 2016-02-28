#!yosys
#
# Synthesize example application
#
# This file is executed by "flowcmd check-app"
#

# remove the following two lines
#puts "ERROR: You have to edit this file [info script] before executing."
#exit 1

# get data from outside this script
set APP_BASE    "$env(APP_BASE)"
set APP_NAME    "$env(APP_NAME)"
set APP_NAME_LC "$env(APP_NAME_LC)"
set INTERACTIVE "$env(INTERACTIVE)"
set EXTRACT_FSM "$env(EXTRACT_FSM)"
set APP_ILANG_OUT "$env(APP_ILANG_OUT)"
set APP_VLOG_OUT  "$env(APP_VLOG_OUT)"
set APP_ILANG_BASE [file rootname "$APP_ILANG_OUT"]   ;# strip off extension

# Configure
set TOP_MODULE "$APP_NAME"

# setup variables
set VLOG_DIR $APP_BASE/verilog

# setup TCL commands to be equal to the yosys commands (except "proc" -> "procs")
yosys -import

read_verilog $VLOG_DIR/$APP_NAME_LC.v

# build parametric modules as needed
hierarchy

# Replace processes by MUXes and D-FFs
procs

# Don't flatten here, because otherwise the fsm_expand in extract.tcl would
# generate one huge FSM plus one small FSM. We use the "natural" border of
# these sub-modules to limit fsm_expand when integrating combinational logic.

# Optimize
opt -mux_undef -mux_bool

if { $EXTRACT_FSM == "true" } {
  # Extract FSMs
  fsm -norecode -nomap ;#-expand

  # Optimize
  opt
  # write KISS files
  #$# select {<myapp>/n:*$fsm*<...>_State*}
  #$# fsm_export -o /tmp/<...>.kiss -origenc
  select -clear
}

if { $INTERACTIVE == "true" } {
  show $APP_NAME
  shell
} else {
  # write netlist
  select "$TOP_MODULE" %s   ;# select top module and all its submodules
  write_ilang -selected "$APP_ILANG_OUT"
  write_verilog -norename -noexpr -selected "$APP_VLOG_OUT"
  # write flattened version for "flowcmd insert-trfsm"
  select -module "$TOP_MODULE"
  flatten
  write_ilang -selected "${APP_ILANG_BASE}-flat.il"
}
