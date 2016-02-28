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

read_verilog $VLOG_DIR/spifsm.v
read_verilog $VLOG_DIR/max6682mean.v

# build parametric modules as needed
hierarchy

# Replace processes by MUXes and D-FFs
procs

# Optimize
opt -mux_undef -mux_bool

if { $EXTRACT_FSM == "true" } {
  # Extract FSMs
  fsm -norecode -nomap ;#-expand

  # Optimize
  opt
  # write KISS files
  select {MAX6682/n:*$fsm*SPI_FSM_State*}
  fsm_export -o /tmp/spifsm.kiss -origenc
  select -clear
  select {MAX6682/n:*$fsm*SensorFSM_State*}
  fsm_export -o /tmp/sensorfsm.kiss -origenc
  select -clear
}

if { $INTERACTIVE == "true" } {
  show $APP_NAME
  shell
} else {
  # write netlist
  write_ilang "$APP_ILANG_OUT"
  write_verilog -norename -noexpr -selected "$APP_VLOG_OUT"
  # write flattened version for "flowcmd insert-trfsm"
  select -module "$TOP_MODULE"
  flatten
  write_ilang -selected "${APP_ILANG_BASE}-flat.il"
}
