#!yosys
#
# Cell extraction for application
#
# This file is executed by "flowcmd extract"
#

# remove the following two lines
puts "ERROR: You have to edit this file [info script] before executing."
exit 1

# get data from outside this script
set CELLLIB_FILE  "$env(CELLLIB_FILE)"
set CELLLIB_MAPS  "$env(CELLLIB_MAPS)"
set APP_BASE      "$env(APP_BASE)"
set APP_NAME      "$env(APP_NAME)"
set APP_NAME_LC   "$env(APP_NAME_LC)"
set INTERACTIVE   "$env(INTERACTIVE)"
set APP_ILANG_IN  "$env(APP_ILANG_IN)"
set APP_ILANG_OUT "$env(APP_ILANG_OUT)"
set APP_VLOG_OUT  "$env(APP_VLOG_OUT)"

# Configure
set TOP_MODULE "$APP_NAME"

# setup TCL commands to be equal to the yosys commands (except "proc" -> "procs")
yosys -import

read_ilang $APP_ILANG_IN

# convert $pmux to $mux
techmap -share_map pmux2mux.v

# extract cells
select "$TOP_MODULE" %s    ;# select top module and all its submodules
extract -constports \
  -map "$CELLLIB_FILE"
opt

# map back topological and reduced variants to use the main variant                    
techmap -map "$CELLLIB_MAPS"

# be a bit more tolerant when simplifying the circuit
opt -mux_undef -mux_bool

fsm_expand
opt_clean
fsm_opt   ;# merge signals with same output values
opt

# flatten SensorFSM and SPIFSM into top module
flatten
hierarchy -top "$TOP_MODULE"
opt

# make sub-ranges and concatenations explicit
read_ilang "$CELLLIB_FILE"
select $TOP_MODULE/s:2:1000             ;# select only multi-bit signals and no single-bit signals
splice -sel_by_wire       ;# concat signals
select -clear

opt "$TOP_MODULE"

# extract once again for Byte2Word
select -module "$TOP_MODULE" ;# select top module and all its submodules
extract \
  -map "$CELLLIB_FILE"
opt -purge

stat

if { $INTERACTIVE == "true" } {
  select -clear
  show -lib "$CELLLIB_FILE" "$TOP_MODULE"
  shell
} else {
  # write netlist
  select -module "$TOP_MODULE"
  write_ilang -selected "$APP_ILANG_OUT"
  write_verilog -norename -noexpr -selected "$APP_VLOG_OUT"
}
