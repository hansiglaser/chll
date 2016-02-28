#!trfsmgen
#
# Replace Yosys' $fsm cells with TR-FSMs
#

# remove the following two lines
#puts "ERROR: You have to edit this file [info script] before executing."
#exit 1

# get data from outside this script
set CHIP_BASE     "$env(CHIP_BASE)"
set APP_BASE      "$env(APP_BASE)"
set APP_NAME      "$env(APP_NAME)"
set APP_NAME_LC   "$env(APP_NAME_LC)"
set APP_ILANG_OUT "$env(APP_ILANG_OUT)"
set APP_OUT_DIR   "$env(APP_OUT_DIR)"
set APP_OUT_BASE  "$env(APP_OUT_BASE)"
set APP_VLOG_OUT  "$env(APP_VLOG_OUT)"
set MODE          "$env(MODE)"
set SCRIPT_BASE   [file dirname [info script]]

# functions
source $CHIP_BASE/chll/functions.tcl

# stand-alone usage of this script: generate our custom TR-FSMs
set Local true
puts "## Creating TR-FSM instances"
set trfsm [create_trfsm 8 15 5 5 10 10 5 5]
set TRFSMs [list $trfsm]
set TRFSMShortNames [list "FSM"]
# create TR-FSM wrappers
puts "## Creating TR-FSM wrapper modules"
set Wrappers [CreateWrappers $TRFSMs $TRFSMShortNames "${APP_OUT_DIR}/${APP_OUT_BASE}"]

##############################################################################
## Read netlist, find $fsm cells #############################################
##############################################################################

source "$SCRIPT_BASE/insert-trfsm-read.tcl"

##############################################################################
## Map and replace $fsm to TR-FSMs ###########################################
##############################################################################

source "$SCRIPT_BASE/insert-trfsm-replace.tcl"
