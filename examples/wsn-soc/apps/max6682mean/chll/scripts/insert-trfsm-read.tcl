#!trfsmgen
#
# Read application netlist and find $fsm cells
#
# This script is sourced by insert-trfsm.tcl, check-exapps.tcl and some others.
#
# Required variables: $APP_NAME, $APP_ILANG_OUT, $Local
#
# Resulting variables: $module, $FSMInstances, $FSMs, $AllFSMs($APP_NAME)
#

# remove the following two lines
#puts "ERROR: You have to edit this file [info script] before executing."
#exit 1

##############################################################################
## Read netlist, find $fsm cells #############################################
##############################################################################

# read app module
puts "## Importing module $APP_NAME from $APP_ILANG_OUT"
set module [read_ilang "$APP_ILANG_OUT" "$APP_NAME"]

# Find $fsm instances in the module
if $Local {
  # use dedicated order
  set FSMInstances [list]
  lappend FSMInstances [find_cells -one $module -instance ".*SPI_FSM_State.*"]
  lappend FSMInstances [find_cells -one $module -instance ".*SensorFSM_State.*"]
} else {
  # find all $fsm cells
  set FSMInstances [find_cells $module -cell "\\\$fsm"]         ;# get cell objects
}

set FSMs [list]
foreach FSMInstance $FSMInstances {
  set CellName [get_cell_name $FSMInstance]
  puts "## Extracting FSM $CellName"
  set FSM [get_fsm $FSMInstance]
  lappend FSMs $FSM
  # print some information
  if $Local {
    set Name           [get_name             $FSM]
    set NumInputs      [get_input_count      $FSM]
    set NumOutputs     [get_output_count     $FSM]
    set NumStates      [get_state_count      $FSM]
    set NumTransitions [get_transition_count $FSM]
    puts "Read FSM '$Name' with $NumInputs inputs, $NumOutputs outputs, $NumStates states and $NumTransitions transitions"
    print_fsm $FSM
  }
  #set FSMKISS [read_kiss /tmp/spifsm.kiss]
  #print_fsm $FSMKISS
}

set AllFSMs($APP_NAME) $FSMs

if $Local {
  # add more stuff to AllFSMs just to test the nice table formatter
  set SPIFSM    [lindex $FSMs 0]
  set SensorFSM [lindex $FSMs 1]
  set AllFSMs(Hansi1) [list $SensorFSM $SPIFSM]
  set AllFSMs(Hansi2) [list $SensorFSM $SensorFSM $SPIFSM $SensorFSM $SPIFSM]

  print_fsms AllFSMs
}
