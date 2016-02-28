#!trfsmgen
#
# Setup the example applications
#
# This script is sourced by insert-trfsms.tcl.
#

# remove the following two lines
puts "ERROR: You have to edit this file [info script] before executing."
exit 1

puts "## Creating TR-FSM instances"
set TRFSMs [list]
# These must be in ascending order, because the "map_fsms" goes through in the
# given order and uses the first TR-FSM which fits.
#$# lappend TRFSMs [create_trfsm  5  5 4 5  5  3 2 1]
#$# lappend TRFSMs [create_trfsm  8 15 5 5 10 10 5 5]
#$# lappend TRFSMs [create_trfsm 10 10 5 5  5 10 5 5]
#$# lappend TRFSMs [create_trfsm 15 15 5 5 15 15 5 5]

# create short names
set TRFSMShortNames [list]
set n 0
foreach TRFSM $TRFSMs {
  lappend TRFSMShortNames "TRFSM$n"
  incr n
}

# create TR-FSM wrappers
# this is very similar to CreateWrappers in ./chll/functions.tcl but doesn't write files
puts "## Creating TR-FSM wrapper modules"
set Wrappers [list]
for {set Idx 0} {$Idx < [llength $TRFSMs]} {incr Idx} {
  set TRFSM       [lindex $TRFSMs          $Idx]
  set ShortName   [lindex $TRFSMShortNames $Idx]
  # create wrapper module for TR-FSM
  set Wrapper [create_trfsm_wrapper $TRFSM $ShortName]
  lappend Wrappers $Wrapper
}
