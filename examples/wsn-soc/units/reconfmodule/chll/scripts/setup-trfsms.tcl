#!trfsmgen
#
# Setup the example applications
#
# This script is sourced by insert-trfsms.tcl.
#

# remove the following two lines
#puts "ERROR: You have to edit this file [info script] before executing."
#exit 1

puts "## Creating TR-FSM instances"
set TRFSMs [list]
# These must be in ascending order, because the "map_fsms" goes through in the
# given order and uses the first TR-FSM which fits.

#                             I  O  S  n0 n1 n2 n3 n4
lappend TRFSMs [create_trfsm  6 10  5  10 10  4  4  2]
lappend TRFSMs [create_trfsm 10 15  6  10 20  6  6  4]

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
