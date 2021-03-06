#!tcl
#
# Information on FSM<->TRFSM mapping
#
# Auto-generated by ../scripts//insert-trfsm-replace.tcl
#

# FSMs
set FSMs [list {\State} ]

# $fsm cell instances
set FSMInstances [list {$fsm$\State$713} ]

# FSM -> TR-FSM mapping: associative array, key = index into $FSMs and
# $FSMInstances, value = index into $TRFSMs
set FSMMapping(0) 0

# TR-FSM -> FSM mapping: associative array, key = index into $TRFSMs,
# value = index into $FSMs and $FSMInstances, -1 means unused
set TRFSMMapping(0) 0
set TRFSMMapping(1) -1

