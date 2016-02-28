#!flowproc
#
# Print a list of all reconfigurable signals
#
# This script is executed by "flowcmd list-reconf-signals" and includes the
# script "setup-reconf-signals.tcl".
#

# execute setup-reconf-signals.tcl
set SCRIPT_BASE [file dirname [info script]]
source $SCRIPT_BASE/setup-reconf-signals.tcl

# Print a list of all reconfigurable signals
puts ""
puts "#### Final list of reconfigurable signals: ####"
list_reconf_signals

puts "#### Signals with unknown direction"
list_reconf_signals -dir unknown
