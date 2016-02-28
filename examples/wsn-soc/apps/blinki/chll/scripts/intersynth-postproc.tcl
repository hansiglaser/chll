#!flowproc
#
# Read application-specific output of InterSynth and create wrappers, ...
#
# Included by: .../units/reconfmodule/chll/scripts/check-intersynth.tcl
# Required variables: $App, $AppLC, $AppBase, $PHASE
#

# remove the following two lines
#puts "ERROR: You have to edit this file [info script] before executing."
#exit 1

set OutDir      "$AppBase/chll/out"
set ILangFile   "$OutDir/${AppLC}-extract-intersynth-trfsm.il"

# setup application
set Script      "$AppBase/chll/scripts/setup.tcl"
# set environment variables used by TCL script
set APP_NAME      "$App"
source $Script

source "$AppBase/chll/scripts/read-netlist.tcl"
source "$AppBase/chll/scripts/read-intersynth.tcl"

set WrapperFilename "$OutDir/${AppLC}-wrapintersynth.vhd"
puts "## Creating wrapper for application $App as $WrapperFilename"
select_app "$App"
# create module which wraps the InterSynth module
app_wrap_intersynth
# write to file
write_netlist -wrapis -vhdl -module "$WrapperFilename"

# read FSM <-> TR-FSM mapping to variables $FSM, $FSMInstances, $FSMMapping and
# $TRFSMMapping
source "$OutDir/${AppLC}-extract-intersynth-fsms.tcl"
# TODO: shouldn't this be done only once in
# .../units/reconfmodule/chll/scripts/check-intersynth.tcl?

set InterSynthInst [get_reconf_module -isinstname]

# FSM -> TRFSM mapping
# --------------------
# This requiers the full instance name (incl. path) of each TR-FSM and is
# therefore only possible after InterSynth was run.
#
# Inserting TR-FSMs is also performed before with
#   flowcmd insert-trfsm -fsm
#   flowcmd insert-trfsm -extract
#   flowcmd insert-trfsms
# and therefore also requires the information for the variable $TRFSMBase. See
# apps/<app>/chll/scripts/insert-trfsm.tcl for a very similar loop.
#
puts "## Creating bitstream files for application $App"
set mtf [open "$OutDir/${AppLC}-intersynth-bitstream-modelsim.do" "w"]
puts $mtf "#!modelsim"
puts $mtf "#"
puts $mtf "# Apply TR-FSM bitstreams"
puts $mtf "#"
puts $mtf "# Auto-generated by [info script]"
puts $mtf "#"
puts $mtf ""
set ltf [open "$OutDir/${AppLC}-intersynth-bitstream-lec.do" "w"]
puts $ltf "//!lec"
puts $ltf "//"
puts $ltf "// Apply TR-FSM bitstreams"
puts $ltf "//"
puts $ltf "// Auto-generated by [info script]"
puts $ltf "//"
puts $ltf ""
set lef [open "$OutDir/${AppLC}-intersynth-encoding-lec.do" "w"]
puts $lef "//!lec"
puts $lef "//"
puts $lef "// Apply FSM recoding"
puts $lef "//"
puts $lef "// Auto-generated by [info script]"
puts $lef "//"
puts $lef ""
foreach {TRFSMIdx FSMIdx} [array get TRFSMMapping] {
  set TRFSM [lindex $TRFSMShortNames $TRFSMIdx]
  set TRFSMLC [string tolower $TRFSM]
  set ISCell [get_intersynth_instance "$TRFSM" 0]
  if {$FSMIdx >= 0} {
    # TR-FSM $TRFSMIdx is used by FSM $FSMIdx
    set FSM [lindex $FSMs $FSMIdx]
    puts "TRFSM \"$TRFSM\" is at InterSynth cell \"$ISCell\" (used by FSM \"$FSM\")"
    # ModelSim
    puts $mtf "# $TRFSM used by $FSM"
    puts $mtf "set TRFSMBase \"/${AppLC}_tb/DUT/$InterSynthInst/${ISCell}/TRFSM_1\""
    puts $mtf "do $OutDir/${AppLC}-extract-intersynth-${TRFSMLC}-bitstream-modelsim.do"
    # TODO: replace $OutDir above against a variable set by the config.do scripts in sim-*
    # LEC
    puts $ltf "// $TRFSM used by $FSM"
    puts $ltf "setenv TRFSMBase \"/$InterSynthInst/${ISCell}/TRFSM_1\""
    puts $ltf "dofile \$APPCHLL_PATH/${AppLC}-extract-intersynth-${TRFSMLC}-bitstream-lec.do"
    # LEC FSM Encoding
    puts $lef "// $TRFSM used by $FSM"
    puts $lef "read fsm encoding \$APPCHLL_PATH/${AppLC}-extract-intersynth-${TRFSMLC}-encoding-lec.txt -golden"
  } else {
    # TR-FSM $TRFSMIdx is unused
    puts "TRFSM \"$TRFSM\" is at InterSynth cell \"$ISCell\" (unused)"
    # ModelSim
    puts $mtf "# $TRFSM is unused"
    # LEC
    puts $ltf "// $TRFSM is unused"
    # LEC FSM Encoding
    puts $lef "// $TRFSM is unused"
  }
}
close $mtf
close $ltf
close $lef

# create Verilog module to look into the module from the VHDL testbench
# setup all external names into the array "ExtNames": key = output name, 
#   value = reg.ex. for internal signal
unset -nocomplain ExtNames
#$# set ExtNames(XYZFSM_Done) "XYZFSM.*Done.*"

# create Verilog file
WriteExtNames ExtNames "$OutDir/extnames-intersynth.v" "ExtNames" "${AppLC}_tb.DUT.$InterSynthInst"
