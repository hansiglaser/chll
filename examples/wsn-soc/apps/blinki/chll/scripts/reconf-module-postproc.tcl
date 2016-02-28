#!flowproc
#
# Read application-specific output of InterSynth and create wrappers, ...
#
# Included by: .../units/reconfmodule/chll/scripts/generate-reconf-module.tcl
# Required variables: TODO: $App, $AppLC, $AppBase, $PHASE
#

# remove the following two lines
puts "ERROR: You have to edit this file [info script] before executing."
exit 1

set OutDir      "$AppBase/chll/out"
set ILangFile   "$OutDir/${AppLC}-extract-intersynth-trfsm.il"

puts "################################################################################"
puts "## Post-processing for application $App"
# contrary to intersynth-postproc, here the application setup was already
# performed, we only have to select the application
select_app "$App"

puts "## Creating wrapper for application $App"
# create module which wraps the InterSynth module
app_wrap_reconf_module
# write to files
set WrapperFilename "$OutDir/${AppLC}-wrapreconfmodule-vhdl2008.vhd"
puts "  Saving to $WrapperFilename"
write_netlist -wraprm_vhdl2008 -vhdl -module "$WrapperFilename"
set WrapperFilename "$OutDir/${AppLC}-wrapreconfmodule-lec.vhd"
puts "  Saving to $WrapperFilename"
write_netlist -wraprm_lec      -vhdl -module "$WrapperFilename"
# netlist adjunct for LEC
write_netlist_adjunct -wraprm_lec_setup   "$OutDir/${AppLC}-wrapreconfmodule-lec-setup.do"
write_netlist_adjunct -wraprm_lec_mapping "$OutDir/${AppLC}-wrapreconfmodule-lec-mapping.do"

# read FSM <-> TR-FSM mapping to variables $FSM, $FSMInstances, $FSMMapping and
# $TRFSMMapping
source "$OutDir/${AppLC}-extract-intersynth-fsms.tcl"
# TODO: shouldn't this be done only once in
# .../units/reconfmodule/chll/scripts/check-intersynth.tcl?

set InterSynthInst   [get_reconf_module -isinstname]
set ReconfModuleInst [get_reconf_module -instname]

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
set mtf [open "$OutDir/${AppLC}-reconfmodule-bitstream-modelsim.do" "w"]
puts $mtf "#!modelsim"
puts $mtf "#"
puts $mtf "# Apply TR-FSM bitstreams"
puts $mtf "#"
puts $mtf "# Auto-generated by [info script]"
puts $mtf "#"
puts $mtf ""
set ltf [open "$OutDir/${AppLC}-reconfmodule-bitstream-lec.do" "w"]
puts $ltf "//!lec"
puts $ltf "//"
puts $ltf "// Apply TR-FSM bitstreams"
puts $ltf "//"
puts $ltf "// Auto-generated by [info script]"
puts $ltf "//"
puts $ltf ""
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
    puts $mtf "set TRFSMBase \"/${AppLC}_tb/DUT/$ReconfModuleInst/$InterSynthInst/${ISCell}/TRFSM_1\""
    puts $mtf "do $OutDir/${AppLC}-extract-intersynth-${TRFSMLC}-bitstream-modelsim.do"
    # LEC
    puts $ltf "// $TRFSM used by $FSM"
    puts $ltf "setenv TRFSMBase \"/$ReconfModuleInst/$InterSynthInst/${ISCell}/TRFSM_1\""
    puts $ltf "dofile \$APPCHLL_PATH/${AppLC}-extract-intersynth-${TRFSMLC}-bitstream-lec.do"
  } else {
    # TR-FSM $TRFSMIdx is unused
    puts "TRFSM \"$TRFSM\" is at InterSynth cell \"$ISCell\" (unused)"
    # ModelSim
    puts $mtf "# $TRFSM is unused"
    # LEC
    puts $ltf "// $TRFSM is unused"
  }
}
set FileBase "$OutDir/${AppLC}-bitdata"
set CfgFileBitdata "$FileBase-bitstream.inc.c"
write_bitstream -bitdata -format vhdl         "bitdata"      $FileBase-bitstream.vhd
write_bitstream -bitdata -format verilog      "bitdata"      $FileBase-bitstream.v
write_bitstream -bitdata -format c            "bitdata"      $CfgFileBitdata
write_bitstream -bitdata -format text         "bitdata"      $FileBase-bitstream.txt
write_bitstream -bitdata -format modelsim     "\$BitDataReg" $FileBase-bitstream-modelsim.do
write_bitstream -bitdata -format lec -revised "\$BitDataReg" $FileBase-bitstream-lec.do
write_bitstream -bitdata -format formality    "\$BitDataReg" $FileBase-bitstream-formality.tcl
# ModelSim
puts $mtf "# InterSynth bitdata"
puts $mtf "set BitDataReg \"/${AppLC}_tb/DUT/$ReconfModuleInst/CfgRegbitdata\""
puts $mtf "do $FileBase-bitstream-modelsim.do"
# LEC
puts $ltf "// InterSynth bitdata"
puts $ltf "setenv BitDataReg \"/$ReconfModuleInst/CfgRegbitdata\""
puts $ltf "dofile \$APPCHLL_PATH/${AppLC}-bitdata-bitstream-lec.do"


set FileBase "$OutDir/${AppLC}-reconfsignals"
set CfgFileReconfSignals "$FileBase-bitstream.inc.c"
write_bitstream -reconfsignals -format vhdl         "ReconfSignals"      $FileBase-bitstream.vhd
write_bitstream -reconfsignals -format verilog      "ReconfSignals"      $FileBase-bitstream.v
write_bitstream -reconfsignals -format c            "ReconfSignals"      $CfgFileReconfSignals
write_bitstream -reconfsignals -format text         "ReconfSignals"      $FileBase-bitstream.txt
write_bitstream -reconfsignals -format modelsim     "\$ReconfSignalsReg" $FileBase-bitstream-modelsim.do
write_bitstream -reconfsignals -format lec -revised "\$ReconfSignalsReg" $FileBase-bitstream-lec.do
write_bitstream -reconfsignals -format formality    "\$ReconfSignalsReg" $FileBase-bitstream-formality.tcl
puts $mtf "# Reconf.signals config register"
puts $mtf "set ReconfSignalsReg \"/${AppLC}_tb/DUT/$ReconfModuleInst/CfgRegReconfSignals\""
puts $mtf "do $OutDir/${AppLC}-reconfsignals-bitstream-modelsim.do"
# LEC
puts $ltf "// Reconf.signals config register"
puts $ltf "setenv ReconfSignalsReg \"/$ReconfModuleInst/CfgRegReconfSignals\""
puts $ltf "dofile \$APPCHLL_PATH/${AppLC}-reconfsignals-bitstream-lec.do"

close $mtf
close $ltf

# create Verilog module to look into the module from the VHDL testbench
# setup all external names into the array "ExtNames": key = output name, 
#   value = reg.ex. for internal signal
unset -nocomplain ExtNames
#$# set ExtNames(<portname>) "<signal regex>"
#$# set ExtNames(<portname>) "<signal regex>"
#$# set ExtNames(<portname>) "<signal regex>"

WriteExtNames ExtNames "$OutDir/extnames-reconfmodule.v"      "ExtNames" "<${AppLC}_tb>.<DUT>.$ReconfModuleInst.$InterSynthInst"
WriteExtNames ExtNames "$OutDir/extnames-core-reconfmodule.v" "ExtNames" "<core_tb>.<DUT>.$ReconfModuleInst.$InterSynthInst"

##############################################################################
puts "## Writing firmware files"
app_write_firmware -reconfmodule -driver_header "$OutDir/$AppLC-driver.h"
app_write_firmware -reconfmodule -driver_source "$OutDir/$AppLC-driver.c"

set fn "$OutDir/$AppLC-config.inc.c"
puts "## Writing firmware include file for config bitstreams $fn"
set f [open $fn "w"]
puts $f "/**"
puts $f " * Include file which itself includes all bitstreams"
puts $f " * Auto-generated by [info script]"
puts $f " */"
puts $f ""
puts $f "#include \"$CfgFileBitdata\""
puts $f "#include \"$CfgFileReconfSignals\""
# include TR-FSM bitstreams, generated by insert-tfsm-replace.tcl, called for
# "flowcmd insert-trfsms"
foreach {TRFSMIdx FSMIdx} [array get TRFSMMapping] {
  set TRFSM   [lindex $TRFSMShortNames $TRFSMIdx]
  set TRFSMLC [string tolower $TRFSM]
  # this lists all bitstream files, including those for unused TR-FSMs
  puts $f "#include \"$OutDir/${AppLC}-extract-intersynth-${TRFSMLC}-bitstream.inc.c\""
}
close $f
