#!flowproc
#
# Included by:
#   .../apps/<app>/chll/scripts/intersynth-postproc.tcl
#   .../units/reconfmodule/chll/scripts/generate-reconf-module.tcl
# Required variables: $App, $AppLC, $OutDir, $PHASE
#

# Read InterSynth results
puts "## Reading InterSynth results for application $App"
read_intersynth -commands "$OutDir/$PHASE-netlist-$AppLC.txt"
read_intersynth -config   "$OutDir/$PHASE-bitdata-$AppLC.cfg"
read_intersynth -check
