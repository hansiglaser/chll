#!flowproc
#
# Included by:
#   .../apps/<app>/chll/scripts/intersynth-postproc.tcl
#   .../units/reconfmodule/chll/scripts/generate-reconf-module.tcl
# Required variables: $App, $ILangFile
#

# Read and check synthesized netlist
puts "## Reading synthesized netlist of application $App from .../[file tail $ILangFile]"
app_read_netlist -extracted $ILangFile

