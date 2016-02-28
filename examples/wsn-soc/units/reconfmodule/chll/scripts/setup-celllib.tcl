#!flowproc
#
# Setup the cell library
#
# This script is sourced by check-celllib.tcl and some others.
#

source $RECONF_BASE/chll/scripts/setup-celllib-arr.tcl

puts "################################################################################"
puts "## Setting up Cell Library"
foreach Cell $CellLib {
  set CellLC [string tolower "$Cell"]
  set CELL_NAME "$Cell"
  set CELL_ILANG_OUT "$CELLLIB_BASE/$CellLC/chll/out/$CellLC.il"
  source "$CELLLIB_BASE/$CellLC/chll/scripts/setup.tcl"
  puts "## Reading and checking synthesized netlist $CELL_ILANG_OUT"
  cell_read_netlist "$CELL_ILANG_OUT"
}

