#!flowproc
#
# Setup the cell library
#
# This script is sourced by setup-celllib.tcl and some others.
#

# remove the following two lines
#puts "ERROR: You have to edit this file [info script] before executing."
#exit 1

# List all cells used for the cell library
set CellLib [list Counter Counter32 AbsDiff WordRegister ByteRegister AddSubCmp ByteMuxOct ByteMuxQuad ByteMuxDual Byte2Word Byte2WordSel WordMuxDual]
