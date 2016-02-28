#!flowproc
#
# This script is source'd by intersynth.tcl and apps/<app>/intersynth-postsi.tcl
#

# number of trees for each conntype
set ConnTypeTrees(Bit)  2
set ConnTypeTrees(Byte) 2
set ConnTypeTrees(Word) 2

# routing cost for each conntype, if unaware, use the bit-width (see InterSynth
# manual)
set ConnTypeCost(Bit)   1.0
set ConnTypeCost(Byte)  8.0
set ConnTypeCost(Word) 16.0


