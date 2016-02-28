#!flowproc
#
# Setup the cell
#

# remove the following two lines
#puts "ERROR: You have to edit this file [info script] before executing."
#exit 1

puts "################################################################################"
puts "## Setup Cell $CELL_NAME"

create_cell "$CELL_NAME"

puts "## Adding ports, parameters, ..."
cell_add_port "Reset_n_i" -map "Reset_n_i"
cell_add_port "Clk_i"     -map "Clk_i"
# control signals
cell_add_port "ResetSig_i"   -in  -conntype "Bit"
cell_add_port "Preset_i"     -in  -conntype "Bit"
cell_add_port "Enable_i"     -in  -conntype "Bit"
cell_add_port "Direction_i"  -in  -conntype "Bit"
# preset value
cell_add_port "PresetVal_i"  -in  -conntype "Word"
# current value
cell_add_port "D_o"          -out -conntype "Word"
# flags
cell_add_port "Overflow_o"   -out -conntype "Bit"
cell_add_port "Zero_o"       -out -conntype "Bit"
