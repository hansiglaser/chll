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
cell_add_port "AddOrSub_i"   -in  -conntype "Bit"  ;# 0 ... Add; 1 ... Sub
cell_add_port "A_i"          -in  -conntype "Word"
cell_add_port "B_i"          -in  -conntype "Word"
cell_add_port "D_o"          -out -conntype "Word"
cell_add_port "Carry_i"      -in  -conntype "Bit"
cell_add_port "Carry_o"      -out -conntype "Bit"
cell_add_port "Zero_o"       -out -conntype "Bit"
cell_add_port "Sign_o"       -out -conntype "Bit"
cell_add_port "Overflow_o"   -out -conntype "Bit"
