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
cell_add_port "H_i" -in  -conntype "Byte"
cell_add_port "L_i" -in  -conntype "Byte"
cell_add_port "Y_o" -out -conntype "Word"
