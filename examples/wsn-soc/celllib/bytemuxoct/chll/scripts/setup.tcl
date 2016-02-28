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
cell_add_port "A_i"   -in  -conntype "Byte"
cell_add_port "B_i"   -in  -conntype "Byte"
cell_add_port "C_i"   -in  -conntype "Byte"
cell_add_port "D_i"   -in  -conntype "Byte"
cell_add_port "E_i"   -in  -conntype "Byte"
cell_add_port "F_i"   -in  -conntype "Byte"
cell_add_port "G_i"   -in  -conntype "Byte"
cell_add_port "H_i"   -in  -conntype "Byte"
cell_add_port "SAB_i" -in  -conntype "Bit"
cell_add_port "SC_i"  -in  -conntype "Bit"
cell_add_port "SD_i"  -in  -conntype "Bit"
cell_add_port "SE_i"  -in  -conntype "Bit"
cell_add_port "SF_i"  -in  -conntype "Bit"
cell_add_port "SG_i"  -in  -conntype "Bit"
cell_add_port "SH_i"  -in  -conntype "Bit"
cell_add_port "Y_o"   -out -conntype "Byte"
