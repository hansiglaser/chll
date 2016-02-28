#!flowproc
#
# Setup the cell
#

# remove the following two lines
puts "ERROR: You have to edit this file [info script] before executing."
exit 1

puts "################################################################################"
puts "## Setup Cell $CELL_NAME"

create_cell "$CELL_NAME"

puts "## Adding ports, parameters, ..."
#$# cell_add_port "Reset_n_i" -map "Reset_n_i"
#$# cell_add_port "Clk_i"     -map "Clk_i"
# value
#$# cell_add_port "<...>_i"  -in  -conntype "<conntype>"
#$# cell_add_port "<...>_o"  -out -conntype "<conntype>"
# control
#$# cell_add_port "<...>_i"  -in  -conntype "<conntype>"
#$# cell_add_port "<...>_o"  -out -conntype "<conntype>"
