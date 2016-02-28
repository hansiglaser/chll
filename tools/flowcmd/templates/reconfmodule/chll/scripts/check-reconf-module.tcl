#!flowproc
#
# This script is executed by "flowcmd check-reconf-module" and includes the script
# "setup-reconf-module.tcl" which itself includes "setup-reconf-signals.tcl".
#

# remove the following two lines
puts "ERROR: You have to edit this file [info script] before executing."
exit 1

# Configure
# pad library
set PADLIB "<Pad Library File>"

# execute setup-reconf-signals.tcl
set SCRIPT_BASE [file dirname [info script]]
source $SCRIPT_BASE/setup-reconf-module.tcl

puts ""
puts "################################################################################"
puts "## Writing VHDL Code of the parent module $PARENT_MODULE for debug purpose"

write_netlist -parent -vhdl -entity    "$UNIT_BASE/chll/out/parent-entity.vhd"
write_netlist -parent -vhdl -component "$UNIT_BASE/chll/out/parent-component.vhd"
write_netlist -parent -vhdl -module    "$UNIT_BASE/chll/out/parent-module.vhd"

puts "## Generating Preliminary Reconfigurable Module sources"

create_reconf_module_netlist -preliminary

write_netlist -preliminary -verilog -instance     "$UNIT_BASE/chll/out/reconflogic-instance.inc.v"
write_netlist -preliminary -vhdl    -instance     "$UNIT_BASE/chll/out/reconflogic-instance.inc.vhd"
write_netlist -preliminary -vhdl    -entity       "$UNIT_BASE/chll/out/reconflogic-e.vhd"
write_netlist -preliminary -vhdl    -component    "$UNIT_BASE/chll/out/reconflogic-cmp.vhd"

puts "## Creating FPGA top level module"
create_chip -chip_name "chip" -arch_name "fpga_top" -parent_inst_name "<parent>_1"
# system
#$# chip_add_pin -direct "Reset_n_i"
#$# chip_add_pin -direct "Clk_i"
#$# chip_add_pin -direct "Cpu_En_i"
#$# chip_add_pin -const "0" "LFXT_Clk_i"
#$# chip_add_pin -direct "Dbg_SCL_i"
#$# chip_add_pin -direct_od -out "Dbg_SDA_Out_o" -in "Dbg_SDA_In_i" -portname "Dbg_SDA_b"
#$# chip_add_pin -direct_inout -out "P1_DOut_o" -enable "P1_En_o" -in "P1_DIn_i" -portname "P1_b"
#$# chip_add_pin -open "I2C_ENQ"

write_netlist -chip -vhdl -entity       "$UNIT_BASE/chll/out/chip-e.vhd"
write_netlist -chip -vhdl -architecture "$UNIT_BASE/chll/out/chip-fpga_top-a.vhd"

puts "## Creating Chip top level module"
create_chip -chip_name "chip" -arch_name "chip_top" -parent_inst_name "<parent>_1"
# read pad library
chip_read_liberty "$PADLIB"
#############################################################################
## Interesting Pads for AMS C35:                                           ##
##   BBC16P   CMOS Bidirectional Buffer with 16 mA drive strength          ##
##   BU16P    Output Buffer with 16 mA drive strength                      ##
##   BUDD16P  Open Drain Output Buffer with 16 mA drive str. and Pull Down ##
##   ICCK8P   CMOS Clock Input Buffer with 8 mA drive strength             ##
##   ICDP     CMOS Input Buffer with Pull Down                             ##
##   ICP      CMOS Input Buffer                                            ##
##   ISDP     Schmitt-Trigger Input Buffer with Pull Down                  ##
##   ISUP     Schmitt-Trigger Input Buffer with Pull Up                    ##
#############################################################################
# system
#$# chip_add_pin -pad_cell "ISUP"   "Reset_n_i"   ;# Schmitt-Trigger Input Buffer with Pull Up
#$# chip_add_pin -pad_cell "ICCK8P" "Clk_i"       ;# CMOS Clock Input Buffer with 8 mA drive strength
#$# chip_add_pin -pad_cell "ICP"  "Cpu_En_i"
#$# chip_add_pin -const "0" "LFXT_Clk_i"
#$# chip_add_pin -pad_cell "ICP"  "Dbg_SCL_i"
#$# chip_add_pin -pad_cell "BBC16P" -out '0' -enable_n "Dbg_SDA_Out_o" -in "Dbg_SDA_In_i" -portname "Dbg_SDA_b"
#$# chip_add_pin -pad_cell "BBC16P" -out "P1_DOut_o" -enable "P1_En_o" -in "P1_DIn_i" -portname "P1_b"
#$# chip_add_pin -pad_cell "BU16P" "UartTxD_o"
#$# chip_add_pin -open "I2C_ENQ"

# entity was already written above, but be careful to use identical pad names as above!
write_netlist -chip -vhdl -architecture "$UNIT_BASE/chll/out/chip-chip_top-a.vhd"

