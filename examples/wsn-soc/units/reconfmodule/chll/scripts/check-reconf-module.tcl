#!flowproc
#
# This script is executed by "flowcmd check-reconf-module" and includes the script
# "setup-reconf-module.tcl" which itself includes "setup-reconf-signals.tcl".
#

# remove the following two lines
#puts "ERROR: You have to edit this file [info script] before executing."
#exit 1

# Configure
# set to "I2C" or "UART", depending on the OpenMSP430 debug interface
set OMSP_DEBUG "I2C"
# pad library
set PADLIB "/path/to/ams/hitkit/3.80/liberty/c35_3.3V/c35_IOLIB_4M.lib"

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
create_chip -chip_name "chip" -arch_name "fpga_top" -parent_inst_name "core_1"
# system
chip_add_pin -direct "Reset_n_i"
chip_add_pin -direct "Clk_i"
chip_add_pin -direct "Cpu_En_i"
# CPU: secondary (slow) clock
chip_add_pin -const "0" "LFXT_Clk_i"
# CPU: Debug Interface
chip_add_pin -direct "Dbg_En_i"
if { $OMSP_DEBUG == "UART" } {
  chip_add_pin -direct "Dbg_UART_RxD_i" 
  chip_add_pin -direct "Dbg_UART_TxD_o"
}
if { $OMSP_DEBUG == "I2C" } {
  chip_add_pin -direct "Dbg_SCL_i"
  chip_add_pin -direct_od -out "Dbg_SDA_Out_o" -in "Dbg_SDA_In_i" -portname "Dbg_SDA_b"
}
# CPU: GPIOs
chip_add_pin -direct_inout -out "P1_DOut_o" -enable "P1_En_o" -in "P1_DIn_i" -portname "P1_b"
chip_add_pin -direct_inout -out "P2_DOut_o" -enable "P2_En_o" -in "P2_DIn_i" -portname "P2_b"
# CPU: UART
chip_add_pin -direct "UartRxD_i"
chip_add_pin -direct "UartTxD_o"
# CPU: SPI
chip_add_pin -direct "MISO_i"
chip_add_pin -direct "MOSI_o"
chip_add_pin -direct "SCK_o"
# I/O
chip_add_pin -direct "Inputs_i"
chip_add_pin -direct "Outputs_o"
# SPI
chip_add_pin -direct "SPIMISO_i"
chip_add_pin -direct "SPIMOSI_o"
chip_add_pin -direct "SPISCK_o"
# I2C
chip_add_pin -direct_od -out "I2CSCL_o"                -portname "I2CSCL_b"   ;# name collision if portname is "I2CSCL_o" :-(
chip_add_pin -direct_od -out "I2CSDA_o" -in "I2CSDA_i" -portname "I2CSDA_b"
if {$INCLUDE_ONEWIRE} {
  # 1-Wire
  chip_add_pin -direct_od -out "OneWire_o" -in "OneWire_i" -portname "OneWire_b"
}
if {$INCLUDE_PWM} {
  # PWM
  chip_add_pin -direct -portname "PWM_i"  "PWMInput_i"  
}
if {$INCLUDE_SENT} {
  # SENT       
  chip_add_pin -direct -portname "SENT_i" "SENTInput_i" 
}
if {$INCLUDE_SPC} {
  # SPC
  chip_add_pin -direct_od -out "SPCTrigger_o" -in "SPCInput_i" -portname "SPC_b"
}
# ADC
chip_add_pin -direct "AdcConvComplete_i"
chip_add_pin -direct "AdcDoConvert_o"
chip_add_pin -direct "AdcValue_i"

write_netlist -chip -vhdl -entity       "$UNIT_BASE/chll/out/chip-e.vhd"
write_netlist -chip -vhdl -architecture "$UNIT_BASE/chll/out/chip-fpga_top-a.vhd"

puts "## Creating Chip top level module"
create_chip -chip_name "chip" -arch_name "chip_top" -parent_inst_name "core_1"
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
chip_add_pin -pad_cell "ISUP"   "Reset_n_i"   ;# Schmitt-Trigger Input Buffer with Pull Up
chip_add_pin -pad_cell "ICCK8P" "Clk_i"       ;# CMOS Clock Input Buffer with 8 mA drive strength
chip_add_pin -pad_cell "ICP"  "Cpu_En_i"
# CPU: secondary (slow) clock
chip_add_pin -const "0" "LFXT_Clk_i"
# CPU: Debug Interface
chip_add_pin -pad_cell "ICP"  "Dbg_En_i"
if { $OMSP_DEBUG == "UART" } {
  chip_add_pin -pad_cell "ICP"   "Dbg_UART_RxD_i" 
  chip_add_pin -pad_cell "BU16P" "Dbg_UART_TxD_o"
}
if { $OMSP_DEBUG == "I2C" } {
  chip_add_pin -pad_cell "ICP"  "Dbg_SCL_i"
  chip_add_pin -pad_cell "BBC16P" -out '0' -enable_n "Dbg_SDA_Out_o" -in "Dbg_SDA_In_i" -portname "Dbg_SDA_b"
}
# CPU: GPIOs
chip_add_pin -pad_cell "BBC16P" -out "P1_DOut_o" -enable "P1_En_o" -in "P1_DIn_i" -portname "P1_b"
chip_add_pin -pad_cell "BBC16P" -out "P2_DOut_o" -enable "P2_En_o" -in "P2_DIn_i" -portname "P2_b"
# CPU: UART
chip_add_pin -pad_cell "ICP"   "UartRxD_i"
chip_add_pin -pad_cell "BU16P" "UartTxD_o"
# CPU: SPI
chip_add_pin -pad_cell "ICP"   "MISO_i"
chip_add_pin -pad_cell "BU16P" "MOSI_o"
chip_add_pin -pad_cell "BU16P" "SCK_o"
# I/O
chip_add_pin -pad_cell "ICP"   "Inputs_i"
chip_add_pin -pad_cell "BU16P" "Outputs_o"
# SPI
chip_add_pin -pad_cell "ICP"   "SPIMISO_i"
chip_add_pin -pad_cell "BU16P" "SPIMOSI_o"
chip_add_pin -pad_cell "BU16P" "SPISCK_o"
# I2C
chip_add_pin -pad_cell "BUDD16P" -portname "I2CSCL_b"  "I2CSCL_o"  ;# Open Drain Output Buffer with 16 mA drive strength, name collision if portname is "I2CSCL_o" :-(
chip_add_pin -pad_cell "BBC16P"  -out '0' -enable_n "I2CSDA_o"  -in "I2CSDA_i"  -portname "I2CSDA_b"
if {$INCLUDE_ONEWIRE} {
  # 1-Wire
  chip_add_pin -pad_cell "BBC16P"  -out '0' -enable_n "OneWire_o" -in "OneWire_i" -portname "OneWire_b" 
}
if {$INCLUDE_PWM} {
  # PWM
  chip_add_pin -pad_cell "ICP"  -portname "PWM_i"  "PWMInput_i"  
}
if {$INCLUDE_SENT} {
  # SENT       
  chip_add_pin -pad_cell "ICP"  -portname "SENT_i" "SENTInput_i" 
}
if {$INCLUDE_SPC} {
  # SPC
  chip_add_pin -pad_cell "BBC16P"  -out {'0'} -enable_n "SPCTrigger_o" -in "SPCInput_i" -portname "SPC_b"
}
# ADC
chip_add_pin -pad_cell "ICP"   "AdcConvComplete_i"
chip_add_pin -pad_cell "BU16P" "AdcDoConvert_o"
chip_add_pin -pad_cell "ICP"   "AdcValue_i"

# entity was already written above, but be careful to use identical pad names as above!
write_netlist -chip -vhdl -architecture "$UNIT_BASE/chll/out/chip-chip_top-a.vhd"

