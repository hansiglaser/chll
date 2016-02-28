#!/bin/bash
#
# Start the simulation
#
# Be sure to create a Makefile first and compile all Verilog and VHDL files.
#

if [ ! -e compiled.verilog -a ! -e compiled.vhdl ] ; then
  echo "Error: Please first compile either the Verilog or the VHDL netlist and libraries"
  exit 1
fi

# generate firmware LSB/MSB files
FIRMWARE="../firmware/firmware.do"
FIRMWARE_LSB="firmware-lsb.do"
FIRMWARE_MSB="firmware-msb.do"
if [ ! -e "$FIRMWARE_LSB" -o "$FIRMWARE" -nt "$FIRMWARE_LSB" ] ; then
  echo "Generating firmware LSB/MSB files"
  sed -r 's/^(.*] 16#)..(...*)$/\1\2/' "$FIRMWARE" > "$FIRMWARE_LSB"
  sed -r 's/^(.*] 16#..)..(.*)$/\1\2/' "$FIRMWARE" > "$FIRMWARE_MSB"
fi

if [ -e compiled.verilog ] ; then
  ###########################################################
  # Verilog UniSim
	cat <<- EOF > vsim.do  # this requires tabs instead of spaces
	vsim -L unisim -voptargs=+acc -t ps chip_tb glbl
	log -r /*
	do ../sim-chip-reconfmodule/wave-max6682.do
	run 1ns
	set PMEM_REG sim:/chip_tb/DUT/core_1/PMem_0/mem_reg_0/genblk1/INT_RAMB_TDP/mem
	do firmware-lsb.do
	set PMEM_REG sim:/chip_tb/DUT/core_1/PMem_0/mem_reg_1/genblk1/INT_RAMB_TDP/mem
	do firmware-msb.do
	run -all
	EOF
  echo "Simulating Verilog netlist (approx. 3 minutes)"
  vsim -do vsim.do
elif [ -e compiled.vhdl ] ; then
  ###########################################################
  # VHDL UniSim
	cat <<- EOF > vsim.do  # this requires tabs instead of spaces
	vsim -L unisim -voptargs=+acc -t ps chip_tb
	do ../sim-chip-reconfmodule/wave-max6682.do
	set PMEM_REG sim:/chip_tb/DUT/core_1/PMem_0/mem_reg_0/TDP/RAMB36E1_TDP_inst/prcs_clk/mem
	do firmware-lsb.do
	set PMEM_REG sim:/chip_tb/DUT/core_1/PMem_0/mem_reg_1/TDP/RAMB36E1_TDP_inst/prcs_clk/mem
	do firmware-msb.do
	run -all
	EOF
  echo "Simulating VHDL netlist (approx. 7+8=15 minutes)"
  vsim -do vsim.do
fi
