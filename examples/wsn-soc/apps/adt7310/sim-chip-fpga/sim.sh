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
	do ../sim-chip-reconfmodule/wave-adt7310.do
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
	do ../sim-chip-reconfmodule/wave-adt7310.do
	set PMEM_REG sim:/chip_tb/DUT/core_1/PMem_0/mem_reg_0/TDP/RAMB36E1_TDP_inst/prcs_clk/mem
	do firmware-lsb.do
	set PMEM_REG sim:/chip_tb/DUT/core_1/PMem_0/mem_reg_1/TDP/RAMB36E1_TDP_inst/prcs_clk/mem
	do firmware-msb.do
	run -all
	EOF
  echo "Simulating VHDL netlist (approx. 7+8=15 minutes)"
  vsim -do vsim.do
fi

exit

##############################################################################
# Everything below here was with experiments using the faster Xilinx Formal
# Library. Unfortunately there are lots of 'X' values and I couldn't find the
# reason for this problems with two days of work.
#
# I guess the problem are the combinational feedback loops without timing
# information.
#
# It is possible to force all signals inside MyInterSynthModule to '0' (see
# "force.do" below), but these are set to 'X' immediately with the first
# rising edge of the Clk_i signal, after Reset_n_i is deasserted.
#
# Investigating the 'X' source by "opening" the delta cycle steps just reveals
# any random internal signal (e.g. "sw_0_0_7_up0") but there should not be
# an 'X'
#
# If I cut out the OneWire master (i.e., connect "\<const0>" instead of the
# original signals to MyReconfLogic), a lot less problems occur (I don't
# remember exactly). The remaining problem is that force.do sets all signals
# inside MyInterSynthModule to '0', but some of these LUTs create '1's as
# their default state.
#
# When simulating e.g. the ADT7310 (ex.)app., TRFSM1.Out6_o should be forwarded
# to Counter[0].Preset_i (cell_125 and cell_112). One of the LUT6s in this path
# generates a '1' regardless of the signal value, so ModelSim doesn't set a
# "new" output value and therefore doesn't overwrite the (wrong) force to '0'.
#


# to trace signal drivers, you have to add "-debugDB"
# to save all signal values (instead of only those in the wave window) use "log -r /*"
#
#vsim -debugDB -L unisim -voptargs="+acc -keep_delta -hazards +initreg+0" -t ps chip_tb
vsim -debugDB -L unisim -voptargs=+acc -t ps chip_tb
log -r /*
#log /*
#log /chip_tb/*
#log /chip_tb/DUT/*
#log /chip_tb/DUT/core_1/*
#log sim:/chip_tb/DUT/core_1/openMSP430_0/*
##log -r sim:/chip_tb/DUT/core_1/openMSP430_0/*
#log /chip_tb/DUT/core_1/MyReconfigLogic_0/*
#log -r /chip_tb/DUT/core_1/MyReconfigLogic_0/MyInterSynthModule_0/*
#do ../sim-chip-reconfmodule/wave-adt7310.do
do wave.do
#do /tmp/force.do
force -deposit sim:/chip_tb/DUT/core_1/MyReconfigLogic_0/SENT_OutByte_i 0
force -deposit sim:/chip_tb/DUT/core_1/MyReconfigLogic_0/SPC_OutByte_i 0
force -deposit sim:/chip_tb/DUT/core_1/MyReconfigLogic_0/I2C_Busy_i 0
#force -freeze sim:/chip_tb/DUT/core_1/MyReconfigLogic_0/MyInterSynthModule_0/sw_0_0_7_up0 1
run 1ns
# Verilog:
#set PMEM_REG sim:/chip_tb/DUT/core_1/PMem_0/mem_reg_0/genblk1/INT_RAMB_TDP/mem
#do firmware-lsb.do
#set PMEM_REG sim:/chip_tb/DUT/core_1/PMem_0/mem_reg_1/genblk1/INT_RAMB_TDP/mem
#do firmware-msb.do
# VHDL: 
set PMEM_REG sim:/chip_tb/DUT/core_1/PMem_0/mem_reg_0/TDP/RAMB36E1_TDP_inst/prcs_clk/mem
do firmware-lsb.do
set PMEM_REG sim:/chip_tb/DUT/core_1/PMem_0/mem_reg_1/TDP/RAMB36E1_TDP_inst/prcs_clk/mem
do firmware-msb.do
run 100us
run 200ms

# generate force.do:
# edit project_1.v, select all "wire"s of MyInterSynthModule, save to /tmp/iwire.txt
# sed -r 's#  wire [^a-zA-Z\]*#force -deposit \{sim:/chip_tb/DUT/core_1/MyReconfigLogic_0/MyInterSynthModule_0/#' /tmp/iwire.txt | sed -r 's/;/ } 0/' | grep -v '\#' > /tmp/force.do


###############################################################################
# XSim also works from within Vivado. It internally uses a VHDL netlist. To
# fill the PMem, use the command "add_force", see ug835-vivado-tcl-commands.pdf
# p. 55.
#
# One problem with XSim is that it doesn't support cross-language identifiers
# as used in extnames-chip-reconflogic.v.
###############################################################################
