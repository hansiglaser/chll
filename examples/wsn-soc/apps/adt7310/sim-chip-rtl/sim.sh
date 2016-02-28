#!/bin/bash
#
# Start the simulation
#
# Be sure to create a Makefile first and compile all Verilog and VHDL files.
#

vsim -t ps -voptargs=+acc chip_tb -do "do wave-adt7310.do ; set PMEM_REG \"sim:/chip_tb/DUT/core_1/PMem_0/mem\" ; do ../firmware/firmware-wrapapp.do ; run -all"
