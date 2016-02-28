#!/bin/bash
#
# Start the simulation
#
# Be sure to create a Makefile first and compile all Verilog and VHDL files.
#

vsim -t ps -voptargs=+acc core_tb -do "do wave-adt7410.do ; set PMEM_REG \"sim:/core_tb/DUT/PMem_0/mem\" ; do ../firmware/firmware.do ; run -all"
