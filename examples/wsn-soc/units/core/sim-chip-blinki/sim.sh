#!/bin/bash
#
# Start the simulation
#
# Be sure to create a Makefile first and compile all Verilog and VHDL files.
#

vsim -t 100ps -voptargs=+acc Chip_tb -do "do wave.do ; set PMEM_REG \"sim:/chip_tb/DUT/core_1/PMem_0/mem\" ; do ../firmware/blinki/blinki.do ; run -all"
