#!/bin/bash
#
# Start the simulation
#
# Be sure to create a Makefile first and compile all Verilog and VHDL files.
#

vsim -t 100ps -voptargs=+acc Core_tb -do "do wave.do ; set PMEM_REG \"sim:/core_tb/DUT/PMem_0/mem\" ; do ../firmware/blinki/blinki.do ; run -all"
