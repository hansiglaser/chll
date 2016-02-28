#!/bin/bash
#
# Start the simulation
#
# Be sure to create a Makefile first and compile all Verilog and VHDL files.
#

vsim -t ps -voptargs=+acc ExtADCSimple_tb -do "do wave-app.do ; do config.do ; run -all"
