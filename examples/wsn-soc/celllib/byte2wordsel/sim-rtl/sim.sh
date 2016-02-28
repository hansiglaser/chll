#!/bin/bash
#
# Start the simulation
#
# Be sure to create a Makefile first and compile all Verilog and VHDL files.
#

vsim -t ps -voptargs=+acc byte2wordsel_tb -do "do wave-byte2wordsel.do ; run -all"
