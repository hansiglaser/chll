#!/bin/bash
#
# Start the simulation
#
# Be sure to create a Makefile first and compile all Verilog and VHDL files.
#

vsim -t ps -voptargs=+acc slowadt7410_tb -do "do wave-slowadt7410.do ; run -all"
