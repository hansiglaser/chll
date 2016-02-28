#!/bin/bash
#
# Start the simulation
#
# Be sure to create a Makefile first and compile all Verilog and VHDL files.
#

vsim -t ps -voptargs=+acc wordmuxdual_tb -do "do wave-wordmuxdual.do ; run -all"
