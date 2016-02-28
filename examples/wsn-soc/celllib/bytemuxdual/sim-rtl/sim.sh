#!/bin/bash
#
# Start the simulation
#
# Be sure to create a Makefile first and compile all Verilog and VHDL files.
#

vsim -t ps -voptargs=+acc bytemuxdual_tb -do "do wave-bytemuxdual.do ; run -all"
