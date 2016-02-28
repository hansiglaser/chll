#!/bin/bash
#
# Start the simulation
#
# Be sure to create a Makefile first and compile all Verilog and VHDL files.
#

vsim -t ps -voptargs=+acc bytemuxoct_tb -do "do wave-bytemuxoct.do ; run -all"
