#!/bin/bash
#
# Start the simulation
#
# Be sure to create a Makefile first and compile all Verilog and VHDL files.
#

vsim -voptargs=+acc SimpleSPI_tb -do "do wave.do ; run -all"
