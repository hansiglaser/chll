#!/bin/bash
#
# Start the simulation
#
# Be sure to create a Makefile first and compile all Verilog and VHDL files.
#

vsim -t ps -voptargs=+acc adt7310p32s16l_tb -do "do wave-adt7310.do ; run -all"
