#!/bin/bash
#
# Start the simulation
#
# Be sure to create a Makefile first and compile all Verilog and VHDL files.
#

vsim -t ps -voptargs=+acc wrapreconfmodule_cfg -do "do wave-adt7410.do ; do config.do ; run -all"
