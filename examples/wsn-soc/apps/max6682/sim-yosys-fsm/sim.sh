#!/bin/bash
#
# Start the simulation
#
# Be sure to create a Makefile first and compile all Verilog and VHDL files.
#

vsim -t ps -voptargs=+acc MAX6682_tb_verilog_cfg -do "do wave-app.do ; run -all"
