#!/bin/bash
#
# Start the simulation
#
# Be sure to create a Makefile first and compile all Verilog and VHDL files.
#

vsim -t ps -voptargs=+acc core_tb -do "do wave-<app>.do ; set PMEM_REG \"sim:/core_tb/DUT/<memory/path>
