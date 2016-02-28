#!/bin/bash
#
# Start the simulation
#
# Be sure to create a Makefile first and compile all Verilog and VHDL files.
#

vsim -t ps -voptargs=+acc chip_tb -do "do wave-<app>.do ; set PMEM_REG \"sim:/chip_tb/DUT/core_1/<memory/path>\" ; do ../firmware/firmware.do ; log -r /* ; run -all"
