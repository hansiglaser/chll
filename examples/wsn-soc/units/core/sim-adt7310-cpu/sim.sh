#!/bin/bash
#
# Start the simulation
#
# Be sure to create a Makefile first and compile all Verilog and VHDL files.
#

vsim -t 100ps -voptargs=+acc Core_ADT7310_tb -do "do wave.do ; set PMEM_REG \"sim:/Core_ADT7310_tb/DUT/PMem_0/mem\" ; do ../firmware/adt7310-cpu/firmware.do ; run -all"
