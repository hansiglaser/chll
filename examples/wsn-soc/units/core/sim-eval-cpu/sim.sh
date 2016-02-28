#!/bin/bash
#
# Start the simulation
#
# Be sure to create a Makefile first and compile all Verilog and VHDL files.
#

vsim -t 100ps -voptargs=+acc Chip_Eval_tb -do "do wave.do ; set PMEM_REG \"sim:/chip_eval_tb/DUT/core_1/PMem_0/mem\" ; do ../../../../../eval/hardware/mcu-modules/ide/Testchip2-CPU/firmware.do ; run -all"
