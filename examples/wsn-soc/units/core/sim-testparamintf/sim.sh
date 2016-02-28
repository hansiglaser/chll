#!/bin/bash
#
# Start the simulation
#
# Be sure to create a Makefile first and compile all VHDL files.
#

vsim -t 100ps -voptargs=+acc Core_tb_TestParamIntf_cfg -do "do wave.do ; do ../firmware/testparamintf/testparamintf.do ; run -all"
