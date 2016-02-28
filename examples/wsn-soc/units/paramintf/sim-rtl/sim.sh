#!/bin/bash
#
# Start the simulation
#
# Be sure to create a Makefile first and compile all VHDL files.
#

vsim ParamIntf_tb_behavior_cfg -do "do wave.do ; run -all"
