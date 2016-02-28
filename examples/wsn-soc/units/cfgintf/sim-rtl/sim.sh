#!/bin/bash
#
# Start the simulation
#
# Be sure to create a Makefile first and compile all VHDL files.
#

vsim CfgIntf_tb_behavior_cfg -do "do wave-cfgintf.do ; run -all"
