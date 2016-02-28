#!/bin/bash
#
# Start the simulation
#
# Be sure to create a Makefile first and compile all Verilog and VHDL files.
#

if [ $# -ne 1 ] ; then
  echo "Usage: $0 [-vhdl|-verilog|-model]"
  exit 1
fi

if [ "$1" = "-vhdl" ] ; then
  vsim -t ps -voptargs=+acc MAX6682_tb_vhdl_cfg -do "do wave-app.do ; run -all"
elif [ "$1" = "-verilog" ] ; then
  vsim -t ps -voptargs=+acc MAX6682_tb_verilog_cfg -do "do wave-app.do ; run -all"
elif [ "$1" = "-model" ] ; then
  vsim max6682_model_tb -do "do wave-model.do ; run -all"
else
  echo "Error: Invalid parameter $1"
  exit 1;
fi
