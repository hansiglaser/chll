#!/bin/bash
#
# Start the simulation
#
# Be sure to create a Makefile first and compile all Verilog and VHDL files.
#

if [ $# -ne 1 ] ; then
  echo "Usage: $0 [-verilog|-tv1]"
  exit 1
fi

if [ "$1" = "-verilog" ] ; then
  vsim -t ps -voptargs=+acc addsubcmp_tb_verilog_cfg -do "do wave-addsubcmp.do ; run -all"
elif [ "$1" = "-tv1" ] ; then
  vsim -t ps -voptargs=+acc addsubcmp_tb_verilogTV1_cfg -do "do wave-addsubcmp.do ; run -all"
else
  echo "Error: Invalid parameter $1"
  exit 1;
fi
