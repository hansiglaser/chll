#!/bin/bash
#
# Start the simulation
#
# Be sure to create a Makefile first and compile all Verilog and VHDL files.
#

if [ $# -ne 1 ] ; then
  echo "Usage: $0 [-poll|-intr]"
  exit 1
fi

if [ "$1" = "-poll" ] ; then
  vsim -t 100ps -voptargs=+acc Core_Uart_tb -do "do wave.do ; set PMEM_REG \"sim:/core_uart_tb/DUT/PMem_0/mem\" ; do ../firmware/uart-poll/uart-poll.do ; run -all"
elif [ "$1" = "-intr" ] ; then
  vsim -t 100ps -voptargs=+acc Core_Uart_tb -do "do wave.do ; set PMEM_REG \"sim:/core_uart_tb/DUT/PMem_0/mem\" ; do ../firmware/uart-intr/uart-intr.do ; run -all"
else
  echo "Error: Invalid parameter $1"
  exit 1;
fi

