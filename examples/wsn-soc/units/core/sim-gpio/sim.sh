#!/bin/bash
#
# Start the simulation
#
# Be sure to create a Makefile first and compile all Verilog and VHDL files.
#

cat <<EOF > vsim.do

do wave.do
set PMEM_REG "sim:/core_tb/DUT/PMem_0/mem"
do ../firmware/gpio/gpio.do
force -deposit {sim:/core_tb/DUT/P1_DIn_s[3]} St1 @0.22ms -cancel @0.23ms
run -all
EOF

vsim -t 100ps -voptargs=+acc Core_tb -do "do vsim.do"
