#!/bin/bash
#
# Start the simulation
#
# Be sure to create a Makefile first and compile all Verilog and VHDL files.
#

# generate firmware LSB/MSB files
FIRMWARE="../firmware/blinki/blinki.do"
FIRMWARE_LSB="firmware-lsb.do"
FIRMWARE_MSB="firmware-msb.do"
FIRMWARE_RAM0="firmware-ram0.do"
FIRMWARE_RAM1="firmware-ram1.do"
FIRMWARE_RAM2="firmware-ram2.do"
FIRMWARE_RAM3="firmware-ram3.do"
if [ ! -e "$FIRMWARE_RAM0" -o "$FIRMWARE" -nt "$FIRMWARE_RAM0" ] ; then
  echo "Generating firmware LSB/MSB files"
  sed -r 's/^(.*] 16#)..(...*)$/\1\2/' "$FIRMWARE" > "$FIRMWARE_LSB"
  sed -r 's/^(.*] 16#..)..(.*)$/\1\2/' "$FIRMWARE" > "$FIRMWARE_MSB"
  grep '#0[0-7]..#' $FIRMWARE_LSB > $FIRMWARE_RAM0
  grep '#0[0-7]..#' $FIRMWARE_MSB > $FIRMWARE_RAM1
  grep '#0[89A-Fa-f]..#' $FIRMWARE_LSB | sed -r 's/#08/#00/;s/#09/#01/;s/#0[Aa]/#02/;s/#0[Bb]/#03/;s/#0[Cc]/#04/;s/#0[Dd]/#05/;s/#0[Ee]/#06/;s/#0[Ff]/#07/' > $FIRMWARE_RAM2
  grep '#0[89A-Fa-f]..#' $FIRMWARE_MSB | sed -r 's/#08/#00/;s/#09/#01/;s/#0[Aa]/#02/;s/#0[Bb]/#03/;s/#0[Cc]/#04/;s/#0[Dd]/#05/;s/#0[Ee]/#06/;s/#0[Ff]/#07/' > $FIRMWARE_RAM3
fi

cat <<- EOF > vsim.do
vsim -t ps -voptargs=+acc Chip_tb
do wave.do
set PMEM_REG sim:/chip_tb/DUT/core_1/PMem_0/ram_0/memory
do firmware-ram0.do
set PMEM_REG sim:/chip_tb/DUT/core_1/PMem_0/ram_1/memory
do firmware-ram1.do
set PMEM_REG sim:/chip_tb/DUT/core_1/PMem_0/ram_2/memory
do firmware-ram2.do
set PMEM_REG sim:/chip_tb/DUT/core_1/PMem_0/ram_3/memory
do firmware-ram3.do
#log -r /*
run -all
EOF

vsim -do vsim.do
