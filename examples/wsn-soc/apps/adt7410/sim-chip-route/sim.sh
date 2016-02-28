#!/bin/bash
#
# Start the simulation
#
# Be sure to create a Makefile first and compile all Verilog and VHDL files.
#

if [ $# -ne 1 ] ; then
  echo "Usage: $0 [-min|-max]"
  exit 1
fi

SDF_DIR="../../../sta-route/output"

if [ "$1" = "-min" ] ; then
  OPCOND="min"
  SDF="$SDF_DIR/chip_BEST_rc_best.sdf"
elif [ "$1" = "-max" ] ; then
  OPCOND="max"
  SDF="$SDF_DIR/chip_WORST_rc_worst.sdf"
  # Encounter only saved rc_worst and rc_best SPEF files, therefore we only
  # calculate SDF files for these two operating conditions. Both SDF files have
  # two values each, but they are identical.
else
  echo "Error: Invalid parameter $1"
  exit 1;
fi

# generate firmware LSB/MSB files
FIRMWARE="../firmware/firmware.do"
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

# Simulation absolutely requires the SDF delay information, because the DMem
# RAM memgen128x8 is extremely picky on setup and hold time violations and
# erases the whole memory with 'X' on errors.
# Simulation is quite fast with SDF (25 sec.), so thats ok.

cat <<- EOF > vsim.do
# suppress warnings on unconnected Q and QN outputs of D-FF
# add "-debugdb" to enable causality tracing
vsim -t 1ps -voptargs="+acc -suppress 2685,2718" -sdf$OPCOND /chip_tb/DUT=$SDF +sdf_verbose -sdfnoerror chip_tb
do wave-adt7410.do
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
#run 300us
EOF

vsim -do vsim.do
