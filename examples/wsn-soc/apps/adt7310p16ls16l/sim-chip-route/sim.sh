#!/bin/bash
#
# Start the simulation
#
# Be sure to create a Makefile first and compile all Verilog and VHDL files.
#

function Usage() {
  echo "Usage: $0 -min|-max [-100kHz|-10MHz] [-vcd|-saif]"
  exit 1
}

SDF_DIR="../../../sta-route/output"

OPCOND=""
FREQ=""
PERIODE="10 us"   # default value
VCD=""
SAIF=""

while [ $# -gt 0 ] ; do
  case "$1" in
    -min)
      [ -n "$OPCOND" ] && Usage   # only one -min or -max is allowed
      OPCOND="min"
      SDF="$SDF_DIR/chip_BEST_rc_best.sdf"
      ;;
    -max)
      [ -n "$OPCOND" ] && Usage   # only one -min or -max is allowed
      OPCOND="max"
      SDF="$SDF_DIR/chip_WORST_rc_worst.sdf"
      # Encounter only saved rc_worst and rc_best SPEF files, therefore we only
      # calculate SDF files for these two operating conditions. Both SDF files have
      # two values each, but they are identical.
      ;;
    -100kHz)
      [ -n "$FREQ" ] && Usage   # only one -100kHz or -10MHz is allowed
      FREQ="100kHz"
      PERIODE="10 us"
      ;;
    -10MHz)
      [ -n "$FREQ" ] && Usage   # only one -100kHz or -10MHz is allowed
      FREQ="10MHz"
      PERIODE="100 ns"
      ;;
    -vcd)
      [ -z "$OPCOND" ] && Usage    # -vcd requies -min|-max before it
      [ -z "$FREQ"   ] && Usage    # -vcd requies -100kHz|10MHz before it
      VCD="adt7310p16ls16l-sim-chip-route-${OPCOND}-${FREQ}.vcd"
      ;;
    -saif)
      [ -z "$OPCOND" ] && Usage    # -saif requies -min|-max before it
      [ -z "$FREQ"   ] && Usage    # -saif requies -100kHz|10MHz before it
      SAIF="adt7310p16ls16l-sim-chip-route-${OPCOND}-${FREQ}-" # will be appended with "${Mode}.saif"
      ;;
    *)
      echo "Error: Invalid parameter $1"
      Usage
      ;;
  esac
  shift
done

# at least the operating condition must be specified
[ -z "$OPCOND" ] && Usage

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
vsim -t 1ps -voptargs="+acc -suppress 2685,2718" -sdf$OPCOND /chip_tb/DUT=$SDF +sdf_verbose -sdfnoerror -GClkPeriode="$PERIODE" chip_tb
do wave.do
set PMEM_REG sim:/chip_tb/DUT/core_1/PMem_0/ram_0/memory
do firmware-ram0.do
set PMEM_REG sim:/chip_tb/DUT/core_1/PMem_0/ram_1/memory
do firmware-ram1.do
set PMEM_REG sim:/chip_tb/DUT/core_1/PMem_0/ram_2/memory
do firmware-ram2.do
set PMEM_REG sim:/chip_tb/DUT/core_1/PMem_0/ram_3/memory
do firmware-ram3.do

EOF

if [ -n "$VCD" ] ; then
  cat <<- EOF >> vsim.do

  # Value Change Dump
  vcd file $VCD
  vcd comment "ADT7310P16LS16L Reconf. Ex.App sim-chip-route"
  vcd add  -r -optcells /chip_tb/DUT/*

EOF
fi
if [ -n "$SAIF" ] ; then
  cat <<- EOF >> vsim.do

  # power analysis
  do power-modes-${FREQ}.tcl
  do power-funcs.tcl
  PowerSetup "$SAIF" "modes" 
  power add -r /chip_tb/DUT/*  
  power off

EOF
fi

cat <<- EOF >> vsim.do
#log -r /*

# resume after "run -all" after the "Finished"-assertion has occured
set broken 0
onbreak {
  set broken 1
  resume
}

# avoid warnings of numeric_std functions with 'X' values, see http://www.ht-lab.com/howto/modelsim/Modelsim_tips.html
set StdArithNoWarnings 1
run 0 ns;
set StdArithNoWarnings 0

run -all
#run 300us
EOF

if [ -n "$VCD" ] ; then
  cat <<- EOF >> vsim.do
  vcd flush
  quit
EOF
fi
if [ -n "$SAIF" ] ; then
  cat <<- EOF >> vsim.do

  PowerModeStopAll
  quit
EOF
fi

#cat vsim.do
#exit
#vsim -do vsim.do
