#!/bin/bash
#
# Create a Makefile.
#
# This tool uses ModelSim's "vmake" to generate a Makefile. Therefore all
# VHDL files are first compiled by hand to tell ModelSim their order. Then
# the Makefile is generated.
#

# determine script base directory, since this might be a symbolic link, we
# can't use "readlink" but have to use this rather non-elegant sub-shell
BASEDIR="$(dirname "$0")"
BASEDIR="$(cd "$BASEDIR" ; pwd)"

APPVHDL_PATH=../vhdl
APPVLOG_PATH=../verilog
APPTEST_PATH=../tb

VCOM_OPTS=""
#VCOM_OPTS="-preserve"
#VCOM_OPTS="+acc -fsmverbose"

VLOG_OPTS=""
#VLOG_OPTS="+acc -fsmverbose"

if ! which vlib > /dev/null ; then
  echo "ERROR: Couldn't find ModelSim executables."
  exit 1
fi

if [ -f Makefile ] ; then
  echo "ERROR: Makefile already exists. Please remove it before calling $0"
  exit 1
fi

# check whether this is used for sim-rtl, sim-yosys, sim-yosys-fsm or sim-yosys-trfsm
case "$BASEDIR" in
  */sim-rtl)
    SIM_MODE="${BASEDIR##*/}"
    ;;
  *)
    echo "Can't determine simulation mode from base directory $BASEDIR."
    exit 1
    ;;
esac
echo "Simulation mode: $SIM_MODE"

# create library work
if [ ! -d work ] ; then
  echo "Creating library 'work'"
  vlib work
  vmap work work
fi

# compile all files

case "$SIM_MODE" in
  sim-rtl)
    vcom ${VCOM_OPTS} -work work -93    $APPVHDL_PATH/simplespi.vhd
    vcom ${VCOM_OPTS} -work work -2002  $APPTEST_PATH/simplespi_tb.vhd
    ;;
  *)
    echo "Unknown simulation mode $SIM_MODE"
    exit 1
    ;;
esac

# create Makefile
echo "Writing Makefile"
vmake -nolinewrap > Makefile
