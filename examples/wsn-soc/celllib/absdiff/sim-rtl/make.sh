#!/bin/bash
#
# Create a Makefile.
#
# This tool uses ModelSim's "vmake" to generate a Makefile. Therefore all
# VHDL files are first compiled by hand to tell ModelSim their order. Then
# the Makefile is generated.
#

CELLVHDL_PATH=../vhdl
CELLVLOG_PATH=../verilog
CELLTEST_PATH=../tb
TRFSMTEST_PATH=../../trfsm/tb

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

# create library work
if [ ! -d work ] ; then
  echo "Creating library 'work'"
  vlib work
  vmap work work
fi

# compile all files
vlog ${VLOG_OPTS} -work work        $CELLVLOG_PATH/absdiff.v
vcom ${VCOM_OPTS} -work work -2002  $TRFSMTEST_PATH/tbfuncs-p.vhd
vcom ${VCOM_OPTS} -work work -2002  $CELLTEST_PATH/absdiff_tb.vhd

# create Makefile
echo "Writing Makefile"
vmake -nolinewrap > Makefile
