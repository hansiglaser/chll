#!/bin/bash
#
# Create a Makefile.
#
# This tool uses ModelSim's "vmake" to generate a Makefile. Therefore all
# VHDL files are first compiled by hand to tell ModelSim their order. Then
# the Makefile is generated.
#

APPVHDL_PATH=../vhdl
APPVLOG_PATH=../verilog
APPTEST_PATH=../tb
PKG_PATH=../../../vhdl_packs

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
vcom ${VCOM_OPTS} -work work -93    $PKG_PATH/utils-p.vhd

vcom ${VCOM_OPTS} -work work -93    $APPVHDL_PATH/cfgintf.vhd

vcom ${VCOM_OPTS} -work work -93    $APPTEST_PATH/cfgintf_tb.vhd

# create Makefile
echo "Writing Makefile"
vmake -nolinewrap > Makefile
