#!yosys
#
# Characterize parent module which instantiates the reconfigurable module
#
# This file is executed through FlowCmd.
#
# Required Environment Variables:
#   UNIT_BASE     base directory of the reconfmodule unit 
#   STUB_FILE     destination file name for the stub nets list of the parent module
#   PARENT_ILANG  destination file name for the netlist in ILang format of the parent module
#
# Required Yosys Plugins:
#   stubnets.so
#   ports.so
#

# remove the following two lines
#puts "ERROR: You have to edit this file [info script] before executing."
#exit 1

# Configure
set PARENT_MODULE "Core"

# get data from outside this script
set UNIT_BASE "$env(UNIT_BASE)"
set STUB_FILE "$env(STUB_FILE)"
set PARENT_ILANG "$env(PARENT_ILANG)"

# setup variables
set VLOG_DIR $UNIT_BASE/../core/verilog
set OMSP_DIR $UNIT_BASE/../openmsp430/verilog

# setup TCL commands to be equal to the yosys commands (except "proc" -> "procs")
yosys -import

# read HDL
# OpenMSP430
read_verilog -lib $OMSP_DIR/openMSP430_defines.v
read_verilog -lib $OMSP_DIR/openMSP430.v
# read_verilog $OMSP_DIR/omsp_frontend.v
# read_verilog $OMSP_DIR/omsp_execution_unit.v
# read_verilog $OMSP_DIR/omsp_register_file.v
# read_verilog $OMSP_DIR/omsp_alu.v
# read_verilog $OMSP_DIR/omsp_sfr.v
# read_verilog $OMSP_DIR/omsp_clock_module.v
# read_verilog $OMSP_DIR/omsp_mem_backbone.v
# read_verilog $OMSP_DIR/omsp_watchdog.v
# read_verilog $OMSP_DIR/omsp_dbg.v
# read_verilog $OMSP_DIR/omsp_dbg_uart.v
# read_verilog $OMSP_DIR/omsp_dbg_i2c.v
# read_verilog $OMSP_DIR/omsp_dbg_hwbrk.v
# read_verilog $OMSP_DIR/omsp_multiplier.v
# read_verilog $OMSP_DIR/omsp_sync_reset.v
# read_verilog $OMSP_DIR/omsp_sync_cell.v
# read_verilog $OMSP_DIR/omsp_scan_mux.v
# read_verilog $OMSP_DIR/omsp_and_gate.v
# read_verilog $OMSP_DIR/omsp_wakeup_cell.v
# read_verilog $OMSP_DIR/omsp_clock_gate.v
# read_verilog $OMSP_DIR/omsp_clock_mux.v
# OpenMSP430 RAMs
read_verilog -lib $OMSP_DIR/../tb/ram.v
# OpenMSP430 Peripherals
read_verilog -lib $OMSP_DIR/omsp_gpio.v
read_verilog -lib $OMSP_DIR/omsp_timerA.v
read_verilog -lib $OMSP_DIR/omsp_uart.v

# SimpleSPI
read_verilog -lib $VLOG_DIR/simplespi.v   ;# empty module definition

# core
read_verilog -lib $VLOG_DIR/master.v   ;# empty module definitions for bus masters
read_verilog -DReconfModuleNone -I$OMSP_DIR $VLOG_DIR/core.v

# build parametric modules as needed
hierarchy

# Replace processes by MUXes and D-FFs
procs

# Optimize without opt_muxtree to keep the SENT and SPC output MUXes!
opt_const
opt_share -nomux
#opt_muxtree
opt_reduce
opt_share
opt_rmdff
opt_clean
opt_const

# Determine unused ports in the top module
stubnets -o "$STUB_FILE" "$PARENT_MODULE"

# write netlist
write_ilang "$PARENT_ILANG"

