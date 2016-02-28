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
PKG_PATH=../../../vhdl_packs
UNITS_PATH="../../../units"
RECONF_PATH="$UNITS_PATH/reconfmodule"
RECONF_OUT_PATH="$RECONF_PATH/chll/out"
APPS_PATH=../../../apps/
OMSP_PATH="$UNITS_PATH/openmsp430"
CFGINTF_PATH="$UNITS_PATH/cfgintf"
PARAMINTF_PATH="$UNITS_PATH/paramintf"
SPI_PATH="$UNITS_PATH/simplespi"
C35_PATH="/path/to/ams/hitkit/3.80/verilog/c35b4"
MEM_PATH="../../../mem"
SYNTH_OUT_PATH="../../../synthesis/output"
ROUTE_OUT_PATH="../../../routing/output"

VIVADO_PATH="/path/to/Xilinx/Vivado/2014.2/"

VCOM_OPTS=""
#VCOM_OPTS="-preserve"
#VCOM_OPTS="+acc -fsmverbose"

#VLOG_OPTS="+define+ReconfModuleConstDrivers +define+SIMULATION +incdir+${OMSP_PATH}/verilog -lint -pedanticerrors"
VLOG_OPTS="+define+SIMULATION +incdir+${OMSP_PATH}/verilog +incdir+${RECONF_OUT} -lint -pedanticerrors"
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
  */sim-rtl|\
  */sim-blinki|\
  */sim-gpio|\
  */sim-chip-blinki|\
  */sim-ams-blinki|\
  */sim-chip-dbg_i2c|\
  */sim-chip-dbg_i2c-fpga|\
  */sim-chip-dbg_i2c-gate|\
  */sim-chip-dbg_i2c-route|\
  */sim-ams-dbg_i2c|\
  */sim-testcfgintf|\
  */sim-testparamintf|\
  */sim-uart|\
  */sim-adt7310-cpu|\
  */sim-adt7310-cpu-route|\
  */sim-eval-cpu)
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

function CompileBusMaster() {
  vcom ${VCOM_OPTS} -work work -87    $PKG_PATH/reconfigpkg-p.vhd
  vcom ${VCOM_OPTS} -work work -87    $PKG_PATH/uartpkg-p.vhd
  vcom ${VCOM_OPTS} -work work -87    $PKG_PATH/busmasters-p.vhd
  vcom ${VCOM_OPTS} -work work -87    $UNITS_PATH/fifo/vhdl/fifodualportram-e.vhd
  vcom ${VCOM_OPTS} -work work -87    $UNITS_PATH/fifo/vhdl/fifodualportram-rtl-a.vhd
  vcom ${VCOM_OPTS} -work work -87    $UNITS_PATH/fifo/vhdl/fifobinarycounter-e.vhd
  vcom ${VCOM_OPTS} -work work -87    $UNITS_PATH/fifo/vhdl/fifobinarycounter-rtl-a.vhd
  vcom ${VCOM_OPTS} -work work -87    $UNITS_PATH/fifo/vhdl/fifosynccmp-e.vhd
  vcom ${VCOM_OPTS} -work work -87    $UNITS_PATH/fifo/vhdl/fifosynccmp-rtl-a.vhd
  vcom ${VCOM_OPTS} -work work -87    $UNITS_PATH/fifo/vhdl/fifosynctop-e.vhd
  vcom ${VCOM_OPTS} -work work -87    $UNITS_PATH/fifo/vhdl/fifosynctop-structure-a.vhd
  vcom ${VCOM_OPTS} -work work -93    $UNITS_PATH/i2c_master/vhdl/clkdiv-e.vhd
  vcom ${VCOM_OPTS} -work work -93    $UNITS_PATH/i2c_master/vhdl/clkdiv-rtl-a.vhd
  vcom ${VCOM_OPTS} -work work -93    $UNITS_PATH/i2c_master/vhdl/i2ccore-e.vhd
  vcom ${VCOM_OPTS} -work work -93    $UNITS_PATH/i2c_master/vhdl/i2ccore-rtl-a.vhd
  vcom ${VCOM_OPTS} -work work -93    $UNITS_PATH/i2c_master/vhdl/i2c_master-e.vhd
  vcom ${VCOM_OPTS} -work work -93    $UNITS_PATH/i2c_master/vhdl/i2c_master-struct-a.vhd
  vcom ${VCOM_OPTS} -work work -93    $UNITS_PATH/i2c_master/vhdl/i2ctransfercontroller-e.vhd
  vcom ${VCOM_OPTS} -work work -93    $UNITS_PATH/i2c_master/vhdl/i2ctransfercontroller-rtl-a.vhd
}

function CompileReconfModule() {
  # Reconf.Module
  #vcom ${VCOM_OPTS} -work work -93 -noindexcheck  $RECONF_OUT_PATH/reconflogic.vhd
  vcom ${VCOM_OPTS} -work work -93    $RECONF_OUT_PATH/reconflogic-e.vhd
}

function CompileAMSRAM() {
  # 2k x 8 size optimized RAM for PMem
  vlog ${VLOG_OPTS} -work work        $MEM_PATH/sram2kx8/sram2kx8.v
  # 128 x 8 RAM for DMem
  vcom ${VCOM_OPTS} -work work -93    $MEM_PATH/memgen128x8/vhdl/sram128x8_pkg.vhd
  vcom ${VCOM_OPTS} -work work -93    $MEM_PATH/memgen128x8/vhdl/sram128x8.vhd
  vcom ${VCOM_OPTS} -work work -93    $MEM_PATH/memgen128x8/vhdl/sram128x8_comp.vhd
  # wrapper
  #vcom ${VCOM_OPTS} -work work -93    $APPVHDL_PATH/pmem-ams.vhd
  vcom ${VCOM_OPTS} -work work -93    $APPVHDL_PATH/pmem-ams-latch.vhd
  vcom ${VCOM_OPTS} -work work -93    $APPVHDL_PATH/dmem-ams.vhd
  # alternative wrapper internally using OpenMSP430's tb/ram.v
  #vlog ${VLOG_OPTS} -work work        ${APPVLOG_PATH}/pmem-ram.v
  #vlog ${VLOG_OPTS} -work work        ${APPVLOG_PATH}/dmem-ram.v
}

function CompileOpenMSP430() {
  # OpenMSP430
  OLD_VLOG_OPTS="$VLOG_OPTS"
  VLOG_OPTS="$* +acc +define+SIMULATION +incdir+${OMSP_PATH}/verilog +incdir+${RECONF_OUT_PATH} -lint -pedanticerrors $VLOG_OPTS"
  vlog ${VLOG_OPTS} -work work        ${OMSP_PATH}/verilog/openMSP430_defines.v
  vlog ${VLOG_OPTS} -work work        ${OMSP_PATH}/verilog/openMSP430.v
  vlog ${VLOG_OPTS} -work work        ${OMSP_PATH}/verilog/omsp_frontend.v
  vlog ${VLOG_OPTS} -work work        ${OMSP_PATH}/verilog/omsp_execution_unit.v
  vlog ${VLOG_OPTS} -work work        ${OMSP_PATH}/verilog/omsp_register_file.v
  vlog ${VLOG_OPTS} -work work        ${OMSP_PATH}/verilog/omsp_alu.v
  vlog ${VLOG_OPTS} -work work        ${OMSP_PATH}/verilog/omsp_sfr.v
  vlog ${VLOG_OPTS} -work work        ${OMSP_PATH}/verilog/omsp_clock_module.v
  vlog ${VLOG_OPTS} -work work        ${OMSP_PATH}/verilog/omsp_mem_backbone.v
  vlog ${VLOG_OPTS} -work work        ${OMSP_PATH}/verilog/omsp_watchdog.v
  vlog ${VLOG_OPTS} -work work        ${OMSP_PATH}/verilog/omsp_dbg.v
  vlog ${VLOG_OPTS} -work work        ${OMSP_PATH}/verilog/omsp_dbg_uart.v
  vlog ${VLOG_OPTS} -work work        ${OMSP_PATH}/verilog/omsp_dbg_i2c.v
  vlog ${VLOG_OPTS} -work work        ${OMSP_PATH}/verilog/omsp_dbg_hwbrk.v
  vlog ${VLOG_OPTS} -work work        ${OMSP_PATH}/verilog/omsp_multiplier.v
  vlog ${VLOG_OPTS} -work work        ${OMSP_PATH}/verilog/omsp_sync_reset.v
  vlog ${VLOG_OPTS} -work work        ${OMSP_PATH}/verilog/omsp_sync_cell.v
  vlog ${VLOG_OPTS} -work work        ${OMSP_PATH}/verilog/omsp_scan_mux.v
  vlog ${VLOG_OPTS} -work work        ${OMSP_PATH}/verilog/omsp_and_gate.v
  vlog ${VLOG_OPTS} -work work        ${OMSP_PATH}/verilog/omsp_wakeup_cell.v
  vlog ${VLOG_OPTS} -work work        ${OMSP_PATH}/verilog/omsp_clock_gate.v
  # try process specific clock gate cell
  #vlog ${VLOG_OPTS} ${C35_PATH}/udp.v
  #vlog ${VLOG_OPTS} ${C35_PATH}/c35_CORELIB.v
  #vlog ${VLOG_OPTS} ${C35_PATH}/c35_IOLIB_4M.v
  #vlog ${VLOG_OPTS} -work work        ${OMSP_PATH}/verilog/omsp_clock_gate-dlsg1.v
  vlog ${VLOG_OPTS} -work work        ${OMSP_PATH}/verilog/omsp_clock_mux.v
  # OpenMSP430 Peripherals
  vlog ${VLOG_OPTS} -work work        ${OMSP_PATH}/verilog/omsp_gpio.v
  vlog ${VLOG_OPTS} -work work        ${OMSP_PATH}/verilog/omsp_timerA.v
  vlog ${VLOG_OPTS} -work work        ${OMSP_PATH}/verilog/omsp_uart.v
  # OpenMSP430 Debug Utility
  vlog ${VLOG_OPTS} -work work        ${OMSP_PATH}/tb/msp_debug.v

  vlog ${VLOG_OPTS} -work work        ${OMSP_PATH}/tb/ram.v

  #../tb/tb_openMSP430.v
  #../tb/io_cell.v

  VLOG_OPTS="$OLD_VLOG_OPTS"
}

function CompileCore() {
  CompileOpenMSP430 "$@"
  OLD_VLOG_OPTS="$VLOG_OPTS"
  VLOG_OPTS="$* +acc +define+SIMULATION +incdir+${OMSP_PATH}/verilog +incdir+${RECONF_OUT_PATH} -lint -pedanticerrors $VLOG_OPTS"

  # SimpleSPI
  vcom ${VCOM_OPTS} -work work -93    $SPI_PATH/vhdl/simplespi.vhd

  # Config Interface
  vcom ${VCOM_OPTS} -work work -93    $PKG_PATH/utils-p.vhd
  vcom ${VCOM_OPTS} -work work -93    $PKG_PATH/config-p.vhd
  vcom ${VCOM_OPTS} -work work -93    $CFGINTF_PATH/vhdl/cfgintf.vhd

  # Param Interface
  vcom ${VCOM_OPTS} -work work -93    $PARAMINTF_PATH/vhdl/paramintf.vhd
  vcom ${VCOM_OPTS} -work work -93    $PARAMINTF_PATH/vhdl/paramoutreg.vhd

  vlog ${VLOG_OPTS} -work work        $APPVLOG_PATH/master.v

  # Core
  vlog ${VLOG_OPTS} -work work        $APPVLOG_PATH/core.v

  VLOG_OPTS="$OLD_VLOG_OPTS"
}

function CompileXilinxUnisimVerilog() {
  # Generate list of required Xilinx cells:
  #  1) only compile the design netlist exported by Xilinx Vivado
  #  2) run ./sim.sh --> complains a lot about design units not found
  #  3) extract list of these missing cells
  #    $ sed -rn "/Instantiation/s/^.*'(.*)'.*$/\1/p" transcript | sort -u > xilinx-cells.txt
  export MODULES_FILE="./xilinx-cells.txt"
  export XILINX_SRC_PATH="$VIVADO_PATH/data/verilog/src"
  # create library unisim
  if [ ! -d unisim ] ; then
    echo "Creating library 'unisim'"
    vlib unisim
    vmap unisim unisim
  fi
  # There are a few different sets of sources:
  #  ./xeclib/*.v     "Xilinx Formal Library Component"
  #  ./unisims/*.v    "Xilinx Timing Simulation Library Component"
  #  ./retarget/*.v   "Xilinx Retarget Simulation Library"
  # where the second is way more complicated (184788 LOC vs. 40976) and
  # (probably?) also a lot slower in simulation.
  #
  # see also: http://www.xilinx.com/support/answers/56713.html

#  # Use fast "Xilinx Formal Library"
#  for i in $(cat "$MODULES_FILE") ; do
#    vlog ${VLOG_OPTS} -work unisim    "$XILINX_SRC_PATH/xeclib/${i}.v"
#  done
#  # Warning: ModelSim/QuestaSim initializes all signals inside the
#  # ReconfModule with 'X'. Two days of trying around didn't bring any
#  # solution. It seems that the combinational loops and not using timing
#  # information in the library cause the problems. For more information
#  # see ../sim-chip-fpga/sim.sh

  # Use "Xilinx Timing Simulation Library"
  # Note: you have to load "glbl" in addition to your design when running "vsim", see
  vlog ${VLOG_OPTS} -work work    "$XILINX_SRC_PATH/glbl.v"
  if [ ! -r "$MODULES_FILE" ] ; then
    # not enough information yet
    echo "Warning: The list of required Xilinx UniSim cells is not yet known."
    echo "  Compiling full set of cells. To speed up you can create a list of"
    echo "  required cells. See this script for instructions"
    # compile everything
    for i in $(ls -1 "$XILINX_SRC_PATH/unisims/") ; do
      vlog ${VLOG_OPTS} -work unisim    "$XILINX_SRC_PATH/unisims/${i}"
    done
  else
    for i in $(cat "$MODULES_FILE") ; do
      vlog ${VLOG_OPTS} -work unisim    "$XILINX_SRC_PATH/unisims/${i}.v"
    done
  fi
}

function CompileXilinxUnisimVHDL() {
  # Generate list of required Xilinx cells:
  # 1) only compile the design netlist exported by Xilinx Vivado
  # 2) run ./sim.sh --> complains a lot about design units not found
  # 3) extract list of these missing design units
  #   $ sed -rn "/Instantiation/s/^.*'(.*)'.*$/\1/p" transcript | sort -u > xilinx-cells.txt
  # 4)
  export MODULES_FILE="./xilinx-cells.txt"
  export XILINX_SRC_PATH="$VIVADO_PATH/data/vhdl/src/unisims"
  # create library unisim
  if [ ! -d unisim ] ; then
    echo "Creating library 'unisim'"
    vlib unisim
    vmap unisim unisim
  fi
  vcom ${VCOM_OPTS} -work unisim -93   "$XILINX_SRC_PATH/unisim_VPKG.vhd"
  vcom ${VCOM_OPTS} -work unisim -93   "$XILINX_SRC_PATH/unisim_VCOMP.vhd"
  if [ ! -r "$MODULES_FILE" ] ; then
    # not enough information yet
    echo "Warning: The list of required Xilinx UniSim cells is not yet known."
    echo "  Compiling full set of cells. To speed up you can create a list of"
    echo "  required cells. See this script for instructions"
    # compile everything
    for i in $(ls -1 "$XILINX_SRC_PATH/primitive/") ; do
      vcom ${VCOM_OPTS} -work unisim -93   "$XILINX_SRC_PATH/primitive/${i}"
    done
  else
    for i in $(cat "$MODULES_FILE") ; do
      vcom ${VCOM_OPTS} -work unisim -93   "$XILINX_SRC_PATH/primitive/${i}.vhd"
    done
  fi
}

function CompileUart() {
  vcom ${VCOM_OPTS} -work work -87    $PKG_PATH/uartpkg-p.vhd
  vcom ${VCOM_OPTS} -work work -87    $UNITS_PATH/fifo/vhdl/fifodualportram-e.vhd
  vcom ${VCOM_OPTS} -work work -87    $UNITS_PATH/fifo/vhdl/fifodualportram-rtl-a.vhd
  vcom ${VCOM_OPTS} -work work -87    $UNITS_PATH/fifo/vhdl/fifobinarycounter-e.vhd
  vcom ${VCOM_OPTS} -work work -87    $UNITS_PATH/fifo/vhdl/fifobinarycounter-rtl-a.vhd
  vcom ${VCOM_OPTS} -work work -87    $UNITS_PATH/fifo/vhdl/fifosynccmp-e.vhd
  vcom ${VCOM_OPTS} -work work -87    $UNITS_PATH/fifo/vhdl/fifosynccmp-rtl-a.vhd
  vcom ${VCOM_OPTS} -work work -87    $UNITS_PATH/fifo/vhdl/fifosynctop-e.vhd
  vcom ${VCOM_OPTS} -work work -87    $UNITS_PATH/fifo/vhdl/fifosynctop-structure-a.vhd
  vcom ${VCOM_OPTS} -work work -87    $APPTEST_PATH/uart/baudgenerator-e.vhd
  vcom ${VCOM_OPTS} -work work -87    $APPTEST_PATH/uart/baudgenerator-rtl-a.vhd
  vcom ${VCOM_OPTS} -work work -87    $APPTEST_PATH/uart/errorbit-e.vhd
  vcom ${VCOM_OPTS} -work work -87    $APPTEST_PATH/uart/errorbit-rtl-a.vhd
  vcom ${VCOM_OPTS} -work work -87    $APPTEST_PATH/uart/errorindicator-e.vhd
  vcom ${VCOM_OPTS} -work work -87    $APPTEST_PATH/uart/errorindicator-structure-a.vhd
  vcom ${VCOM_OPTS} -work work -87    $APPTEST_PATH/uart/rxdatastatemachine-e.vhd
  vcom ${VCOM_OPTS} -work work -87    $APPTEST_PATH/uart/rxdatastatemachine-rtl-a.vhd
  vcom ${VCOM_OPTS} -work work -87    $APPTEST_PATH/uart/rxmodule-e.vhd
  vcom ${VCOM_OPTS} -work work -87    $APPTEST_PATH/uart/rxmodule-structure-a.vhd
  vcom ${VCOM_OPTS} -work work -87    $APPTEST_PATH/uart/txdatastatemachine-e.vhd
  vcom ${VCOM_OPTS} -work work -87    $APPTEST_PATH/uart/txdatastatemachine-rtl-a.vhd
  vcom ${VCOM_OPTS} -work work -87    $APPTEST_PATH/uart/txmodule-e.vhd
  vcom ${VCOM_OPTS} -work work -87    $APPTEST_PATH/uart/txmodule-structure-a.vhd
  vcom ${VCOM_OPTS} -work work -87    $APPTEST_PATH/uart/uart-e.vhd
  vcom ${VCOM_OPTS} -work work -87    $APPTEST_PATH/uart/uart-structure-a.vhd
}

case "$SIM_MODE" in
  sim-rtl)
    echo "Unused, see the other sim-* directories"
    exit
    ;;
  sim-blinki|sim-gpio)
    vcom ${VCOM_OPTS} -work work -93    $RECONF_OUT_PATH/reconflogic-e.vhd
    vcom ${VCOM_OPTS} -work work -93    $APPVHDL_PATH/reconflogic-empty-a.vhd
    CompileCore "+define+HIER_MODULE=core_tb.DUT"
    vcom ${VCOM_OPTS} -work work -2002  $APPTEST_PATH/core_tb.vhd
    ;;
  sim-chip-blinki)
    vcom ${VCOM_OPTS} -work work -93    $RECONF_OUT_PATH/reconflogic-e.vhd
    vcom ${VCOM_OPTS} -work work -93    $APPVHDL_PATH/reconflogic-empty-a.vhd
    CompileCore "+define+HIER_MODULE=chip_tb.DUT.core_1"
    vcom ${VCOM_OPTS} -work work -93    $RECONF_OUT_PATH/chip-e.vhd
    vcom ${VCOM_OPTS} -work work -93    $RECONF_OUT_PATH/chip-fpga_top-a.vhd
    vcom ${VCOM_OPTS} -work work -2002  $APPTEST_PATH/chip_tb.vhd
    ;;
  sim-ams-blinki)
    vcom ${VCOM_OPTS} -work work -93    $RECONF_OUT_PATH/reconflogic-e.vhd
    vcom ${VCOM_OPTS} -work work -93    $APPVHDL_PATH/reconflogic-empty-a.vhd

    CompileAMSRAM
    CompileCore "+define+HIER_MODULE=core_tb.DUT +define+USE_AMS_RAM"

    vcom ${VCOM_OPTS} -work work -2002  $APPTEST_PATH/core_tb.vhd
    ;;
  sim-testcfgintf)
    # Test CfgIntf
    vcom ${VCOM_OPTS} -work work -93    $RECONF_OUT_PATH/reconflogic-e.vhd
    vcom ${VCOM_OPTS} -work work -93    $APPVHDL_PATH/reconflogic-testcfgintf-a.vhd
    vcom ${VCOM_OPTS} -work work -93    $APPVHDL_PATH/reconflogic-testcfgintf-cfg-c.vhd
    CompileCore "+define+HIER_MODULE=core_tb.DUT"
    vlog ${VLOG_OPTS} -work work        $APPVLOG_PATH/core-testcfgintf-c.v
    vcom ${VCOM_OPTS} -work work -2002  $APPTEST_PATH/core_tb.vhd
    vcom ${VCOM_OPTS} -work work -2002  $APPTEST_PATH/core_tb-testcfgintf-cfg-c.vhd
    ;;
  sim-testparamintf)
    # Test ParamIntf
    vcom ${VCOM_OPTS} -work work -93    $RECONF_OUT_PATH/reconflogic-e.vhd
    vcom ${VCOM_OPTS} -work work -93    $APPVHDL_PATH/reconflogic-testparamintf-a.vhd
    vcom ${VCOM_OPTS} -work work -93    $APPVHDL_PATH/reconflogic-testparamintf-cfg-c.vhd
    CompileCore "+define+HIER_MODULE=core_tb.DUT"
    vlog ${VLOG_OPTS} -work work        $APPVLOG_PATH/core-testparamintf-c.v
    vcom ${VCOM_OPTS} -work work -2002  $APPTEST_PATH/core_tb.vhd
    vcom ${VCOM_OPTS} -work work -2002  $APPTEST_PATH/core_tb-testparamintf-cfg-c.vhd
    ;;
  sim-chip-dbg_i2c)
    CompileBusMaster
    vcom ${VCOM_OPTS} -work work -93    $RECONF_OUT_PATH/reconflogic-e.vhd
    vcom ${VCOM_OPTS} -work work -93    $APPVHDL_PATH/reconflogic-empty-a.vhd
    CompileCore "+define+HIER_MODULE=chip_tb.DUT.core_1 +define+SkipI2CMaster"
    vcom ${VCOM_OPTS} -work work -93    $RECONF_OUT_PATH/chip-e.vhd
    vcom ${VCOM_OPTS} -work work -93    $RECONF_OUT_PATH/chip-fpga_top-a.vhd
    vcom ${VCOM_OPTS} -work work -2002  $PKG_PATH/hexfile-p.vhd
    vcom ${VCOM_OPTS} -work work -2002  $APPTEST_PATH/chip_dbg_i2c_tb.vhd
    ;;
  sim-chip-dbg_i2c-fpga)
    CompileBusMaster
    echo "Please rename the module \"i2c_master\" in the netlist to e.g. \"i2c_master_fpga\", because it conflicts with our RTL entity"
    if [ "$1" = "-verilog" ] ; then
      CompileXilinxUnisimVerilog
      # see ug835-vivado-tcl-commands.pdf Vivado Design Suite Tcl Command Reference Guide (UG835) p. 1292 "write_verilog"
      vlog ${VLOG_OPTS} -work work        ../zedboard/project_1.v.i2crename
      # leave a hint for ./sim.sh
      rm -f compiled.vhdl
      touch compiled.verilog
    elif [ "$1" = "-vhdl" ] ; then
      CompileXilinxUnisimVHDL
      # see ug835-vivado-tcl-commands.pdf Vivado Design Suite Tcl Command Reference Guide (UG835) p. 1295 "write_vhdl"
      vcom ${VCOM_OPTS} -work work -93    ../zedboard/project_1/project_1.sim/sim_1/synth/func/Chip_tb_func_synth.vhd
      rm -f compiled.verilog
      touch compiled.vhdl
    else
      echo "ERROR: Please specify \"-verilog\" or \"-vhdl\"."
      exit 1
    fi
    vcom ${VCOM_OPTS} -work work -2002  $PKG_PATH/hexfile-p.vhd
    vcom ${VCOM_OPTS} -work work -2002  $APPTEST_PATH/chip_dbg_i2c_tb.vhd
    ;;
  sim-chip-dbg_i2c-gate)
    # Post-Synthesis simulation
    # compile core and pad library
    vlog ${VLOG_OPTS} ${C35_PATH}/udp.v   
    vlog ${VLOG_OPTS} ${C35_PATH}/c35_CORELIB.v
    vlog ${VLOG_OPTS} ${C35_PATH}/c35_IOLIB_4M.v
    # 2k x 8 size optimized RAM for PMem
    vlog ${VLOG_OPTS} -work work        $MEM_PATH/sram2kx8/sram2kx8.v
    # 128 x 8 RAM for DMem
    vcom ${VCOM_OPTS} -work work -93    $MEM_PATH/memgen128x8/vhdl/sram128x8_pkg.vhd
    vcom ${VCOM_OPTS} -work work -93    $MEM_PATH/memgen128x8/vhdl/sram128x8.vhd
    vcom ${VCOM_OPTS} -work work -93    $MEM_PATH/memgen128x8/vhdl/sram128x8_comp.vhd
    # compile chip
    vlog ${VLOG_OPTS} -work work        $SYNTH_OUT_PATH/chip.v
    # testbench
    CompileBusMaster
    vcom ${VCOM_OPTS} -work work -2002  $PKG_PATH/hexfile-p.vhd
    vcom ${VCOM_OPTS} -work work -2002  $APPTEST_PATH/chip_dbg_i2c_tb.vhd
    ;;
  sim-chip-dbg_i2c-route)
    # Post-P&R simulation
    # compile core and pad library
    vlog ${VLOG_OPTS} ${C35_PATH}/udp.v   
    vlog ${VLOG_OPTS} ${C35_PATH}/c35_CORELIB.v
    vlog ${VLOG_OPTS} ${C35_PATH}/c35_IOLIB_4M.v
    # 2k x 8 size optimized RAM for PMem
    vlog ${VLOG_OPTS} -work work        $MEM_PATH/sram2kx8/sram2kx8.v
    # 128 x 8 RAM for DMem
    vcom ${VCOM_OPTS} -work work -93    $MEM_PATH/memgen128x8/vhdl/sram128x8_pkg.vhd
    vcom ${VCOM_OPTS} -work work -93    $MEM_PATH/memgen128x8/vhdl/sram128x8.vhd
    vcom ${VCOM_OPTS} -work work -93    $MEM_PATH/memgen128x8/vhdl/sram128x8_comp.vhd
    # compile chip
    vlog ${VLOG_OPTS} -work work        $ROUTE_OUT_PATH/chip.v
    # testbench
    CompileBusMaster
    vcom ${VCOM_OPTS} -work work -2002  $PKG_PATH/hexfile-p.vhd
    vcom ${VCOM_OPTS} -work work -2002  $APPTEST_PATH/chip_dbg_i2c_tb.vhd
    ;;
  sim-ams-dbg_i2c)
    CompileBusMaster
    vcom ${VCOM_OPTS} -work work -93    $RECONF_OUT_PATH/reconflogic-e.vhd
    vcom ${VCOM_OPTS} -work work -93    $APPVHDL_PATH/reconflogic-empty-a.vhd

    CompileAMSRAM
    CompileCore "+define+HIER_MODULE=chip_tb.DUT.core_1 +define+SkipI2CMaster +define+USE_AMS_RAM"

    vcom ${VCOM_OPTS} -work work -93    $RECONF_OUT_PATH/chip-e.vhd
    vcom ${VCOM_OPTS} -work work -93    $RECONF_OUT_PATH/chip-fpga_top-a.vhd
    vcom ${VCOM_OPTS} -work work -2002  $PKG_PATH/hexfile-p.vhd
    vcom ${VCOM_OPTS} -work work -2002  $APPTEST_PATH/chip_dbg_i2c_tb.vhd
    ;;
  sim-uart)
    CompileUart
    vcom ${VCOM_OPTS} -work work -93    $RECONF_OUT_PATH/reconflogic-e.vhd
    vcom ${VCOM_OPTS} -work work -93    $APPVHDL_PATH/reconflogic-empty-a.vhd
    CompileCore "+define+HIER_MODULE=core_uart_tb.DUT"
    vcom ${VCOM_OPTS} -work work -2002  $APPTEST_PATH/core_uart_tb.vhd
    ;;
  sim-adt7310-cpu)
    CompileUart
    vcom ${VCOM_OPTS} -work work -93    $RECONF_OUT_PATH/reconflogic-e.vhd
    vcom ${VCOM_OPTS} -work work -93    $APPVHDL_PATH/reconflogic-empty-a.vhd
    CompileCore "+define+HIER_MODULE=core_adt7310_tb.DUT"
    # ADT7310 sensor model
    vcom ${VCOM_OPTS} -work work -2002 $APPS_PATH/adt7310/tb/adt7310-model.vhd
    # testbench
    vcom ${VCOM_OPTS} -work work -2002  $APPTEST_PATH/core_adt7310_tb.vhd
    ;;
  sim-adt7310-cpu-route)
    # Post-P&R simulation of ADT7310 firmware testcase
    # compile core and pad library
    vlog ${VLOG_OPTS} ${C35_PATH}/udp.v   
    vlog ${VLOG_OPTS} ${C35_PATH}/c35_CORELIB.v
    vlog ${VLOG_OPTS} ${C35_PATH}/c35_IOLIB_4M.v
    # 2k x 8 size optimized RAM for PMem
    vlog ${VLOG_OPTS} -work work        $MEM_PATH/sram2kx8/sram2kx8.v
    # 128 x 8 RAM for DMem
    vcom ${VCOM_OPTS} -work work -93    $MEM_PATH/memgen128x8/vhdl/sram128x8_pkg.vhd
    vcom ${VCOM_OPTS} -work work -93    $MEM_PATH/memgen128x8/vhdl/sram128x8.vhd
    vcom ${VCOM_OPTS} -work work -93    $MEM_PATH/memgen128x8/vhdl/sram128x8_comp.vhd
    # compile chip
    vlog ${VLOG_OPTS} -work work        $ROUTE_OUT_PATH/chip.v

    # external UART
    CompileUart
    # ADT7310 sensor model
    vcom ${VCOM_OPTS} -work work -2002 $APPS_PATH/adt7310/tb/adt7310-model.vhd
    # testbench
    vcom ${VCOM_OPTS} -work work -2002  $APPTEST_PATH/chip_adt7310_tb.vhd

#    CompileBusMaster
#    vcom ${VCOM_OPTS} -work work -2002  $PKG_PATH/hexfile-p.vhd
#    vcom ${VCOM_OPTS} -work work -2002  $APPTEST_PATH/chip_dbg_i2c_tb.vhd

    ;;
  sim-eval-cpu)
    # simulate setup of Test Chip Evaluation Platform with Testchip2, Testchip2-CPU firmware
    CompileUart
    vcom ${VCOM_OPTS} -work work -93    $RECONF_OUT_PATH/reconflogic-e.vhd
    vcom ${VCOM_OPTS} -work work -93    $APPVHDL_PATH/reconflogic-empty-a.vhd
    CompileBusMaster
    CompileCore "+define+HIER_MODULE=chip_eval_tb.DUT.core_1"
    # compile pad library
    vlog ${VLOG_OPTS} ${C35_PATH}/udp.v   
    vlog ${VLOG_OPTS} ${C35_PATH}/c35_IOLIB_4M.v
    vcom ${VCOM_OPTS} -work work -93    $RECONF_OUT_PATH/chip-e.vhd
    vcom ${VCOM_OPTS} -work work -93    $RECONF_OUT_PATH/chip-chip_top-a.vhd
    vcom ${VCOM_OPTS} -work work -2002  $APPTEST_PATH/chip_tb.vhd
    # ADT7310 sensor model
    vcom ${VCOM_OPTS} -work work -2002 $APPS_PATH/adt7310/tb/adt7310-model.vhd
    # testbench
    vcom ${VCOM_OPTS} -work work -2002  $APPTEST_PATH/chip_eval_tb.vhd
    ;;
  *)
    echo "Unknown simulation mode $SIM_MODE"
    exit 1
    ;;
esac

# Mixed VHDL+Verilog Configurations
# =================================
#
# This is quite complex, because VHDL configurations can't "configure into"
# Verilog modules. Therefore we have to setup several configurations.
#
# Example: Testing the CfgIntf
#   Hierarchy:
#    Core_tb(behavior)                       (VHDL, units/core/tb/core_tb.vhd)
#      DUT : Core                            (Verilog, units/core/verilog/core.v)
#        MyReconfigLogic_0 : MyReconfigLogic (Verilog, reconfmodule/chll/out/reconflogic-instance.inc.v)
#          MyReconfigLogic(TestCfgIntf)      (VHDL, units/core/vhdl/reconflogic-testcfgintf.vhd)
#
# To achieve this, we simulate the (VHDL) configuration Core_tb_TestCfgIntf_cfg
# defined in core_tb.vhd. This specifies the instance of DUT to use the
# configuration CoreTestCfgIntf, which is defined in vcfg.v. This specified the
# instance of MyReconfigLogic_0 to use the configuration
# MyReconfigLogicTestCfgIntf_cfg (using all lower case letters, because VHDL
# identifiers are translated), which is defined in reconflogic-testcfgintf.vhd.
# And this finally specifies to use the architecture TextCfgIntf for the module
# MyReconfigLogic.
#
# For Verilog Configurations see
#    http://www.eda.org/sv-bc/hm/att-2972/ADV01_Configs_51_55.pdf
#    http://www.altera.com/literature/hb/qts/qts_qii51008.pdf
#
# 2014-07-09: this is not used any more, all Wrap<App> simulations are done in apps/<app>/sim-core-rtl/

# create Makefile
echo "Writing Makefile"
vmake -nolinewrap > Makefile
