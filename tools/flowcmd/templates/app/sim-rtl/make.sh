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
APPCHLL_PATH=../chll/out
CELLLIB_PATH=../../../celllib
PKG_PATH=../../../vhdl_packs
UNITS_PATH="../../../units"
RECONF_PATH="$UNITS_PATH/reconfmodule"
RECONF_OUT_PATH="$RECONF_PATH/chll/out"
CORE_PATH="$UNITS_PATH/core"
CFGINTF_PATH="$UNITS_PATH/cfgintf"
PARAMINTF_PATH="$UNITS_PATH/paramintf"
SYNTH_OUT_PATH="../../../synthesis/output"
ROUTE_OUT_PATH="../../../routing/output"

TRFSM_VHDL_PATH=$CELLLIB_PATH/trfsm/vhdl
YOSYS_PATH=/path/to/yosys

VIVADO_PATH="/opt/Xilinx/Vivado/2014.2/"

C35_PATH="/path/to/ams/hitkit/3.80/verilog/c35b4"
MEM_PATH="../../../mem"

# determine name of the application
CHLL_APP_FILE="$BASEDIR/../.chll-app"
if [ ! -r "$CHLL_APP_FILE" ] ; then
  echo "Error: Can't find CHLL App file $CHLL_APP_FILE"
  exit 1
fi
APP_NAME="$(cat $CHLL_APP_FILE)"
APP_NAME_LC="${APP_NAME,,}"    # lower case

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

# check whether this is used for sim-rtl, sim-yosys, sim-yosys-fsm, sim-yosys-trfsm, ...
case "$BASEDIR" in
  */sim-rtl|\
  */sim-yosys|\
  */sim-yosys-fsm|\
  */sim-yosys-trfsm|\
  */sim-yosys-extract|\
  */sim-yosys-extract-trfsm|\
  */sim-yosys-extract-intersynth|\
  */sim-intersynth|\
  */sim-reconfmodule|\
  */sim-core-rtl|\
  */sim-core-reconfmodule|\
  */sim-chip-rtl|\
  */sim-chip-reconfmodule|\
  */sim-chip-fpga|\
  */sim-chip-gate|\
  */sim-chip-route)
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

function CompileYosysLibs() {
  vlog ${VLOG_OPTS} -work work        $YOSYS_PATH/techlibs/common/simcells.v
  vlog ${VLOG_OPTS} -work work        $YOSYS_PATH/techlibs/common/simlib.v
}
function CompileConfigPkg() {
  if [ -z "$DidConfigPkg" ] ; then
    vcom ${VCOM_OPTS} -work work -87    $PKG_PATH/config-p.vhd
    DidConfigPkg="true"
  fi
}
function CompileTRFSM() {
  # TR-FSM
  CompileConfigPkg
  vcom ${VCOM_OPTS} -work work -93    $TRFSM_VHDL_PATH/trfsmparts-p.vhd
  vcom ${VCOM_OPTS} -work work -93    $TRFSM_VHDL_PATH/ConfigRegister-e.vhd
  vcom ${VCOM_OPTS} -work work -93    $TRFSM_VHDL_PATH/ConfigRegister-rtl-a.vhd
  vcom ${VCOM_OPTS} -work work -93    $TRFSM_VHDL_PATH/InputPatternGate-e.vhd
  vcom ${VCOM_OPTS} -work work -93    $TRFSM_VHDL_PATH/InputPatternGate-rtl-a.vhd
  vcom ${VCOM_OPTS} -work work -93    $TRFSM_VHDL_PATH/InputSwitchingMatrix-e.vhd
  vcom ${VCOM_OPTS} -work work -93    $TRFSM_VHDL_PATH/InputSwitchingMatrix-rtl-a.vhd
  vcom ${VCOM_OPTS} -work work -93    $TRFSM_VHDL_PATH/LargeMux-e.vhd
  vcom ${VCOM_OPTS} -work work -93    $TRFSM_VHDL_PATH/LargeMux-rtl-a.vhd
  vcom ${VCOM_OPTS} -work work -93    $TRFSM_VHDL_PATH/StateRegister-e.vhd
  vcom ${VCOM_OPTS} -work work -93    $TRFSM_VHDL_PATH/StateRegister-rtl-a.vhd
  vcom ${VCOM_OPTS} -work work -93    $TRFSM_VHDL_PATH/StateSelectionGate-e.vhd
  vcom ${VCOM_OPTS} -work work -93    $TRFSM_VHDL_PATH/StateSelectionGate-rtl-a.vhd
  vcom ${VCOM_OPTS} -work work -93    $TRFSM_VHDL_PATH/TransitionRow-e.vhd
  vcom ${VCOM_OPTS} -work work -93    $TRFSM_VHDL_PATH/TransitionRow-struct-a.vhd
  vcom ${VCOM_OPTS} -work work -93    $TRFSM_VHDL_PATH/trfsm-e.vhd
  vcom ${VCOM_OPTS} -work work -93    $TRFSM_VHDL_PATH/trfsm-struct-a.vhd
}
function CompileCellLib() {
  # cell library
#$#  vlog ${VLOG_OPTS} -work work        $CELLLIB_PATH/<cell1>/verilog/<cell1>.v
#$#  vlog ${VLOG_OPTS} -work work        $CELLLIB_PATH/<cell1>/verilog/<cell1>.v
}
function CompileInterSynth() {
  # wrapper and new module
#$#  vcom ${VCOM_OPTS} -work work -93    $RECONF_OUT_PATH/<trfsmN>-wrapper.vhd
#$#  vcom ${VCOM_OPTS} -work work -93    $RECONF_OUT_PATH/<trfsmN>-wrapper.vhd
  # Standard cells for InterSynth module
  vlog ${VLOG_OPTS} -work work        $RECONF_OUT_PATH/presilicon-stdcells.v
  # InterSynth module
  vlog ${VLOG_OPTS} -work work        $RECONF_OUT_PATH/presilicon.v
}
function CompilePeriphIntf() {
  # Config Interface
  CompileConfigPkg
  vcom ${VCOM_OPTS} -work work -93    $PKG_PATH/utils-p.vhd
  vcom ${VCOM_OPTS} -work work -93    $CFGINTF_PATH/vhdl/cfgintf.vhd
  
  # Param Interface
  vcom ${VCOM_OPTS} -work work -93    $PARAMINTF_PATH/vhdl/paramintf.vhd
  vcom ${VCOM_OPTS} -work work -93    $PARAMINTF_PATH/vhdl/paramoutreg.vhd
}
function CompileReconfModule() {
  CompilePeriphIntf
  # Reconf.Module
  vcom ${VCOM_OPTS} -work work -93 -noindexcheck  $RECONF_OUT_PATH/reconflogic.vhd
}
function CompileCore() {
  OLD_VLOG_OPTS="$VLOG_OPTS"
  VLOG_OPTS="+define+SIMULATION +incdir+${RECONF_OUT_PATH} -lint -pedanticerrors $VLOG_OPTS"
  
  # Core
  vlog ${VLOG_OPTS} -work work        $CORE_PATH/verilog/core.v

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

# files used for all simulation modes
#$# vcom ${VCOM_OPTS} -work work -87   filename.vhd

# application design
case "$SIM_MODE" in
  sim-rtl)
    # sim-rtl: use user's Verilog code
    vlog ${VLOG_OPTS} -work work        $APPVLOG_PATH/${APP_NAME_LC}.v
    vlog ${VLOG_OPTS} -work work        $APPTEST_PATH/extnames.v
    ;;
  sim-yosys)
    # sim-yosys: use Yosys synthesis output
    CompileYosysLibs
    vlog ${VLOG_OPTS} -work work        $APPCHLL_PATH/${APP_NAME_LC}.v
    vlog ${VLOG_OPTS} -work work        $APPTEST_PATH/extnames.v
    ;;
  sim-yosys-fsm)
    # sim-yosys-fsm: use Yosys synthesis output with extracted FSMs as $fsm cells
    CompileYosysLibs    # use $fsm module definition from Yosys's simlib
    vlog ${VLOG_OPTS} -work work        $APPCHLL_PATH/${APP_NAME_LC}-fsm.v
    vlog ${VLOG_OPTS} -work work        $APPTEST_PATH/extnames.v
    ;;
  sim-yosys-trfsm)
    # sim-yosys-trfsm: use Yosys synthesis output with extracted FSMs replaced by TR-FSMs
    CompileYosysLibs    # Yosys's simlib for all cells except $fsm
    CompileTRFSM
    # wrapper and new module
    #$# vcom ${VCOM_OPTS} -work work -93   $APPCHLL_PATH/xyzfsm-wrapper.vhd
    # top module
    vlog ${VLOG_OPTS} -work work        $APPCHLL_PATH/${APP_NAME_LC}-trfsm.v
    vlog ${VLOG_OPTS} -work work        $APPTEST_PATH/extnames.v
    ;;
  sim-yosys-extract)
    # sim-yosys-extract: use Yosys synthesis output with extracted FSMs as $fsm
    # cells and extracted cells from the cell library
    CompileYosysLibs    # use $fsm module definition from Yosys's simlib
    CompileTRFSM
    CompileCellLib
    # module
    vlog ${VLOG_OPTS} -work work        $APPCHLL_PATH/${APP_NAME_LC}-extract.v
    vlog ${VLOG_OPTS} -work work        $APPTEST_PATH/extnames.v
    ;;
  sim-yosys-extract-trfsm)
    # sim-yosys-extract-trfsm: use Yosys synthesis output with extracted FSMs replaced by TR-FSMs
    CompileYosysLibs    # Yosys's simlib for all cells except $fsm
    CompileTRFSM
    CompileCellLib
    # wrapper and new module
    #$# vcom ${VCOM_OPTS} -work work -93   $APPCHLL_PATH/${APP_NAME_LC}-extract-xyzfsm-wrapper.vhd
    # top module
    vlog ${VLOG_OPTS} -work work        $APPCHLL_PATH/${APP_NAME_LC}-extract-trfsm.v
    vlog ${VLOG_OPTS} -work work        $APPTEST_PATH/extnames.v
    ;;
  sim-yosys-extract-intersynth)
    # sim-yosys-extract-intersynth: use Yosys synthesis output with extracted
    # FSMs replaced by the final TR-FSMs used by InterSynth
    CompileYosysLibs    # Yosys's simlib for all cells except $fsm
    CompileTRFSM
    CompileCellLib
    # wrapper and new module
    #$# vcom ${VCOM_OPTS} -work work -93   $RECONF_OUT_PATH/<trfsmM>-wrapper.vhd
    # top module
    vlog ${VLOG_OPTS} -work work        $APPCHLL_PATH/${APP_NAME_LC}-extract-intersynth.v
    vlog ${VLOG_OPTS} -work work        $APPTEST_PATH/extnames.v
    ;;
  sim-intersynth)
    # sim-intersynth: use wrapped InterSynth module looking like this
    # application module
    CompileYosysLibs    # Yosys's simlib for all cells except $fsm
    CompileTRFSM
    CompileCellLib
    CompileInterSynth
    # top module: wrapper
    vcom ${VCOM_OPTS} -work work -93    $APPCHLL_PATH/${APP_NAME_LC}-wrapintersynth.vhd
    vlog ${VLOG_OPTS} -work work        $APPCHLL_PATH/extnames-intersynth.v
    ;;
  sim-reconfmodule)
    # sim-reconfmodule: use wrapped ReconfModule module looking like this
    # application module
    CompileYosysLibs    # Yosys's simlib for all cells except $fsm
    CompileTRFSM
    CompileCellLib
    CompileInterSynth
    CompileReconfModule
    # top module: wrapper
    vcom ${VCOM_OPTS} -work work -2008  $APPCHLL_PATH/${APP_NAME_LC}-wrapreconfmodule-vhdl2008.vhd
    vlog ${VLOG_OPTS} -work work        $APPCHLL_PATH/extnames-reconfmodule.v
    ;;
  sim-core-rtl)
    # sim-core-rtl: use user's Verilog code
    vlog ${VLOG_OPTS} -work work        $APPVLOG_PATH/${APP_NAME_LC}.v

    CompilePeriphIntf
    # application wrapper
    vcom ${VCOM_OPTS} -work work -93 -noindexcheck $APPCHLL_PATH/reconflogic-wrap${APP_NAME_LC}.vhd
    # core
    CompileCore

    # access internal signals
    vlog ${VLOG_OPTS} -work work        $APPTEST_PATH/extnames-core-rtl.v
    
    ;;
  sim-core-reconfmodule)
    # sim-core-reconfmodule: use generated reconfigurable module, chip core is the top level
    CompileYosysLibs    # Yosys's simlib for all cells except $fsm
    CompileTRFSM
    CompileCellLib
    CompileInterSynth
    CompileReconfModule
    CompileCore

    # access internal signals
    vlog ${VLOG_OPTS} -work work        $APPCHLL_PATH/extnames-core-reconfmodule.v

    ;;
  sim-chip-rtl)
    # sim-chip-rtl: use user's Verilog code, chip including pads is the top level
    vlog ${VLOG_OPTS} -work work        $APPVLOG_PATH/${APP_NAME_LC}.v

    CompilePeriphIntf
    # application wrapper
    vcom ${VCOM_OPTS} -work work -93 -noindexcheck $APPCHLL_PATH/reconflogic-wrap${APP_NAME_LC}.vhd
    # core
    CompileCore

    vcom ${VCOM_OPTS} -work work -93    $RECONF_OUT_PATH/chip-e.vhd
    vcom ${VCOM_OPTS} -work work -93    $RECONF_OUT_PATH/chip-fpga_top-a.vhd

    # access internal signals
    vlog ${VLOG_OPTS} -work work        $APPTEST_PATH/extnames-chip-rtl.v
    ;;
  sim-chip-reconfmodule)
    # sim-chip-reconfmodule: use generated reconfigurable module, chip including pads is the top level
    CompileYosysLibs    # Yosys's simlib for all cells except $fsm
    CompileTRFSM
    CompileCellLib
    CompileInterSynth
    CompileReconfModule
    CompileCore

    vcom ${VCOM_OPTS} -work work -93    $RECONF_OUT_PATH/chip-e.vhd
    vcom ${VCOM_OPTS} -work work -93    $RECONF_OUT_PATH/chip-fpga_top-a.vhd

    # access internal signals
    vlog ${VLOG_OPTS} -work work        $APPCHLL_PATH/extnames-chip-reconfmodule.v
    ;;
  sim-chip-fpga)
    # sim-chip-fpga: FPGA gate-level netlist simulation
    if [ "$1" = "-verilog" ] ; then
      CompileXilinxUnisimVerilog
      # see ug835-vivado-tcl-commands.pdf Vivado Design Suite Tcl Command Reference Guide (UG835) p. 1292 "write_verilog"
      vlog ${VLOG_OPTS} -work work        ../../../units/core/zedboard/project_1.v.ori
      # leave a hint for ./sim.sh
      rm -f compiled.vhdl
      touch compiled.verilog
    elif [ "$1" = "-vhdl" ] ; then
      CompileXilinxUnisimVHDL
      # see ug835-vivado-tcl-commands.pdf Vivado Design Suite Tcl Command Reference Guide (UG835) p. 1295 "write_vhdl"
      vcom ${VCOM_OPTS} -work work -93    ../../../units/core/zedboard/project_1/project_1.sim/sim_1/synth/func/Chip_tb_func_synth.vhd
      rm -f compiled.verilog
      touch compiled.vhdl
    else
      echo "ERROR: Please specify \"-verilog\" or \"-vhdl\"."
      exit 1
    fi
    # access internal signals
    vlog ${VLOG_OPTS} -work work        $APPCHLL_PATH/extnames-chip-reconfmodule.v
    ;;
  sim-chip-gate)
    # sim-chip-gate: chip Post-Synthesis gate-level netlist simulation
    # compile core and pad library
    vlog ${VLOG_OPTS} ${C35_PATH}/udp.v   
    vlog ${VLOG_OPTS} ${C35_PATH}/c35_CORELIB.v
    vlog ${VLOG_OPTS} ${C35_PATH}/c35_IOLIB_4M.v
    # compile chip
    vlog ${VLOG_OPTS} -work work        $SYNTH_OUT_PATH/chip.v
    # access internal signals
    vlog ${VLOG_OPTS} -work work        $APPCHLL_PATH/extnames-chip-reconfmodule.v
    ;;
  sim-chip-route)
    # sim-chip-route: chip Post-P&R gate-level netlist simulation
    # compile core and pad library
    vlog ${VLOG_OPTS} ${C35_PATH}/udp.v   
    vlog ${VLOG_OPTS} ${C35_PATH}/c35_CORELIB.v
    vlog ${VLOG_OPTS} ${C35_PATH}/c35_IOLIB_4M.v
    # compile chip
    vlog ${VLOG_OPTS} -work work        $ROUTE_OUT_PATH/chip.v
    # access internal signals
    vlog ${VLOG_OPTS} -work work        $APPCHLL_PATH/extnames-chip-reconfmodule.v
    ;;
  *)
    echo "Unknown simulation mode $SIM_MODE"
    exit 1
    ;;
esac

# Peripheral models
#$# vcom ${VCOM_OPTS} -work work -2002  $APPTEST_PATH/xyz-model.vhd
# testbench
if [ -z "${SIM_MODE%%sim-core-*}" ] ; then
  # sim-core-*
  vcom ${VCOM_OPTS} -work work -93    $APPTEST_PATH/core_${APP_NAME_LC}_tb.vhd
elif [ -z "${SIM_MODE%%sim-chip-*}" ] ; then
  # sim-chip-*
  vcom ${VCOM_OPTS} -work work -93    $APPTEST_PATH/chip_${APP_NAME_LC}_tb.vhd
else
  # sim-*
  vcom ${VCOM_OPTS} -work work -2008  $APPTEST_PATH/${APP_NAME_LC}_tb.vhd
fi

# special configuration to disconnect the ParamOutReg modules
if [ "$SIM_MODE" == "sim-reconfmodule" ] ; then
  vcom ${VCOM_OPTS} -work work -2008  $APPTEST_PATH/wrapreconfmodule-c.vhd
fi

# create Makefile
echo "Writing Makefile"
vmake -nolinewrap > Makefile
