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
OMSP_PATH="$UNITS_PATH/openmsp430"
CFGINTF_PATH="$UNITS_PATH/cfgintf"
PARAMINTF_PATH="$UNITS_PATH/paramintf"

TRFSM_VHDL_PATH=$CELLLIB_PATH/trfsm/vhdl
YOSYS_PATH=/path/to/yosys

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
  */sim-core-reconfmodule)
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
  vlog ${VLOG_OPTS} -work work        $CELLLIB_PATH/absdiff/verilog/absdiff.v
  vlog ${VLOG_OPTS} -work work        $CELLLIB_PATH/addsubcmp/verilog/addsubcmp.v
  vlog ${VLOG_OPTS} -work work        $CELLLIB_PATH/byte2word/verilog/byte2word.v
  vlog ${VLOG_OPTS} -work work        $CELLLIB_PATH/bytemuxoct/verilog/bytemuxoct.v
  vlog ${VLOG_OPTS} -work work        $CELLLIB_PATH/bytemuxquad/verilog/bytemuxquad.v
  vlog ${VLOG_OPTS} -work work        $CELLLIB_PATH/bytemuxdual/verilog/bytemuxdual.v
  vlog ${VLOG_OPTS} -work work        $CELLLIB_PATH/byteregister/verilog/byteregister.v
  vlog ${VLOG_OPTS} -work work        $CELLLIB_PATH/counter/verilog/counter.v
  vlog ${VLOG_OPTS} -work work        $CELLLIB_PATH/counter32/verilog/counter32.v
  vlog ${VLOG_OPTS} -work work        $CELLLIB_PATH/wordregister/verilog/wordregister.v
  vlog ${VLOG_OPTS} -work work        $CELLLIB_PATH/byte2wordsel/verilog/byte2wordsel.v
  vlog ${VLOG_OPTS} -work work        $CELLLIB_PATH/wordmuxdual/verilog/wordmuxdual.v
}
function CompileInterSynth() {
  # wrapper and new module
  vcom ${VCOM_OPTS} -work work -93    $RECONF_OUT_PATH/trfsm0-wrapper.vhd
  vcom ${VCOM_OPTS} -work work -93    $RECONF_OUT_PATH/trfsm1-wrapper.vhd
  vcom ${VCOM_OPTS} -work work -93    $RECONF_OUT_PATH/trfsm2-wrapper.vhd
  vcom ${VCOM_OPTS} -work work -93    $RECONF_OUT_PATH/trfsm3-wrapper.vhd
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
  # Reconf.Module
  vcom ${VCOM_OPTS} -work work -93    $PKG_PATH/utils-p.vhd
  vcom ${VCOM_OPTS} -work work -93    $UNITS_PATH/cfgintf/vhdl/cfgintf.vhd
  vcom ${VCOM_OPTS} -work work -93    $UNITS_PATH/paramintf/vhdl/paramintf.vhd
  vcom ${VCOM_OPTS} -work work -93    $UNITS_PATH/paramintf/vhdl/paramoutreg.vhd
  vcom ${VCOM_OPTS} -work work -93 -noindexcheck  $RECONF_OUT_PATH/reconflogic.vhd
}
function CompileCore() {
  OLD_VLOG_OPTS="$VLOG_OPTS"
  VLOG_OPTS="+define+SIMULATION +incdir+${OMSP_PATH}/verilog +incdir+${RECONF_OUT_PATH} -lint -pedanticerrors $VLOG_OPTS"
  # Config Interface
  vcom ${VCOM_OPTS} -work work -93    $PKG_PATH/utils-p.vhd
  vcom ${VCOM_OPTS} -work work -93    $CFGINTF_PATH/vhdl/cfgintf.vhd
  
  # Param Interface
  vcom ${VCOM_OPTS} -work work -93    $PARAMINTF_PATH/vhdl/paramintf.vhd
  vcom ${VCOM_OPTS} -work work -93    $PARAMINTF_PATH/vhdl/paramoutreg.vhd
  
  # Core
  vlog ${VLOG_OPTS} -work work        $CORE_PATH/verilog/core.v

  VLOG_OPTS="$OLD_VLOG_OPTS"
}

# files used for all simulation modes
#$# vcom ${VCOM_OPTS} -work work -87   filename.vhd

# application design
case "$SIM_MODE" in
  sim-rtl)
    # sim-rtl: use user's Verilog code
    vlog ${VLOG_OPTS} -work work        $APPVLOG_PATH/${APP_NAME_LC}.v
    ;;
  sim-yosys)
    # sim-yosys: use Yosys synthesis output
    CompileYosysLibs
    vlog ${VLOG_OPTS} -work work        $APPCHLL_PATH/${APP_NAME_LC}.v
    ;;
  sim-yosys-fsm)
    # sim-yosys-fsm: use Yosys synthesis output with extracted FSMs as $fsm cells
    CompileYosysLibs    # use $fsm module definition from Yosys's simlib
    vlog ${VLOG_OPTS} -work work        $APPCHLL_PATH/${APP_NAME_LC}-fsm.v
    ;;
  sim-yosys-trfsm)
    # sim-yosys-trfsm: use Yosys synthesis output with extracted FSMs replaced by TR-FSMs
    CompileYosysLibs    # Yosys's simlib for all cells except $fsm
    CompileTRFSM
    # wrapper and new module
    vcom ${VCOM_OPTS} -work work -93    $APPCHLL_PATH/${APP_NAME_LC}-fsm-ledfsm-wrapper.vhd
    # top module
    vlog ${VLOG_OPTS} -work work        $APPCHLL_PATH/${APP_NAME_LC}-trfsm.v
    ;;
  sim-yosys-extract)
    # sim-yosys-extract: use Yosys synthesis output with extracted FSMs as $fsm
    # cells and extracted cells from the cell library
    CompileYosysLibs    # use $fsm module definition from Yosys's simlib
    CompileTRFSM
    CompileCellLib
    # module
    #$# vlog ${VLOG_OPTS} -work work        $APPCHLL_PATH/myapp-extract.v
    ;;
  sim-yosys-extract-trfsm)
    # sim-yosys-extract-trfsm: use Yosys synthesis output with extracted FSMs replaced by TR-FSMs
    CompileYosysLibs    # Yosys's simlib for all cells except $fsm
    CompileTRFSM
    CompileCellLib
    # wrapper and new module
    #$# vcom ${VCOM_OPTS} -work work -93   $APPCHLL_PATH/myapp-extract-xyzfsm-wrapper.vhd
    # top module
    #$# vlog ${VLOG_OPTS} -work work       $APPCHLL_PATH/myapp-extract-trfsm.v
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
    #$# vlog ${VLOG_OPTS} -work work       $APPCHLL_PATH/myapp-intersynth.v
    ;;
  sim-intersynth)
    # sim-intersynth: use wrapped InterSynth module looking like this
    # application module
    CompileYosysLibs    # Yosys's simlib for all cells except $fsm
    CompileTRFSM
    CompileCellLib
    CompileInterSynth
    # top module: wrapper
    #$# vcom ${VCOM_OPTS} -work work -93    $APPCHLL_PATH/myapp-wrapintersynth.vhd
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
