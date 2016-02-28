# Include Bash script file

# configure
TRACE_DIR=/tmp/depends
FLOWCMD=../flowcmd/flowcmd
CELLS=""    # set to "" to auto-detect
APPS=""     # set to "" to auto-detect

# check that we are executed from a chip base directory
if [ ! -r ".chll" ] ; then
  echo "Error: Execute this script from a chip base directory."
  exit 1
fi
CHIP_BASE="$(pwd)"

# determine Ex.Apps.
if [ -z "$APPS" ] ; then
  APPS="$( (
    echo "source $CHIP_BASE/units/reconfmodule/chll/scripts/setup-exapps.tcl"
    echo "foreach App \$ExApps {"
    echo "  puts -nonewline \"\$App \""
    echo "}"
  ) | tclsh)"
fi

# determine Cells in cell library
if [ -z "$CELL" ] ; then
  CELLS="$( (
    echo "source $CHIP_BASE/units/reconfmodule/chll/scripts/setup-celllib-arr.tcl"
    echo "foreach Cell \$CellLib {"
    echo "  puts -nonewline \"\$Cell \""
    echo "}"
  ) | tclsh)"
fi
