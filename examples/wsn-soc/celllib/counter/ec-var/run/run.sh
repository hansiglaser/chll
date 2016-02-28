#!/bin/bash
#
#
#

SCRIPT_DIR="../scripts"
LOG_DIR="../log"
LOGFILE="${LOG_DIR}/run-$(date +%Y%m%d%H%M%S).log"
CELL="$(cat ../../.chll-cell)"

if [ $# -ne 1 ] ; then
  echo "Usage: $0 [-tv1]"
  exit 1
fi

if [ "$1" = "-tv1" ] ; then
  export CELL
  export VARIANT=TV1
else
  echo "Error: Invalid parameter $1"
  exit 1;
fi

fm_shell -64bit -file  ${SCRIPT_DIR}/check-equiv.tcl  | tee "${LOGFILE}"
