#!/bin/bash
#
#
#

SCRIPT_DIR="../scripts"
LOG_DIR="../log"
LOGFILE="${LOG_DIR}/run-$(date +%Y%m%d%H%M%S).log"

LEC="lec"
LEC_OPTS="-lpgxl -64 -nogui"

${LEC} ${LEC_OPTS} -dofile ${SCRIPT_DIR}/check-equiv.do | tee "${LOGFILE}"
