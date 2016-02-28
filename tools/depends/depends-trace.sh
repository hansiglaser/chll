#!/bin/bash
#
# Determine dependencies from design flow
#
# This script executes a full reconfigurable chip design flow and uses "strace"
# to log all file access. It must be executed from a chip base directory.
# 
# Then use depends-eval.sh to evaluate these traces to determine input and
# output files for each step. These are used as dependencies and targets for a
# Makefile.
#
# The only save place to run this script is in your home directory, because
# depends-eval.sh will filter a large set of paths (esp. /tmp, /usr/, ...).
#

SCRIPT_BASE="$(dirname $0)"
source "$SCRIPT_BASE/depends-common.inc.sh"

# prepare output directory
rm -rf "$TRACE_DIR"
mkdir -p "$TRACE_DIR"

FILE_NUM=1

function TraceFlow() {
  STEP="$1"
  shift
  # create filename, use '-1', '-2', ... if we would overwrite existing files
  BASE_FILE="${TRACE_DIR}/$(printf '%03d' ${FILE_NUM})-${STEP}"
  FILE_NUM=$(( $FILE_NUM + 1 ))
  CMD_FILE="${BASE_FILE}.sh"
  echo "#!/bin/sh" > "$CMD_FILE"
  echo "cd" $(pwd) >> "$CMD_FILE"
  echo $FLOWCMD $STEP "$@" >> "$CMD_FILE"
  TRACE_FILE="${BASE_FILE}.strace"
  STDOUT_FILE="${BASE_FILE}.stdout"
  STDERR_FILE="${BASE_FILE}.stderr"
  echo "## $FLOWCMD $STEP $@ ############"
  strace -f -o "$TRACE_FILE" -e trace=open,execve,rename $FLOWCMD $STEP "$@" > "$STDOUT_FILE" #hangs: 2> >(tee "$STDERR_FILE" >&2)
}

echo "Tracing to $TRACE_DIR"

#TraceFlow new-chip [-o directory]
#TraceFlow new-unit unit_name
#TraceFlow import-unit unit_name source_path
#TraceFlow new-reconf-module

cd units/reconfmodule
TraceFlow characterize-parent
TraceFlow list-reconf-signals
TraceFlow check-reconf-module

for APP in $APPS ; do
  echo "Preparing Ex.App. $APP"
  APP_LC=${APP,,}
  #TraceFlow new-app app_name
  cd ../../apps/$APP_LC

  TraceFlow app-templates
  TraceFlow check-app
  TraceFlow extract-fsm
  TraceFlow insert-trfsm -fsm
  # all those sim-* directories only read files, which don't add relevant dependencies
done

for CELL in $CELLS ; do
  echo "Preparing Cell $CELL"
  CELL_LC=${CELL,,}
  cd ../../celllib/$CELL_LC

  #TraceFlow new-cell cell_name
  TraceFlow cell-templates
  TraceFlow check-cell
done

cd ../../units/reconfmodule

TraceFlow check-celllib

for APP in $APPS ; do
  echo "Extracting Ex.App. $APP"
  APP_LC=${APP,,}
  cd ../../apps/$APP_LC
  
  TraceFlow extract
  TraceFlow check-extract
  TraceFlow insert-trfsm -extract
done

cd ../../units/reconfmodule

TraceFlow check-exapps
TraceFlow insert-trfsms
TraceFlow intersynth
TraceFlow check-intersynth
TraceFlow generate-reconf-module

#TraceFlow check-app [-interactive|-post-si]
#TraceFlow insert-trfsm -fsm|-extract|-post-si
#TraceFlow intersynth [-post-si]

# TODO: how can we find dependencies between units?

