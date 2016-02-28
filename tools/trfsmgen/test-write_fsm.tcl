#!trfsmgen
#
# $ ./trfsmgen -b -f test-write_fsm.tcl
#
set s27 [read_kiss s27.kiss2]
print_fsm $s27
write_fsm $s27 -origenc -format ilang s27.il

