# Define power modes / power analysis periode
#
# This file deifines the periodes for a 10MHz clock (100ns = 100.000ps)
#
#                  --Name--  --Start-- ----End----
set modes(0) [list "Boot"       299999     5899999]
set modes(1) [list "Config"    5899999   895099999]
set modes(2) [list "Param"   895099999   910199999]
set modes(3) [list "Run"     910199999 99999999999]
# no multi-dimensional arrays supported by TCL, therefore use a list


