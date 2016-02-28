# Define power modes / power analysis periode
#
# This file deifines the periodes for a 100kHz clock (10us = 10.000.000ps)
#
#                  --Name--  ---Start--- -----End-----
set modes(0) [list "Boot"       29999999     589999999]
set modes(1) [list "Config"    589999999   89509999999]
set modes(2) [list "Param"   89509999999   91019999999]
set modes(3) [list "Run"     91019999999 9999999999999]
# no multi-dimensional arrays supported by TCL, therefore use a list


