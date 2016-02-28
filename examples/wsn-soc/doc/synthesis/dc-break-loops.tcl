#
# Synopsys Design Compiler synthesis script snippet to disable the
# combinational loops in the interconnect
#
# Copy this script into your synthesis script and adjust the variable
# $INTERSYNTH_PATH as well as the timing values below.
#
# The approach is to create a new sub-module inside the InterSynth module which
# contains the logic of the interconnect. Then all timing paths going through
# the interconnect are disabled. Finally, a maximum delay and input and output
# delays are applied.
#

set INTERSYNTH_PATH "core_1/MyReconfigLogic_0/MyInterSynthModule_0/"

# put all logic of the interconnect to a new sub-module
group -design_name "Muxes" -cell_name "Muxes_1" [get_cells ${INTERSYNTH_PATH}* -filter {is_hierarchical == false}]
# get its pins
set Muxes_In  [get_pins ${INTERSYNTH_PATH}Muxes_1/* -filter {@pin_direction == in && @name !~ bitdata*}]
set Muxes_Cfg [get_pins ${INTERSYNTH_PATH}Muxes_1/bitdata]
set Muxes_Out [get_pins ${INTERSYNTH_PATH}Muxes_1/* -filter {@pin_direction == out}]
# get all pins which come from or go to outside and not to/from cells
set Muxes_Ext_In  [filter $Muxes_In  {name !~ cell_*}]
set Muxes_Ext_Out [filter $Muxes_Out {name !~ cell_*}]
# get all pins which come from or go to cells
set Muxes_Int_In  [filter $Muxes_In  {name =~ cell_*}]
set Muxes_Int_Out [filter $Muxes_Out {name =~ cell_*}]

# disable all timing arcs going through the interconnect
set_disable_timing ${INTERSYNTH_PATH}Muxes_1

# constrain delay through interconnect
set_max_delay -from $Muxes_In -to $Muxes_Out 23.0
set_min_delay -from $Muxes_In -to $Muxes_Out  0.0001    ;# 0.1ps, short paths can be really fast, so don't require long inverter chains to delay the signal

# constrain delay arriving at interconnect inputs
# assume interconnect input signals to change 0.5ns to 22ns after Clk
set_output_delay -min  0.5 -clock CLK $Muxes_Ext_In
set_output_delay -max 22.0 -clock CLK $Muxes_Ext_In
set_output_delay -min  0.5 -clock CLK $Muxes_Int_In
set_output_delay -max 19.0 -clock CLK $Muxes_Int_In
# assume interconnect output signals to change 0.5 to 22+17ns after Clk
set_input_delay  -min  0.5 -clock CLK $Muxes_Ext_Out
set_input_delay  -max 39.0 -clock CLK $Muxes_Ext_Out
set_input_delay  -min  0.5 -clock CLK $Muxes_Int_Out
set_input_delay  -max 36.0 -clock CLK $Muxes_Int_Out
# 40.0 includes the set_input_delay from below for signals coming from the Interconnect, going through e.g. the OneWire_Master and coming back to the InterConnect
set_max_delay 80.0           -to $Muxes_Ext_In
set_max_delay 76.0           -to $Muxes_Int_In
set_max_delay 70.0 -from CLK -to $Muxes_Ext_In
set_max_delay 69.0 -from CLK -to $Muxes_Int_In
# timing exceptions (like set_max_delay) with a -from or -to clock take
# precedene over those without

set_max_delay 84.0         -from $Muxes_Ext_Out
set_max_delay 81.0         -from $Muxes_Int_Out
set_max_delay 77.0 -to CLK -from $Muxes_Ext_Out
set_max_delay 75.0 -to CLK -from $Muxes_Int_Out

# remove CONST_* and Byte2Word cells, because they are simple feedthrough
# (do that after the set_disable_timing, because otherwise we can't reach the ports)
ungroup [get_cells "${INTERSYNTH_PATH}*" -filter "@ref_name =~ CONST_*"]
ungroup [get_cells "${INTERSYNTH_PATH}*" -filter "@ref_name =~ Byte2Word"]

