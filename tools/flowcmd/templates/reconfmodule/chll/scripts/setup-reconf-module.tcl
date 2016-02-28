#!flowproc
#
# Create and setup the reconfigurable module
#

# remove the following two lines
puts "ERROR: You have to edit this file [info script] before executing."
exit 1

set SCRIPT_BASE [file dirname [info script]]
source $SCRIPT_BASE/setup-reconf-signals.tcl

puts "################################################################################"
puts "## Setup Reconfigurable Module"

# create the reconfigurable module
create_reconf_module -typename "<MyReconfigLogic>" -instname "<MyReconfigLogic_0>" -istypename "<MyInterSynthModule>" -isinstname "<MyInterSynthModule_0>"
# setup important signals
set_reset "<Reset_n_i>"
set_clock "<Clk_i>"

# add the reonfigurable signals, setup in setup-reconf-signals.tcl
add_reconf_signals

puts "## Setup <CPU> Peripheral Interface"

create_peripheral_interface -type "<type>" -peraddr "Per_Addr_s" -perdin "Per_DIn_s" -perwr "Per_Wr_s" -peren "Per_En_s"
create_peripheral_instance "CfgIntf"   -baseaddr <Addr> -perdout "CfgIntf_DOut_s"
create_peripheral_instance "ParamIntf" -baseaddr <Addr> -perdout "ParamIntf_DOut_s"

puts "## Creating Config Interface"

# Create the configuration interface, which is used by the CPU to supply the
# bitstreams to the reconfigurable module.
create_config_interface "CfgIntf" -instname "CfgIntf_0"

puts "## Creating Param Interface"

# Create the parameterization interface, which is used by the CPU to supply
# and query the parameter values to and from the reconfigurable module.
create_param_interface "ParamIntf" -instname "ParamIntf_0"

puts "## Setting Connections of Reconfigurable Signals"

# System Signals
#$# set_reconf_signal_connection -direct  -name "Clk_i"
#$# set_reconf_signal_connection -direct  -name "Reset_n_i"
# <...>
#$# set_reconf_signal_connection -dynamic -name "Inputs_i"
#$# set_reconf_signal_connection -const 0 -name "AdcClk_o"
#$# set_reconf_signal_connection -dynamic -name "Adc.*" ! -name "AdcClk_o"
#$# set_reconf_signal_connection -param   -name "I2C_Divider800"
#$# set_reconf_signal_connection -config  -name "I2C_F100_400_n"
#$# set_reconf_signal_connection -dynamic -name "I2C_.*" ! -name "I2C_Divider800" ! -name "I2C_F100_400_n" ! -name ".*ENQ"

# test cases for value parser
# set_reconf_signal_connection -const \"0011010100001111\"  -name "OneWire_ODRDSlotInitTime"
# set_reconf_signal_connection -const 16'b0011010100001111  -name "OneWire_ODRDSlotSampleTime"
# set_reconf_signal_connection -const 16'h1234              -name "OneWire_ODResetLowTime"
# set_reconf_signal_connection -const 16'd12345             -name "OneWire_ODResetPrecenceIntervalDuration"
# set_reconf_signal_connection -const 16'o12345             -name "OneWire_ODResetTime"
# set_reconf_signal_connection -const 01234                 -name "OneWire_ODResetWaitForDetectionDuration"
# set_reconf_signal_connection -const 12345                 -name "OneWire_ODSlotDuration"
# set_reconf_signal_connection -const 0x1234                -name "OneWire_ODSlotLowDataTime"
# set_reconf_signal_connection -const {$1234}               -name "OneWire_ODWRSlotHighDataTime"
# set_reconf_signal_connection -const 1                     -name "OneWire_MatchROM"

puts "## Creating Connection Types"

create_conntype "<Bit>"   1
create_conntype "<Byte>"  8
create_conntype "<Word>" 16

puts "## Assigning Connection Types to Reconfig Signals"

#$# set_reconf_signal_conntype "Bit" -array -name "Inputs_i"
#$# set_reconf_signal_conntype "Bit" -width 1
#$# set_reconf_signal_conntype "Byte" -width 8 ! -name "SPI_SPPR_SPR" ! -name {ReconfModule.*}
#$# set_reconf_signal_conntype "Byte" -pad_left \"0000\" -name "I2C_ReadCount"
#$# set_reconf_signal_conntype "Byte" -name "SENT_OutByte"
#$# set_reconf_signal_conntype "Word" -pad_left \"000000\" -name "AdcValue_i"

puts "## Checking Reconfig Signals"

check_reconf_signals
