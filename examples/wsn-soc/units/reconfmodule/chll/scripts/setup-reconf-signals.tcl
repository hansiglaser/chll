#!flowproc
#
# Use list of unused signals in parent module to setup the list of signals
# used by the reconfigurable module.
#
#

# remove the following two lines
#puts "ERROR: You have to edit this file [info script] before executing."
#exit 1

# Configure
set PARENT_MODULE "Core"
set OMSP_DEBUG "I2C"

set INCLUDE_ONEWIRE false
set INCLUDE_PWM     false
set INCLUDE_SENT    false
set INCLUDE_SPC     false

# get data from outside this script
set UNIT_BASE    "$env(UNIT_BASE)"
set STUB_FILE    "$env(STUB_FILE)"
set PARENT_ILANG "$env(PARENT_ILANG)"

# read ports of the parent module
read_parent "$PARENT_ILANG" "$PARENT_MODULE"

# read file provided by special yosys pass to extract unused signals
read_unused_signals "$STUB_FILE"

# amend list of signals
del_reconf_signals -name ".*ScanDataOut"       ;# unused scan chain

del_reconf_signals -name "AClk_s"              ;# OpenMSP430, only in its (not used) "ASIC" configuration
#del_reconf_signals -name "SMClk_s"             ;# OpenMSP430, only in its (not used) "ASIC" configuration
del_reconf_signals -name "DCO_Enable_s"        ;# OpenMSP430, only in its (not used) "ASIC" configuration
del_reconf_signals -name "DCO_Wakeup_s"        ;# OpenMSP430, only in its (not used) "ASIC" configuration
del_reconf_signals -name "LFXT_Enable_s"       ;# OpenMSP430, only in its (not used) "ASIC" configuration
del_reconf_signals -name "LFXT_Wakeup_s"       ;# OpenMSP430, only in its (not used) "ASIC" configuration
if { $OMSP_DEBUG == "UART" } {
  del_reconf_signals -name "Dbg_SDA_Out_s"       ;# OpenMSP430, we don't use the I2C debug interface
}
if { $OMSP_DEBUG == "I2C" } {
  del_reconf_signals -name "Dbg_UART_TxD_s"      ;# OpenMSP430, we don't use the UART debug interface
}
del_reconf_signals -name "IRQ_Ack_s"           ;# OpenMSP430, we don't use the interrupt acknowledge signals
del_reconf_signals -name {P2_DOut_s}           ;# OpenMSP430, P2_DOut_s[7:5] are directly connected to P2_DOut_o[7:5], but stubnets doesn't find that
del_reconf_signals -name {P2_En_s}             ;# OpenMSP430, P2_En_s  [7:5] are directly connected to P2_En_o  [7:5], but stubnets doesn't find that
del_reconf_signals -name {P2_Sel_s}            ;# OpenMSP430, P2_Sel_s [7:5] is unused
del_reconf_signals -name {P[4-6]_DOut_s}       ;# OpenMSP430, we don't use Ports 4-6, but Port 3 as CPU<->ReconfModule Interface 
del_reconf_signals -name {P[3-6]_En_s}         ;# OpenMSP430, we don't use Ports 3-6, including Port 3 enable signals
del_reconf_signals -name {P[3-6]_Sel_s}        ;# OpenMSP430, we don't use Ports 3-6, including Port 3 select signals
del_reconf_signals -name "CfgIntf_DOut_s"      ;# to connect DataOut_o of the config interface inside the reconfigurable module
del_reconf_signals -name "ParamIntf_DOut_s"    ;# to connect DataOut_o of the param  interface inside the reconfigurable module

use_signal_alias "ReconfModuleIRQs_s"          ;# replace Yosys IRQ_s(1:0, 13:11) by ReconfModuleIRQs_s
use_signal_alias "ReconfModuleIn_s"            ;# replace Yosys P3_DIn_s
use_signal_alias "ReconfModuleOut_s"           ;# replace Yosys P3_DOut_s

add_reconf_signal "I2C_Errors"                 ;# make this vector accessible as parameter

add_reconf_signal "Clk_i"
add_reconf_signal -rename "Reset_n_i" "Reset_n_s"

### add_reconf_signal -cell "I2C_Master" -output "SDA_o"   ;# fork output of submodule as our input
