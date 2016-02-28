setenv APPVHDL_PATH "../../vhdl"
setenv APPVLOG_PATH "../../verilog"
setenv APPCHLL_PATH "../../chll/out"
setenv CELLLIB_PATH "../../../../celllib"
setenv PKG_PATH "../../../../vhdl_packs"
setenv RECONV_PATH  "../../../../units/reconfmodule/chll/out"
setenv APP_NAME    "TMP421"
setenv APP_NAME_LC "tmp421"
setenv TRFSM_VHDL_PATH "$CELLLIB_PATH/trfsm/vhdl"
setenv YOSYS_PATH /path/to/yosys

setenv LOG_PATH "../log"
setenv RPT_PATH "../reports"
setenv SCR_PATH "../scripts"

set log file $LOG_PATH/lec.log -append

//////////////////////////////////////////////////////////////////////////////
// read golden
//////////////////////////////////////////////////////////////////////////////

set case sensitivity off
set hdl diagnosis on

// compile top
read design -noelaborate -golden -verbose \
  -verilog2k $APPVLOG_PATH/i2cfsm.v \
  -verilog2k $APPVLOG_PATH/sensorfsm.v \
  -verilog2k $APPVLOG_PATH/$APP_NAME_LC.v

// elaborate and implement
elaborate design -golden -root $APP_NAME

//////////////////////////////////////////////////////////////////////////////
// read revised
//////////////////////////////////////////////////////////////////////////////

// compile TR-FSM
read design -noelaborate -revised \
  -vhdl 93 $PKG_PATH/config-p.vhd \
  -vhdl 93 $TRFSM_VHDL_PATH/trfsmparts-p.vhd \
  -vhdl 93 $TRFSM_VHDL_PATH/ConfigRegister-e.vhd \
  -vhdl 93 $TRFSM_VHDL_PATH/ConfigRegister-rtl-a.vhd \
  -vhdl 93 $TRFSM_VHDL_PATH/InputPatternGate-e.vhd \
  -vhdl 93 $TRFSM_VHDL_PATH/InputPatternGate-rtl-a.vhd \
  -vhdl 93 $TRFSM_VHDL_PATH/InputSwitchingMatrix-e.vhd \
  -vhdl 93 $TRFSM_VHDL_PATH/InputSwitchingMatrix-rtl-a.vhd \
  -vhdl 93 $TRFSM_VHDL_PATH/LargeMux-e.vhd \
  -vhdl 93 $TRFSM_VHDL_PATH/LargeMux-rtl-a.vhd \
  -vhdl 93 $TRFSM_VHDL_PATH/StateRegister-e.vhd \
  -vhdl 93 $TRFSM_VHDL_PATH/StateRegister-rtl-a.vhd \
  -vhdl 93 $TRFSM_VHDL_PATH/StateSelectionGate-e.vhd \
  -vhdl 93 $TRFSM_VHDL_PATH/StateSelectionGate-rtl-a.vhd \
  -vhdl 93 $TRFSM_VHDL_PATH/TransitionRow-e.vhd \
  -vhdl 93 $TRFSM_VHDL_PATH/TransitionRow-struct-a.vhd \
  -vhdl 93 $TRFSM_VHDL_PATH/trfsm-e.vhd \
  -vhdl 93 $TRFSM_VHDL_PATH/trfsm-struct-a.vhd

read design -noelaborate -revised \
  -define SIMLIB_NOMEM -define SIMLIB_NOPOW \
  -verilog2k $YOSYS_PATH/techlibs/common/simcells.v \
  -verilog2k $YOSYS_PATH/techlibs/common/simlib.v

// compile TR-FSM wrapper module
read design -noelaborate -revised \
  -vhdl 93 $RECONV_PATH/trfsm0-wrapper.vhd \
  -vhdl 93 $RECONV_PATH/trfsm1-wrapper.vhd

// compile cell library
read design -noelaborate -revised \
  -verilog2k $CELLLIB_PATH/absdiff/verilog/absdiff.v \
  -verilog2k $CELLLIB_PATH/addsubcmp/verilog/addsubcmp.v \
  -verilog2k $CELLLIB_PATH/byte2word/verilog/byte2word.v \
  -verilog2k $CELLLIB_PATH/byte2wordsel/verilog/byte2wordsel.v \
  -verilog2k $CELLLIB_PATH/bytemuxoct/verilog/bytemuxoct.v \
  -verilog2k $CELLLIB_PATH/bytemuxquad/verilog/bytemuxquad.v \
  -verilog2k $CELLLIB_PATH/bytemuxdual/verilog/bytemuxdual.v \
  -verilog2k $CELLLIB_PATH/byteregister/verilog/byteregister.v \
  -verilog2k $CELLLIB_PATH/counter/verilog/counter.v \
  -verilog2k $CELLLIB_PATH/counter32/verilog/counter32.v \
  -verilog2k $CELLLIB_PATH/wordregister/verilog/wordregister.v \
  -verilog2k $CELLLIB_PATH/wordmuxdual/verilog/wordmuxdual.v

// standard cells (CONST)
read design -noelaborate -revised \
  -verilog2k $RECONV_PATH/presilicon-stdcells.v 

// InterSynth module
read design -noelaborate -revised \
  -verilog2k $RECONV_PATH/presilicon.v 

// compile top wrapper
read design -noelaborate -revised \
  -vhdl 93 $APPCHLL_PATH/$APP_NAME_LC-wrapintersynth.vhd

// elaborate and implement
elaborate design -revised -root $APP_NAME

// apply bitstreams to TR-FSMs
dofile $APPCHLL_PATH/$APP_NAME_LC-intersynth-bitstream-lec.do

//////////////////////////////////////////////////////////////////////////////
// FSM Recoding
//////////////////////////////////////////////////////////////////////////////
// Either do (1) or (2)
//
// 1) set unused state register bits to 0
//add instance constraint 0 /PureSensorFSM_1/TRFSM_1/StateRegister_1/State_o_reg[2] -revised  
//add instance constraint 0 /PureSensorFSM_1/TRFSM_1/StateRegister_1/State_o_reg[3] -revised  
//add instance constraint 0 /PureSensorFSM_1/TRFSM_1/StateRegister_1/State_o_reg[4] -revised  
//
// 2) read FSM encoding
// FSMs are in sub-modules, and I didn't find out how to use "read fsm
// encoding" to specify the sub-module. Therefore we have to flatten the
// golden design and prepend the .fromstates items in the encoding files
// with the instance path
flatten -golden
dofile $APPCHLL_PATH/$APP_NAME_LC-intersynth-encoding-lec.do

report instance constraints > $RPT_PATH/$APP_NAME_LC-instance-constraints.rpt

report rule check -verbose  > $RPT_PATH/$APP_NAME_LC-rule-check-verbose.rpt

//set flatten model -gated_clock

report environment > $RPT_PATH/$APP_NAME_LC-environment.rpt

set system mode lec

// here it probably says: "(F12) Converted 2145 DFF(s) to DLAT(s) due to
// disabled clock port(s)" and reports all ConfigRegister's D-FF to be
// converted to DLATches, which are unmapped points. This is ok.

map key points  

report unmapped points           > $RPT_PATH/$APP_NAME_LC-unmapped-points.rpt
report key point -unmapped -both > $RPT_PATH/$APP_NAME_LC-key-point-unmapped.rpt

// stop-here-with-invalid-command

add compare points -all

report compare points            > $RPT_PATH/$APP_NAME_LC-compare-points.rpt

// increase compare effort to avoid aborts
set compare effort auto
compare -abort_print

save hier_compare result
report hier_compare result

report design data                > $RPT_PATH/$APP_NAME_LC-design-data.rpt
report verification -verbose      > $RPT_PATH/$APP_NAME_LC-verification.rpt
report statistics                 > $RPT_PATH/$APP_NAME_LC-statistics.rpt
report design similarity -verbose > $RPT_PATH/$APP_NAME_LC-design-similarity.rpt

report compare data -noneq        > $RPT_PATH/$APP_NAME_LC-compare-data-noneq.rpt

//// Aborts: Conformal_User.pdf p. 210ff
// Aborts are compare points that have not been conclusively compared. By the time aborts are
// reported, the Conformal software has applied multiple algorithms without a deterministic
// result. Aborts can have several causes including don’t cares, large cones, and large numbers
// of inputs. Cones with these attributes will result in increased runtime. To deal with extremely
// long runtimes, developers have limited the amount of time which the tool can use on a specific
// compare point. When the Conformal software exceeds this limit, the compare point will be
// reported as an abort. By setting a time limit for each compare point, the Conformal software
// avoids the appearance that it is locked up when it is still processing the compare point.
//
// analyze abort
// analyze abort -compare
// analyze datapath -module -verbose -resourcefile resourcefile.rpt -isolate_abort_module

// diagnose -summary

// prove /core_1/TopTimer_1/Core_Inst/EEProm_Inst/ClkDiv_reg[0] /core_1/TopTimer_1/Core_Inst/EEProm_Inst/ClkDiv_reg_0_/U$1

// exit -force

////// start GUI
// set gui
