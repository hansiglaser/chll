set CELL       "$env(CELL)"
set CELL_LC    [string tolower "$CELL"]
set VARIANT    "$env(VARIANT)"
set VARIANT_LC [string tolower "$VARIANT"]

set APPVHDL_PATH "../../vhdl"
set APPVLOG_PATH "../../verilog"

set LOG_PATH "../log"
set RPT_PATH "../reports"

remove_container r
remove_container i 
remove_library -all

##############################################################################
### Read Reference
##############################################################################

# compile top
read_verilog -container r -libname work -01 $APPVLOG_PATH/$CELL_LC.v

# elaborate and implement
set_top r:/work/$CELL

##############################################################################
### Read Reference
##############################################################################

# compile top
read_verilog -container i -libname work -01 $APPVLOG_PATH/${CELL_LC}_${VARIANT_LC}.v

# elaborate and implement
set_top i:/work/${CELL}_${VARIANT}

# match compare points
match

# increase number of reported failing points from 20 to 100
set verification_failing_point_limit 100

# verify designs
verify 

# if verification did not succeed, diagnose errors
#diagnose

report_aborted_points          > $RPT_PATH/$VARIANT_LC-aborted_points.rpt
report_analysis_results        > $RPT_PATH/$VARIANT_LC-analysis_results.rpt
report_dont_verify_points      > $RPT_PATH/$VARIANT_LC-dont_verify_points.rpt
report_cutpoints               > $RPT_PATH/$VARIANT_LC-cutpoints.rpt
report_design_libraries        > $RPT_PATH/$VARIANT_LC-design_libraries.rpt
report_designs                 > $RPT_PATH/$VARIANT_LC-designs.rpt
report_equivalences            > $RPT_PATH/$VARIANT_LC-equivalences.rpt
report_factor_points           > $RPT_PATH/$VARIANT_LC-factor_points.rpt
report_failing_points          > $RPT_PATH/$VARIANT_LC-failing_points.rpt
report_input_value_range       > $RPT_PATH/$VARIANT_LC-input_value_range.rpt
report_libraries               > $RPT_PATH/$VARIANT_LC-libraries.rpt
report_loops                   > $RPT_PATH/$VARIANT_LC-loops.rpt
report_matched_points          > $RPT_PATH/$VARIANT_LC-matched_points.rpt
report_not_compared_points     > $RPT_PATH/$VARIANT_LC-not_compared_points.rpt
report_passing_points          > $RPT_PATH/$VARIANT_LC-passing_points.rpt
report_probe_points            > $RPT_PATH/$VARIANT_LC-probe_points.rpt
report_probe_status            > $RPT_PATH/$VARIANT_LC-probe_status.rpt
report_status                  > $RPT_PATH/$VARIANT_LC-status.rpt
report_user_matches            > $RPT_PATH/$VARIANT_LC-user_matches.rpt
report_unmatched_points        > $RPT_PATH/$VARIANT_LC-unmatched_points.rpt
report_unread_endpoints -all   > $RPT_PATH/$VARIANT_LC-unread_endpoints.rpt
report_unverified_points       > $RPT_PATH/$VARIANT_LC-unverified_points.rpt
