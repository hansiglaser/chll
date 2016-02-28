#!flowproc
#
# Setup the application
#

# remove the following two lines
#puts "ERROR: You have to edit this file [info script] before executing."
#exit 1

puts "################################################################################"
puts "## Setup Application $APP_NAME"

create_application "$APP_NAME"

puts "## Adding ports, parameters, ..."
# system signals
app_add_port "Reset_n_i"
app_add_port "Clk_i"
# interface to CPU
app_add_port "Enable_i"      -map "ReconfModuleIn_s"   -index 0
app_add_port "CpuIntr_o"     -map "ReconfModuleIRQs_s" -index 0
# interface to sensor
app_add_port "SensorPower_o" -map "Outputs_o" -index 0
app_add_port "SensorStart_o" -map "Outputs_o" -index 1
app_add_port "SensorReady_i" -map "Inputs_i"  -index 0
# interface to ADC
app_add_port "AdcStart_o"    -map "AdcDoConvert_o"
app_add_port "AdcDone_i"     -map "AdcConvComplete_i"
app_add_port "AdcValue_i"
# parameters
app_add_param -in -conntype "Word" -default 10    "PeriodCounterPreset_i"
app_add_param -in -conntype "Word" -default 10    "Threshold_i"
# result value
app_add_param -out -conntype "Word" "SensorValue_o"
