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
# blinking LED output
app_add_port "LED_o" -map "Outputs_o" -index 0
# parameters
app_add_param -in -conntype "Word" -default 0     "PeriodH_i"
app_add_param -in -conntype "Word" -default 10    "PeriodL_i"
