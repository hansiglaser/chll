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
# interface to CPU
app_add_port "ExtIntrOut_o" -map "ReconfModuleIRQs_s" -index 1
# input
app_add_port "ExtIntrIn_i"  -map "Inputs_i" -index 7
# ReconfModuleIRQs_o_7 is directly in the lowest branch with Inputs_i_5..7 in
# the second tree of conntype Bit
