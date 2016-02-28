#!flowproc
#
# Setup the application
#

# remove the following two lines
puts "ERROR: You have to edit this file [info script] before executing."
exit 1

puts "################################################################################"
puts "## Setup Application $APP_NAME"

create_application "$APP_NAME"

puts "## Adding ports, parameters, ..."
# system signals
#$# app_add_port "Reset_n_i"
#$# app_add_port "Clk_i"
# interface to CPU
#$# app_add_port "<...>_i"      -map "ReconfModuleIn_s"   -index 0
#$# app_add_port "<...>_o"      -map "ReconfModuleOut_s"  -index 0
#$# app_add_port "<...>Intr_o"  -map "ReconfModuleIRQs_s" -index 0
# interface to sensor
#$# app_add_port "<...>_o" -map "Outputs_o" -index 0
# parameters
#$# app_add_param -in -conntype "<conntype>" -default <nnn> "<...>_i"
# result value
#$# app_add_param -out -conntype "<conntype>" "<...>_o"
# interface to <peripheral>
#$# app_set_port_value "<...>" '1'
#$# app_set_port_value "<...>" {"00000000"}
#$# app_add_port "<...>_i"         -map "<...>"
