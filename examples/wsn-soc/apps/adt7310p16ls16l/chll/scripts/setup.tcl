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
app_add_port "ADT7310CS_n_o" -map "Outputs_o" -index 0
# parameters
# 24000 * 10us (100kHz Clk) = 240ms
app_add_param -in -conntype "Word" -default 24000 "SPICounterPreset_i"
app_add_param -in -conntype "Word" -default 30    "Threshold_i"
app_add_param -in -conntype "Word" -default 10    "PeriodCounterPreset_i"
# result value
app_add_param -out -conntype "Word" "SensorValue_o"
# interface to SPI master
app_set_port_value "SPI_CPOL" '1'
app_set_port_value "SPI_CPHA" '1'
app_set_port_value "SPI_LSBFE" '0'
app_set_port_value "SPI_SPPR_SPR" {"00000000"}
app_add_port "SPI_Data_i"         -map "SPI_DataOut"
app_add_port "SPI_Write_o"        -map "SPI_Write"
app_add_port "SPI_ReadNext_o"     -map "SPI_ReadNext"
app_add_port "SPI_Data_o"         -map "SPI_DataIn"
app_add_port "SPI_FIFOFull_i"     -map "SPI_FIFOFull"
app_add_port "SPI_FIFOEmpty_i"    -map "SPI_FIFOEmpty"
app_add_port "SPI_Transmission_i" -map "SPI_Transmission"
