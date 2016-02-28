# Pin location and IO standard constraints

# Martin Schm??lzer's Logic Exerciser used the following pin assignments:
#
# Pmod JA1:
#   JA1    =  1  <-> Y11  : Dbg_En_i
#   JA2    =  2  <-> AA11 : Dbg_SCL_i
#   JA3    =  3  <-> Y10  : Dbg_SDA_b
#   JA4    =  4  <-> AA9  : UartRxD_i
#   JA7    =  7  <-> AB11 : UartTxD_o
#   JA8    =  8  <-> AB10 : unused
#   JA9    =  9  <-> AB9  : unused (reserved for Reset(_n)_i)
#   JA10   = 10  <-> AA8  : unused (reserved for Clk_i)
#   GND    =  5
#   GND    = 11
#   VCC3V3 =  6
#   VCC3V3 = 12
#
# Pmod JB1:
#   JB1    =  1  <-> W12  : SimpleSPI SCK
#   JB2    =  2  <-> W11  : SimpleSPI SS_n
#   JB3    =  3  <-> V10  : SimpleSPI MOSI
#   JB4    =  4  <-> W8   : SimpleSPI MISO
#   JB7    =  7  <-> V12  : unused
#   JB8    =  8  <-> W10  : unused
#   JB9    =  9  <-> V9   : unused
#   JB10   = 10  <-> V8   : unused
#   GND    =  5
#   GND    = 11
#   VCC3V3 =  6
#   VCC3V3 = 12
#
# SPI sensors at Pmod JC1:
#   JC1_P  =  1  <-> AB7  : reconf. SCK
#   JC1_N  =  2  <-> AB6  : reconf. SS_n
#   JC2_P  =  3  <-> Y4   : reconf. MOSI
#   JC2_N  =  4  <-> AA4  : reconf. MISO
#   JC3_O  =  7  <-> R6   : unused
#   JC3_N  =  8  <-> T6   : unused
#   JC4_P  =  9  <-> T4   : unused
#   JC4_N  = 10  <-> U4   : unused
#   GND    =  5
#   GND    = 11
#   VCC3V3 =  6
#   VCC3V3 = 12
#
# I2C sensors at Pmod JD1:
#   JD1_P  =  1  <-> V7   : SCL
#   JD1_N  =  2  <-> W7   : SDA
#   JD2_P  =  3  <-> V5   : unused
#   JD2_N  =  4  <-> V4   : unused
#   JD3_O  =  7  <-> W6   : unused
#   JD3_N  =  8  <-> W5   : unused
#   JD4_P  =  9  <-> U6   : unused
#   JD4_N  = 10  <-> U5   : unused
#   GND    =  5
#   GND    = 11
#   VCC3V3 =  6
#   VCC3V3 = 12
#
# 100MHz oscillator for PL, IC17, Fox 767-100-136, bank 13, 3.3V
#   GCLK         <-> Y9
#
# LEDs (bank 33, 3.3V)
#   LD0          <-> T22
#   LD1          <-> T21
#   LD2          <-> U22
#   LD3          <-> U21
#   LD4          <-> V22
#   LD5          <-> W22
#   LD6          <-> U19
#   LD7          <-> U14
#
# Switches (banks 34 and 35, VAdj)
#   SW0          <-> F22
#   SW1          <-> G22
#   SW2          <-> H22
#   SW3          <-> F21
#   SW4          <-> H19
#   SW5          <-> H18
#   SW6          <-> H17
#   SW7          <-> M15
#
# Push buttons (bank 34, VAdj)
#   BTNC         <-> P16
#   BTND         <-> R16
#   BTNL         <-> N15
#   BTNR         <-> R18
#   BTNU         <-> T18
#
# FMC (banks 34 and 35, VAdj)
#   FMC_LA00_CC_N <-> M20
#   FMC_LA00_CC_P <-> M19
#   FMC_LA01_CC_N <-> N20
#   FMC_LA01_CC_P <-> N19
#   FMC_LA02_N    <-> P18
#   FMC_LA02_P    <-> P17
#   FMC_LA03_N    <-> P22
#   FMC_LA03_P    <-> N22
#   FMC_LA04_N    <-> M22
#   FMC_LA04_P    <-> M21
#   FMC_LA05_N    <-> K18
#   FMC_LA05_P    <-> J18
#   FMC_LA06_N    <-> L22
#   FMC_LA06_P    <-> L21
#   FMC_LA07_N    <-> T17
#   FMC_LA07_P    <-> T16
#   FMC_LA08_N    <-> J22
#   FMC_LA08_P    <-> J21
#   FMC_LA09_N    <-> R21
#   FMC_LA09_P    <-> R20
#   FMC_LA10_N    <-> T19
#   FMC_LA10_P    <-> R19
#   FMC_LA11_N    <-> N18
#   FMC_LA11_P    <-> N17
#   FMC_LA12_N    <-> P21
#   FMC_LA12_P    <-> P20
#   FMC_LA13_N    <-> M17
#   FMC_LA13_P    <-> L17
#   FMC_LA14_N    <-> K20
#   FMC_LA14_P    <-> K19
#   FMC_LA15_N    <-> J17
#   FMC_LA15_P    <-> J16
#   FMC_LA16_N    <-> K21
#   FMC_LA16_P    <-> J20
#   FMC_LA17_CC_N <-> B20
#   FMC_LA17_CC_P <-> B19
#   FMC_LA18_CC_N <-> C20
#   FMC_LA18_CC_P <-> D20
#   FMC_LA19_N    <-> G16
#   FMC_LA19_P    <-> G15
#   FMC_LA20_N    <-> G21
#   FMC_LA20_P    <-> G20
#   FMC_LA21_N    <-> E20
#   FMC_LA21_P    <-> E19
#   FMC_LA22_N    <-> F19
#   FMC_LA22_P    <-> G19
#   FMC_LA23_N    <-> D15
#   FMC_LA23_P    <-> E15
#   FMC_LA24_N    <-> A19
#   FMC_LA24_P    <-> A18
#   FMC_LA25_N    <-> C22
#   FMC_LA25_P    <-> D22
#   FMC_LA26_N    <-> E18
#   FMC_LA26_P    <-> F18
#   FMC_LA27_N    <-> D21
#   FMC_LA27_P    <-> E21
#   FMC_LA28_N    <-> A17
#   FMC_LA28_P    <-> A16
#   FMC_LA29_N    <-> C18
#   FMC_LA29_P    <-> C17
#   FMC_LA30_N    <-> B15
#   FMC_LA30_P    <-> C15
#   FMC_LA31_N    <-> B17
#   FMC_LA31_P    <-> B16
#   FMC_LA32_N    <-> A22
#   FMC_LA32_P    <-> A21
#   FMC_LA33_N    <-> B22
#   FMC_LA33_P    <-> B21

set_property PACKAGE_PIN P16  [get_ports "Reset_i"]             ;# BTNC
set_property PACKAGE_PIN Y9   [get_ports "Clk_i"]               ;# GCLK 100MHz 
set_property PACKAGE_PIN Y11  [get_ports "Dbg_En_i"]            ;# Pmod JA1 (pin 1)
set_property PACKAGE_PIN AA11 [get_ports "Dbg_SCL_i"]           ;# Pmod JA2 (pin 2)
set_property PACKAGE_PIN Y10  [get_ports "Dbg_SDA_b"]           ;# Pmod JA3 (pin 3)
#if { $SPI_Module == "Reconf" } {
#  set_property PACKAGE_PIN T22  [get_ports "P1_b[0]"]           ;# LD0
#} elseif { $SPI_Module == "CPU" } {
  set_property PACKAGE_PIN AB6  [get_ports "P1_b[0]"]           ;# Pmod JC1_N (pin 2, SS_n)
#}
set_property PACKAGE_PIN T21  [get_ports "P1_b[1]"]             ;# LD1
set_property PACKAGE_PIN U22  [get_ports "P1_b[2]"]             ;# LD2
set_property PACKAGE_PIN U21  [get_ports "P1_b[3]"]             ;# LD3
set_property PACKAGE_PIN V22  [get_ports "P1_b[4]"]             ;# LD4
set_property PACKAGE_PIN W22  [get_ports "P1_b[5]"]             ;# LD5
set_property PACKAGE_PIN U19  [get_ports "P1_b[6]"]             ;# LD6
set_property PACKAGE_PIN U14  [get_ports "P1_b[7]"]             ;# LD7
set_property PACKAGE_PIN F22  [get_ports "P2_b[0]"]             ;# SW0 (10k between pin an switch --> no danger)
set_property PACKAGE_PIN G22  [get_ports "P2_b[1]"]             ;# SW1
set_property PACKAGE_PIN H22  [get_ports "P2_b[2]"]             ;# SW2
set_property PACKAGE_PIN F21  [get_ports "P2_b[3]"]             ;# SW3
set_property PACKAGE_PIN H19  [get_ports "P2_b[4]"]             ;# SW4
set_property PACKAGE_PIN H18  [get_ports "P2_b[5]"]             ;# SW5
set_property PACKAGE_PIN H17  [get_ports "P2_b[6]"]             ;# SW6
set_property PACKAGE_PIN M15  [get_ports "P2_b[7]"]             ;# SW7
set_property PACKAGE_PIN AA9  [get_ports "UartRxD_i"]           ;# Pmod JA4 (pin 4)
set_property PACKAGE_PIN AB11 [get_ports "UartTxD_o"]           ;# Pmod JA7 (pin 7)

#if { $SPI_Module == "Reconf" } {
#  set_property PACKAGE_PIN W8   [get_ports "MISO_i"]            ;# Pmod JB4 (pin 4)
#  set_property PACKAGE_PIN V10  [get_ports "MOSI_o"]            ;# Pmod JB3 (pin 3)
#  set_property PACKAGE_PIN W12  [get_ports "SCK_o"]             ;# Pmod JB1 (pin 1)
#} elseif { $SPI_Module == "CPU" } {
  set_property PACKAGE_PIN AA4  [get_ports "MISO_i"]            ;# Pmod JC2_N (pin 4)
  set_property PACKAGE_PIN Y4   [get_ports "MOSI_o"]            ;# Pmod JC2_P (pin 3)
  set_property PACKAGE_PIN AB7  [get_ports "SCK_o"]             ;# Pmod JC1_P (pin 1)
#}

set_property PACKAGE_PIN M20  [get_ports "Inputs_i[0]"]         ;# (unused) FMC_LA00_CC_N
set_property PACKAGE_PIN M19  [get_ports "Inputs_i[1]"]         ;# (unused) FMC_LA00_CC_P
set_property PACKAGE_PIN N20  [get_ports "Inputs_i[2]"]         ;# (unused) FMC_LA01_CC_N
set_property PACKAGE_PIN N19  [get_ports "Inputs_i[3]"]         ;# (unused) FMC_LA01_CC_P
set_property PACKAGE_PIN P18  [get_ports "Inputs_i[4]"]         ;# (unused) FMC_LA02_N   
set_property PACKAGE_PIN P17  [get_ports "Inputs_i[5]"]         ;# (unused) FMC_LA02_P   
set_property PACKAGE_PIN P22  [get_ports "Inputs_i[6]"]         ;# (unused) FMC_LA03_N   
set_property PACKAGE_PIN N22  [get_ports "Inputs_i[7]"]         ;# (unused) FMC_LA03_P
#if { $SPI_Module == "Reconf" } {
#  set_property PACKAGE_PIN AB6  [get_ports "Outputs_o[0]"]      ;# Pmod JC1_N (pin 2, SS_n)
#} elseif { $SPI_Module == "CPU" } {
  set_property PACKAGE_PIN T22  [get_ports "Outputs_o[0]"]      ;# LD0
#}
set_property PACKAGE_PIN M21  [get_ports "Outputs_o[1]"]        ;# (unused) FMC_LA04_P
set_property PACKAGE_PIN K18  [get_ports "Outputs_o[2]"]        ;# (unused) FMC_LA05_N
set_property PACKAGE_PIN J18  [get_ports "Outputs_o[3]"]        ;# (unused) FMC_LA05_P
set_property PACKAGE_PIN L22  [get_ports "Outputs_o[4]"]        ;# (unused) FMC_LA06_N
set_property PACKAGE_PIN L21  [get_ports "Outputs_o[5]"]        ;# (unused) FMC_LA06_P
set_property PACKAGE_PIN T17  [get_ports "Outputs_o[6]"]        ;# (unused) FMC_LA07_N
set_property PACKAGE_PIN T16  [get_ports "Outputs_o[7]"]        ;# (unused) FMC_LA07_P
#if { $SPI_Module == "Reconf" } {
#  set_property PACKAGE_PIN AA4  [get_ports "SPIMISO_i"]         ;# Pmod JC2_N (pin 4)
#  set_property PACKAGE_PIN Y4   [get_ports "SPIMOSI_o"]         ;# Pmod JC2_P (pin 3)
#  set_property PACKAGE_PIN AB7  [get_ports "SPISCK_o"]          ;# Pmod JC1_P (pin 1)
#} elseif { $SPI_Module == "CPU" } {
  set_property PACKAGE_PIN W8   [get_ports "SPIMISO_i"]         ;# Pmod JB4 (pin 4)
  set_property PACKAGE_PIN V10  [get_ports "SPIMOSI_o"]         ;# Pmod JB3 (pin 3)
  set_property PACKAGE_PIN W12  [get_ports "SPISCK_o"]          ;# Pmod JB1 (pin 1)
#}
set_property PACKAGE_PIN V7   [get_ports "I2CSCL_b"]            ;# Pmod JD1_P (pin 1)
set_property PACKAGE_PIN W7   [get_ports "I2CSDA_b"]            ;# Pmod JD1_N (pin 2)
#set_property PACKAGE_PIN J22  [get_ports "OneWire_b"]           ;# (unused) FMC_LA08_N   
#set_property PACKAGE_PIN J21  [get_ports "PWM_i"]               ;# (unused) FMC_LA08_P   
#set_property PACKAGE_PIN R21  [get_ports "SENT_i"]              ;# (unused) FMC_LA09_N   
#set_property PACKAGE_PIN R20  [get_ports "SPC_b"]               ;# (unused) FMC_LA09_P   
set_property PACKAGE_PIN R19  [get_ports "AdcConvComplete_i"]   ;# (unused) FMC_LA10_P   
set_property PACKAGE_PIN N18  [get_ports "AdcDoConvert_o"]      ;# (unused) FMC_LA11_N   
set_property PACKAGE_PIN L17  [get_ports "AdcValue_i[0]"]       ;# (unused) FMC_LA13_P   
set_property PACKAGE_PIN K20  [get_ports "AdcValue_i[1]"]       ;# (unused) FMC_LA14_N   
set_property PACKAGE_PIN K19  [get_ports "AdcValue_i[2]"]       ;# (unused) FMC_LA14_P   
set_property PACKAGE_PIN J17  [get_ports "AdcValue_i[3]"]       ;# (unused) FMC_LA15_N   
set_property PACKAGE_PIN J16  [get_ports "AdcValue_i[4]"]       ;# (unused) FMC_LA15_P   
set_property PACKAGE_PIN K21  [get_ports "AdcValue_i[5]"]       ;# (unused) FMC_LA16_N   
set_property PACKAGE_PIN J20  [get_ports "AdcValue_i[6]"]       ;# (unused) FMC_LA16_P   
set_property PACKAGE_PIN B20  [get_ports "AdcValue_i[7]"]       ;# (unused) FMC_LA17_CC_N
set_property PACKAGE_PIN B19  [get_ports "AdcValue_i[8]"]       ;# (unused) FMC_LA17_CC_P
set_property PACKAGE_PIN C20  [get_ports "AdcValue_i[9]"]       ;# (unused) FMC_LA18_CC_N

# Set the bank voltages
# Bank 13, Vcco = 3.3V
set_property IOSTANDARD LVCMOS33 [get_ports -filter { IOBANK == 13 } ]
# Bank 33, Vcco = 3.3V
set_property IOSTANDARD LVCMOS33 [get_ports -filter { IOBANK == 33 } ]
# Bank 34, Vcco = Vadj
set_property IOSTANDARD LVCMOS18 [get_ports -filter { IOBANK == 34 } ]
# Bank 35, Vcco = Vadj
set_property IOSTANDARD LVCMOS18 [get_ports -filter { IOBANK == 35 } ]



