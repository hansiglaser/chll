#!/bin/bash
#
# Start the simulation
#
# Be sure to create a Makefile first and compile all Verilog and VHDL files.
#

function Usage() {
  echo "Usage: $0 -min|-max device"
  echo ""
  echo "device is either ''Testchip2CPU-(Polling|LPM1|LPM3)'' or ''Testchip2CHLL-ADT7310(|P16S16|P32S16|P32S32|P32LS16|P32S16L|P32LS16L|P16L16L|P16LS32L|P32LS32L)''"
  exit 1
}

SDF_DIR="../../../sta-route/output"
FIRMWARE_BASE="../../../../../eval/hardware/mcu-modules/ide"

if [ $# -ne 2 ] ; then
  Usage
fi

if [ "$1" = "-min" ] ; then
  OPCOND="min"
  SDF="$SDF_DIR/chip_BEST_rc_best.sdf"
elif [ "$1" = "-max" ] ; then
  OPCOND="max"
  SDF="$SDF_DIR/chip_WORST_rc_worst.sdf"
  # Encounter only saved rc_worst and rc_best SPEF files, therefore we only
  # calculate SDF files for these two operating conditions. Both SDF files have
  # two values each, but they are identical.
else
  Usage
fi

case "$2" in
  Testchip2CPU-Polling)
    Mode="CPU"
    SPISelect=0
    ImpulseGen=0
    Firmware="$FIRMWARE_BASE/Testchip2-CPU/firmware-polling.do"
    ;;
  Testchip2CPU-LPM1)
    Mode="CPU"
    SPISelect=0
    ImpulseGen=0
    Firmware="$FIRMWARE_BASE/Testchip2-CPU/firmware-lpm1.do"
    ;;
  Testchip2CPU-LPM3)
    Mode="CPU"
    SPISelect=0
    ImpulseGen=1
    Firmware="$FIRMWARE_BASE/Testchip2-CPU/firmware-lpm3.do"
    ;;
  Testchip2CHLL-ADT7310)
    Mode="CHLL"
    SPISelect=1
    ImpulseGen=0
    App="ADT7310"
    PeriodCounter=16
    SPICounter=32
    Firmware="$FIRMWARE_BASE/Testchip2-CHLL/firmware-adt7310.do"
    ;;
  Testchip2CHLL-ADT7310P16S16)
    Mode="CHLL"
    SPISelect=1
    ImpulseGen=0
    App="ADT7310P16S16"
    PeriodCounter=16
    SPICounter=16
    Firmware="$FIRMWARE_BASE/Testchip2-CHLL/firmware-adt7310p16s16.do"
    ;;
  Testchip2CHLL-ADT7310P32S16)
    Mode="CHLL"
    SPISelect=1
    ImpulseGen=0
    App="ADT7310P32S16"
    PeriodCounter=32
    SPICounter=16
    Firmware="$FIRMWARE_BASE/Testchip2-CHLL/firmware-adt7310p32s16.do"
    ;;
  Testchip2CHLL-ADT7310P32S32)
    Mode="CHLL"
    SPISelect=1
    ImpulseGen=0
    App="ADT7310P32S32"
    PeriodCounter=32
    SPICounter=32
    Firmware="$FIRMWARE_BASE/Testchip2-CHLL/firmware-adt7310p32s32.do"
    ;;
  Testchip2CHLL-ADT7310P32LS16)
    Mode="CHLL"
    SPISelect=1
    ImpulseGen=0
    App="ADT7310P32LS16"
    PeriodCounter=32
    SPICounter=16
    Firmware="$FIRMWARE_BASE/Testchip2-CHLL/firmware-adt7310p32ls16.do"
    ;;
  Testchip2CHLL-ADT7310P32S16L)
    Mode="CHLL"
    SPISelect=1
    ImpulseGen=0
    App="ADT7310P32S16L"
    PeriodCounter=32
    SPICounter=16
    Firmware="$FIRMWARE_BASE/Testchip2-CHLL/firmware-adt7310p32s16l.do"
    ;;
  Testchip2CHLL-ADT7310P32LS16L)
    Mode="CHLL"
    SPISelect=1
    ImpulseGen=0
    App="ADT7310P32LS16L"
    PeriodCounter=32
    SPICounter=16
    Firmware="$FIRMWARE_BASE/Testchip2-CHLL/firmware-adt7310p32ls16l.do"
    ;;
  Testchip2CHLL-ADT7310P16LS16L)
    Mode="CHLL"
    SPISelect=1
    ImpulseGen=0
    App="ADT7310P16LS16L"
    PeriodCounter=16
    SPICounter=16
    Firmware="$FIRMWARE_BASE/Testchip2-CHLL/firmware-adt7310p16ls16l.do"
    ;;
  Testchip2CHLL-ADT7310P16LS32L)
    Mode="CHLL"
    SPISelect=1
    ImpulseGen=0
    App="ADT7310P16LS32L"
    PeriodCounter=16
    SPICounter=32
    Firmware="$FIRMWARE_BASE/Testchip2-CHLL/firmware-adt7310p16ls32l.do"
    ;;
  Testchip2CHLL-ADT7310P32LS32L)
    Mode="CHLL"
    SPISelect=1
    ImpulseGen=0
    App="ADT7310P32LS32L"
    PeriodCounter=32
    SPICounter=32
    Firmware="$FIRMWARE_BASE/Testchip2-CHLL/firmware-adt7310p32ls32l.do"
    ;;
  *)
    echo "Device must start with either ''Testchip2CPU-(Polling|LPM1|LPM3)'' or ''Testchip2CHLL-ADT7310(|P16S16|P32S16|P32S32|P32LS16|P32S16L|P32LS16L|P16LS16L|P16LS32L|P32LS16L)'', but ''$2'' doesn''t work"
    Usage
    ;;
esac
Device="$2"
 
SAIF_BASE="sim-chip-power-${OPCOND}-${Device}" # will be appended with testpoint parameters and ".saif"
VCD_FILE="/opt/tmp/test.vcd.gz" # use empty string to disable VCD, use ".gz" as suffix to enable compression

echo "Writing SAIF files to $SAIF_BASE-*"
if [ -n "$VCD_FILE" ] ; then
  echo "Writing VCD file $VCD_FILE"
fi

# generate firmware LSB/MSB files
FIRMWARE_LSB="firmware-lsb.do"
FIRMWARE_MSB="firmware-msb.do"
FIRMWARE_RAM0="firmware-ram0.do"
FIRMWARE_RAM1="firmware-ram1.do"
FIRMWARE_RAM2="firmware-ram2.do"
FIRMWARE_RAM3="firmware-ram3.do"
echo "Generating firmware LSB/MSB files from $Firmware"
sed -r 's/^(.*] 16#)..(...*)$/\1\2/' "$Firmware" > "$FIRMWARE_LSB"
sed -r 's/^(.*] 16#..)..(.*)$/\1\2/' "$Firmware" > "$FIRMWARE_MSB"
grep '#0[0-7]..#' $FIRMWARE_LSB > $FIRMWARE_RAM0
grep '#0[0-7]..#' $FIRMWARE_MSB > $FIRMWARE_RAM1
grep '#0[89A-Fa-f]..#' $FIRMWARE_LSB | sed -r 's/#08/#00/;s/#09/#01/;s/#0[Aa]/#02/;s/#0[Bb]/#03/;s/#0[Cc]/#04/;s/#0[Dd]/#05/;s/#0[Ee]/#06/;s/#0[Ff]/#07/' > $FIRMWARE_RAM2
grep '#0[89A-Fa-f]..#' $FIRMWARE_MSB | sed -r 's/#08/#00/;s/#09/#01/;s/#0[Aa]/#02/;s/#0[Bb]/#03/;s/#0[Cc]/#04/;s/#0[Dd]/#05/;s/#0[Ee]/#06/;s/#0[Ff]/#07/' > $FIRMWARE_RAM3

# Simulation absolutely requires the SDF delay information, because the DMem
# RAM memgen128x8 is extremely picky on setup and hold time violations and
# erases the whole memory with 'X' on errors.
# Simulation is quite fast with SDF (25 sec.), so thats ok.

cat <<- EOF > vsim.do
# suppress warnings on unconnected Q and QN outputs of D-FF
# add "-debugdb" to enable causality tracing
vsim -t 1ps \
  -voptargs="+acc -suppress 2685,2718" \
  -sdf$OPCOND /chip_power_tb/DUT=$SDF \
  +sdf_verbose \
  -sdfnoerror \
  -GSPISelect=$SPISelect \
  -GImpulseGen=$ImpulseGen \
  chip_power_tb
do wave.do
set PMEM_REG sim:/chip_power_tb/DUT/core_1/PMem_0/ram_0/memory
do firmware-ram0.do
set PMEM_REG sim:/chip_power_tb/DUT/core_1/PMem_0/ram_1/memory
do firmware-ram1.do
set PMEM_REG sim:/chip_power_tb/DUT/core_1/PMem_0/ram_2/memory
do firmware-ram2.do
set PMEM_REG sim:/chip_power_tb/DUT/core_1/PMem_0/ram_3/memory
do firmware-ram3.do

# power analysis
array unset "Testpoints"
do power-testpoints.tcl
do power-funcs.tcl
PowerSetup "$SAIF_BASE" "$VCD_FILE" "Testpoints"

#log -r /*

# resume after "run -all" after the "Finished"-assertion has occured
set broken 0
onbreak {
  set broken 1
  resume
}

# avoid warnings of numeric_std functions with 'X' values, see http://www.ht-lab.com/howto/modelsim/Modelsim_tips.html
set StdArithNoWarnings 1
run 0 ns;
set StdArithNoWarnings 0

# add a trigger to clear 'X'es in DMem to '0'
# unfortunately this doesn't work, there is something which sets the whole
# DMem back to 'X' latest after the next clock edge :-(
#proc ClearDMem {} {
#  global now;
#  echo "Clearing DMem at de-assertion of Reset_n_i at \$now"
#  for {set i 0} {\$i<128} {incr i} {
#    force -deposit /chip_power_tb/DUT/core_1/DMem_0/sram128x8_0/PLAN_MEM\[\$i\] "00000000"
#    force -deposit /chip_power_tb/DUT/core_1/DMem_0/sram128x8_1/PLAN_MEM\[\$i\] "00000000"
#  }
#}
#when -fast {Reset_n_i'event and Reset_n_i = '1'} ClearDMem

run -all
#run 300us

#PowerModeStopAll
#quit
EOF

#cat vsim.do
#exit
#vsim -do vsim.do
