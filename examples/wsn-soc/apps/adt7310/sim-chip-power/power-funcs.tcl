

proc PowerTrigger {} {
  global PowerTestpointsArr
  global "$PowerTestpointsArr"
  global PowerTestpointsIdx
  global PowerSaifName
  global PowerVcdFile
  global now;
  global PowerActive;
  if {!$PowerActive} {
    # not doing power logging --> start
    echo "$now: [clock format [clock seconds] -format {%Y-%m-%d %H:%M:%S}]"
    echo "Start of power analysis periode at $now"
    power reset -all -r *
    power on
    set PowerActive $now
    # VCD
    if { [string length "$PowerVcdFile"] > 0 } {
      vcd on
    }
  } else {
    # currently doing power logging --> stop
    set duration [expr $now - $PowerActive]
    set ClkDiv [examine -radix unsigned "/chip_power_tb/ClkDiv"]
    # 10,000ps for 100MHz clock
    set Clks [expr $duration / (10000.0 * ($ClkDiv+1))]
    echo "End   of power analysis periode at $now ($duration ps, $Clks clock cycles), saving to $PowerSaifName"
    echo "$now: [clock format [clock seconds] -format {%Y-%m-%d %H:%M:%S}]"
    power report -all -bsaif "$PowerSaifName"
    power off
    # now we have to update the "DURATION" field, because at least Questa Sim
    # 10.0 specifies the total simulation runtime instead of the current power
    # logging duration (don't use $tend but $now, because $tend might be larger
    # than the current/total simulation time)
    echo "Updating total duration in SAIF file to $duration"
    exec sed -ri "s/^(\\(DURATION) \[0-9\]*\\)/\\1 $duration)/" "$PowerSaifName"
    # VCD
    if { [string length "$PowerVcdFile"] > 0 } {
      vcd flush
      vcd off
      echo "$ vcd2saif -64 -input $PowerVcdFile -output vcd$PowerSaifName.gz -time $PowerActive $now"
    }
    # finally we setup the next testpoint
    set PowerActive 0
    if {[array size $PowerTestpointsArr] > $PowerTestpointsIdx} {
      # yes, there are more entries
      SetupNextTestpoint
    } else {
      # no more entries --> stop simulation
      stop
    }
  }
}

# use an empty VcdFile to disable VCD
proc PowerSetup {SaifBase VcdFile TestpointsArr} {
  global PowerTestpointsArr
  global PowerTestpointsIdx
  global PowerSaifBase
  global PowerSaifName
  global PowerVcdFile
  set PowerTestpointsArr "$TestpointsArr"
  set PowerTestpointsIdx 0
  set PowerSaifBase      "$SaifBase"
  set PowerSaifName      ""   ;# will be set by SetupNextTestpoint
  set PowerVcdFile       "$VcdFile"
  global "$PowerTestpointsArr"

  global PowerActive
  set PowerActive 0

  echo "Setting up power stuff with SaifBase = $PowerSaifBase and TestpointsArr = $PowerTestpointsArr"
  # setup the first testpoint at 100us into simulation time
  when -fast "\$now == 100000000" "SetupNextTestpoint"
  when -fast {Trigger_s'event and Trigger_s = '1'} PowerTrigger

  power add -nocellnet -r /chip_power_tb/DUT/*  
  #power add -r /chip_power_tb/DUT/*
  power off

  # VCD
  if { [string length "$PowerVcdFile"] > 0 } {
    vcd file "$PowerVcdFile"
    vcd add -r -optcells /chip_power_tb/DUT/*
    vcd off
  }
}

proc SetupTestpoint {ClkDiv ClkDivRTC Threshold CycleTime BusDivider} {
  force -deposit /chip_power_tb/HaveTestpoint true
  force -deposit /chip_power_tb/ClkDiv        10#$ClkDiv
  force -deposit /chip_power_tb/ClkDivRTC     10#$ClkDivRTC
  force -deposit /chip_power_tb/Threshold     10#$Threshold
  force -deposit /chip_power_tb/CycleTime     10#$CycleTime
  force -deposit /chip_power_tb/BusDivider    10#$BusDivider
}

proc SetupNextTestpoint {} {
  global PowerTestpointsArr
  global PowerTestpointsIdx
  global "$PowerTestpointsArr"
  global PowerSaifBase
  global PowerSaifName
  global now;
  set Arr [array get $PowerTestpointsArr $PowerTestpointsIdx]  ;# returns a list of pairs with array keys and values
  if {[llength $Arr] != 2} {
    echo "ERROR: There is no testpoint $PowerTestpointsIdx."
    stop
    return
  }
  set Testpoint [lindex $Arr 1]   ;# get value
  set ClkDiv     [lindex $Testpoint 0]
  set ClkDivRTC  [lindex $Testpoint 1]
  set Threshold  [lindex $Testpoint 2]
  set CycleTime  [lindex $Testpoint 3]
  set BusDivider [lindex $Testpoint 4]
  echo "##########################################################################################################################################"
  echo "Setting up testpoint $PowerTestpointsIdx with ClkDiv = $ClkDiv, ClkDivRTC = $ClkDivRTC, Threshold = $Threshold, CycleTime = $CycleTime, BusDivider = $BusDivider"
  echo "$now: [clock format [clock seconds] -format {%Y-%m-%d %H:%M:%S}]"
  SetupTestpoint $ClkDiv $ClkDivRTC $Threshold $CycleTime $BusDivider
  incr PowerTestpointsIdx
  set PowerSaifName "${PowerSaifBase}-${ClkDiv}_${ClkDivRTC}_${Threshold}_${CycleTime}_${BusDivider}.saif"
}

