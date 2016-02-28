

proc PowerModeStart {PowerModeIndex} {
  global PowerModesArr
  global "$PowerModesArr"
  global PowerSaifBase
  global now
  global PowerModesActive

  set mode [lindex [array get $PowerModesArr $PowerModeIndex] 1]
  set name   [lindex $mode 0]
  set tstart [lindex $mode 1]
  set tend   [lindex $mode 2]

  # keep list of currently active power modes
  if {[lsearch $PowerModesActive $PowerModeIndex] != -1} {
    echo "Cannot start power mode $PowerModeIndex \"$name\", it is already active."
    stop
  }
  lappend PowerModesActive $PowerModeIndex

  echo "Start of power analysis periode $name at $now"
  power reset -all -r *
  power on
}

proc PowerModeStop {PowerModeIndex} {
  global PowerModesArr
  global "$PowerModesArr"
  global PowerSaifBase
  global now
  global PowerModesActive

  set mode [lindex [array get $PowerModesArr $PowerModeIndex] 1]
  set name   [lindex $mode 0]
  set tstart [lindex $mode 1]
  set tend   [lindex $mode 2]

  # keep list of currently active power modes
  if {[lsearch $PowerModesActive $PowerModeIndex] == -1} {
    echo "Cannot stop power mode $PowerModeIndex \"$name\", it is not active."
    stop
  }
  # remove from list
  set PowerModesActive [lsearch -inline -all -not -exact $PowerModesActive $PowerModeIndex]

  set Filename "${PowerSaifBase}${name}.saif"
  echo "End   of power analysis periode $name at $now, saving to $Filename"
  power report -all -bsaif "$Filename"
  power off
  # now we have to update the "DURATION" field, because at least Questa Sim
  # 10.0 specifies the total simulation runtime instead of the current power
  # logging duration (don't use $tend but $now, because $tend might be larger
  # than the current/total simulation time)
  set duration [expr $now - $tstart]
  echo "Updating total duration in SAIF file to $duration"
  exec sed -ri "s/^(\\(DURATION) \[0-9\]*\\)/\\1 $duration)/" "$Filename"
}

proc PowerModeStopAll {} {
  global PowerModesActive
  if {[llength $PowerModesActive] == 0} {
    return
  }
  echo "Stopping running power mode logs."
  foreach {PowerModeIndex} $PowerModesActive {
    PowerModeStop $PowerModeIndex
  }
}

proc PowerSetup {SaifBase ModesArr} {
  global PowerModesArr
  global PowerSaifBase
  set PowerSaifBase "$SaifBase"
  set PowerModesArr "$ModesArr"
  global "$PowerModesArr"
  # list to track currently active power modes
  global PowerModesActive
  set PowerModesActive [list ]

  echo "Setting up power stuff with SaifBase = $PowerSaifBase and ModesArr = $PowerModesArr"
  stop
  foreach {id mode} [array get $PowerModesArr] {
    set name   [lindex $mode 0]
    set tstart [lindex $mode 1]
    set tend   [lindex $mode 2]
    echo "$name $tstart $tend"
  
    when -fast "\$now == $tstart" "PowerModeStart $id"
    when -fast "\$now == $tend"   "PowerModeStop $id"
  }
}
