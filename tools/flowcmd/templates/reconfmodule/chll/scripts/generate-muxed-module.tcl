#!trfsmgen
#
# Generate module which directly instantiates all (Ex.)Apps with MUXes
#

# get data from outside this script
set RECONF_BASE   "$env(RECONF_BASE)"
set CELLLIB_BASE  "$env(CELLLIB_BASE)"
set CELLLIB_FILE  "$env(CELLLIB_FILE)"
set CELLLIB_MAPS  "$env(CELLLIB_MAPS)"
set APPS_BASE     "$env(APPS_BASE)"
set INTERSYNTH_FILE "$env(INTERSYNTH_FILE)"

# setup whole reconf.module
source $RECONF_BASE/chll/scripts/setup-reconf-module.tcl
# setup example applications
source $RECONF_BASE/chll/scripts/setup-exapps.tcl

foreach App $ExApps {
  set AppLC       [string tolower "$App"]
  set AppBase     "${APPS_BASE}/$AppLC"
  set Script      "$AppBase/chll/scripts/setup.tcl"

  # set environment variables used by TCL script
  set APP_NAME      "$App"

  # setup application
  source $Script
}

puts ""
puts "##############################################################################################"
list_apps
puts ""

generate_muxed_module -module "MuxedModuleExApps" -filename "$RECONF_BASE/chll/out/muxedmodule-exapps.vhd"

puts ""
puts "##############################################################################################"
puts "## Read post-si apps"
puts "##############################################################################################"
puts ""

foreach Dir [glob -type d "$APPS_BASE/*" ] {
  if [file exists "$Dir/.chll-app"] {
    set fp [open "$Dir/.chll-app" r]
    set fc [read $fp]   ;# $fc is now e.g. "ADT7310\n"
    close $fp
    set fc [split $fc "\n"]   ;# $fc is now e.g. "ADT7310 {}"
    set App [lindex $fc 0]
    set ExAppIdx [lsearch $ExApps $App]
    if {$ExAppIdx >= 0} {
      puts "Skipping Ex.App $App"
      continue
    }
    set AppLC       [string tolower "$App"]
    set AppBase     "${APPS_BASE}/$AppLC"
    set Script      "$AppBase/chll/scripts/setup.tcl"

    # set environment variables used by TCL script
    set APP_NAME      "$App"

    # setup application
    source $Script
  }
}

puts ""
puts "##############################################################################################"
list_apps
puts ""
generate_muxed_module -module "MuxedModuleAllApps" -filename "$RECONF_BASE/chll/out/muxedmodule-allapps.vhd"
