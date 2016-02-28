#!flowproc
#
# Check synthesized netlists of all cells
#

# get data from outside this script
set RECONF_BASE   "$env(RECONF_BASE)"
set CELLLIB_BASE  "$env(CELLLIB_BASE)"
set CELLLIB_FILE  "$env(CELLLIB_FILE)"
set CELLLIB_MAPS  "$env(CELLLIB_MAPS)"

# helper procedure to append the contents of $Filename to the open file $of
proc AppendModules {of Filename} {
  global ExtractOrder 
  set rf [open "$Filename" "r"]
  while {1} {
    set line [gets $rf]
    if {[eof $rf]} {
      break;
    }
    # for every module in the ILang file, notify the user and add the extract_order attribute
    if [regexp {^module \\(\w+)} $line fullmatch Module] {
      puts "Adding module $Module to cell library"
      puts $of "attribute \\extract_order $ExtractOrder"
      set ExtractOrder [ expr $ExtractOrder + 1 ]
    }
    puts $of $line
  }
  close $rf
}

# setup whole reconf.module
source $RECONF_BASE/chll/scripts/setup-reconf-module.tcl
# setup cell library
source $RECONF_BASE/chll/scripts/setup-celllib.tcl

puts "################################################################################"
puts "## Merging all cells into $CELLLIB_FILE"

# create temporary file to copy individual ILang files to
set of [open "$CELLLIB_FILE.new" "w"]

set CellMapsFiles [list]

set ExtractOrder 1
foreach Cell $CellLib {
  set CellLC [string tolower "$Cell"]
  set CELL_NAME "$Cell"
  set CELL_ILANG_OUT "$CELLLIB_BASE/$CellLC/chll/out/$CellLC.il"

  # assemble cell library
  set CellRefs "$CELLLIB_BASE/$CellLC/chll/out/$CellLC-impl.il" ;# contains netlists for "extract"
  set CellMaps "$CELLLIB_BASE/$CellLC/chll/out/$CellLC-map.il"  ;# contains netlists to map back using "techmap"
  lappend CellMapsFiles $CellMaps
  puts $of "# Cell $Cell with all variants from $CellRefs"
  AppendModules $of "$CellRefs"
  puts $of ""

  # TODO: the above is not clean, because we create file names inside this TCL
  #   script, which are normally created by flowcmd, and therefore get a
  #   possible inconsistency
}

close $of

file rename -force $CELLLIB_FILE.new $CELLLIB_FILE

# create library for back mapping of topological and reduced variants to use
# the main variant
exec cat {*}$CellMapsFiles > $CELLLIB_MAPS

puts ""
puts "################################################################################"
puts "## Cell Library:"

list_cells

puts "## Done."
