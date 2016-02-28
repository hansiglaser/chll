#!trfsmgen
#
# Generate InterSynth netlist
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
# setup cell library
source $RECONF_BASE/chll/scripts/setup-celllib.tcl
# read TR-FSM wrapper cells using this auto-generated script by
# setup-trfsms.tcl invoked by "flowcmd insert-trfsms"
source $RECONF_BASE/chll/out/setup-trfsm-cells.tcl
# setup example applications
source $RECONF_BASE/chll/scripts/setup-exapps.tcl

foreach App $ExApps {
  set AppLC       [string tolower "$App"]
  set AppBase     "${APPS_BASE}/$AppLC"
  set Script      "$AppBase/chll/scripts/setup.tcl"
  set OutDir      "$AppBase/chll/out"
  set ILangFile   "$OutDir/${AppLC}-extract-intersynth-trfsm.il"

  # set environment variables used by TCL script
  set APP_NAME      "$App"

  # setup application
  source $Script

  # Read and check synthesized netlist
  app_read_netlist -extracted $ILangFile
}

puts ""
puts "##############################################################################################"
list_apps
#app_show -all

# show all cell instances
puts ""
puts "##############################################################################################"
puts "## Resources #################################################################################"
puts "##############################################################################################"
puts ""
app_print_usage -all
puts ""
puts "##############################################################################################"
puts ""

puts "## Setting interconnect tree parameters for each connection type"
set SCRIPT_BASE [file dirname [info script]]
source $SCRIPT_BASE/setup-conntype-trees.tcl

foreach {ConnType Trees} [array get ConnTypeTrees] {
  set Cost $ConnTypeCost($ConnType)
  set_conntype_trees "$ConnType" $Trees $Cost
}

puts ""
puts "##############################################################################################"

puts "## Writing connection type information for InterSynth"
write_intersynth -conntypes -o "$RECONF_BASE/chll/out/presilicon-conntypes.txt"

puts "## Writing dynamic port information for InterSynth"
write_intersynth -dyn_ports -o "$RECONF_BASE/chll/out/presilicon-dyn_ports.txt"

puts "## Writing cell types for InterSynth"
write_intersynth -celltypes -o "$RECONF_BASE/chll/out/presilicon-celltypes.txt"

puts "## Writing netlists for InterSynth"
write_intersynth -netlists -o "$RECONF_BASE/chll/out/presilicon-exapps.txt"

puts "## Writing standard cells used for InterSynth"
write_intersynth -stdcells -o "$RECONF_BASE/chll/out/presilicon-stdcells.v"

set Name [get_reconf_module -istypename]

set StatsFile "$RECONF_BASE/chll/out/intersynth-stats.txt"

puts "## Writing command file for InterSynth to $INTERSYNTH_FILE"
set of [open "$INTERSYNTH_FILE" "w"]
puts $of "#!intersynth"
puts $of "#"
puts $of "# Optimize interconnect for example netlists"
puts $of "#"
puts $of "# Auto-generated by [info script]"
puts $of "#"
puts $of ""
puts $of "# Read connection type information"
puts $of "load $RECONF_BASE/chll/out/presilicon-conntypes.txt"
puts $of "# Read dynamic port information"
puts $of "load $RECONF_BASE/chll/out/presilicon-dyn_ports.txt"
puts $of "# Read cell types"
puts $of "load $RECONF_BASE/chll/out/presilicon-celltypes.txt"
puts $of "# Read netlists of example applications"
puts $of "load $RECONF_BASE/chll/out/presilicon-exapps.txt"
puts $of ""
puts $of "# Setup oversizing rules, perform interconnect generation"
puts $of "load $RECONF_BASE/chll/scripts/presilicon.txt"
puts $of ""
puts $of "# Print some information"
puts $of "stats"
puts $of "stats $StatsFile"
puts $of ""
puts $of "# Save results"
puts $of "write_data $RECONF_BASE/chll/out/presilicon-baseconfig.txt baseconfig"
puts $of "write_data $RECONF_BASE/chll/out/presilicon-cellmapping.txt cellmapping"
foreach App $ExApps {
  set AppLC   [string tolower "$App"]
  set AppBase "${APPS_BASE}/$AppLC"
  set OutDir  "$AppBase/chll/out"
  puts $of "write_data $OutDir/presilicon-netlist-$AppLC.txt netlist $App"
  puts $of "write_bitdata $OutDir/presilicon-bitdata-$AppLC.cfg netlist $App"
  puts $of "write_graphviz $OutDir/presilicon-netlist-$AppLC.dot"
  foreach {ConnType Trees} [array get ConnTypeTrees] {
    for {set Tree 0} {$Tree < $Trees} {incr Tree} {
      puts $of "write_tikz $OutDir/presilicon-tree-$AppLC-$ConnType-$Tree.tex $App $ConnType $Tree"
    }
  }
}
puts $of "write_html $RECONF_BASE/chll/out/presilicon.html"
puts $of "write_tcl $RECONF_BASE/chll/out/presilicon.tcl"                 ;# \[ prefix <prefix> \]"
puts $of "write_verilog $RECONF_BASE/chll/out/presilicon.v name $Name"    ;# \[ truth \] \[ config \]"
close $of

# Create LaTeX file which includes the TikZ images of all trees
foreach App $ExApps {
  set AppLC   [string tolower "$App"]
  set AppBase "${APPS_BASE}/$AppLC"
  set Script  "$AppBase/chll/scripts/setup.tcl"
  set OutDir  "$AppBase/chll/out"
  set of [open "$OutDir/presilicon-trees-$AppLC.tex" "w"]
  puts $of "\\documentclass\[landscape\]{article}"
  puts $of "\\usepackage\[a3paper,margin=1.5cm\]{geometry}"
  puts $of "\\usepackage{tikz}"
  puts $of "\\usetikzlibrary{external}"
  puts $of "% If your pictures are too big, enable externalization and use the"
  puts $of "% individual files. Don't forget to execute \"pdflatex\" with the"
  puts $of "% \"-shell-escape\" flag."
  puts $of "%\\tikzexternalize\["
  puts $of "%  prefix=presilicon-trees/"
  puts $of "%\]"
  puts $of "%"
  puts $of "% If you want the images scaled to the page width, enclose the"
  puts $of "% \\begin{tikzpicture} ... \\end{tikzpicture} with"
  puts $of "%   \\resizebox{\\textwidth}{!}{% ... }"
  puts $of "%"
  puts $of "\\begin{document}"
  puts $of ""
  puts $of "% make '_' no longer have a special meaning"
  puts $of "\\catcode`\\_=12"
  puts $of ""
  foreach {ConnType Trees} [array get ConnTypeTrees] {
    for {set Tree 0} {$Tree < $Trees} {incr Tree} {
      puts $of "\\section{$App: $ConnType\[$Tree\]}"
      puts $of "\\tikzsetnextfilename{presilicon-tree-$AppLC-$ConnType-$Tree}"
      puts $of "\\begin{tikzpicture}"
      puts $of "\\input{$OutDir/presilicon-tree-$AppLC-$ConnType-$Tree.tex}"
      puts $of "\\end{tikzpicture}"
      puts $of ""
    }
  }
  puts $of "\\end{document}"
  close $of
}

puts ""
puts "## Done."
