#!yosys
#
# Synthesize cell
#
# This file is executed by "flowcmd check-cell"
#
# Generates the output files $CELL_ILANG_OUT, $CELL_VLOG_OUT with the main
# implementation plus $CELL_ILANG_BASE-impl.il with the reference circuits for
# the "extract" pass plus $CELL_ILANG_BASE-map.il with the netlists for
# back-mapping with the "techmap" pass.
#

# remove the following two lines
#puts "ERROR: You have to edit this file [info script] before executing."
#exit 1

# List all topological variants. Module names must be ${CELL_NAME}_${TopVar}
# Don't call them "Main" because this is used as an internal marker for the
# main variant
set TopVars [list ]

# List all reduced variants. Be sure to sort them from big to small. Same rules
# for module names apply.
set RedVars [list RV1 RV2]

# List all hand-crafted implementations of the reduced variants. Module names
# must be ${CELL_NAME}_${RedVar}_${Impl}. Don't use the topological variants'
# names.
# Additionally to these hand-crafted implementations, implementations with the
# main and all topological variants will be derived automatically.
# Attention: Don't use quotes for the associative array indices!
set RedVarsImpl(RV1) [list Timer]
set RedVarsImpl(RV2) [list Timer]

# get data from outside this script
set CELL_BASE      "$env(CELL_BASE)"
set CELL_NAME      "$env(CELL_NAME)"
set CELL_NAME_LC   "$env(CELL_NAME_LC)"
set INTERACTIVE    "$env(INTERACTIVE)"
set CELL_ILANG_OUT "$env(CELL_ILANG_OUT)"
set CELL_VLOG_OUT  "$env(CELL_VLOG_OUT)"
set CELL_ILANG_BASE [file rootname "$CELL_ILANG_OUT"]   ;# strip off extension

# setup variables
set VLOG_DIR $CELL_BASE/verilog

# setup TCL commands to be equal to the yosys commands (except "proc" -> "procs")
yosys -import

### Read all source files ####################################################

read_verilog $VLOG_DIR/${CELL_NAME_LC}.v
read_verilog $VLOG_DIR/${CELL_NAME_LC}_rv1.v
read_verilog $VLOG_DIR/${CELL_NAME_LC}_rv2.v

### Check that all modules exist #############################################

select -assert-any "${CELL_NAME}"                           ;# main implementation
foreach TopVar $TopVars {
  select -assert-any "${CELL_NAME}_${TopVar}"               ;# topological variants
}
foreach RedVar $RedVars {
  select -assert-any "${CELL_NAME}_${RedVar}"               ;# reduced variant wrappers
  foreach Impl $RedVarsImpl($RedVar) {
    select -assert-any "${CELL_NAME}_${RedVar}_${Impl}"     ;# hand-crafted implementations
  }
}

### Synthesize all netlists ##################################################

# build parametric modules as needed
hierarchy

# Replace processes by MUXes and D-FFs
procs

# Optimize
opt -mux_undef -mux_bool

#if { $INTERACTIVE == "true" } {
#  show $CELL_NAME
#  shell
#  exit
#}

### Check Equivalence ########################################################

design -save PreEC
techmap -map $CELL_BASE/chll/scripts/adff2dff.v
design -save PreECExpose
# Topological variants vs. main implementation
foreach TopVar $TopVars {
  puts "Checking equivalence of ${CELL_NAME} vs. ${CELL_NAME}_${TopVar}"
  # expose internal state variables
  expose -shared "${CELL_NAME}" "${CELL_NAME}_${TopVar}" 
  # create "miter" module
  miter -equiv -make_assert -make_outputs "${CELL_NAME}" "${CELL_NAME}_${TopVar}" "miter"
  select -module "miter"
  flatten
  opt
  sat -tempinduct -seq 1 -set-init-zero -set-at 1 in_Reset_n_i 1 -prove-asserts -verify -show-inputs -show-outputs
  design -load PreECExpose
}

if {0} { ;# disable equivalence checking, because this module has different signal names which "miter" doesn't 
# Hand-crafted implementations of reduced variants vs. wrapped main implementation
foreach RedVar $RedVars {
  foreach Impl $RedVarsImpl($RedVar) {
    puts "Checking equivalence of ${CELL_NAME}_${RedVar} vs. ${CELL_NAME}_${RedVar}_${Impl}"
    # expose internal state variables
    expose -shared "${CELL_NAME}_${RedVar}" "${CELL_NAME}_${RedVar}_${Impl}"
    # create "miter" module
    miter -equiv -make_assert -make_outputs "${CELL_NAME}_${RedVar}" "${CELL_NAME}_${RedVar}_${Impl}" "miter"
    select -module "miter"
    flatten
    opt
    sat -tempinduct -seq 1 -set-init-zero -set-at 1 in_Reset_n_i 1 -prove-asserts -verify -show-inputs -show-outputs
    design -load PreECExpose
  }
}
}
design -load PreEC

### Save netlists ############################################################

# Save main implementation for simulation, synthesis, ...
select "${CELL_NAME}"
write_ilang -selected "$CELL_ILANG_OUT"
write_verilog -norename -noexpr -selected "$CELL_VLOG_OUT"

# Save main implementation for "extract" pass
select "${CELL_NAME}"
dump -outfile "${CELL_ILANG_BASE}-impl.il"

# Save all topological variants for "extract" pass
select -none
foreach TopVar $TopVars {
  select -add "${CELL_NAME}_${TopVar}"
}
dump -append "${CELL_ILANG_BASE}-impl.il"

# Save all hand-crafted variants for "extract" pass
select -none
foreach RedVar $RedVars {
  foreach Impl $RedVarsImpl($RedVar) {
    select -add "${CELL_NAME}_${RedVar}_${Impl}"
  }
}
dump -append "${CELL_ILANG_BASE}-impl.il"

# Remove hand-crafted modules, we don't need them any more
delete
select -clear

### Write library to map back reduced variants ###############################

design -save "NotFlattened"

# select all wrapper modules of reduced variants
select -none
foreach RedVar $RedVars {
  # build list of mapping-equivalent modules
  set Map [list]
  foreach Impl $RedVarsImpl($RedVar) {
    lappend Map "${CELL_NAME}_${RedVar}_${Impl}"
  }
  foreach TopVar $TopVars {
    lappend Map "${CELL_NAME}_${RedVar}_${TopVar}"
  }
  # set attribute "techmap_celltype" to make this cell also match the other
  # implementations
  setattr -mod -set "techmap_celltype" "\"[join $Map { }]\"" "${CELL_NAME}_${RedVar}"

  select -add "${CELL_NAME}_${RedVar}"
}

select -set "MapBack" %

# create a thin wrappers for mapping back: select everything and put that
# inside a sub-module called $CELL_NAME, then save this thin wrapper
foreach TopVar $TopVars {
  set CellName "${CELL_NAME}_${TopVar}"
  #select ${CellName}/w:* ${CellName}/m:* ${CellName}/c:* ${CellName}/p:*
  select ${CellName}
  opt_clean -purge   ;# remove internal signal alias names
  submod -name "$CELL_NAME"
  # add this module to back-mapping selection
  select @MapBack
  select -add "$CellName"
  select -set "MapBack" %
}

write_ilang -selected "${CELL_ILANG_BASE}-map.il"

select -clear

design -load "NotFlattened"

### Flatten and optimize reduced variants with main variant ##################

select -none
foreach RedVar $RedVars {
  yosys rename "${CELL_NAME}_$RedVar" "${CELL_NAME}_${RedVar}_Main"
  select -add "${CELL_NAME}_${RedVar}_Main"
}
flatten
opt
dump -append "${CELL_ILANG_BASE}-impl.il"

### Flatten and optimize reduced variants with topological variants ##########

foreach TopVar $TopVars {
  design -load "NotFlattened"

  # replace main variant
  yosys rename "${CELL_NAME}_${TopVar}" "${CELL_NAME}"

  select -none
  foreach RedVar $RedVars {
    yosys rename "${CELL_NAME}_$RedVar" "${CELL_NAME}_${RedVar}_${TopVar}"
    select -add "${CELL_NAME}_${RedVar}_${TopVar}"
  }
  flatten
  opt
  # TODO: check equivalence
  dump -append "${CELL_ILANG_BASE}-impl.il"
  select -clear
}

select -clear
