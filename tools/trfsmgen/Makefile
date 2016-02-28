
PAS_READLINE  = ../common/pas-readline
PAS_TCL       = ../common/pas-tcl
PAS_BASEUNITS = ../common/baseunits
FLOWPROC       = ../flowproc

FPC=fpc
FPC_OPT = -Fu$(PAS_READLINE)/src -Fu$(PAS_TCL)/src -Fu$(PAS_BASEUNITS) -Fu$(FLOWPROC)
LAZBUILD=lazbuild

all: trfsmgen man/man1/trfsmgen.1tr

trfsmgen: trfsmgen.lpr
	#$(LAZBUILD) $@.lpi
	$(FPC) $(FPC_OPT) -o$@ $<

man/man1/trfsmgen.1tr: trfsmgen.lpr
	# Strange, if just written as script, make starts it using /bin/sh,
	# but this shell does not provide the required features. Therefore
	# we directly start it with bash. :-)
	bash $(PAS_TCL)/bin/genman.sh $<

clean:
	rm -rf *.o *.ppu trfsmgen *.compiled *.res *~ man/ lib/ backup/
