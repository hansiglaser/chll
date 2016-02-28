
PAS_READLINE  = ../common/pas-readline
PAS_TCL       = ../common/pas-tcl
PAS_BASEUNITS = ../common/baseunits

FPC=fpc
FPC_OPT = -Fu$(PAS_READLINE)/src -Fu$(PAS_TCL)/src -Fu$(PAS_BASEUNITS)
LAZBUILD=lazbuild
PLEX=plex
PYACC=pyacc

all: flowproc man/man1/flowproc.1tr testnetlist testilang testliberty

flowproc: flowproc.lpr ilang.pas ilang-parser.pas ilang-lex.pas liberty-parser.pas liberty-token.pas
	#$(LAZBUILD) $@.lpi
	$(FPC) $(FPC_OPT) -o$@ $<

man/man1/flowproc.1tr: flowproc.lpr
	# Strange, if just written as script, make starts it using /bin/sh,
	# but this shell does not provide the required features. Therefore
	# we directly start it with bash. :-)
	bash $(PAS_TCL)/bin/genman.sh $<

testnetlist: testnetlist.lpr
	#$(LAZBUILD) $@.lpi
	$(FPC) $(FPC_OPT) -o$@ $<

ilang-parser.pas: ilang-parser.y
	$(PYACC) ilang-parser.y

ilang-lex.pas: ilang-lex.l ilang-parser.pas
	$(PLEX) ilang-lex.l

testilang: testilang.lpr ilang.pas ilang-parser.pas ilang-lex.pas
	#$(LAZBUILD) $@.lpi
	$(FPC) $(FPC_OPT) -o$@ $<

liberty-parser.pas: liberty-parser.y
	$(PYACC) liberty-parser.y

liberty-token.pas: liberty-token.l liberty-parser.pas
	$(PLEX) liberty-token.l

testliberty: testliberty.lpr liberty.pas liberty-parser.pas liberty-token.pas
	#$(LAZBUILD) $@.lpi
	$(FPC) $(FPC_OPT) -o$@ $<

clean:
	rm -rf *.o *.ppu flowproc *.compiled *.res *~ man/ ilang-parser.pas ilang-lex.pas liberty-parser.pas liberty-token.pas testilang testliberty testnetlist lib/ backup/
