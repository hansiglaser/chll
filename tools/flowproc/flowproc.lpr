(*ronn
flowproc(1tr) -- FlowProc
=========================

*)
Program FlowProc;

{$mode objfpc}{$H+}
{$modeswitch nestedprocvars}

Uses
  {heaptrc, }Classes, SysUtils,BaseUnix,StrUtils,Math,FGL,
  RegExpr,  // prefer RegExpr over RegEx because it offers a simple functional interface too
  HistoryOOP,ReadlineOOP,TCL,TclOOP,TclApp,
  Utils, Tables,
  Bitstream,
  ReconfSignals, Netlist, ReconfModule, ILang, Liberty, ConfigIntf, ParamIntf, PeriphIntf, ReconfApp, ReconfCell, InterSynthHandler;

Type

  PVarArgs = ^TVarArgs;
  TVarArgs = record
    First : Integer;
    ObjC  : Integer;
    ObjV  : PPTcl_Object;
  End;

  { TFlowApp }

  TFlowApp = class(TTCLApp)
  private
    FParent               : TModule;
    FReconfSignals        : TReconfSignals;
    FReconfModule         : TReconfModule;
    FPeriphIntf           : TPeriphIntf;
    FConfigInterface      : TConfigInterface;
    FParamInterface       : TParamInterface;
    FReconfModuleFinished : Boolean;
    FReconfApps           : TReconfApps;
    FCurrentApp           : TReconfApp;
    FReconfCells          : TReconfCells;
    FCurrentCell          : TReconfCell;
    FInterSynthHandler    : TInterSynthHandler;
    FChip                 : TModule;
    FPadLib               : TLibraryList;
    FPadModules           : TModuleList;

    Function SelectFunc(Const ASignal:TReconfSignalBase;Const AData:Pointer):Boolean;
    Procedure ReadParentDescription(AFilename:String);
    Procedure ReadParentNetlist(AFilename,AModule:String);
    Procedure CreatePeriphIntfOpenMSP430(ObjC:Integer;ObjV:PPTcl_Object);
    Procedure CreatePeriphInstOpenMSP430(AName:String;ObjC:Integer;ObjV:PPTcl_Object);
    Procedure CreateConfigInterfaceOpenMSP430(APeriphInst:TPeriphInstOpenMSP430;ObjC:Integer;ObjV:PPTcl_Object);
    Procedure CreateParamInterfaceOpenMSP430 (APeriphInst:TPeriphInstOpenMSP430;ObjC:Integer;ObjV:PPTcl_Object);
    Procedure CreateAppUsage(All:Boolean;Var Cols:TStringIntMap;Out Table:TTable;Out TextT:TTextTable);
  public
    Constructor Create;
    Destructor Destroy; override;

    // common commands
    Procedure Help                  (ObjC:Integer;ObjV:PPTcl_Object);
    Procedure ReadParent            (ObjC:Integer;ObjV:PPTcl_Object);
    Procedure ReadUnusedSignals     (ObjC:Integer;ObjV:PPTcl_Object);
    Procedure AddReconfSignal       (ObjC:Integer;ObjV:PPTcl_Object);
    Procedure DelReconfSignals      (ObjC:Integer;ObjV:PPTcl_Object);
    Procedure UseSignalAlias        (ObjC:Integer;ObjV:PPTcl_Object);
    Procedure ListReconfSignals     (ObjC:Integer;ObjV:PPTcl_Object);
    Procedure CreateReconfModule    (ObjC:Integer;ObjV:PPTcl_Object);
    Procedure GetReconfModule       (ObjC:Integer;ObjV:PPTcl_Object);
    Procedure SetReset              (ObjC:Integer;ObjV:PPTcl_Object);
    Procedure SetClock              (ObjC:Integer;ObjV:PPTcl_Object);
    Procedure AddReconfSignals      (ObjC:Integer;ObjV:PPTcl_Object);
    Procedure CreatePeriphIntf      (ObjC:Integer;ObjV:PPTcl_Object);
    Procedure CreatePeriphInst      (ObjC:Integer;ObjV:PPTcl_Object);
    Procedure CreateConfigInterface (ObjC:Integer;ObjV:PPTcl_Object);
    Procedure CreateParamInterface  (ObjC:Integer;ObjV:PPTcl_Object);
    Procedure SetReconfSigConnection(ObjC:Integer;ObjV:PPTcl_Object);
    Procedure CreateConnType        (ObjC:Integer;ObjV:PPTcl_Object);
    Procedure SetReconfSigConnType  (ObjC:Integer;ObjV:PPTcl_Object);
    Procedure CheckReconfSignals    (ObjC:Integer;ObjV:PPTcl_Object);
    Procedure CreateReconfModNetlist(ObjC:Integer;ObjV:PPTcl_Object);
    Procedure CreateApplication     (ObjC:Integer;ObjV:PPTcl_Object);
    Procedure GetCurrentApp         (ObjC:Integer;ObjV:PPTcl_Object);
    Procedure SelectApp             (ObjC:Integer;ObjV:PPTcl_Object);
    Procedure AppAddPort            (ObjC:Integer;ObjV:PPTcl_Object);
    Procedure AppAddParam           (ObjC:Integer;ObjV:PPTcl_Object);
    Procedure AppSetPortValue       (ObjC:Integer;ObjV:PPTcl_Object);
    Procedure AppShow               (ObjC:Integer;ObjV:PPTcl_Object);
    Procedure ListApps              (ObjC:Integer;ObjV:PPTcl_Object);
    Procedure AppWriteTemplate      (ObjC:Integer;ObjV:PPTcl_Object);
    Procedure AppWriteFirmware      (ObjC:Integer;ObjV:PPTcl_Object);
    Procedure AppReadNetlist        (ObjC:Integer;ObjV:PPTcl_Object);
    Procedure AppPrintUsage         (ObjC:Integer;ObjV:PPTcl_Object);
    Procedure CreateCell            (ObjC:Integer;ObjV:PPTcl_Object);
    Procedure GetCurrentCell        (ObjC:Integer;ObjV:PPTcl_Object);
    Procedure SelectCell            (ObjC:Integer;ObjV:PPTcl_Object);
    Procedure CellAddPort           (ObjC:Integer;ObjV:PPTcl_Object);
    Procedure CellAddConfigChain    (ObjC:Integer;ObjV:PPTcl_Object);
    Procedure CellShow              (ObjC:Integer;ObjV:PPTcl_Object);
    Procedure ListCells             (ObjC:Integer;ObjV:PPTcl_Object);
    Procedure CellWriteTemplate     (ObjC:Integer;ObjV:PPTcl_Object);
    Procedure CellReadNetlist       (ObjC:Integer;ObjV:PPTcl_Object);
    Procedure SetConntypeTrees      (ObjC:Integer;ObjV:PPTcl_Object);
    Procedure WriteIntersynth       (ObjC:Integer;ObjV:PPTcl_Object);
    Procedure ReadIntersynth        (ObjC:Integer;ObjV:PPTcl_Object);
    Procedure AppWrapIntersynth     (ObjC:Integer;ObjV:PPTcl_Object);
    Procedure GetIntersynthInstance (ObjC:Integer;ObjV:PPTcl_Object);
    Procedure AppGetCelltype        (ObjC:Integer;ObjV:PPTcl_Object);
    Procedure AppGetISInstNum       (ObjC:Integer;ObjV:PPTcl_Object);
    Procedure AppGetISInstance      (ObjC:Integer;ObjV:PPTcl_Object);
    Procedure AppGetSignal          (ObjC:Integer;ObjV:PPTcl_Object);

    Procedure FinishReconfModule    (ObjC:Integer;ObjV:PPTcl_Object);
    Procedure WriteNetlist          (ObjC:Integer;ObjV:PPTcl_Object);
    Procedure WriteNetlistAdjunct   (ObjC:Integer;ObjV:PPTcl_Object);

    Procedure AppWrapReconfModule   (ObjC:Integer;ObjV:PPTcl_Object);
    Procedure WriteBitstream        (ObjC:Integer;ObjV:PPTcl_Object);

    Procedure CreateChip            (ObjC:Integer;ObjV:PPTcl_Object);
    Procedure ChipReadLiberty       (ObjC:Integer;ObjV:PPTcl_Object);
    Procedure ChipAddPin            (ObjC:Integer;ObjV:PPTcl_Object);

    Procedure GenerateMuxedModule   (ObjC:Integer;ObjV:PPTcl_Object);
  End;

{ TFlowApp }

Constructor TFlowApp.Create;
Begin
  inherited Create('.flowproc_history','FlowProc');

  // Register/override in the Tcl engine our new functions
  // common commands
  FCmdLine.CreateCommandExit   ('exit');
  FCmdLine.CreateCommandHistory('history');
  FCmdLine.CreateCommandMan    ('man',FpGetCwd + '/man');
  // own commands
  FTCL.CreateObjCommand('help',                        @Self.Help,                  nil);
  FTCL.CreateObjCommand('read_parent',                 @Self.ReadParent,            nil);
  FTCL.CreateObjCommand('read_unused_signals',         @Self.ReadUnusedSignals,     nil);
  FTCL.CreateObjCommand('add_reconf_signal',           @Self.AddReconfSignal,       nil);
  FTCL.CreateObjCommand('use_signal_alias',            @Self.UseSignalAlias,        nil);
  FTCL.CreateObjCommand('del_reconf_signals',          @Self.DelReconfSignals,      nil);
  FTCL.CreateObjCommand('list_reconf_signals',         @Self.ListReconfSignals,     nil);
  FTCL.CreateObjCommand('create_reconf_module',        @Self.CreateReconfModule,    nil);
  FTCL.CreateObjCommand('get_reconf_module',           @Self.GetReconfModule,       nil);
  FTCL.CreateObjCommand('set_reset',                   @Self.SetReset,              nil);
  FTCL.CreateObjCommand('set_clock',                   @Self.SetClock,              nil);
  FTCL.CreateObjCommand('add_reconf_signals',          @Self.AddReconfSignals,      nil);
  FTCL.CreateObjCommand('create_peripheral_interface', @Self.CreatePeriphIntf,      nil);
  FTCL.CreateObjCommand('create_peripheral_instance',  @Self.CreatePeriphInst,      nil);
  FTCL.CreateObjCommand('create_config_interface',     @Self.CreateConfigInterface, nil);
  FTCL.CreateObjCommand('create_param_interface',      @Self.CreateParamInterface,  nil);
  FTCL.CreateObjCommand('set_reconf_signal_connection',@Self.SetReconfSigConnection,nil);
  FTCL.CreateObjCommand('create_conntype',             @Self.CreateConnType,        nil);
  FTCL.CreateObjCommand('set_reconf_signal_conntype',  @Self.SetReconfSigConnType,  nil);
  FTCL.CreateObjCommand('check_reconf_signals',        @Self.CheckReconfSignals,    nil);
  FTCL.CreateObjCommand('create_reconf_module_netlist',@Self.CreateReconfModNetlist,nil);
  FTCL.CreateObjCommand('create_application',          @Self.CreateApplication,     nil);
  FTCL.CreateObjCommand('get_current_app',             @Self.GetCurrentApp,         nil);
  FTCL.CreateObjCommand('select_app',                  @Self.SelectApp,             nil);
  FTCL.CreateObjCommand('app_add_port',                @Self.AppAddPort,            nil);
  FTCL.CreateObjCommand('app_add_param',               @Self.AppAddParam,           nil);
  FTCL.CreateObjCommand('app_set_port_value',          @Self.AppSetPortValue,       nil);
  FTCL.CreateObjCommand('app_show',                    @Self.AppShow,               nil);
  FTCL.CreateObjCommand('list_apps',                   @Self.ListApps,              nil);
  FTCL.CreateObjCommand('app_write_template',          @Self.AppWriteTemplate,      nil);
  FTCL.CreateObjCommand('app_write_firmware',          @Self.AppWriteFirmware,      nil);
  FTCL.CreateObjCommand('app_read_netlist',            @Self.AppReadNetlist,        nil);
  FTCL.CreateObjCommand('app_print_usage',             @Self.AppPrintUsage,         nil);
  FTCL.CreateObjCommand('create_cell',                 @Self.CreateCell,            nil);
  FTCL.CreateObjCommand('get_current_cell',            @Self.GetCurrentCell,        nil);
  FTCL.CreateObjCommand('select_cell',                 @Self.SelectCell,            nil);
  FTCL.CreateObjCommand('cell_add_port',               @Self.CellAddPort,           nil);
  FTCL.CreateObjCommand('cell_add_config_chain',       @Self.CellAddConfigChain,    nil);
  FTCL.CreateObjCommand('cell_show',                   @Self.CellShow,              nil);
  FTCL.CreateObjCommand('list_cells',                  @Self.ListCells,             nil);
  FTCL.CreateObjCommand('cell_write_template',         @Self.CellWriteTemplate,     nil);
  FTCL.CreateObjCommand('cell_read_netlist',           @Self.CellReadNetlist,       nil);
  FTCL.CreateObjCommand('set_conntype_trees',          @Self.SetConntypeTrees,      nil);
  FTCL.CreateObjCommand('write_intersynth',            @Self.WriteIntersynth,       nil);
  FTCL.CreateObjCommand('read_intersynth',             @Self.ReadIntersynth,        nil);
  FTCL.CreateObjCommand('app_wrap_intersynth',         @Self.AppWrapIntersynth,     nil);
  FTCL.CreateObjCommand('get_intersynth_instance',     @Self.GetIntersynthInstance, nil);
  FTCL.CreateObjCommand('app_get_celltype',            @Self.AppGetCelltype,        nil);
  FTCL.CreateObjCommand('app_get_intersynth_instance_num',@Self.AppGetISInstNum,    nil);
  FTCL.CreateObjCommand('app_get_intersynth_instance', @Self.AppGetISInstance,      nil);
  FTCL.CreateObjCommand('app_get_signal',              @Self.AppGetSignal,          nil);


  FTCL.CreateObjCommand('finish_reconf_module',        @Self.FinishReconfModule,    nil);
  FTCL.CreateObjCommand('write_netlist',               @Self.WriteNetlist,          nil);
  FTCL.CreateObjCommand('write_netlist_adjunct',       @Self.WriteNetlistAdjunct,   nil);

  FTCL.CreateObjCommand('app_wrap_reconf_module',      @Self.AppWrapReconfModule,   nil);
  FTCL.CreateObjCommand('write_bitstream',             @Self.WriteBitstream,        nil);

  FTCL.CreateObjCommand('create_chip',                 @Self.CreateChip,            nil);
  FTCL.CreateObjCommand('chip_read_liberty',           @Self.ChipReadLiberty,       nil);
  FTCL.CreateObjCommand('chip_add_pin',                @Self.ChipAddPin,            nil);

  FTCL.CreateObjCommand('generate_muxed_module',       @Self.GenerateMuxedModule,   nil);

  FReconfSignals := TReconfSignals.Create;
  FReconfApps := TReconfApps.Create;
  FReconfCells := TReconfCells.Create;
  FInterSynthHandler := TInterSynthHandler.Create(FReconfSignals,FReconfApps,FReconfCells);
  FPadModules := TModuleList.Create;
End;

Destructor TFlowApp.Destroy;
Begin
  FPadModules.Free;
  FInterSynthHandler.Free;
  FReconfCells.Free;
  FReconfApps.Free;
  FReconfSignals.Free;
  Inherited Destroy;
End;

(*****************************************************************************)
(***  TCL Functions: Common Commands  ****************************************)
(*****************************************************************************)

(*ronn
help(1tr) -- Provide a list of all commands and variables
=========================================================

## SYNOPSYS

`help` [<word>]

## DESCRIPTION

The command `help` prints a list of all commands with their parameters and of
all variables.

If `help` is supplied with the parameter <word>, it returns a list of all
commands which contain <word> (similar to apropos(1)).

## MODES

This command is available in all modes.

## SEE ALSO

man(1tr)

*)
Procedure TFlowApp.Help(ObjC:Integer;ObjV:PPTcl_Object);
Begin
  if ObjC = 2 then // one argument -> "apropos"
    Begin
      FCmdLine.Apropos(ObjV^[1].AsPChar);
      Exit;
    End;
  WriteLn('FlowProc');
  WriteLn('  help [word]');
  WriteLn('  man [page]');
  WriteLn('  history');
  WriteLn('  exit');
  WriteLn('  read_parent filename module');
  WriteLn('  read_unused_signals filename');
  WriteLn('  del_reconf_signals criteria...');
  WriteLn('  add_reconf_signal ...');
  WriteLn('  use_signal_alias signal');
  WriteLn('  list_reconf_signals [criteria...]');
  WriteLn('  write_reconf_signals filename');
  WriteLn('  read_reconf_signals filename');
  WriteLn('  create_reconf_module [-typename typename] [-instname instname] [-istypename typename] [-isinstname instname]');
  WriteLn('  get_reconf_module -typename|-instname|-istypename|-isinstname');
  WriteLn('  set_reset [signal_name]');
  WriteLn('  set_clock [signal_name]');
  WriteLn('  add_reconf_signals');
  WriteLn('  create_peripheral_interface -type "openmsp430" -peraddr "Per_Addr_s" -perdin "Per_DIn_s" -perwr "Per_Wr_s" -peren "Per_En_s"');
  WriteLn('  create_peripheral_instance "CfgIntf" -baseaddr 0x0180 -perdout "CfgIntf_DOut_s"');
  WriteLn('  create_config_interface periph_inst [options]');
  WriteLn('  create_param_interface periph_inst [options]');
  WriteLn('  set_reconf_signal_connection -const/-config/-param/-direct/-specialdirect/-dynamic ... -name "blah_i"');
  WriteLn('  create_conntype');
  WriteLn('  set_reconf_signal_conntype');
  WriteLn('  check_reconf_signals [-noerror]');
  WriteLn('  create_reconf_module_netlist -preliminary|-wrapapp|-intersynth [-appinstname instname]');
  WriteLn('  create_application "appname"');
  WriteLn('  get_current_app');
  WriteLn('  select_app "appname"');
  WriteLn('  app_add_port "port" [-map "reconfsignal"] [-index N]');
  WriteLn('  app_add_param -in|-out -conntype "conntype" "name" [-default value]');
  WriteLn('  app_set_port_value "signal" value [-nowarn]');
  WriteLn('  app_show ["appname"]');
  WriteLn('  list_apps [-show]');
  WriteLn('  app_write_template -vhdl|-verilog [-testbench] -o filename');
  WriteLn('  app_write_firmware -wrapapp|-reconfmodule -driver_header|-driver_source filename');
  WriteLn('  app_read_netlist [-extracted [-ignore cell_list] [-post-si]] filename.il');
  WriteLn('  app_print_usage [-all]');
  WriteLn('  create_cell "cellname"');
  WriteLn('  get_current_cell');
  WriteLn('  select_cell "cellname"');
  WriteLn('  cell_add_port "port" [-map "reconfsignal"] [-index N] [-config -width n]');
  WriteLn('  cell_add_config_chain -length n [-mode "CfgMode_i"] [-clk "CfgClk_i"] [-shift "CfgShift_i"] [-datain "CfgDataIn_i"] [-dataout "CfgDataOut_o"]');
  WriteLn('  cell_show ["cellname"]');
  WriteLn('  list_cells [-show]');
  WriteLn('  cell_write_template -vhdl|-verilog [-testbench] -o filename');
  WriteLn('  cell_read_netlist filename.il');
  WriteLn('  set_conntype_trees conntype {-full|<trees> <cost>}');
  WriteLn('  write_intersynth [-conntypes] [-dyn_ports] [-celltypes] [-netlist app] [-netlists] [-stdcells] -o filename');
  WriteLn('  read_intersynth -commands|-config|-check|-showinfo filename');
  WriteLn('  app_wrap_intersynth');
  WriteLn('  get_intersynth_instance celltype instnum');
  WriteLn('  app_get_celltype instname');
  WriteLn('  app_get_intersynth_instance_num instname');
  WriteLn('  app_get_intersynth_instance instname');
  WriteLn('  app_get_signal [-regex regex]|name [-driver|-sinks|-anysink|-width|-mapping]');

  WriteLn('  finish_reconf_module');
  WriteLn('  write_netlist -parent|-preliminary|-reconfmodule|-wrapapp|-wrapis|-wraprm_vhdl2008|-wraprm_lec|-chip -verilog|-vhdl -entity|-component|-architecture|-module|-instance filename');
  WriteLn('  write_netlist_adjunct -wraprm_lec_setup|-wraprm_lec_mapping filename');

  WriteLn('  app_wrap_reconf_module');
  WriteLn('  write_bitstream -bitdata|-reconfsignals -format vhdl|verilog|c|text|modelsim|lec|formality [options] prefix filename');

  WriteLn('  create_chip [-chip_name "chip"] [-arch_name "arch"] [-parent_inst_name "core_1"]');
  WriteLn('  chip_add_pin [-padcell xxx -connect EN ...|-direct|-direct_inout -in "name" -out "name" -enable "name"|-direct_od [-in "name"] -out "name"|-const "0"|-open|...] [-portname "portname"] [-regex regex]|name');

  WriteLn('  new_app name');
  WriteLn('  add_input ...');
  WriteLn('  add_output ...');
End;

(*ronn
man(1tr) -- online reference manual
===================================

## SYNOPSYS

`man` [<page>]

## DESCRIPTION

Display the manual page for the given <page>. If no <page> is given, a list of
all man pages and short descriptions is given, similar to `whatis`(1).

## EXAMPLES

  `man help`

## SEE ALSO

help(1tr)

*)

(*ronn
history(1tr) -- display the history list
========================================

## SYNOPSYS

`history`

## DESCRIPTION

Prints all previously entered commands. These are also stored in the file
_~/.flowproc_history_ on program exit and loaded on program start.

*)

(*ronn
exit(1tr) -- quit the program
=============================

## SYNOPSYS

`exit`

## DESCRIPTION

Quit the program.

*)

Procedure AssertParams(ObjC,Num:Integer;ObjV:PPTcl_Object;Params:String);
Begin
  if ObjC <> Num then
    raise Exception.Create('bad # arg: ' + ObjV^[0].AsPChar + ' ' + Params);
End;

(*ronn
TODO
*)
Procedure TFlowApp.ReadParent(ObjC:Integer;ObjV:PPTcl_Object);
Begin
  AssertParams(ObjC,3,ObjV,'filename module');
  // read_parent filename module
  ReadParentNetlist(ObjV^[1].AsPChar,ObjV^[2].AsPChar);
End;

Procedure TFlowApp.ReadParentDescription(AFilename:String);
Var SignalSortID : Integer;

  Procedure ParseLine(St:String);
  Var P : Integer;
  Type TCharSet = Set of Char;

    Function GetUntil(Ch:Char;Required:Boolean=true):String;
    Var I : Integer;
    Begin
      I := PosEx(Ch,St,P);
      if I = 0 then
        Begin
          if Required then
            raise Exception.Create('GetUntil('''+Ch+'''): Invalid syntax at column '+IntToStr(P)+ ' in St = '''+St+'''')
          else
            I := Length(St)+1;
        End;
      Result := Copy(St,P,I-P);
      P := I+1;
    End;

    Function GetUntil(Ch:TCharSet;Required:Boolean=true):String;
    Var I : Integer;
    Begin
      I := P;
      While (I <= Length(St)) and not (St[I] in Ch) do
        Inc(I);
      if I > Length(St) then
        Begin
          if Required then
            raise Exception.Create('GetUntil: Invalid syntax at column '+IntToStr(P)+ ' in St = '''+St+'''')
          // else: I is already Length+1
        End;
      Result := Copy(St,P,I-P);
      P := I+1;
    End;

    Procedure SkipWhitespace;
    Begin
      While (St[P] in [' ',^I]) and (P < Length(St)) do
        Inc(P);
    End;

  Var ID      : Integer;
      Name    : String;
      H,L     : Integer;
      Dir     : String;
      TheType : TType;
  Begin
    P := 1;
    // format:
    // id name[high:low] dir
    ID   := StrToInt(GetUntil([' ',^I]));
    SkipWhitespace;
    Name := GetUntil('[');
    H    := StrToInt(GetUntil(':'));
    L    := StrToInt(GetUntil(']'));
    SkipWhitespace;
    Dir  := GetUntil([' ',^I],false);
    if (H = 0) and (L = 0) then TheType := TypeBit
    else TheType := TType.Create('std_logic_vector',dirDown,H,L);
    if ID > 0 then
      FParent.AddPort(TPort.Create(Name,StrToPortDir(Dir),TheType),ID)
    else
      Begin
        FParent.AddSignal(TSignal.Create(Name,TheType),SignalSortID);
        Inc(SignalSortID);
      End;
  End;

Var T    : Text;
    Line : Integer;
    St   : String;
Begin
  Assign(T,AFilename);
  Reset(T);
  Line := 1;
  SignalSortID := 0;
  try
    try
      // first line: module name
      ReadLn(T,St); Inc(Line);
      FParent := TModule.Create(St);
      while not EOF(T) do
        Begin
          ReadLn(T,St);
          ParseLine(St);
          Inc(Line);
        End;
    except
      on E : Exception do
        raise Exception.CreateFmt('Parse error in line %d: %s',[Line,E.Message]);
    End;
  Finally
    Close(T);
  End;
  FReconfSignals.Parent := FParent;
End;

Procedure TFlowApp.ReadParentNetlist(AFilename,AModule:String);
Var Modules : TModuleList;
Begin
  Modules := Nil;
  CreateYosysCells(Modules);
  ILang.Parse(AFilename,Modules);
  if Modules.IndexOf(AModule) < 0 then
    raise Exception.Create('Didn''t read a module named '''+AModule+'''');
  FParent := Modules[AModule];
  FReconfSignals.Parent := FParent;
  Modules.Free;
  //WriteLn(FParent.GetVHDL);
End;

(*ronn
read_unused_signals(1fl) -- read list of unused ports
===================================================

## SYNPOSYS

`read_unused_signals` <filename>

## DESCRIPTION

The design methodology aids the designer to develop the reconfigurable module.
First he has to develop the chip parent module with all the chip inventory except
the reconfigurable modules. This implies that numerous cell ports and parent
module ports are unused at that stage.

In the next step a list of these unused ports is automatically generated. Use
`read_unused_signals` to import this list. Then use the commands
`add_reconf_signal`(1fl) and `del_reconf_signals`(1fl) to amend this list to
your needs.

## EXAMPLE

    # read file provided by special yosys pass to extract unused ports
    read_unused_signals "parent-stubnets.txt"
    # amend list of signals
    del_reconf_signals -name "Cfg.*"
    del_reconf_signals -dir in -cell "I2C_Master"
    add_reconf_signal -cell "I2C_Master" -output "SDA_o"   ;# fork output of submodule as our input
    add_reconf_signal -input "Clk_i"
    add_reconf_signal -input "Reset_n_i"
    # TODO: vermutlich muss man hier auch noch die ConnectionTypes definieren und zuweisen
    # TODO: Meta-Info und -Signale Richtung CPU usw.: Config- und Param-Interface, ...
    # save complete list of signals
    write_reconf_signals "reconf_signals.dat"  ;# or this could be a TCL script, but it is simpler with a pure data file

## SEE ALSO

add_reconf_signal(1fl), del_reconf_signals(1fl)

*)
Procedure TFlowApp.ReadUnusedSignals(ObjC:Integer;ObjV:PPTcl_Object);
Begin
  AssertParams(ObjC,2,ObjV,'filename');
  // read_unused_signals filename
  ObjC := FReconfSignals.ReadUnusedSignals(ObjV^[1].AsPChar);
  WriteLn('Read ',ObjC,' unused ports.');
End;

Function TFlowApp.SelectFunc(Const ASignal:TReconfSignalBase;Const AData:Pointer):Boolean;
Var ObjI : Integer;
    ObjC : Integer;
    ObjV : PPTcl_Object;
    Negate : Boolean;

  Procedure Next(AOption:String);
  Begin
    Inc(ObjI);
    if ObjI = ObjC then
      raise Exception.Create('Option '+AOption+' needs an argument');
  End;

  Function CheckVector(AHigh,ALow:Integer) : Boolean;
  Var High,Low : Integer;
      SHigh,SLow : String;
  Begin
    SLow := ObjV^[ObjI].AsString;
    High := Pos(':',SLow);
    if High = 0 then
      raise Exception.Create('Invalid format of vector specification "'+SLow+'"');
    SHigh := Copy(SLow,1,High-1);
    SLow  := Copy(SLow,High+1,Length(SLow)-High);
    High := StrToInt(SHigh);
    Low  := StrToInt(SLow);
    Result := ((AHigh = High) and (ALow = Low));
  End;

Begin
  Result := true;
  Negate := false;
  ObjI := PVarArgs(AData)^.First;
  ObjC := PVarArgs(AData)^.ObjC;
  ObjV := PVarArgs(AData)^.ObjV;
  While ObjI < ObjC do
    Begin
      Case ObjV^[ObjI].AsString of
        '-name'     : Begin
                        Next('-name');
                        Result := Result and (ExecRegExpr('^'+ObjV^[ObjI].AsString+'$',ASignal.FName) xor Negate);
                      End;
        '-dir'      : Begin
                        Next('-dir');
                        Result := Result and ((StrToPortDir(ObjV^[ObjI].AsString) = ASignal.GetDirection) xor Negate);
                      End;

        '-width'    : Begin
                        Next('-width');
                        Result := Result and ((ObjV^[ObjI].AsInteger(FTCL) = (ASignal.GetSignal.FType.GetWidthInt)) xor Negate);
                      End;
        '-low'      : Begin
                        Next('-low');
                        Result := Result and ((ObjV^[ObjI].AsInteger(FTCL) = ASignal.GetSignal.FType.GetRight) xor Negate);
                      End;
        '-high'     : Begin
                        Next('-high');
                        Result := Result and ((ObjV^[ObjI].AsInteger(FTCL) = ASignal.GetSignal.FType.GetLeft) xor Negate);
                      End;
        '-vector'   : Begin
                        Next('-vector');
                        Result := Result and (CheckVector(ASignal.GetSignal.FType.GetLeft,ASignal.GetSignal.FType.GetLeft) xor Negate);
                      End;
        '-cell'     : Begin
                        Next('-cell');
                        Result := Result and (((ASignal is TReconfSignal) and ExecRegExpr('^'+ObjV^[ObjI].AsString+'$',(ASignal as TReconfSignal).FConnections[0].FInstance.FModule.FName)) xor Negate);
                      End;
        '-port'     : Begin
                        Next('-port');
                        Result := Result and (((ASignal is TReconfSignal) and ExecRegExpr('^'+ObjV^[ObjI].AsString+'$',(ASignal as TReconfSignal).FConnections[0].FPort.FName)) xor Negate);
                      End;
        '-portlow'  : Begin
                        Next('-portlow');
                        Result := Result and (((ASignal is TReconfSignal) and (ObjV^[ObjI].AsInteger(FTCL) = ASignal.GetSignal.FType.GetRight)) xor Negate);
                      End;
        '-porthigh' : Begin
                        Next('-porthigh');
                        Result := Result and (((ASignal is TReconfSignal) and (ObjV^[ObjI].AsInteger(FTCL) = ASignal.GetSignal.FType.GetLeft)) xor Negate);
                      End;
        '-portvec'  : Begin
                        Next('-portvec');
                        Result := Result and (((ASignal is TReconfSignal) and CheckVector(ASignal.GetSignal.FType.GetLeft,ASignal.GetSignal.FType.GetRight)) xor Negate);
                      End;
        '!'         : Begin
                        Negate := true;
                        // don't get to the end of this loop, because this would reset Negate to false
                        Inc(ObjI);
                        Continue;
                      End;
      else
        raise Exception.Create('bad arg: "'+ObjV^[ObjI].AsPChar+'"');
      End;
      Negate := false;
      Inc(ObjI);
    End;
End;

Function SelectStr(AVarArgs:TVarArgs):String;
Begin
  Result := '';
  With AVarArgs Do
    Begin
      While First < ObjC do
        Begin
          Result += ObjV^[First].AsString + ' ';
          Inc(First);
        End;
    End;
  SetLength(Result,Length(Result)-1);
End;

(*ronn
list_reconf_signals(1fl) -- list reconfigurable signals
=======================================================

## SYNPOSYS

`list_reconf_signals` [<criteria...>]

## DESCRIPTION

If no criteria are given, all reconfigurable signals are listed.

If one or more criteria are given, all reconfigurable signals are checked
against them. If all creteria are met, the signal is listed. This means
that a logical AND of all criteria is required to list a signal. To get a
logical OR combination, just execute this function multiple times.

 * **-name <regex>**: matches if the name of the reconfigurable signal matches
   the regular expression <regex>.

 * **-dir <direction>**: matches if the port direction is <direction>. Allowed values are
   'unknown', 'in', 'out' and 'inout' (case sensitive).

 * **-width <width>**: matches if the signal width is equal to <low>.

 * **-low <low>**: matches if the signal's low bit index is equal to <low>.

 * **-high <high>**: matches if the signal's high bit index is equal to <high>.

 * **-vector <high>:<low>**: matches if the signal's high bit index is equal to
   <high> and the signal's low bit index is equal to <low>.

 * **-cell <regex>**: matches if the signal is connected to a cell (=sub-module)
   which instance name matches the regular expression <regex>.

 * **-port <regex>**: matches if the signal is connected to a cell port
   which name matches the regular expression <regex>.

 * **-portlow <low>**: matches if the cell port's low bit index is equal to <low>.

 * **-porthigh <high>**: matches if the cell port's high bit index is equal to <high>.

 * **-portvec <high>:<low>**: matches if the cell port's high bit index is
   equal to <high> and the cell port's low bit index is equal to <low>.

 * **!**: negate the following criteria.

Perl regular expression options are supported. E.g., for case insensitive
matches use

    list_reconf_signals -name "(?i)clk.*"

Hint: Don't use single-quotes since these have no special meaning in TCL. Use
either double quotes or curly braces.

    list_reconf_signals -name 'Clk.*'   ;# doesn't work as expected
    list_reconf_signals -name "Clk.*"   ;# works, but be careful with variable expansion
    list_reconf_signals -name {Clk.*}   ;# literal quoute

Note: For each signal, all options are parsed. If a criterion evaluates to
false, the next criteria are not checked (although the options are still
parsed).

## SEE ALSO

del_reconf_signals(1fl)

*)

Procedure TFlowApp.ListReconfSignals(ObjC:Integer;ObjV:PPTcl_Object);
Var MyVarArgs : TVarArgs;
Begin
  if ObjC = 1 then
    Begin
      // no options specified --> list all
      FReconfSignals.ListReconfSignals(Nil,Nil);
      Exit;
    End;
  // options specified: use SelectFunc
  MyVarArgs.First := 1;
  MyVarArgs.ObjC  := ObjC;
  MyVarArgs.ObjV  := ObjV;
  FReconfSignals.ListReconfSignals(@SelectFunc,@MyVarArgs);
End;

(*ronn
add_reconf_signal(1fl) -- add a reconfigurable signal
=====================================================

## SYNPOSYS

`add_reconf_signal` [-rename "newname"] "signal"

## DESCRIPTION

TODO

## EXAMPLE

For an example see `read_unused_signals`(1fl)

## SEE ALSO

read_unused_signals(1fl), del_reconf_signals(1fl)

*)

Procedure TFlowApp.AddReconfSignal(ObjC:Integer;ObjV:PPTcl_Object);
Var ObjI    : Integer;
    Signal  : String;
    NewName : String;
Begin
  if ObjC < 2 then
    raise Exception.Create('bad # arg: ' + ObjV^[0].AsPChar + ' [-rename "newname"] "signal"');
  Signal  := '';
  NewName := '';
  ObjI := 1;
  While ObjI < ObjC do
    Begin
      if (ObjV^[ObjI].AsString = '-rename') and (ObjI+1 < ObjC) then
        Begin
          Inc(ObjI);
          NewName := ObjV^[ObjI].AsString;
        End
      else
        Signal := ObjV^[ObjI].AsString;
      Inc(ObjI);
    End;

  FReconfSignals.AddSignal(Signal,NewName);
End;

(*ronn
del_reconf_signals(1fl) -- delete reconfigurable signals
========================================================

## SYNPOSYS

`del_reconf_signals` <criteria...>

## DESCRIPTION

Delete the reconfigurable signals which are specifed by the given criteria.

See `list_reconf_signals`(1fl) for a description of the criteria.

## EXAMPLE

For an example see `read_unused_signals`(1fl)

## SEE ALSO

list_reconf_signals(1fl), add_reconf_signal(1fl)

*)

Procedure TFlowApp.DelReconfSignals(ObjC:Integer;ObjV:PPTcl_Object);
Var MyVarArgs : TVarArgs;
Begin
  if ObjC < 2 then
    raise Exception.Create('bad # arg: ' + ObjV^[0].AsPChar + ' [options...]');

  MyVarArgs.First := 1;
  MyVarArgs.ObjC  := ObjC;
  MyVarArgs.ObjV  := ObjV;
  if FReconfSignals.DelSignals(@SelectFunc,@MyVarArgs) <= 0 then
    raise Exception.Create('No signals matched '+SelectStr(MyVarArgs));
End;

(*ronn
TODO
*)
Procedure TFlowApp.UseSignalAlias(ObjC:Integer;ObjV:PPTcl_Object);
Begin
  if ObjC <> 2 then
    raise Exception.Create('bad # arg: ' + ObjV^[0].AsPChar + ' signal');
  FReconfSignals.UseSignalAlias(ObjV^[1].AsString);
End;

(*ronn
TODO
*)
Procedure TFlowApp.CreateReconfModule(ObjC:Integer;ObjV:PPTcl_Object);
Var I                  : Integer;
    ReconfigTypeName   : String;
    ReconfigInstName   : String;
    InterSynthTypeName : String;
    InterSynthInstName : String;
Begin
  // defaults
  ReconfigTypeName   := 'ReconfigLogic';
  ReconfigInstName   := 'ReconfigLogic_0';
  InterSynthTypeName := 'InterSynthModule';
  InterSynthInstName := 'InterSynthModule_0';
  // create_reconf_module [-typename typename] [-instname instname] [-istypename typename] [-isinstname instname]
  if ObjC > 9 then
    raise Exception.Create('bad # arg: ' + ObjV^[0].AsPChar + ' [-typename typename] [-instname instname] [-istypename typename] [-isinstname instname]');

  // names
  I := 1;
  While I < ObjC do
    Begin
      if I+1 >= ObjC then
        raise Exception.Create('bad # arg: ' + ObjV^[0].AsPChar + ' [-typename typename] [-instname instname] [-istypename typename] [-isinstname instname]');
      Case ObjV^[I].AsString of
        '-typename'   : Begin ReconfigTypeName   := ObjV^[I+1].AsString; Inc(I); End;
        '-instname'   : Begin ReconfigInstName   := ObjV^[I+1].AsString; Inc(I); End;
        '-istypename' : Begin InterSynthTypeName := ObjV^[I+1].AsString; Inc(I); End;
        '-isinstname' : Begin InterSynthInstName := ObjV^[I+1].AsString; Inc(I); End;
      else
        raise Exception.Create('bad arg: ' + ObjV^[I].AsPChar);
      End;
      Inc(I);
    End;

  if assigned(FReconfModule) then
    raise Exception.Create('Reconfigurable module already exists');

  // create the module
  FReconfModule := TReconfModule.Create(FParent,ReconfigTypeName,ReconfigInstName,InterSynthTypeName,InterSynthInstName);
End;

Procedure TFlowApp.GetReconfModule(ObjC:Integer;ObjV:PPTcl_Object);
Begin
  // get_reconf_module -typename|-instname|-istypename|-isinstname
  if ObjC <> 2 then
    raise Exception.Create('bad # arg: ' + ObjV^[0].AsPChar + ' -typename|-instname|-istypename|-isinstname');
  Case ObjV^[1].AsString of
    '-typename'   : FTCL.SetResult(FReconfModule.FReconfigTypeName);
    '-instname'   : FTCL.SetResult(FReconfModule.FReconfigInstName);
    '-istypename' : FTCL.SetResult(FReconfModule.FInterSynthTypeName);
    '-isinstname' : FTCL.SetResult(FReconfModule.FInterSynthInstName);
  else
    raise Exception.Create('bad arg: ' + ObjV^[1].AsPChar);
  End;
End;

(*ronn
TODO
*)
Procedure TFlowApp.SetReset(ObjC:Integer;ObjV:PPTcl_Object);
Var SignalName : String;
    ResetName  : String;
    ObjI       : Integer;
Begin
  // set_reset [-rename "resetname"] [signalname]
  SignalName := 'Reset_n_i';  // default
  ResetName  := '';
  ObjI := 1;
  While ObjI < ObjC do
    Begin
      if (ObjV^[ObjI].AsString = '-rename') and (ObjI+1 < ObjC) then
        Begin
          Inc(ObjI);
          ResetName := ObjV^[ObjI].AsString;
        End
      else
        SignalName := ObjV^[ObjI].AsString;
      Inc(ObjI);
    End;
  if ResetName = '' then ResetName := SignalName;

  if not assigned(FReconfModule) then
    raise Exception.Create('Reconfigurable module does not yet exists');

  if assigned(FReconfModule.FParentReset) then
    raise Exception.Create('Reset is already set');

  FReconfModule.FParentReset := FParent.GetSignal(SignalName);
  FReconfModule.FResetName   := ResetName;
  if not assigned(FReconfModule.FParentReset) then
    raise Exception.Create('Reset signal '''+SignalName+''' not found in '''+FParent.FName+'''');
End;

(*ronn
TODO
*)
Procedure TFlowApp.SetClock(ObjC:Integer;ObjV:PPTcl_Object);
Var SignalName : String;
    ClockName  : String;
    ObjI       : Integer;
Begin
  // set_clock [-rename "clockname"] [signalname]
  SignalName := 'Clk_i';  // default
  ClockName  := '';
  ObjI := 1;
  While ObjI < ObjC do
    Begin
      if (ObjV^[ObjI].AsString = '-rename') and (ObjI+1 < ObjC) then
        Begin
          Inc(ObjI);
          ClockName := ObjV^[ObjI].AsString;
        End
      else
        SignalName := ObjV^[ObjI].AsString;
      Inc(ObjI);
    End;
  if ClockName = '' then ClockName := SignalName;

  if not assigned(FReconfModule) then
    raise Exception.Create('Reconfigurable module does not yet exists');

  if assigned(FReconfModule.FParentClock) then
    raise Exception.Create('Clock is already set');

  FReconfModule.FParentClock := FParent.GetSignal(SignalName);
  FReconfModule.FClockName   := ClockName;
  if not assigned(FReconfModule.FParentClock) then
    raise Exception.Create('Clock signal '''+SignalName+''' not found in '''+FParent.FName+'''');
End;

(*ronn
TODO
*)
Procedure TFlowApp.AddReconfSignals(ObjC:Integer;ObjV:PPTcl_Object);
Begin
  // add_reconf_signals
  if ObjC <> 1 then
    raise Exception.Create('bad # arg: ' + ObjV^[0].AsPChar);

  if not assigned(FReconfModule) then
    raise Exception.Create('Reconfigurable module does not yet exists');

  if assigned(FReconfModule.FReconfSignals) then
    raise Exception.Create('Reconfigurable signals are already added');

  FReconfModule.AddReconfSignals(FReconfSignals);
End;

(*ronn
TODO
*)
Procedure TFlowApp.CreatePeriphIntf(ObjC:Integer;ObjV:PPTcl_Object);
Begin
  // create_peripheral_interface -type type [options]
  if ObjC < 3 then
    raise Exception.Create('bad # arg: ' + ObjV^[0].AsPChar + ' -type type [options]');
  if ObjV^[1].AsPChar <> '-type' then
    raise Exception.Create('bad arg: ' + ObjV^[0].AsPChar + ' -type type [options]');
  if assigned(FPeriphIntf) then
    raise Exception.Create('You have already created a peripheral interface');

  Case ObjV^[2].AsString of
    'openmsp430' : CreatePeriphIntfOpenMSP430(ObjC,ObjV);
  else
    raise Exception.Create('Unsupported periph interface type ' + ObjV^[2].AsPChar);
  End;
End;

(*ronn
TODO
*)
Procedure TFlowApp.CreatePeriphInst(ObjC:Integer;ObjV:PPTcl_Object);
Begin
  // create_peripheral_instance name options
  if ObjC < 2 then
    raise Exception.Create('bad # arg: ' + ObjV^[0].AsPChar + ' name [options]');
  if not assigned(FPeriphIntf) then
    raise Exception.Create('You first have to create a peripheral interface');

  if FPeriphIntf is TPeriphIntfOpenMSP430 then
    CreatePeriphInstOpenMSP430(ObjV^[1].AsString,ObjC,ObjV)
  else
    raise Exception.Create('Unsupported periph interface type ' + ObjV^[2].AsPChar);
End;

(*ronn
TODO
*)
Procedure TFlowApp.CreateConfigInterface(ObjC:Integer;ObjV:PPTcl_Object);
Begin
  // create_config_interface periph_inst [options]
  if ObjC < 2 then
    raise Exception.Create('bad # arg: ' + ObjV^[0].AsPChar + ' periph_inst [options]');
  if assigned(FConfigInterface) then
    raise Exception.Create('You have already created a config interface');
  if FReconfModuleFinished then
    raise Exception.Create('You can''t create a config interface after the reconfigurable module is finished');
  if not assigned(FPeriphIntf) then
    raise Exception.Create('You first have to create a peripheral interface');
  if FPeriphIntf.FInstances.IndexOf(ObjV^[1].AsString) < 0 then
    raise Exception.Create('Invalid peripheral instance '''+ObjV^[1].AsString+'''');

  if FPeriphIntf is TPeriphIntfOpenMSP430 then
    CreateConfigInterfaceOpenMSP430(FPeriphIntf.FInstances[ObjV^[1].AsString] as TPeriphInstOpenMSP430,ObjC,ObjV)
  else
    raise Exception.Create('Unsupported periph interface type ' + ObjV^[2].AsPChar);
End;

(*ronn
TODO
*)
Procedure TFlowApp.CreateParamInterface(ObjC:Integer;ObjV:PPTcl_Object);
Begin
  // create_param_interface periph_inst [options]
  if ObjC < 2 then
    raise Exception.Create('bad # arg: ' + ObjV^[0].AsPChar + ' periph_inst [options]');
  if assigned(FParamInterface) then
    raise Exception.Create('You have already created a param interface');
  if FReconfModuleFinished then
    raise Exception.Create('You can''t create a param interface after the reconfigurable module is finished');
  if not assigned(FPeriphIntf) then
    raise Exception.Create('You first have to create a peripheral interface');
  if FPeriphIntf.FInstances.IndexOf(ObjV^[1].AsString) < 0 then
    raise Exception.Create('Invalid peripheral instance '''+ObjV^[1].AsString+'''');

  if FPeriphIntf is TPeriphIntfOpenMSP430 then
    CreateParamInterfaceOpenMSP430(FPeriphIntf.FInstances[ObjV^[1].AsString] as TPeriphInstOpenMSP430,ObjC,ObjV)
  else
    raise Exception.Create('Unsupported periph interface type ' + ObjV^[2].AsPChar);
End;

(*ronn
TODO
*)
Procedure TFlowApp.SetReconfSigConnection(ObjC:Integer;ObjV:PPTcl_Object);
Var MyVarArgs : TVarArgs;

  Procedure Next(AInc:Integer=1);
  Begin
    Inc(MyVarArgs.First,AInc);
    if MyVarArgs.First >= ObjC then
      raise Exception.Create('Option '+ObjV^[MyVarArgs.First-AInc].AsString+' needs an argument');
  End;

Var SigConn : TSigConnBase;

Begin
  // set_reconf_signal_connection -const/-config/-param/-direct/-dynamic ... -name "blah_i" ...
  if ObjC < 2 then
    raise Exception.Create('bad # arg: ' + ObjV^[0].AsPChar + ' -const/-config/-param/-direct/-specialdirect/-dynamic [options...] [selection]');

  MyVarArgs.First := 1;
  MyVarArgs.ObjC  := ObjC;
  MyVarArgs.ObjV  := ObjV;

  Case ObjV^[MyVarArgs.First].AsString of
    '-const'    : Begin
                    Next(2);
                    SigConn := TSigConnConst.Create;
                    // evaluate constant value: VHDL ("00110101", ...), Verilog (8'b00110101), decimal (53), octal (065), hex (0x35, $35)
                    // TODO: how to handle negative values?
                    (SigConn as TSigConnConst).FValue := ParseSignalValue(ObjV^[MyVarArgs.First-1].AsString,(SigConn as TSigConnConst).FWidth);
                  End;
    '-config'   : Begin
                    Next;
                    SigConn := TSigConnConfig.Create;
                  End;
    '-param'    : Begin
                    // TODO: padding if the width doesn't match the param cells
                    Next;
                    SigConn := TSigConnParam.Create;
                  End;
    '-direct'   : Begin
                    // TODO: array ports
                    // TODO: Clk_i, Reset_n_i
                    Next;
                    SigConn := TSigConnDirect.Create;
                  End;
    '-dynamic'  : Begin
                    Next;
                    SigConn := TSigConnDyn.Create;
                  End;
  else
    raise Exception.Create('Invalid signal connection '''+ObjV^[MyVarArgs.First].AsString+'''');
  End;
  // perform assignment
  if FReconfModule.FReconfSignals.Foreach(@SelectFunc,@MyVarArgs,@FReconfModule.FReconfSignals.SetSigConn,SigConn) <= 0 then
    raise Exception.Create('No signals matched '+SelectStr(MyVarArgs));
  SigConn.Free;
End;

(*ronn
TODO
*)
Procedure TFlowApp.CreateConnType(ObjC:Integer;ObjV:PPTcl_Object);
Begin
  // create_conntype "Bit" 1
  if ObjC <> 3 then
    raise Exception.Create('bad # arg: ' + ObjV^[0].AsPChar);
  FReconfSignals.FConnTypes.Add(ObjV^[1].AsString,TConnType.Create(ObjV^[1].AsString,ObjV^[2].AsInteger(FTCL)));
End;

(*ronn
TODO
*)
Procedure TFlowApp.SetReconfSigConnType(ObjC:Integer;ObjV:PPTcl_Object);
Var MyVarArgs : TVarArgs;

  Procedure Next(AInc:Integer=1);
  Begin
    Inc(MyVarArgs.First,AInc);
    if MyVarArgs.First >= ObjC then
      raise Exception.Create('Option '+ObjV^[MyVarArgs.First-AInc].AsString+' needs an argument');
  End;

Var ConnTypeData : TConnTypeData;

Begin
  // set_reconf_signal_conntype "Bit" -array / -pad "000000" ... -name "blah_i" ...
  if ObjC < 3 then
    raise Exception.Create('bad # arg: ' + ObjV^[0].AsPChar + ' "Bit" [options] [selection]');


  if FReconfSignals.FConnTypes.IndexOf(ObjV^[1].AsString) < 0 then
    raise Exception.Create('Unknown connection type '''+ObjV^[1].AsString+'''');
  ConnTypeData.FConnType        := FReconfSignals.FConnTypes[ObjV^[1].AsString];
  ConnTypeData.FConnTypeOptions.Clear;

  MyVarArgs.First := 2;
  MyVarArgs.ObjC  := ObjC;
  MyVarArgs.ObjV  := ObjV;

  While MyVarArgs.First < ObjC do
    Begin
      Case ObjV^[MyVarArgs.First].AsString of
        '-array'     : Begin
                         ConnTypeData.FConnTypeOptions.FArray := true;
                       End;
        '-pad_left'  : Begin
                         Next;
                         ConnTypeData.FConnTypeOptions.FPadLeft := ParseSignalValue(ObjV^[MyVarArgs.First].AsString,ConnTypeData.FConnTypeOptions.FPadLeftWidth);
                       End;
        '-pad_right' : Begin
                         Next;
                         ConnTypeData.FConnTypeOptions.FPadRight := ParseSignalValue(ObjV^[MyVarArgs.First].AsString,ConnTypeData.FConnTypeOptions.FPadRightWidth);
                       End;
      else
        // unknown option --> hand over to SelectFunc
        Break;
      End;
      Next;
    End;
  // perform assignment
  if FReconfModule.FReconfSignals.Foreach(@SelectFunc,@MyVarArgs,@FReconfModule.FReconfSignals.SetConnType,@ConnTypeData) <= 0 then
    raise Exception.Create('No signals matched '+SelectStr(MyVarArgs));
End;

(*ronn
TODO

If everything is ok, the config and param information for the reconf.signals is
setup.
*)
Procedure TFlowApp.CheckReconfSignals(ObjC:Integer;ObjV:PPTcl_Object);
Var NoError : Boolean;
    Errors  : Integer;
    I       : Integer;
Begin
  // check_reconf_signal [-noerror]
  Errors := 0;
  if ObjC = 1 then
    NoError := False
  else if (ObjC = 2) and (ObjV^[1].AsString = '-noerror') then
    NoError := True
  else
    raise Exception.Create('bad arg: ' + ObjV^[0].AsPChar + ' [-noerror]');

  I := FReconfSignals.CheckSigConn;
  if I > 0 then
    WriteLn('Problem: Signals without signal connection: ',I);
  Errors := Errors + I;

  I := FReconfSignals.CheckConnType;
  if I > 0 then
    WriteLn('Problem: Signals without connection type: ',I);
  Errors := Errors + I;

  if Errors > 0 then
    Begin
      if not NoError then
        raise Exception.Create('check_reconf_signal found '+IntToStr(Errors)+' errors');
    End
  else
    Begin
      WriteLn('All reconf.signals have set a connection and a connection type.');
      FReconfSignals.SetupConfigAndParam;
    End;
End;

Type TNetlistType = (ntUnset,ntParent,ntPreliminary,ntWrapApp,ntWrapIS,ntWrapRMVHDL2008,ntWrapRMLEC,ntReconfModule,ntChip);

(*ronn
TODO
*)
Procedure TFlowApp.CreateReconfModNetlist(ObjC:Integer;ObjV:PPTcl_Object);
Var NetlistType        : TNetlistType;
    AppInstName        : String;
    I                  : Integer;
Begin
  // create_reconf_module_netlist -preliminary|-wrapapp|-intersynth [-appinstname instname]
  if ObjC > 4 then
    raise Exception.Create('bad # arg: ' + ObjV^[0].AsPChar + ' -preliminary|-wrapapp|-intersynth [-appinstname instname]');

  if not assigned(FReconfModule) then
    raise Exception.Create('Reconfigurable module does not yet exists');

  // netlist type
  NetlistType := ntUnset;
  if ObjV^[1].AsString = '-preliminary' then
    NetlistType := ntPreliminary
  else if ObjV^[1].AsString = '-wrapapp' then
    Begin
      if not assigned(FCurrentApp) then
        raise Exception.Create('Can''t create a netlist wrapping an application, because no application was created.');
      AppInstName := FCurrentApp.FName+'_0';  // default
      NetlistType := ntWrapApp;
    End
  else if ObjV^[1].AsString = '-intersynth' then
    NetlistType := ntReconfModule
  else
    raise Exception.Create('bad arg: ' + ObjV^[0].AsPChar + ' -preliminary|-wrapapp|-intersynth [-appinstname instname]');
  // names
  I := 2;
  While I < ObjC do
    Begin
      if I+1 >= ObjC then
        raise Exception.Create('bad # arg: ' + ObjV^[0].AsPChar + ' -preliminary|-wrapapp|-intersynth [-appinstname instname]');
      Case ObjV^[I].AsString of
        '-appinstname': Begin AppInstName        := ObjV^[I+1].AsString; Inc(I); End;
      else
        raise Exception.Create('bad arg: ' + ObjV^[I].AsPChar);
      End;
      Inc(I);
    End;
  // create netlist
  if NetlistType = ntPreliminary then
    Begin
      // TODO: check if there is already a netlist
      FReconfModule.FPreliminary := TReconfModuleNetlist.Create(FReconfModule,FReconfModule.FReconfigTypeName,FReconfModule.FReconfigInstName);
    End
  else if NetlistType = ntWrapApp then
    Begin
      // TODO: check if there is already a netlist
      FCurrentApp.FWrapper := TReconfModuleNetlistWrapApp.Create(FReconfModule,FReconfModule.FReconfigTypeName,FReconfModule.FReconfigInstName,FCurrentApp,AppInstName,FParamInterface);
    End
  else if NetlistType = ntReconfModule then
    Begin
      if not FReconfModuleFinished then
        raise Exception.Create('You first have to use finish_reconf_module');
      // TODO: check if there is already a netlist
      FReconfModule.FNetlist := TReconfModuleNetlistWithInterSynth.Create(FReconfModule,FInterSynthHandler,FReconfModule.FReconfigTypeName,FReconfModule.FReconfigInstName,FReconfModule.FInterSynthTypeName,FReconfModule.FInterSynthInstName,FConfigInterface,FParamInterface);
    End
  else
    raise Exception.Create('Invalid netlist type selected');
End;

(*ronn
TODO
*)
Procedure TFlowApp.CreateApplication(ObjC:Integer;ObjV:PPTcl_Object);
Begin
  // create_application "appname"
  if ObjC <> 2 then
    raise Exception.Create('bad # arg: ' + ObjV^[0].AsPChar + ' "appname"');

  if not assigned(FReconfModule) then
    raise Exception.Create('Reconfigurable module does not yet exists');

  if not assigned(FReconfModule.FReconfSignals) then
    raise Exception.Create('Reconfigurable signals are not yet added');

  if FReconfApps.FReconfApps.IndexOf(ObjV^[1].AsString) >= 0 then
    raise Exception.Create('An application named '''+ObjV^[1].AsString+''' already exists.');

  FCurrentApp := TReconfApp.Create(ObjV^[1].AsString,FReconfModule);
  FReconfApps.FReconfApps.Add(ObjV^[1].AsString,FCurrentApp);
End;

(*ronn
TODO
*)
Procedure TFlowApp.GetCurrentApp(ObjC:Integer;ObjV:PPTcl_Object);
Begin
  // get_current_app
  if ObjC <> 1 then
    raise Exception.Create('bad # arg: ' + ObjV^[0].AsPChar);

  if not Assigned(FCurrentApp) then
    WriteLn('Warning: No application selected');

  FTCL.SetResult(FCurrentApp.FName);
End;

(*ronn
TODO
*)
Procedure TFlowApp.SelectApp(ObjC:Integer;ObjV:PPTcl_Object);
Var I : Integer;
Begin
  // select_app "appname"
  if ObjC <> 2 then
    raise Exception.Create('bad # arg: ' + ObjV^[0].AsPChar + ' "appname"');

  I := FReconfApps.FReconfApps.IndexOf(ObjV^[1].AsString);
  if I < 0 then
    raise Exception.Create('No application named '''+ObjV^[1].AsString+''' exists.');

  FCurrentApp := FReconfApps.FReconfApps.Data[I];
End;

(*ronn
TODO

Without "-map", the name of the port in the Ex.App is identical to the name of
the reconfsignal.

TODO: if the dynamic ReconfSignal is an array port (e.g. Inputs_i), each port of
the application which is mapped to this must also specify the index into that
array.

Should this be done here with app_add_port, or later with an app_map_port or
even app_constrain_port_index or something? Can InterSynth do a (random) mapping
or do we have to specify the mapping ourselves?

  if (ReconfSignal.FSigConn as TSigConnDyn).FConnTypeOptions.FArray then

*)
Procedure TFlowApp.AppAddPort(ObjC:Integer;ObjV:PPTcl_Object);
Var Name             : String;
    ReconfSignalName : String;
    IndexL,IndexR    : Integer;
    ReconfSignal     : TReconfSignalBase;
    I                : Integer;
Begin
  // app_add_port "port" [-map "reconfsignal"] [-index N]

  if not assigned(FCurrentApp) then
    raise Exception.Create('You first have to create an application');

  Name := ObjV^[1].AsString;
  ReconfSignalName := Name;  // default
  IndexL           := -1;
  IndexR           := -1;
  I := 2;
  While I < ObjC do
    Begin
      if (ObjV^[I].AsString = '-map') and (ObjC > I) then
        Begin
          Inc(I);
          ReconfSignalName := ObjV^[I].AsString;
        End
      else if (ObjV^[I].AsString = '-index') and (ObjC > I) then
        Begin
          Inc(I);
          ParseIndex(ObjV^[I].AsString,IndexL,IndexR);
        End
      else
        raise Exception.Create('bad arg '+IntToStr(I)+': ' + ObjV^[0].AsPChar + ' "port" [-map "reconfsignal"] [-index N]');
      Inc(I);
    End;

  if FCurrentApp.HasPort(Name) then
    raise Exception.Create('The application already has a port named '''+Name+'''');

  I := FReconfModule.FReconfSignals.FReconfSignals.IndexOf(ReconfSignalName);
  if I < 0 then
    raise Exception.Create('No reconfig signal '''+ReconfSignalName+''' available');
  ReconfSignal := FReconfModule.FReconfSignals.FReconfSignals.Data[I];

  if ReconfSignal.FSigConn is TSigConnDyn then
    Begin
      if (ReconfSignal.FSigConn as TSigConnDyn).FConnTypeOptions.FArray then
        Begin
          // array signal --> user must specify index
          // currently we don't support automatic mapping
          if IndexL < 0 then
            raise Exception.Create('Reconf.signal '+ReconfSignal.FName+' is an array signal, therefore you have to specify an index');
          if (ReconfSignal.FSigConn as TSigConnDyn).FConnType.FWidth > 1 then
            Begin
              if IndexR < 0 then
                raise Exception.Create('Connection type '+(ReconfSignal.FSigConn as TSigConnDyn).FConnType.FName+
                                       ' is a vector of width '+IntToStr((ReconfSignal.FSigConn as TSigConnDyn).FConnType.FWidth)+
                                       ', therefore you have to specify an index range');
              if (IndexL-IndexR+1) <> (ReconfSignal.FSigConn as TSigConnDyn).FConnType.FWidth then
                raise Exception.CreateFmt('Index range width must equal the width of the connection type (%d)',[(ReconfSignal.FSigConn as TSigConnDyn).FConnType.FWidth]);
              if (IndexR mod (ReconfSignal.FSigConn as TSigConnDyn).FConnType.FWidth) <> 0 then
                raise Exception.CreateFmt('Index range must start at an integer multiple of the width of the connection type (%d)',[(ReconfSignal.FSigConn as TSigConnDyn).FConnType.FWidth]);
              // prepare for assignment below
              IndexL := IndexR div (ReconfSignal.FSigConn as TSigConnDyn).FConnType.FWidth;
            End
          else
            IndexR := IndexL;
          // IndexR now is the bit position, IndexL is the index of the array item
          // e.g. Width = 1, using 3rd item (=    2) --> IndexL = 2, IndexR = 2
          // e.g. Width = 4, using 3rd item (= 11:8) --> IndexL = 2, IndexR = 8
        End
      else
        Begin
          // no array --> user should not specify index
          if IndexL >= 0 then    // if IndexR is specified, IndexL is specified too
            raise Exception.Create('Reconf.signal '+ReconfSignal.FName+' (conntype '+(ReconfSignal.FSigConn as TSigConnDyn).FConnType.FName+') is no array signal, therefore no index allowed');
        End;
      FCurrentApp.AddDynamicPort(Name,ReconfSignal,IndexR); // IndexR was prepared in the lines above, internally checks whether this index was already used
    End
  else if ReconfSignal.FSigConn is TSigConnDirect then
    Begin
      FCurrentApp.AddDirectPort(Name,ReconfSignal);
    End
  else
    raise Exception.Create('Reconfig signals with a connection of '+ReconfSignal.FSigConn.ClassName+' can''t be used with app_add_port');
  // TODO: use a beautified string instead of TSigConn*.ClassName
End;

(*ronn
TODO

Dual-use function:
 1) add a parameter to the application, e.g. Threshold, ...
    --> specify -in|-out -conntype "conntype"
 2) use a connection/usage = param reconf.signal for the application
    --> skip -in|-out -conntype "conntype"

TODO: I think the below function automatically assumes, that the param goes
through the MUX tree and therefore has a conntype.

TODO: Array ports are not yet considered here.
*)
Procedure TFlowApp.AppAddParam(ObjC:Integer;ObjV:PPTcl_Object);
Var Dir         : TPortDirection;
    ConnType    : TConnType;
    Name        : String;
    HaveDefault : Boolean;
    Default     : Integer;
    ObjI        : Integer;
Begin
  // app_add_param [-in|-out -conntype "conntype"] [-default value] "name"
  if (ObjC < 2) or (ObjC > 7) then
    raise Exception.Create('bad # arg: ' + ObjV^[0].AsPChar + ' [-in|-out -conntype "conntype"] [-default value] "name"');

  if not assigned(FCurrentApp) then
    raise Exception.Create('You first have to create an application');

  // set defaults
  Dir         := dirUnknown;
  ConnType    := Nil;
  Name        := '';
  HaveDefault := False;
  // parse arguments
  ObjI := 1;
  While ObjI < ObjC do
    Begin
      if      ObjV^[ObjI].AsString = '-in'  then Dir := dirIn
      else if ObjV^[ObjI].AsString = '-out' then Dir := dirOut
      else if ObjV^[ObjI].AsString = '-conntype' then
        Begin
          Inc(ObjI);
          if ObjI >= ObjC then
            raise Exception.Create('bad arg: ' + ObjV^[0].AsPChar + ' [-in|-out -conntype "conntype"] [-default value] "name"');
          if FReconfModule.FReconfSignals.FConnTypes.IndexOf(ObjV^[ObjI].AsString) < 0 then
            raise Exception.Create('Unknown conntype '''+ObjV^[ObjI].AsString+'''');
          ConnType := FReconfModule.FReconfSignals.FConnTypes[ObjV^[ObjI].AsString];
        End
      else if ObjV^[ObjI].AsString = '-default' then
        Begin
          Inc(ObjI);
          if ObjI >= ObjC then
            raise Exception.Create('bad arg: ' + ObjV^[0].AsPChar + ' [-in|-out -conntype "conntype"] [-default value] "name"');
          HaveDefault := True;
          // TODO: perhaps we should use ParseSignalValue, see AppSetPortValue below
          Default := ObjV^[ObjI].AsInteger(FTCL);
        End
      else if ObjI <> ObjC-1 then
        raise Exception.Create('bad arg: ' + ObjV^[0].AsPChar + ' [-in|-out -conntype "conntype"] [-default value] "name"')
      else
        Name := ObjV^[ObjI].AsString;

      Inc(ObjI);
    End;

  if Name = '' then
    raise Exception.Create('You have to specify a name of the parameter');

  if (Dir <> dirUnknown) and (ConnType <> Nil) then
    Begin
      // add a parameter to the application
      if FCurrentApp.HasPort(Name) then
        raise Exception.Create('The application already has a port named '''+Name+'''');
      FCurrentApp.AddParamPort(Name,ConnType,Dir,HaveDefault,Default);
    End
  else if (Dir = dirUnknown) and (ConnType = Nil) then
    Begin
      // use a connection/usage = param reconf.signal for the application
      // TODO: see below at app_set_port_value and TReconfApp.FConstantValues,
      // generalize this a bit for TSigConnParam
      raise Exception.Create('Implement me!');
    End
  else
    raise Exception.Create('Either specify both, direction and conntype, or none of them');
End;

(*ronn
TODO
*)
Procedure TFlowApp.AppSetPortValue(ObjC:Integer;ObjV:PPTcl_Object);
Var Name   : String;
    Value  : Integer;
    Width  : Integer;
    Signal : TReconfSignalBase;
    NoWarn : Boolean;
Begin
  // app_set_port_value "signal" value [-nowarn]
  if (ObjC <> 3) and (ObjC <> 4) then
    raise Exception.Create('bad # arg: ' + ObjV^[0].AsPChar + ' "signal" value [-nowarn]');

  if not assigned(FCurrentApp) then
    raise Exception.Create('You first have to create an application');

  Name   := ObjV^[1].AsString;
  Value  := ParseSignalValue(ObjV^[2].AsString,Width);
  NoWarn := False;
  if ObjC = 4 then
    Begin
      if ObjV^[3].AsString = '-nowarn' then NoWarn := true
      else
        raise Exception.Create('bad arg: ' + ObjV^[0].AsPChar + ' "signal" value [-nowarn]');
    End;

  if FReconfModule.FReconfSignals.FReconfSignals.IndexOf(Name) < 0 then
    raise Exception.Create('No reconfig signal '''+Name+''' available');
  Signal := FReconfModule.FReconfSignals.FReconfSignals[Name];

  if Signal.GetDirection <> dirOut then
    raise Exception.Create('You can only set a value to outputs.');

  if (Width > 0) and (Signal.GetSignal.FType.GetWidthInt <> Width) then
    raise Exception.CreateFmt('Width of given value (%d) doesn''t match the width of the signal (%d)',[Width,Signal.GetSignal.FType.GetWidthInt]);

  if NoWarn and not (Signal.FSigConn is TSigConnConst) then
    raise Exception.Create('The option ''-nowarn'' is only allowed for signals set as constant');

  if Signal.FSigConn is TSigConnConst then
    Begin
      // this case is allowed so that the user can document all settings
      if Value <> (Signal.FSigConn as TSigConnConst).FValue then
        raise Exception.CreateFmt('Signal %s is set constant to value $%x and can not be set to $%x',[Name,(Signal.FSigConn as TSigConnConst).FValue,Value]);
      if not NoWarn then
        WriteLn('Warning: Signal ',Name,' is already set to constant value $',IntToHex(Value,1));
    End
  else if Signal.FSigConn is TSigConnConfig then
    // nothing to do
  else if Signal.FSigConn is TSigConnParam then
    // nothing to do
  else if Signal.FSigConn is TSigConnDyn then
    // nothing to do
  else
    raise Exception.Create('Can''t set value of signal with connection/usage is '''+Signal.FSigConn.ClassName+'''');
  // TODO: use a beautified string instead of TSigConn*.ClassName

  FCurrentApp.AddConstantValue(Signal,Value);
End;

(*ronn
TODO
*)
Procedure TFlowApp.AppShow(ObjC:Integer;ObjV:PPTcl_Object);
Var App : TReconfApp;
Begin
  // app_show ["appname"]
  if ObjC = 1 then
    App := FCurrentApp
  else if ObjC = 2 then
    App := FReconfApps.FReconfApps[ObjV^[1].AsString]
  else
    raise Exception.Create('bad # arg: ' + ObjV^[0].AsPChar + ' "appname"');

  App.Show;
End;

(*ronn
TODO
*)
Procedure TFlowApp.ListApps(ObjC:Integer;ObjV:PPTcl_Object);
Var Show : Boolean;
    I    : Integer;
Begin
  // list_apps [-show]
  if ObjC = 1 then
    Show := false
  else if (ObjC = 2) and (ObjV^[1].AsString = '-show') then
    Show := true
  else
    raise Exception.Create('bad # arg: ' + ObjV^[0].AsPChar + ' [-show]');

  WriteLn('Applications: ',FReconfApps.FReconfApps.Count);
  For I := 0 to FReconfApps.FReconfApps.Count-1 do
    Begin
      if Show then FReconfApps.FReconfApps.Data[I].Show
      else WriteLn('  ',FReconfApps.FReconfApps.Keys[I]);
    End;
End;

(*ronn
TODO
*)
Procedure TFlowApp.AppWriteTemplate(ObjC:Integer;ObjV:PPTcl_Object);
Var VHDL      : Boolean;
    Testbench : Boolean;
    Filename  : String;
Begin
  // app_write_template -vhdl|-verilog [-testbench] -o filename
  if (ObjC < 4) or (ObjC > 5) then
    raise Exception.Create('bad # arg: ' + ObjV^[0].AsPChar + ' -vhdl|-verilog [-testbench] -o filename');

  if      ObjV^[1].AsString = '-vhdl'    then VHDL := true
  else if ObjV^[1].AsString = '-verilog' then VHDL := false
  else
    raise Exception.Create('bad arg: ' + ObjV^[0].AsPChar + ' -vhdl|-verilog [-testbench] -o filename');

  if ObjC = 5 then
    Begin
      if ObjV^[2].AsString = '-testbench' then
        Testbench := true
      else
        raise Exception.Create('bad arg: ' + ObjV^[0].AsPChar + ' -vhdl|-verilog [-testbench] -o filename')
    End
  else
    Testbench := false;

  if ObjV^[ObjC-1-1].AsString <> '-o' then
    raise Exception.Create('bad arg: ' + ObjV^[0].AsPChar + ' -vhdl|-verilog [-testbench] -o filename');

  Filename := ObjV^[ObjC-1].AsString;

  if not assigned(FCurrentApp) then
    raise Exception.Create('You first have to create an application');

  if not assigned(FCurrentApp.FModule) then
    FCurrentApp.GenerateNetlist;

  if VHDL and not Testbench then
    Begin
      WriteLn('Writing VHDL module to ',Filename);
      FilePutContents(Filename,FCurrentApp.FModule.GetVHDL);
    End
  else if VHDL and Testbench then
    Begin
      WriteLn('Writing VHDL testbench to ',Filename);
      FilePutContents(Filename,FCurrentApp.FTestbench.GetVHDL);
    End
  else if not Testbench then
    Begin
      WriteLn('Writing Verilog module to ',Filename);
      FilePutContents(Filename,FCurrentApp.FModule.WriteVerilogDeclaration);
    End
  else
    Begin
      WriteLn('Writing Verilog testbench to ',Filename);
      FilePutContents(Filename,FCurrentApp.FTestbench.WriteVerilogDeclaration);
    End;
End;

(*ronn
TODO

The following assumptions/requirements are made:
 - the driver header files for the CfgIntf and ParamIntf "cfgintf.h" and
   "paramintf.h", respectively, are included by the driver source
 - a file "<app>-config.inc.c" is included by the driver source, which must
   itself include all config bitstreams
 - names of the config bitstream constants must match the names of the
   T*ConfigRegister
 - the driver provides a function "init_app_<app>()", which performs the
   configuration and setting the parameters
*)
Procedure TFlowApp.AppWriteFirmware(ObjC:Integer;ObjV:PPTcl_Object);

  Procedure WriteDriverHeader(Filename:String;ConfigRegisters:TAddressedConfigRegisters;Parameters:TAddressedParameters;App:TReconfApp);
  Var St    : String;
      Guard : String;
      I     : Integer;
      B     : Boolean;
      Name  : String;
  Begin
    // store filename
    FCurrentApp.FDriverHeaderFilename := Filename;

    Guard := '__' + UpperCase(StrTr(ExtractFileName(Filename),'.-','_'));

    St := '///////////////////////////////////////////////////////////////////////////////' + LineEnding;
    St += '// Automatically generated: ' + GetCmdLine(ObjC,ObjV) + LineEnding;
    St += '///////////////////////////////////////////////////////////////////////////////' + LineEnding;
    St += LineEnding;
    St += '#ifndef ' + Guard + LineEnding;
    St += '#define ' + Guard + LineEnding;
    St += LineEnding;

    // config chains
    if assigned(ConfigRegisters) then
      Begin
        St += '// Config Chains' + LineEnding;
        For I := 0 to ConfigRegisters.FConfigRegisters.Count-1 do
          With ConfigRegisters.FConfigRegisters.Data[I],FConfigRegister do
            St += '#define CFGREG_'+UpperCase(FName)+'_ADDR ' + StringOfChar(' ',39-Length(FName)) + IntToStr(FAddress) + LineEnding;
        St += LineEnding;
      End;

    // param write (CPU -> ReconfModule)
    St += '// Param Write (CPU -> ReconfModule)' + LineEnding;
    For I := 0 to Parameters.FParameters.Count-1 do
      With Parameters.FParameters.Data[I],FParameter do
        Begin
          if FDir <> pdWrite then
            Continue;
          St += '#define PARAM_'+UpperCase(FName)+'_ADDR ' + StringOfChar(' ',40-Length(FName)) + '0x'+IntToHex(FAddress,4) + LineEnding;
        End;

    // Param Read (ReconfModule -> CPU)
    St += LineEnding;
    St += '// Param Read (ReconfModule -> CPU)' + LineEnding;
    For I := 0 to Parameters.FParameters.Count-1 do
      With Parameters.FParameters.Data[I],FParameter do
        Begin
          if FDir <> pdRead then
            Continue;
          St += '#define PARAM_'+UpperCase(FName)+'_ADDR ' + StringOfChar(' ',40-Length(FName)) + '0x'+IntToHex(FAddress,4) + LineEnding;
        End;
    // Note: The parameter addresses have a spacing of 1 (i.e. 0, 1, 2, ...),
    // although e.g. the OpenMSP430 CPU uses word-addressing (spacing of 2, i.e.
    // 0, 2, 4, ...).
    // Note that there is no connection between these two addressing schemes,
    // because the parameter address is written to the memory-mapped register
    // "Param Config and Address" (PCA) of the ParamIntf and then conducted via
    // its outputs ParamWrAddr_o and ParamRdAddr_o.

    // When run with "-reconfmodule", we get parameters like
    // "PARAM_PARAMIN_WORD_0_ADDR", ... instead of the constants for the
    // application, e.g. "PARAM_PERIODCOUNTERPRESET_I_ADDR". For the
    // hand-written (ex.)app. driver, we additionally provide constants with
    // the better names.
    //
    // we have to do a back-mapping here.
    // This is quite a dirty implementation.
    B := false;
    For I := 0 to App.FParamPorts.Count-1 do
      With App.FParamPorts.Data[I] do
        if FMapping >= 0 then
          Begin
            if FDirection = dirIn then
              Name := 'ParamIn_'+FConnType.FName+'_'+IntToStr(FMapping)
            else
              Name := 'ParamOut_'+FConnType.FName+'_'+IntToStr(FMapping);
            if Parameters.FParameters.IndexOf(Name) < 0 then
              Continue;    // parameter not defined, so we assume that this is the -wrapapp run
            if not B then
              Begin
                St += LineEnding;
                St += '// Param Mapping' + LineEnding;
                B := true;
              End;
            St += '#define PARAM_'+UpperCase(FName)+'_ADDR ' + StringOfChar(' ',40-Length(FName)) + 'PARAM_'+UpperCase(Name)+'_ADDR' + LineEnding
          End;

    // signal mapping for array ports
    St += LineEnding;
    St += '// Signal Mapping for Array Ports' + LineEnding;
    For I := 0 to App.FDynamicPorts.Count-1 do
      With App.FDynamicPorts.Data[I] do
        Begin
          if FIndex < 0 then
            Continue;   // not an array signal
          St += '#define ARRAY_MAP_'+UpperCase(FReconfSignal.FName)+'_'+UpperCase(FName) + StringOfChar(' ',41-Length(FName)-Length(FReconfSignal.FName)) + IntToStr(FIndex div (FReconfSignal.FSigConn as TSigConnDyn).FConnType.FWidth) + LineEnding;
        End;

    St += LineEnding;
    St += 'void init_app_'+LowerCase(FCurrentApp.FName) + '();' + LineEnding;

    St += LineEnding;
    St += '#endif  // ' + Guard + LineEnding;

    FilePutContents(Filename,St);
  End;

  Procedure WriteDriverSource(Filename:String;ConfigRegisters:TAddressedConfigRegisters;Parameters:TAddressedParameters;App:TReconfApp);
  Var St    : String;
      I,J   : Integer;
      V     : Integer;
      B     : Boolean;
      Name  : String;
  Begin
    St := '///////////////////////////////////////////////////////////////////////////////' + LineEnding;
    St += '// Automatically generated: ' + GetCmdLine(ObjC,ObjV) + LineEnding;
    St += '///////////////////////////////////////////////////////////////////////////////' + LineEnding;
    St += LineEnding;
    St += '#include "'+ExtractFileName(FCurrentApp.FDriverHeaderFilename)+'"' + LineEnding;
    St += LineEnding;
    St += '#include "cfgintf.h"' + LineEnding;
    St += '#include "paramintf.h"' + LineEnding;
    St += LineEnding;

    // include bitstreams
    if assigned(ConfigRegisters) then
      Begin
        St += '// include bitstreams' + LineEnding;
        St += '#include "' + LowerCase(FCurrentApp.FName) + '-config.inc.c"' + LineEnding;
        St += LineEnding;
      End;

    // (Ex.)App. initialization function: perform configuration + parameterization
    St += 'void init_app_'+LowerCase(FCurrentApp.FName) + '() {' + LineEnding;
    if assigned(ConfigRegisters) then
      Begin
        St += '  // Configuration' + LineEnding;
        St += '  ConfigBegin();' + LineEnding;
        For I := 0 to ConfigRegisters.FConfigRegisters.Count-1 do
          With ConfigRegisters.FConfigRegisters.Data[I],FConfigRegister do
            St += '  Configure(CFGREG_'+UpperCase(FName)+'_ADDR,' + StringOfChar(' ',30-Length(FName)) + '&Cfg' + FName + ');' + LineEnding;
      End
    else
      Begin
        St += '  // No Configuration, therefore the app is already running' + LineEnding;
      End;
    St += LineEnding;
    St += '  // Param default values' + LineEnding;
    For I := 0 to Parameters.FParameters.Count-1 do
      Begin
        if Parameters.FParameters.Data[I].FParameter.FDir <> pdWrite then
          Continue;
        if Parameters.FParameters.Data[I].FParameter.FHaveDefault then
          V := Parameters.FParameters.Data[I].FParameter.FDefault
        else
          V := 0;

        // (Ex.)App. constant value for parameter
        if App.FConstantValues.IndexOf(Parameters.FParameters.Data[I].FParameter.FName) >= 0 then
          V := App.FConstantValues[Parameters.FParameters.Data[I].FParameter.FName].FValue;

        // When run with "-reconfmodule", we get parameters like
        // "PARAM_PARAMIN_WORD_0_ADDR", ... instead of the constants for the
        // application, e.g. "PARAM_PERIODCOUNTERPRESET_I_ADDR". Therefore
        // we have to do a back-mapping here.
        // This is quite a dirty implementation.
        B := false;
        For J := 0 to App.FParamPorts.Count-1 do
          if App.FParamPorts.Data[J].FMapping >= 0 then
            Begin
              if App.FParamPorts.Data[J].FDirection = dirIn then
                Name := 'ParamIn_'
              else
                Name := 'ParamOut_';
              Name += App.FParamPorts.Data[J].FConnType.FName+'_'+IntToStr(App.FParamPorts.Data[J].FMapping);
              // if App-parameter J is mapped to InterSynth-module-parameter I
              // (precisely: global parameter I), then we use the App-param.
              if Parameters.FParameters.Data[I].FParameter.FName = Name then
                Begin
                  Name := App.FParamPorts.Data[J].FName;
                  if App.FParamPorts.Data[J].FHaveDefault then
                    V := App.FParamPorts.Data[J].FDefault;
                  B := true;
                  break;
                End;
            End;
        if not B then
          Name := Parameters.FParameters.Data[I].FParameter.FName;   // no back-mapping, use original name

        St += '  ParamWrite(PARAM_'+UpperCase(Name)+'_ADDR, ' + StringOfChar(' ',40-Length(Name)) + '0x'+IntToHex(V,4) + ');' + LineEnding;
      End;

    if assigned(ConfigRegisters) then
      Begin
        St += LineEnding;
        St += '  // Done with setup, release reconf.module from config mode' + LineEnding;
        St += '  ConfigEnd();' + LineEnding;
      End;
    St += '}' + LineEnding;

    FilePutContents(Filename,St);
  End;

Var Filename        : String;
    ConfigRegisters : TAddressedConfigRegisters;
    Parameters      : TAddressedParameters;
Begin
  // app_write_firmware -wrapapp|-reconfmodule -driver_header|-driver_source filename
  AssertParams(ObjC,4,ObjV,' -wrapapp|-reconfmodule -driver_header|-driver_source filename');

  if not assigned(FCurrentApp) then
    raise Exception.Create('You first have to create an application');

  if      ObjV^[1].AsPChar = '-wrapapp' then
    Begin
      if not assigned(FCurrentApp.FWrapper) then
        raise Exception.Create('You have to create a wrapper using "create_reconf_module_netlist -wrapapp"');
      ConfigRegisters := Nil;   // not yet defined
      Parameters      := FCurrentApp.FWrapper.FParameters;
    End
  else if ObjV^[1].AsPChar = '-reconfmodule' then
    Begin
      if not assigned(FReconfModule.FParameters) then
        raise Exception.Create('You have to create a wrapper using "create_reconf_module_netlist -intersynth"');
      ConfigRegisters := TAddressedConfigRegisters(FReconfModule.FConfigRegisters);
      Parameters      := TAddressedParameters     (FReconfModule.FParameters);
    End
  else
    raise Exception.Create('bad arg: ' + ObjV^[0].AsPChar + ' -wrapapp|-reconfmodule -driver_header|-driver_source filename');

  Filename := ObjV^[3].AsString;

  if      ObjV^[2].AsPChar = '-driver_header' then
    WriteDriverHeader(Filename,ConfigRegisters,Parameters,FCurrentApp)
  else if ObjV^[2].AsPChar = '-driver_source' then
    WriteDriverSource(Filename,ConfigRegisters,Parameters,FCurrentApp)
  else
    raise Exception.Create('bad arg: ' + ObjV^[0].AsPChar + ' -wrapapp|-reconfmodule -driver_header|-driver_source filename');
End;

(*ronn
TODO
*)
Procedure TFlowApp.AppReadNetlist(ObjC:Integer;ObjV:PPTcl_Object);
Var ObjI      : Integer;
    Extracted : Boolean;
    Modules   : TModuleList;
    Filename  : String;
    I         : Integer;
    Ignore    : TFPGStringList;
    PostSi    : Boolean;
Begin
  // app_read_netlist [-extracted [-ignore cell_list] [-post-si]] filename.il
  if (ObjC < 2) or (ObjC > 6) then
    raise Exception.Create('bad # arg: ' + ObjV^[0].AsPChar + ' [-extracted [-ignore cell_list] [-post-si]] filename.il');

  ObjI      := 1;
  Extracted := false;
  Ignore    := Nil;
  Filename  := ObjV^[ObjC-1].AsPChar;
  PostSi    := false;
  While ObjI < ObjC-1 do   // '-1' to omit the last argument: filename.il
    Begin
      Case ObjV^[ObjI].AsString of
        '-extracted' :  Begin
                          if Extracted then    // got '-extracted' twice
                            raise Exception.Create('bad arg: ' + ObjV^[0].AsPChar + ' [-extracted [-ignore cell_list] [-post-si]] filename.il');
                          Extracted := true;
                          Ignore    := TFPGStringList.Create;
                        End;
        '-ignore' :     Begin
                          if not Extracted then    // didn't get '-extracted' before
                            raise Exception.Create('bad arg: ' + ObjV^[0].AsPChar + ' [-extracted [-ignore cell_list] [-post-si]] filename.il');
                          Inc(ObjI);
                          if ObjI >= ObjC then
                            raise Exception.Create('bad # arg: ' + ObjV^[0].AsPChar + ' [-extracted [-ignore cell_list] [-post-si]] filename.il');
                          For I := 0 to FTCL.ListObjLength(ObjV^[ObjI].PObj)-1 do
                            Ignore.Add(Tcl_GetString(FTCL.ListObjIndex(ObjV^[ObjI].PObj,I)));
                        End;
        '-post-si' :    Begin
                          if not Extracted then    // didn't get '-extracted' before
                            raise Exception.Create('bad arg: ' + ObjV^[0].AsPChar + ' [-extracted [-ignore cell_list] [-post-si]] filename.il');
                          PostSi := true;
                        End;
      else
        raise Exception.Create('bad arg '''+ObjV^[ObjI].AsString+''': ' + ObjV^[0].AsPChar + ' [-extracted [-ignore cell_list] [-post-si]] filename.il');
      End;
      Inc(ObjI);
    End;

  if not assigned(FCurrentApp) then
    raise Exception.Create('You first have to create an application');

  if Extracted and not assigned(FCurrentCell) then   // use FCurrentCell as a hint that there is at least one cell in the cell library
    raise Exception.Create('You first have to define a cell library');

  if PostSi and (FInterSynthHandler.FReadCellCount.Count = 0) then
    raise Exception.Create('You have to read the InterSynth results');

  if Extracted then
    // pre-seed modules list with cells from cell library
    Modules := FReconfCells.GetCells
  else
    Modules := Nil;
  ILang.Parse(Filename,Modules);
  if Modules.IndexOf(FCurrentApp.FName) < 0 then
    raise Exception.Create('Didn''t read a module named '''+FCurrentApp.FName+'''');
  FCurrentApp.FYosysNetlist := Modules[FCurrentApp.FName];
  Modules.Free;
  // check the netlist
  ObjC := FCurrentApp.CheckNetlist;  // reuse variable
  if ObjC > 0 then
    raise Exception.Create('The netlist differs in '+IntToStr(ObjC)+' elements.')
  else
    WriteLn('The application netlist (port definitions) is OK.');

  if not Extracted then
    Exit;

  // check the extracted netlist
  I := FCurrentApp.CheckNetlistExtracted(FReconfCells,Ignore);
  if ObjC > 0 then
    raise Exception.Create('The extracted application netlist has '+IntToStr(I)+' errors.')
  else if Ignore.Count > 0 then
    WriteLn('The extracted application netlist is OK, but some cell types might have been ignored.')
  else
    WriteLn('The extracted application netlist is OK.');

  if not PostSi then
    Begin
      Ignore.Free;
      Exit;
    End;

  // check usage
  I := FInterSynthHandler.CheckUsage(FCurrentApp,Ignore);
  if I > 0 then
    raise Exception.Create('The extracted application netlist has '+IntToStr(I)+' errors.')
  else if Ignore.Count > 0 then
    WriteLn('The application netlist usage of InterSynth module cells is OK, but some cell types might have been ignored.')
  else
    WriteLn('The application netlist usage of InterSynth module cells is OK.');
  Ignore.Free;
End;

Procedure TFlowApp.CreateAppUsage(All : Boolean; Var Cols : TStringIntMap; Out Table : TTable; Out TextT:TTextTable);

  Function NextCol : Integer;
  Var I : Integer;
  Begin
    // find maximum column index
    Result := 0;
    For I := 0 to Cols.Count-1 do
      if Cols.Data[I] > Result then
        Result := Cols.Data[I];
    // increment to next column index
    Result := Result + 1;
  End;

  Procedure AddAppUsage(AApp:TReconfApp;Var Row : Integer);
  Var I,J   : Integer;
      Usage : TCellUsage;
  Begin
    Table.SetVal(Row,0,AApp.FName);
    Usage := AApp.GetUsage;
    For I := 0 to Usage.Count-1 do
      Begin
        J := Cols.IndexOf(Usage.Keys[I]);    // find column label
        if J < 0 then
          Begin
            // unknown cell --> append
            J := NextCol;      // column index
            Cols.Add(Usage.Keys[I], J);
            Table.SetVal(0, J, Usage.Keys[I]);
          End
        else
          J := Cols.Data[J];   // column index
        Table.SetVal(Row, J, Usage.Data[I]);
      End;
    Usage.Free;
    Inc(Row);
  End;

Var I   : Integer;
    Row : Integer;

Begin
  if not assigned(Cols) then
    Cols := TStringIntMap.Create;
  // first column: $fsm
  Cols.Add('$fsm',NextCol);
  // add all known cell types
  For I := 0 to FReconfCells.FReconfCells.Count-1 do
    Cols.Add(FReconfCells.FReconfCells.Keys[I], NextCol);

  // create table and add columns
  Table := TTable.Create;
  Table.SetVal(0, 0,'App');
  For I := 0 to Cols.Count-1 do
    Table.SetVal(0, Cols.Data[I], Cols.Keys[I]);

  // add application usage
  Row := 1;
  if not All then
    AddAppUsage(FCurrentApp,Row)
  else
    Begin
      For I := 0 to FReconfApps.FReconfApps.Count-1 do
        AddAppUsage(FReconfApps.FReconfApps.Data[I],Row);
      // add "min" row
      Table.AddAggregateRow([1..Row-1],[1..Cols.Count],High(Integer),@Min);
      Table.SetVal(Row,0,'Min');
      // add "max" row
      Table.AddAggregateRow([1..Row-1],[1..Cols.Count],Low(Integer),@Max);
      Table.SetVal(Row+1,0,'Max');
    End;

  // format table
  TextT := TTextTable.Create(Table);
  For I := 0 to Cols.Count-1 do
    TextT.SetJustifyCol(Cols.Data[I],jjRight);
  TextT.SetColSep(1,' | ');
  TextT.SetRowSep(1,'-');
  if All then
    TextT.SetRowSep(Row,'-');
End;

(*ronn
TODO
*)
Procedure TFlowApp.AppPrintUsage(ObjC:Integer;ObjV:PPTcl_Object);
Var All   : Boolean;
    Cols  : TStringIntMap;     // map "Cell Name" -> "Column Index"
    Table : TTable;
    I,J   : Integer;
    TextT : TTextTable;
    Ports : TStringIntMap;
Begin
  // app_print_usage [-all]
  if ObjC = 1 then
    All := False
  else if (ObjC = 2) then
    Begin
      if ObjV^[1].AsString = '-all' then
        All := true
      else
        raise Exception.Create('bad arg: ' + ObjV^[0].AsPChar + ' [-all]');
    End
  else
    raise Exception.Create('bad # arg: ' + ObjV^[0].AsPChar + ' [-all]');

  Cols := Nil;
  CreateAppUsage(All,Cols,Table,TextT);

  WriteLn(TextT.GetTable);
  Cols.Free;
  TextT.Free;
  Table.Free;
  if not All then
    Exit;

  // warn if reconf.signals are unused
  Ports := TStringIntMap.Create;
  // get all dynamic reconf.signals
  For I := 0 to FReconfSignals.FReconfSignals.Count-1 do
    if FReconfSignals.FReconfSignals.Data[I].FSigConn is TSigConnDyn then
      Ports.Add(FReconfSignals.FReconfSignals.Data[I].GetPortName,0);
  // mark used
  For J := 0 to FReconfApps.FReconfApps.Count-1 do
    With FReconfApps.FReconfApps.Data[J] do
      Begin
        For I := 0 to FDynamicPorts.Count-1 do
          Ports[FDynamicPorts.Data[I].FReconfSignal.GetPortName] := Ports[FDynamicPorts.Data[I].FReconfSignal.GetPortName] + 1;
        For I := 0 to FConstantValues.Count-1 do
          if FConstantValues.Data[I].FReconfSignal.FSigConn is TSigConnDyn then
            Ports[FConstantValues.Data[I].FReconfSignal.GetPortName] := Ports[FConstantValues.Data[I].FReconfSignal.GetPortName] + 1;
      End;
  // search if some are unused
  J := 0;
  For I := 0 to Ports.Count-1 do
    if Ports.Data[I] = 0 then
      Inc(J);
  if J > 0 then
    Begin
      WriteLn;
      WriteLn('Warning: There are ',J,' reconf.signals with connection/usage dynamic which',LineEnding,
        'are not used by any ex.app. This will result in a badly optimized interconnect and',LineEnding,
        'most probably to unroutable signals and therefore unusable peripherals:');
      For I := 0 to Ports.Count-1 do
        if Ports.Data[I] = 0 then
          WriteLn('  ',Ports.Keys[I]);
    End;
  Ports.Free;
End;

(*
TODO
*)
Procedure TFlowApp.CreateCell(ObjC:Integer;ObjV:PPTcl_Object);
Begin
  // create_cell "cellname"
  if ObjC <> 2 then
    raise Exception.Create('bad # arg: ' + ObjV^[0].AsPChar + ' "cellname"');

  if not assigned(FReconfModule) then
    raise Exception.Create('Reconfigurable module does not yet exists');

  if not assigned(FReconfModule.FReconfSignals) then
    raise Exception.Create('Reconfigurable signals are not yet added');

  if FReconfCells.FReconfCells.IndexOf(ObjV^[1].AsString) >= 0 then
    raise Exception.Create('A cell named '''+ObjV^[1].AsString+''' already exists.');

  FCurrentCell := TReconfCell.Create(ObjV^[1].AsString,FReconfModule);
  FReconfCells.FReconfCells.Add(ObjV^[1].AsString,FCurrentCell);
End;

(*
TODO
*)
Procedure TFlowApp.GetCurrentCell(ObjC:Integer;ObjV:PPTcl_Object);
Begin
  // get_current_cell
  if ObjC <> 1 then
    raise Exception.Create('bad # arg: ' + ObjV^[0].AsPChar);

  if not Assigned(FCurrentCell) then
    WriteLn('Warning: No cell selected');

  FTCL.SetResult(FCurrentCell.FName);
End;

(*
TODO
*)
Procedure TFlowApp.SelectCell(ObjC:Integer;ObjV:PPTcl_Object);
Var I : Integer;
Begin
  // select_cell "cellname"
  if ObjC <> 2 then
    raise Exception.Create('bad # arg: ' + ObjV^[0].AsPChar + ' "cellname"');

  I := FReconfCells.FReconfCells.IndexOf(ObjV^[1].AsString);
  if I < 0 then
    raise Exception.Create('No cell named '''+ObjV^[1].AsString+''' exists.');

  FCurrentCell := FReconfCells.FReconfCells.Data[I];
End;

(*
TODO
*)
Procedure TFlowApp.CellAddPort(ObjC:Integer;ObjV:PPTcl_Object);
Var Name             : String;
    ReconfSignalName : String;
    Dir              : TPortDirection;
    ConnType         : TConnType;
    ReconfSignal     : TReconfSignalBase;
    ConfigWidth      : Integer;
    ObjI             : Integer;
Begin
  // cell_add_port "port" [-in|-out -conntype "conntype"] [-map "reconfsignal"] [-config -width n]
  if not assigned(FCurrentCell) then
    raise Exception.Create('You first have to create a cell');

  // set defaults
  Dir              := dirUnknown;
  ConnType         := Nil;
  ReconfSignalName := '';
  ConfigWidth      := -1;
  // parse arguments
  Name := ObjV^[1].AsString;
  ObjI := 2;
  While ObjI < ObjC do
    Begin
      if      (ObjV^[ObjI].AsString = '-in')  then Dir := dirIn
      else if (ObjV^[ObjI].AsString = '-out') then Dir := dirOut
      else if (ObjV^[ObjI].AsString = '-conntype') and (ObjC > 1) then
        Begin
          Inc(ObjI);
          if FReconfModule.FReconfSignals.FConnTypes.IndexOf(ObjV^[ObjI].AsString) < 0 then
            raise Exception.Create('Unknown conntype '''+ObjV^[ObjI].AsString+'''');
          ConnType := FReconfModule.FReconfSignals.FConnTypes[ObjV^[ObjI].AsString];
        End
      else if (ObjV^[ObjI].AsString = '-map') and (ObjC > 1) then
        Begin
          Inc(ObjI);
          ReconfSignalName := ObjV^[ObjI].AsString;
          if FReconfModule.FReconfSignals.FReconfSignals.IndexOf(ReconfSignalName) < 0 then
            raise Exception.Create('Unknown reconf signal '''+ReconfSignalName+'''');
        End
      else if (ObjV^[ObjI].AsString = '-config') and (ObjC > 2) and (ObjV^[ObjI+1].AsString = '-width') then
        Begin
          Inc(ObjI,2);
          ConfigWidth := ObjV^[ObjI].AsInteger(FTCL);
          if ConfigWidth < 1 then       // this also handles negative numbers
            raise Exception.Create('Config signals must have a width of at least one bit');
        End
      else
        raise Exception.Create('bad arg: ' + ObjV^[0].AsPChar + ' "port" [-in|-out -conntype "conntype"] [-map "reconfsignal"] [-config -width n]');
      Inc(ObjI);
    End;

  if FCurrentCell.HasPort(Name) then
    raise Exception.Create('The cell already has a port named '''+Name+'''');

  if (Dir <> dirUnknown) and not assigned(ConnType) then
    raise Exception.Create('With -in or -out you also need -conntype "conntype"');

  if Ord(Boolean(Dir <> dirUnknown)) + Ord(Boolean(ReconfSignalName <> '')) + Ord(Boolean(ConfigWidth >= 1)) <> 1 then
    raise Exception.Create('Use either ''-in|-out -conntype "conntype"'' or ''-map "reconfsignal"'' or ''[-config -width n]''');

  if Dir <> dirUnknown then
    Begin
      // dynamic port
      FCurrentCell.AddDynamicPort(Name,Dir,ConnType);
    End
  else if ReconfSignalName <> '' then
    Begin
      // direct port
      ObjI := FReconfModule.FReconfSignals.FReconfSignals.IndexOf(ReconfSignalName);   // reuse variable ObjI
      ReconfSignal := FReconfModule.FReconfSignals.FReconfSignals.Data[ObjI];
      FCurrentCell.AddDirectPort(Name,ReconfSignal);
    End
  else if ConfigWidth >= 1 then
    Begin
      // config port
      FCurrentCell.AddConfigPort(Name,ConfigWidth);;
    End
  else
    raise Exception.Create('Internal error: nothing to do');
End;

(*
TODO
*)
Procedure TFlowApp.CellAddConfigChain(ObjC:Integer;ObjV:PPTcl_Object);
Var ObjI : Integer;
Begin
  // cell_add_config_chain -length n [-mode "CfgMode_i"] [-clk "CfgClk_i"] [-shift "CfgShift_i"] [-datain "CfgDataIn_i"] [-dataout "CfgDataOut_o"]
  if not assigned(FCurrentCell) then
    raise Exception.Create('You first have to create a cell');

  if assigned(FCurrentCell.FConfigChain) then
    raise Exception.Create('The cell already has a config chain');

  FCurrentCell.FConfigChain := TConfigChain.Create; // also sets default values
  // parse arguments
  ObjI := 1;
  While ObjI < ObjC do
    Begin
      if      (ObjV^[ObjI].AsString = '-length') and (ObjC > 1) then
        Begin
          Inc(ObjI);
          FCurrentCell.FConfigChain.FChainLen := ObjV^[ObjI].AsInteger(FTCL);
        End
      else if (ObjV^[ObjI].AsString = '-mode') and (ObjC > 1) then
        Begin
          Inc(ObjI);
          FCurrentCell.FConfigChain.FCfgMode := ObjV^[ObjI].AsString;
        End
      else if (ObjV^[ObjI].AsString = '-clk') and (ObjC > 1) then
        Begin
          Inc(ObjI);
          FCurrentCell.FConfigChain.FCfgClk := ObjV^[ObjI].AsString;
        End
      else if (ObjV^[ObjI].AsString = '-shift') and (ObjC > 1) then
        Begin
          Inc(ObjI);
          FCurrentCell.FConfigChain.FCfgShift := ObjV^[ObjI].AsString;
        End
      else if (ObjV^[ObjI].AsString = '-datain') and (ObjC > 1) then
        Begin
          Inc(ObjI);
          FCurrentCell.FConfigChain.FCfgDataIn := ObjV^[ObjI].AsString;
        End
      else if (ObjV^[ObjI].AsString = '-dataout') and (ObjC > 1) then
        Begin
          Inc(ObjI);
          FCurrentCell.FConfigChain.FCfgDataOut:= ObjV^[ObjI].AsString;
        End
      else
        raise Exception.Create('bad arg: ' + ObjV^[0].AsPChar + ' -length n [-mode "CfgMode_i"] [-clk "CfgClk_i"] [-shift "CfgShift_i"] [-datain "CfgDataIn_i"] [-dataout "CfgDataOut_o"]');
      Inc(ObjI);
    End;

  if FCurrentCell.FConfigChain.FChainLen <= 0 then
    raise Exception.Create('You have to set the length of the config chain.');
End;

(*
TODO
*)
Procedure TFlowApp.CellShow(ObjC:Integer;ObjV:PPTcl_Object);
Var Cell : TReconfCell;
Begin
  // cell_show ["cellname"]
  if ObjC = 1 then
    Cell := FCurrentCell
  else if ObjC = 2 then
    Cell := FReconfCells.FReconfCells[ObjV^[1].AsString]
  else
    raise Exception.Create('bad # arg: ' + ObjV^[0].AsPChar + ' cellname"');

  Cell.Show;
End;

(*
TODO
*)
Procedure TFlowApp.ListCells(ObjC:Integer;ObjV:PPTcl_Object);
Var Show : Boolean;
    I    : Integer;
Begin
  // list_cells [-show]
  if ObjC = 1 then
    Show := false
  else if (ObjC = 2) and (ObjV^[1].AsString = '-show') then
    Show := true
  else
    raise Exception.Create('bad # arg: ' + ObjV^[0].AsPChar + ' [-show]');

  WriteLn('Cells: ',FReconfCells.FReconfCells.Count);
  For I := 0 to FReconfCells.FReconfCells.Count-1 do
    Begin
      if Show then FReconfCells.FReconfCells.Data[I].Show
      else WriteLn('  ',FReconfCells.FReconfCells.Keys[I]);
    End;
End;

(*
TODO
*)
Procedure TFlowApp.CellWriteTemplate(ObjC:Integer;ObjV:PPTcl_Object);
Var VHDL      : Boolean;
    Testbench : Boolean;
    Filename  : String;
Begin
  // cell_write_template -vhdl|-verilog [-testbench] -o filename
  if (ObjC < 4) or (ObjC > 5) then
    raise Exception.Create('bad # arg: ' + ObjV^[0].AsPChar + ' -vhdl|-verilog [-testbench] -o filename');

  if      ObjV^[1].AsString = '-vhdl'    then VHDL := true
  else if ObjV^[1].AsString = '-verilog' then VHDL := false
  else
    raise Exception.Create('bad arg: ' + ObjV^[0].AsPChar + ' -vhdl|-verilog [-testbench] -o filename');

  if ObjC = 5 then
    Begin
      if ObjV^[2].AsString = '-testbench' then
        Testbench := true
      else
        raise Exception.Create('bad arg: ' + ObjV^[0].AsPChar + ' -vhdl|-verilog [-testbench] -o filename')
    End
  else
    Testbench := false;

  if ObjV^[ObjC-1-1].AsString <> '-o' then
    raise Exception.Create('bad arg: ' + ObjV^[0].AsPChar + ' -vhdl|-verilog [-testbench] -o filename');

  Filename := ObjV^[ObjC-1].AsString;

  if not assigned(FCurrentCell) then
    raise Exception.Create('You first have to create a cell');

  if not assigned(FCurrentCell.FModule) then
    FCurrentCell.GenerateNetlist;

  if VHDL and not Testbench then
    Begin
      WriteLn('Writing VHDL module to ',Filename);
      FilePutContents(Filename,FCurrentCell.FModule.GetVHDL);
    End
  else if VHDL and Testbench then
    Begin
      WriteLn('Writing VHDL testbench to ',Filename);
      FilePutContents(Filename,FCurrentCell.FTestbench.GetVHDL);
    End
  else if not Testbench then
    Begin
      WriteLn('Writing Verilog module to ',Filename);
      FilePutContents(Filename,FCurrentCell.FModule.WriteVerilogDeclaration);
    End
  else
    Begin
      WriteLn('Writing Verilog testbench to ',Filename);
      FilePutContents(Filename,FCurrentCell.FTestbench.WriteVerilogDeclaration);
    End;
End;

(*
TODO
*)
Procedure TFlowApp.CellReadNetlist(ObjC:Integer;ObjV:PPTcl_Object);
Var Modules  : TModuleList;
    Filename : String;
Begin
  AssertParams(ObjC,2,ObjV,'filename.il');
  // cell_read_netlist filename.il
  Filename := ObjV^[1].AsPChar;

  if not assigned(FCurrentCell) then
    raise Exception.Create('You first have to create a cell');

  Modules := Nil;
  ILang.Parse(Filename,Modules);
  if Modules.IndexOf(FCurrentCell.FName) < 0 then
    raise Exception.Create('Didn''t read a module named '''+FCurrentCell.FName+'''');
  FCurrentCell.FYosysNetlist := Modules[FCurrentCell.FName];
  Modules.Free;
  // check the netlist
  ObjC := FCurrentCell.CheckNetlist;  // reuse variable
  if ObjC > 0 then
    raise Exception.Create('The netlist differs in '+IntToStr(ObjC)+' elements.')
  else
    WriteLn('The cell netlist (port definitions) is OK.');
End;

(*
TODO
*)
Procedure TFlowApp.SetConntypeTrees(ObjC:Integer;ObjV:PPTcl_Object);
Var ConnTypeSt : String;
    ConnType   : TConnType;
Begin
  // set_conntype_trees conntype {-full|<trees> <cost>}
  if (ObjC < 3) or (ObjC > 4) then
    raise Exception.Create('bad # arg: ' + ObjV^[0].AsPChar + 'conntype {-full|<trees> <cost>}');

  ConnTypeSt := ObjV^[1].AsString;
  if FReconfSignals.FConnTypes.IndexOf(ConnTypeSt) < 0 then
    raise Exception.Create('Unknown connection type '''+ConnTypeSt+'''');
  ConnType := FReconfSignals.FConnTypes[ConnTypeSt];

  if ObjC = 3 then
    Begin
      // -full
      if ObjV^[2].AsPChar <> '-full' then
        raise Exception.Create('bad arg: ' + ObjV^[0].AsPChar + 'conntype {-full|<trees> <cost>}');
      ConnType.FTrees := -1;   // full tree
    End
  else
    Begin
      ConnType.FTrees := ObjV^[2].AsInteger(FTCL);
      ConnType.FCost  := StrToFloat(ObjV^[3].AsString);
    End;
End;

(*
TODO
*)
Procedure TFlowApp.WriteIntersynth(ObjC:Integer;ObjV:PPTcl_Object);
Var T    : Text;
    ObjI : Integer;
Begin
  // write_intersynth [-conntypes] [-dyn_ports] [-celltypes] [-netlist app] [-netlists] [-stdcells] -o filename
  if ObjC < 4 then
    raise Exception.Create('bad # arg: ' + ObjV^[0].AsPChar + ' [-conntypes] [-dyn_ports] [-celltypes] [-netlist app] [-netlists] [-stdcells] -o filename');

  if ObjV^[ObjC-2].AsPChar <> '-o' then
    raise Exception.Create('bad arg: ' + ObjV^[0].AsPChar + ' [-conntypes] [-dyn_ports] [-celltypes] [-netlist app] [-netlists] [-stdcells] -o filename');

  Assign(T,ObjV^[ObjC-1].AsString);
  Rewrite(T);

  try
    ObjI := 1;
    While ObjI < ObjC-2 do
      Begin
        Case ObjV^[ObjI].AsString of
          '-conntypes' : FInterSynthHandler.WriteConnTypes(T);
          '-dyn_ports' : FInterSynthHandler.WriteDynPorts(T);
          '-celltypes' : FInterSynthHandler.WriteCellTypes(T);
          '-netlist'   : Begin Inc(ObjI); FInterSynthHandler.WriteNetlist(T,ObjV^[ObjI].AsString); End;
          '-netlists'  : FInterSynthHandler.WriteNetlists(T);
          '-stdcells'  : FInterSynthHandler.WriteStdCells(T);
        else
          raise Exception.Create('bad arg '''+ObjV^[ObjI].AsString+''': ' + ObjV^[0].AsPChar + ' [-conntypes] [-dyn_ports] [-celltypes] [-netlist app] [-netlists] [-stdcells] -o filename');
        End;
        Inc(ObjI);
        if ObjI < ObjC-2 then
          WriteLn(T);
      End;
  Finally
    Close(T);
  End;
End;

(*ronn
TODO
*)
Procedure TFlowApp.ReadIntersynth(ObjC:Integer;ObjV:PPTcl_Object);

  Procedure ShowInfo;
  Var Cols  : TStringIntMap;     // map "Cell Name" -> "Column Index"
      Table : TTable;
      TextT : TTextTable;
  Begin

    Cols := Nil;
    CreateAppUsage(true,Cols,Table,TextT);

    FInterSynthHandler.ShowInfo(Cols,Table,TextT);

    Cols.Free;
    TextT.Free;
    Table.Free;
  End;

Begin
  // read_intersynth -commands|-config|-check|-showinfo filename
  if ObjC < 2 then
    raise Exception.Create('bad # arg: ' + ObjV^[0].AsPChar + ' -commands|-config|-check|-showinfo filename');
  if (ObjV^[1].AsPChar <> '-check') and (ObjV^[1].AsPChar <> '-showinfo') and (ObjC <> 3) then
    raise Exception.Create('bad # arg: ' + ObjV^[0].AsPChar + ' -commands|-config|-check|-showinfo filename');
  Case ObjV^[1].AsString of
    '-commands' : FInterSynthHandler.ReadCommands(ObjV^[2].AsString);
    '-config'   : FInterSynthHandler.ReadConfig  (ObjV^[2].AsString);
    '-check'    : FInterSynthHandler.CheckMissing;
    '-showinfo' : ShowInfo;
  else
    raise Exception.Create('bad arg: ' + ObjV^[0].AsPChar + ' -commands|-config|-check|-showinfo filename');
  End;
End;

(*ronn
TODO
*)
Procedure TFlowApp.AppWrapIntersynth(ObjC:Integer;ObjV:PPTcl_Object);
Begin
  // app_wrap_intersynth
  if ObjC <> 1 then
    raise Exception.Create('bad # arg: ' + ObjV^[0].AsPChar);

  if not assigned(FCurrentApp) then
    raise Exception.Create('You first have to create an application');

  FCurrentApp.GenerateInterSynthWrapper(FInterSynthHandler,FReconfModule.FInterSynthTypeName,FReconfModule.FInterSynthInstName);
End;

(*ronn
TODO

cell type + instance number --> InterSynth module instance name

*)
Procedure TFlowApp.GetIntersynthInstance(ObjC:Integer;ObjV:PPTcl_Object);
Begin
  // get_intersynth_instance celltype instnum
  if ObjC <> 3 then
    raise Exception.Create('bad # arg: ' + ObjV^[0].AsPChar + ' celltype instnum');

  FTCL.SetResult(FInterSynthHandler.GetCellInstance(ObjV^[1].AsString,ObjV^[2].AsInteger(FTCL)));
End;

(*ronn
TODO

original netlist instance --> cell type

*)
Procedure TFlowApp.AppGetCelltype(ObjC:Integer;ObjV:PPTcl_Object);
Begin
  // app_get_celltype instname
  if ObjC <> 2 then
    raise Exception.Create('bad # arg: ' + ObjV^[0].AsPChar + ' instname');

  if not assigned(FCurrentApp) then
    raise Exception.Create('You first have to create an application');

  FTCL.SetResult(FCurrentApp.FYosysNetlist.FInstances[ObjV^[1].AsString].FName);
End;

(*ronn
TODO

original netlist instance --> instance number in InterSynth module

*)
Procedure TFlowApp.AppGetISInstNum(ObjC:Integer;ObjV:PPTcl_Object);
Begin
  // app_get_intersynth_instance_num instname
  if ObjC <> 2 then
    raise Exception.Create('bad # arg: ' + ObjV^[0].AsPChar + ' instname');

  if not assigned(FCurrentApp) then
    raise Exception.Create('You first have to create an application');

  FTCL.SetObjResult(FInterSynthHandler.GetNodeMap(FCurrentApp.FName,FCurrentApp.FYosysNetlist.FInstances[ObjV^[1].AsString].FName));
End;

(*ronn
TODO

original netlist instance --> InterSynth module instance name

*)
Procedure TFlowApp.AppGetISInstance(ObjC:Integer;ObjV:PPTcl_Object);
Begin
  // app_get_intersynth_instance instname
  if ObjC <> 2 then
    raise Exception.Create('bad # arg: ' + ObjV^[0].AsPChar + ' instname');

  if not assigned(FCurrentApp) then
    raise Exception.Create('You first have to create an application');

  FTCL.SetResult(FCurrentApp.GetInterSynthInstance(FInterSynthHandler,ObjV^[1].AsString));
End;

(*ronn
TODO

 - searches a signal (or port) of the current application and returns its full
   name
 - if "-driver" is given, the driver is retured instead. this is either a list
   of an instance name plus a port name, or just a port name if this signal is
   an input port
 - if "-sinks" is given, a list of all sinks is returned. each entry of this
   list is either a list of an instance name plus a port name, or just a port
   name if this "sink" is an output port
 - if "-anysink" is given, zero or one (but not more) sinks are returned (same
   return format as for "-driver")
 - if "-width" is given, the width of the signal is retured as an integer

 - if the signal doesn't exist, an error is returned



set driver [app_get_signal -regex "SPIFSM.*Done.*" -driver]
#set driver [app_get_signal -regex "Enable_i" -driver]
puts [llength $driver]
if {[llength $driver] == 2} {
  set inst [lindex $driver 0]
  set port [lindex $driver 1]
  puts "$inst.$port"
  set isinst [app_get_intersynth_instance $inst]
  puts "--> $isinst.$port"
} else {
  puts $driver
}

*)
Procedure TFlowApp.AppGetSignal(ObjC:Integer;ObjV:PPTcl_Object);
Var Signal : TSignal;
    ObjI   : Integer;
    Value  : TValue;
    Sinks  : TValueList;
    List   : PTcl_Obj;
Begin
  // app_get_signal [-regex regex]|name [-driver|-sinks|-anysink|-width|-mapping]
  if (ObjC < 2) or (ObjC > 4) then
    raise Exception.Create('bad # arg: ' + ObjV^[0].AsPChar + ' [-regex regex]|name [-driver|-sinks|-anysink|-width|-mapping]');

  if not assigned(FCurrentApp) then
    raise Exception.Create('You first have to create an application');

  if (ObjC > 2) and (ObjV^[1].AsPChar = '-regex') then
    Begin
      Signal := FCurrentApp.FYosysNetlist.FindSignal(ObjV^[2].AsString);
      if not assigned(Signal) then
        raise Exception.Create('No port or signal matching '''+ObjV^[2].AsString+''' found.');
      ObjI := 3;
    End
  else
    Begin
      Signal := FCurrentApp.FYosysNetlist.GetSignal(ObjV^[1].AsString);
      if not assigned(Signal) then
        raise Exception.Create('No port or signal '''+ObjV^[1].AsString+''' in netlist.');
      ObjI := 2;
    End;
  if ObjI >= ObjC then
    Begin
      // only name was queried
      FTCL.SetResult(Signal.FName);
      Exit;
    End;

  if ObjV^[ObjI].AsPChar = '-driver' then
    Begin
      Value := FCurrentApp.FYosysNetlist.GetDriver(Signal.FName);
      if Value is TPort then
        FTCL.SetResult((Value as TPort).FName)
      else if Value is TConnection then
        Begin
          List := FTCL.NewListObj(0,Nil);
          FTCL.ListObjAppendElement(List,(Value as TConnection).FInstance.FName);
          FTCL.ListObjAppendElement(List,(Value as TConnection).FPort.FName);
          Value.Free;
          FTCL.SetObjResult(List);
        End
      else
        raise Exception.Create('Invalid return type '+Value.ClassName+' from GetDriver');
    End
  else if ObjV^[ObjI].AsPChar = '-sinks' then
    Begin
      raise Exception.Create('TODO: Not yet implemented');
    End
  else if ObjV^[ObjI].AsPChar = '-anysink' then
    Begin
      Sinks := FCurrentApp.FYosysNetlist.GetSinks(Signal.FName);
      if Sinks.Count = 0 then
        raise Exception.Create('No sinks of signal '''+Signal.FName+''' found');
      Value := Sinks[0];
      if Value is TPort then
        FTCL.SetResult((Value as TPort).FName)
      else if Value is TConnection then
        Begin
          List := FTCL.NewListObj(0,Nil);
          FTCL.ListObjAppendElement(List,(Value as TConnection).FInstance.FName);
          FTCL.ListObjAppendElement(List,(Value as TConnection).FPort.FName);
          Value.Free;
          FTCL.SetObjResult(List);
        End
      else
        raise Exception.Create('Invalid return type '+Value.ClassName+' from GetDriver');
      Sinks.Free;
    End
  else if ObjV^[ObjI].AsPChar = '-width' then
    Begin
      FTCL.SetObjResult(Signal.FType.GetWidthInt);
    End
  else if ObjV^[ObjI].AsPChar = '-mapping' then
    Begin
      With FCurrentApp.FDynamicPorts[Signal.FName] do
        if FIndex >= 0 then
          FTCL.SetResult(FReconfSignal.GetPortName+'('+IntToStr(FIndex)+')')
        else
          FTCL.SetResult(FReconfSignal.GetPortName);
    End
  else
    raise Exception.Create('Invalid argument '+ObjV^[ObjI].AsString);
End;

(*ronn
TODO
*)
Procedure TFlowApp.FinishReconfModule(ObjC:Integer;ObjV:PPTcl_Object);
Var ConfigRegisters : TAddressedConfigRegisters;
    Parameters      : TAddressedParameters;
Begin
  // finish_reconf_module
  if ObjC <> 1 then
    raise Exception.Create('bad # arg: ' + ObjV^[0].AsPChar);

  if not assigned(FReconfModule) then
    raise Exception.Create('Reconfigurable module does not yet exists');

  // check all kind of stuff
  if not assigned(FReconfModule.FParentReset) then
    raise Exception.Create('You have to set a reset');
  if not assigned(FReconfModule.FParentClock) then
    raise Exception.Create('You have to set a clock');
  if not assigned(FReconfModule.FReconfSignals) then
    raise Exception.Create('You have to add the reconfigurable signals');
  if not assigned(FConfigInterface) then
    raise Exception.Create('You need a config interface');
  if not assigned(FParamInterface) then
    raise Exception.Create('You need a param interface');

  if FInterSynthHandler.FReadBitdataSize < 0 then
    raise Exception.Create('You have to use InterSynth and read in its results, including application bitstreams');

  FInterSynthHandler.SetupConfigRegisters;
  FInterSynthHandler.SetupParameters;

  // assign config register addresses
  ConfigRegisters := TAddressedConfigRegisters.Create;
  FReconfModule.FConfigRegisters := ConfigRegisters;
  ConfigRegisters.Add(TConfigRegister(FReconfModule.FReconfSignals.FConfigRegister));
  ConfigRegisters.Add(FInterSynthHandler.FConfigRegisters);
  ConfigRegisters.AssignAddresses;
  ConfigRegisters.List;

  // assign parameter addresses
  Parameters := TAddressedParameters.Create(TParamInterface(FReconfModule.FParamInterface));
  FReconfModule.FParameters := Parameters;
  Parameters.Add(TParameters(FReconfModule.FReconfSignals.FParameters));   // reconf.signals
  Parameters.Add(FInterSynthHandler.FParameters);                          // InterSynth parameters
  Parameters.AssignAddresses;
  Parameters.List;

  FReconfModuleFinished := true;
End;

(*ronn
write_netlist(1fl) -- write code of the reconfigurable module wrapper
====================================================================

## SYNPOSYS

`write_netlist` -parent|-preliminary|-reconfmodule|-wrapapp|-wrapis|-wraprm_vhdl2008|-wraprm_lec|-chip -verilog|-vhdl -entity|-component|-architecture|-module|-instance filename

## DESCRIPTION

TODO

 * **-verilog**: create a Verilog file, this is the default

 * **-vhdl**: create a VHDL file

 * **-instance**: generate code for the instantiation of the reconfigurable module

 * **-module**: generate code for the module of the reconfigurable module

## EXAMPLE

TODO

## SEE ALSO

TODO

*)

Procedure TFlowApp.WriteNetlist(ObjC:Integer;ObjV:PPTcl_Object);
Type THDLFormat  = (hfVerilog,hfVHDL);
     TCodeSelect = (csUnset,csEntity,csComponent,csArchitecture,csModule,csInstance);
Var Filename     : String;
    ObjI         : Integer;
    NetlistType  : TNetlistType;
    HDLFormat    : THDLFormat;
    CodeSelect   : TCodeSelect;
    Module       : TModule;
    Instance     : TInstance;
    T            : Text;
    St           : String;
Begin
  // write_netlist -parent|-preliminary|-reconfmodule|-wrapapp|-wrapis|-wraprm_vhdl2008|-wraprm_lec|-chip -verilog|-vhdl -entity|-component|-architecture|-module|-instance filename
  if ObjC < 4 then
    raise Exception.Create('bad # arg: ' + ObjV^[0].AsPChar + ' -parent|-preliminary|-reconfmodule|-wrapapp|-wrapis|-wraprm_vhdl2008|-wraprm_lec|-chip -verilog|-vhdl -entity|-component|-architecture|-module|-instance filename');
  Filename     := ObjV^[ObjC-1].AsString;
  NetlistType  := ntUnset;
  HDLFormat    := hfVerilog;    // default to Verilog
  CodeSelect   := csUnset;      // must be set

  ObjI := 1;
  While ObjI < ObjC-1 do   // last is filename
    Begin
      // netlist type
      if      ObjV^[ObjI].AsString = '-parent' then
        NetlistType := ntParent
      else if ObjV^[ObjI].AsString = '-preliminary' then
        NetlistType := ntPreliminary
      else if ObjV^[ObjI].AsString = '-wrapapp' then
        Begin
          if not assigned(FCurrentApp) then
            raise Exception.Create('Can''t write a netlist wrapping an application, because no application was created.');
          if not assigned(FCurrentApp.FWrapper) then
            raise Exception.Create('You didn''t create a wrapper using create_reconf_module_netlist -wrapapp');
          NetlistType := ntWrapApp;
        End
      else if ObjV^[ObjI].AsString = '-wrapis' then
        Begin
          if not assigned(FCurrentApp) then
            raise Exception.Create('Can''t write a netlist for an application, because no application was created.');
          if not assigned(FCurrentApp.FISWrapper) then
            raise Exception.Create('You didn''t create a wrapper using app_wrap_intersynth');
          NetlistType := ntWrapIS;
        End
      else if ObjV^[ObjI].AsString = '-wraprm_vhdl2008' then
        Begin
          if not assigned(FCurrentApp) then
            raise Exception.Create('Can''t write a netlist for an application, because no application was created.');
          if not assigned(FCurrentApp.FRMWrapper[rmtVHDL2008]) then
            raise Exception.Create('You didn''t create a wrapper using app_wrap_reconf_module');
          NetlistType := ntWrapRMVHDL2008;
        End
      else if ObjV^[ObjI].AsString = '-wraprm_lec' then
        Begin
          if not assigned(FCurrentApp) then
            raise Exception.Create('Can''t write a netlist for an application, because no application was created.');
          if not assigned(FCurrentApp.FRMWrapper[rmtLEC]) then
            raise Exception.Create('You didn''t create a wrapper using app_wrap_reconf_module');
          NetlistType := ntWrapRMLEC;
        End
      else if ObjV^[ObjI].AsString = '-reconfmodule' then
        Begin
          if not FReconfModuleFinished then
            raise Exception.Create('Reconfigurable module not yet finished');
          NetlistType := ntReconfModule
        End
      else if ObjV^[ObjI].AsString = '-chip' then
        Begin
          if not assigned(FChip) then
            raise Exception.Create('Can''t write a netlist of the chip, because no chip was created.');
          NetlistType := ntChip;
        End
      // HDL format
      else if ObjV^[ObjI].AsString = '-verilog' then
        HDLFormat  := hfVerilog
      else if ObjV^[ObjI].AsString = '-vhdl' then
        HDLFormat  := hfVHDL
      else if ObjV^[ObjI].AsString = '-entity' then
        CodeSelect := csEntity
      else if ObjV^[ObjI].AsString = '-component' then
        CodeSelect := csComponent
      else if ObjV^[ObjI].AsString = '-architecture' then
        CodeSelect := csArchitecture
      else if ObjV^[ObjI].AsString = '-module' then
        CodeSelect := csModule
      else if ObjV^[ObjI].AsString = '-instance' then
        CodeSelect := csInstance
      else
        raise Exception.Create('bad arg: ' + ObjV^[0].AsPChar + ' -parent|-preliminary|-reconfmodule|-wrapapp|-wrapis|-wraprm_vhdl2008|-wraprm_lec|-chip -verilog|-vhdl -entity|-component|-architecture|-module|-instance filename');

      Inc(ObjI);
    End;

  if NetlistType = ntUnset then
    raise Exception.Create('You have to use -parent|-preliminary|-reconfmodule|-wrapapp|-wrapis|-wraprm_vhdl2008|-wraprm_lec|-chip');
  if CodeSelect = csUnset then
    raise Exception.Create('You have to use -entity|-component|-architecture|-module|-instance');
  if (HDLFormat = hfVerilog) and not (CodeSelect in [csModule,csInstance]) then
    raise Exception.Create('Verilog output can only be generated for -module or -instance');
  if (NetlistType = ntParent) and (CodeSelect = csInstance) then
    raise Exception.Create('Can''t create netlist of parent instance');

  St := 'Automatically generated: ' + GetCmdLine(ObjC,ObjV);
  Case NetlistType of
    ntParent         : Begin Module := FReconfModule.FParent;                        Instance := Nil;                                  End;
    ntPreliminary    : Begin Module := FReconfModule.FPreliminary.FModule;           Instance := FReconfModule.FPreliminary.FInstance; End;
    ntWrapApp        : Begin Module := FCurrentApp.FWrapper.FModule;                 Instance := FCurrentApp.FWrapper.FInstance;       End;
    ntWrapIS         : Begin Module := FCurrentApp.FISWrapper;                       Instance := Nil;                                  End;
    ntWrapRMVHDL2008 : Begin Module := FCurrentApp.FRMWrapper[rmtVHDL2008].FWrapper; Instance := Nil;                                  End;
    ntWrapRMLEC      : Begin Module := FCurrentApp.FRMWrapper[rmtLEC].     FWrapper; Instance := Nil;                                  End;
    ntReconfModule   : Begin Module := FReconfModule.FNetlist.FModule;               Instance := FReconfModule.FNetlist.FInstance;     End;
    ntChip           : Begin Module := FChip;                                        Instance := Nil;                                  End;
  else
    raise Exception.Create('Unknown NetlistType value');
  End;
  Case HDLFormat of
    hfVerilog : St := '// ' + St;
    hfVHDL    : St := '-- ' + St;
  else
    raise Exception.Create('Unknown HDLFormat value');
  End;

  Assign(T,Filename);
  Rewrite(T);
  WriteLn(T,St);
  WriteLn(T);
  if HDLFormat = hfVerilog then
    Begin
      Case CodeSelect of
        csModule       : WriteLn(T,Module.WriteVerilogDeclaration);
        csInstance     : WriteLn(T,Instance.WriteVerilogDeclaration);
      else
        raise Exception.Create('Invalid CodeSelect value');
      End;
    End
  else
    Begin
      Case CodeSelect of
        csEntity       : WriteLn(T,Module.GetVHDLHeader + Module.GetVHDLEntity);
        csComponent    : WriteLn(T,Module.GetVHDLComponent);
        csArchitecture : WriteLn(T,Module.GetVHDLArchitecture);
        csModule       : WriteLn(T,Module.GetVHDL);
        csInstance     : WriteLn(T,Instance.GetVHDL);
      else
        raise Exception.Create('Unknown CodeSelect value');
      End;
    End;
  Close(T);
End;

(*ronn
TODO
*)
Procedure TFlowApp.WriteNetlistAdjunct(ObjC:Integer;ObjV:PPTcl_Object);
Var St : String;
Begin
  // write_netlist_adjunct -wraprm_lec_setup|-wraprm_lec_mapping filename
  if ObjC <> 3 then
    raise Exception.Create('bad # arg: ' + ObjV^[0].AsPChar + ' -wraprm_lec_setup|-wraprm_lec_mapping filename');

  if not assigned(FCurrentApp) then
    raise Exception.Create('Can''t write a netlist adjunct for an application, because no application was created.');
  if not assigned(FCurrentApp.FRMWrapper[rmtLEC]) then
    raise Exception.Create('You didn''t create a wrapper using app_wrap_reconf_module');

  Case ObjV^[1].AsString of
    '-wraprm_lec_setup'   : St := (FCurrentApp.FRMWrapper[rmtLEC] as TReconfModuleWrapperLEC).FLECSetup;    // copies only reference of long AnsiString
    '-wraprm_lec_mapping' : St := (FCurrentApp.FRMWrapper[rmtLEC] as TReconfModuleWrapperLEC).FLECMapping;
  else
    raise Exception.Create('bad arg: ' + ObjV^[0].AsPChar + ' -wraprm_lec_setup|-wraprm_lec_mapping filename');
  End;

  FilePutContents(ObjV^[2].AsString,St);
End;

(*ronn
TODO
*)
Procedure TFlowApp.AppWrapReconfModule(ObjC:Integer;ObjV:PPTcl_Object);
Begin
  // app_wrap_reconf_module
  if ObjC <> 1 then
    raise Exception.Create('bad # arg: ' + ObjV^[0].AsPChar);

  if not assigned(FReconfModule.FNetlist) then
    raise Exception.Create('You first have to create an netlist of the reconf. module using ''create_reconf_module_netlist -intersynth''');

  if not assigned(FCurrentApp) then
    raise Exception.Create('You first have to create an application');

  FCurrentApp.GenerateReconfModuleWrapper(rmtVHDL2008,FReconfModule.FNetlist,FInterSynthHandler,FReconfModule.FReconfigInstName);
  FCurrentApp.GenerateReconfModuleWrapper(rmtLEC,     FReconfModule.FNetlist,FInterSynthHandler,FReconfModule.FReconfigInstName);
End;

(*ronn
TODO
*)
Procedure TFlowApp.WriteBitstream(ObjC:Integer;ObjV:PPTcl_Object);
Var Bitstream : TBitstream;
    I         : Integer;
    Format    : String;
    Prefix    : String;
    St        : String;
    Filename  : String;
Begin
  // write_bitstream -bitdata|-reconfsignals -format vhdl|verilog|c|text|modelsim|lec|formality [options] prefix filename
  if ObjC < 6 then
    raise Exception.Create('bad # arg: ' + ObjV^[0].AsPChar + ' -bitdata|-reconfsignals -format vhdl|verilog|c|text|modelsim|lec|formality [options] prefix filename');
  // TODO: check that FCurrentApp is set and that the bitstreams do exist
  Case ObjV^[1].AsString of
    '-bitdata'       : Bitstream := TBitstream.Create(FCurrentApp.FInterSynthBitdata);
    '-reconfsignals' : Bitstream := TBitstream.Create(FCurrentApp.GetReconfSignalsBitstream);   // bit index = char index-1, i.e. the string prints reversed to a VHDL vector
  else
    raise Exception.Create('bad arg: ' + ObjV^[0].AsPChar + ' -bitdata|-reconfsignals -format vhdl|verilog|c|text|modelsim|lec|formality [options] prefix filename');
  End;
  if ObjV^[2].AsPChar <> '-format' then
    raise Exception.Create('bad arg: ' + ObjV^[0].AsPChar + ' -bitdata|-reconfsignals -format vhdl|verilog|c|text|modelsim|lec|formality [options] prefix filename');
  Format   := ObjV^[3].AsPChar;
  Prefix   := ObjV^[ObjC-1-1].AsPChar;  // second-last argument
  Filename := ObjV^[ObjC-1-0].AsPChar;  // last argument
  // TODO: check that Prefix matches FInterSynthHandler.FConfigRegisters['bitdata'].FName and (FReconfModule.FReconfSignals.FConfigRegister as TConfigRegister).FName;
  Case Format of
    'vhdl': Begin
              St := '  constant ' + Prefix + 'Length : integer := ' + IntToStr(Bitstream.Count) + ';' + ^J +
                    '  constant ' + Prefix + 'Cfg    : std_logic_vector(' + Prefix + 'Length-1 downto 0) := "' + Bitstream.GetStringReverse + '";' + ^J;
            End;
    'verilog' : Begin
              St := 'localparam ' + Prefix + '_SIZE = ' + IntToStr(Bitstream.Count) + ';' + ^J +
                    'localparam ' + Prefix + '_VAL  = ' + IntToStr(Bitstream.Count) + '''b' + Bitstream.GetStringReverse + ';' + ^J;
            End;
    'c':    Begin
              St := 'const TConfigChainData Cfg'+Prefix+' = {' + ^J +
                    '  Length: '+ IntToStr(Bitstream.Count) +',' + ^J +
                    '  Data: {' + ^J +
                    Indent(Bitstream.GetCArray,4) +  ^J +
                    '  }' + ^J +
                    '};' + ^J;
            End;
    'text': Begin
              St := Bitstream.GetString + ^J;
            End;
    'modelsim': Begin
              St := Bitstream.GetModelSimTCLString(Prefix);
            End;
    'lec': Begin
              // collect options
              St := '';
              For I := 4 to ObjC-1-1-1 do
                St := St + ' ' + ObjV^[I].AsPChar;
              // leading ' ' is required by GetLEC4TRFSM
              St := Bitstream.GetLECString(Prefix,St);
            End;
    'formality': Begin
              St := Bitstream.GetFormalityString(Prefix);
            End;
    else
      raise Exception.Create('bad arg: ' + ObjV^[0].AsPChar + ' -bitdata|-reconfsignals -format vhdl|verilog|c|text|modelsim|lec|formality [options] filename');
  End;
  FilePutContents(Filename,St);
  Bitstream.Free;
End;

(*ronn
TODO
*)
Procedure TFlowApp.CreateChip(ObjC:Integer;ObjV:PPTcl_Object);
Var ObjI           : Integer;
    ChipModuleName : String;
    ChipArchName   : String;
    ParentInstName : String;
    ParentInst     : TInstance;
Begin
  // create_chip [-chip_name "chip"] [-arch_name "arch"] [-parent_inst_name "core_1"]

  // set defaults
  ChipModuleName := 'chip';
  ChipArchName   := 'struct';
  ParentInstName := 'core_1';

  ObjI := 1;
  While ObjI < ObjC do
    Begin
      if (ObjV^[ObjI].AsString = '-chip_name') and (ObjC > ObjI+1) then
        Begin
          Inc(ObjI);
          ChipModuleName := ObjV^[ObjI].AsString;
        End
      else if (ObjV^[ObjI].AsString = '-arch_name') and (ObjC > ObjI+1) then
        Begin
          Inc(ObjI);
          ChipArchName := ObjV^[ObjI].AsString;
        End
      else if (ObjV^[ObjI].AsString = '-parent_inst_name') and (ObjC > ObjI+1) then
        Begin
          Inc(ObjI);
          ParentInstName := ObjV^[ObjI].AsString;
        End
      else
        raise Exception.Create('bad arg: ' + ObjV^[0].AsPChar + ' [-chip_name "chip"] [-arch_name "arch"] [-parent_inst_name "core_1"]');
      Inc(ObjI);
    End;

  if not assigned(FParent) then
    raise Exception.Create('You have to read a parent module first');

  // create chip module
  FChip := TModule.Create(ChipModuleName);
  FChip.FArchitectureName := ChipArchName;
  ParentInst := TInstance.Create(ParentInstName,FParent);
  FChip.AddInstance(ParentInst);
End;

(*ronn
TODO
*)
Procedure TFlowApp.ChipReadLiberty(ObjC:Integer;ObjV:PPTcl_Object);
Begin
  // chip_read_liberty "filename"
  if ObjC <> 2 then
    raise Exception.Create('bad # arg: ' + ObjV^[0].AsPChar + ' "filename"');

  // read Liberty file
  Liberty.Parse(ObjV^[1].AsString,FPadLib);
  WriteLn('Read ',FPadLib.Data[FPadLib.Count-1].FCells.Count,' cells from Liberty file ',ObjV^[1].AsString);
End;

Type TChipPinType = (cptUnknown,cptPadCell,cptDirect,cptDirectInOut,cptDirectOD,cptConst,cptOpen);

(*ronn
TODO
*)
Procedure TFlowApp.ChipAddPin(ObjC:Integer;ObjV:PPTcl_Object);
Var ObjI        : Integer;
    ChipPinType : TChipPinType;
    PadCell     : TLibertyGroupCell;
    Pad         : TLibertyGroupPin;
    Internal    : TLibertyGroupPin;
    Inv         : Boolean;
    PortName    : String;
    Signals     : TSignalList;
    Port        : TPort;
    Signal      : TSignal;
    Signal2     : TSignal;
    I,J         : Integer;
    ParentInst  : TInstance;
    SigInSt     : String;
    SigOutSt    : String;
    SigEnableSt : String;
    InvIn       : Boolean;
    InvOut      : Boolean;
    InvEnable   : Boolean;
    PortIn      : TPort;
    PortOut     : TPort;
    PortEnable  : TPort;
    ValOut      : TValue;
    ValEnable   : TValue;
    PinIn       : TLibertyGroupPin;
    PinOut      : TLibertyGroupPin;
    PinEnable   : TLibertyGroupPin;
    SigIn       : TSignal;
    SigOut      : TSignal;
    SigEnable   : TSignal;
    PadInst     : TInstance;
    Process     : TProcess;
    Loop        : TForLoop;
    Condition   : TCondition;
    ConstValue  : Integer;
    ConstWidth  : Integer;
    Value       : TValue;
Begin
  // chip_add_pin [-padcell xxx -inv -connect EN ...|-direct|-direct_inout -in "name" -out "name" -enable "name"|-direct_od [-in "name"] -out "name"|-const "0"|-open|...] [-portname "portname"] [-regex regex]|name

  // defaults
  ChipPinType := cptUnknown;
  PadCell     := Nil;
  Inv         := false;
  PortName    := '';
  Signals     := TSignalList.Create;
  SigInSt     := '';
  SigOutSt    := '';
  SigEnableSt := '';
  InvIn       := false;
  InvOut      := false;
  InvEnable   := false;

  ObjI := 1;
  While ObjI < ObjC do
    Begin
      if (ObjV^[ObjI].AsString = '-pad_cell') and (ObjC > ObjI+1) then
        Begin
          Inc(ObjI);
          ChipPinType := cptPadCell;
          For I := 0 to FPadLib.Count-1 do
            Begin
              if FPadLib.Data[I].FCells.IndexOf(ObjV^[ObjI].AsString) >= 0 then
                Begin
                  PadCell := FPadLib.Data[I].FCells[ObjV^[ObjI].AsString];
                  Break;
                End;
            End;
          if not assigned(PadCell) then
            raise Exception.Create('Unknown pad cell '+ObjV^[ObjI].AsString);
          if not PadCell.FPadCell  then
            raise Exception.Create('Cell '+PadCell.FName+' is not a pad cell');
          Pad := PadCell.GetPad;
          if not assigned(Pad) then
            raise Exception.Create('Cell '+PadCell.FName+' doesn''t have a pad pin');
          Internal := PadCell.GetInternal;   // might be Nil
          // create TModule if not yet done
          if FPadModules.IndexOf(PadCell.FName) < 0 then
            FPadModules.Add(PadCell.FName,PadCell.CreateModule);
        End
      else if ObjV^[ObjI].AsString = '-inv' then
        Begin
          if ChipPinType <> cptPadCell then
            raise Exception.Create('-inv is only allowed with -pad_cell');
          Inv := true;
        End
      else if ObjV^[ObjI].AsString = '-direct' then
        Begin
          ChipPinType := cptDirect;
        End
      else if ObjV^[ObjI].AsString = '-direct_inout' then
        Begin
          ChipPinType := cptDirectInOut;
        End
      else if ObjV^[ObjI].AsString = '-direct_od' then
        Begin
          ChipPinType := cptDirectOD;
        End
      else if (ObjV^[ObjI].AsString = '-in') and (ChipPinType in [cptPadCell,cptDirectInOut,cptDirectOD]) and (ObjC > ObjI+1) then
        Begin
          Inc(ObjI);
          SigInSt := ObjV^[ObjI].AsString;
        End
      else if (ObjV^[ObjI].AsString = '-out') and (ChipPinType in [cptPadCell,cptDirectInOut,cptDirectOD]) and (ObjC > ObjI+1) then
        Begin
          Inc(ObjI);
          SigOutSt := ObjV^[ObjI].AsString;
        End
      else if (ObjV^[ObjI].AsString = '-enable') and (ChipPinType in [cptPadCell,cptDirectInOut]) and (ObjC > ObjI+1) then
        Begin
          Inc(ObjI);
          SigEnableSt := ObjV^[ObjI].AsString;
        End
      else if (ObjV^[ObjI].AsString = '-enable_n') and (ChipPinType in [cptPadCell,cptDirectInOut]) and (ObjC > ObjI+1) then
        Begin
          Inc(ObjI);
          SigEnableSt := ObjV^[ObjI].AsString;
          InvEnable := true;
        End
      // TODO: "-in_n", "-out_n"
      else if (ObjV^[ObjI].AsString = '-const') and (ObjC > ObjI+1) then
        Begin
          Inc(ObjI);
          ConstValue := ParseSignalValue(ObjV^[ObjI].AsString,ConstWidth);;
          ChipPinType := cptConst;
        End
      else if (ObjV^[ObjI].AsString = '-open') and (ObjC > ObjI+1) then
        Begin
          ChipPinType := cptOpen;
        End
      else if (ObjV^[ObjI].AsString = '-portname') and (ObjC > ObjI+1) then
        Begin
          Inc(ObjI);
          PortName := ObjV^[ObjI].AsString;
        End
      else if (ObjV^[ObjI].AsString = '-regex') and (ObjC > ObjI+1) then
        Begin
          Inc(ObjI);
          I := FParent.FindSignals(ObjV^[ObjI].AsString,Signals,[spPort]);
          if I = 0 then
            raise Exception.Create('No ports matching '''+ObjV^[ObjI].AsString+''' found.');
        End
      else if (ObjI+1 = ObjC) then
        Begin
          Signal := FParent.GetSignal(ObjV^[ObjI].AsString,[spPort]);
          if not assigned(Signal) then
            raise Exception.Create('No port '''+ObjV^[ObjI].AsString+''' in netlist.');
          Signals.Add(Signal.FName,Signal);
        End
      else
        raise Exception.Create('bad arg: ' + ObjV^[0].AsPChar + ' [-padcell xxx -connect EN ...|-direct|-direct_inout -in "name" -out "name" -enable "name"|-direct_od [-in "name"] -out "name"|-const "0"|-open|...] [-portname "portname"] [-regex regex]|name');
      Inc(ObjI);
    End;

  if ChipPinType = cptUnknown then
    raise Exception.Create('You have to set the chip pin type using -padcell or -direct');

  if (ChipPinType = cptPadCell) and (Signals.Count <> 0) then
    Begin
      if PadCell.FPins.Count <> 2 then
        raise Exception.Create('Set out/in/enable ports with -out/-in/-enable')
    End
  else if (ChipPinType = cptPadCell) and (Signals.Count = 0) then
    Begin
      if PortName    = '' then raise Exception.Create('You have to set the port name with -portname');
    End
  else if ChipPinType in [cptDirectInOut] then
    Begin
      if Signals.Count <> 0 then
        raise Exception.Create('Set out/in/enable ports with -out/-in/-enable');
      if SigInSt     = '' then raise Exception.Create('You have to set the in port with -in');
      if SigOutSt    = '' then raise Exception.Create('You have to set the out ports with -out');
      if SigEnableSt = '' then raise Exception.Create('You have to set the enable port with -enable or -enable_n');
      if PortName    = '' then raise Exception.Create('You have to set the port name with -portname');
    End
  else if ChipPinType in [cptDirectOD] then
    Begin
      if Signals.Count <> 0 then
        raise Exception.Create('Set out/in ports with -out/-in');
      if SigOutSt    = '' then raise Exception.Create('You have to set the out ports with -out');
      if PortName    = '' then raise Exception.Create('You have to set the port name with -portname');
    End
  else
    Begin
      if Signals.Count = 0 then
        raise Exception.Create('No ports selected');
      if (Signals.Count <> 1) and (PortName > '') then
        raise Exception.Create('-portname only works if a single signal is selected');
    End;

  if not assigned(FChip) then
    raise Exception.Create('You first have to create a chip');

  ParentInst := FChip.FindInstance(FParent.FName);

  For I := 0 to Signals.Count-1 do
    Begin
      if ChipPinType = cptPadCell then
        With Signals.Data[I] as TPort do
          Begin
            // here we surely only come for simple input or output ports
            if Pad.FDirection <> FDir then
              raise Exception.Create('Cannot use '+CPortDirectionVerilog[Pad.FDirection]+' pad cell '+PadCell.FName+' for '+CPortDirectionVerilog[FDir]+' port '+FName);
            if not assigned(Internal) then
              raise Exception.Create('Cannot use pad cell '+PadCell.FName+' for '+CPortDirectionVerilog[FDir]+' port '+FName+' due to missing internal port');
            if FDir = dirIn then
              Begin
                // Pad: input, Internal: output
                if Internal.FFunction = '' then
                  raise Exception.Create('Undefined function for internal pin '+Internal.FName+' of pad cell '+PadCell.FName);
                // Examples: "0", "1", "A", "(A)", "(A)'", "((A*B)+C)'", "((A*S1'*S0')+(B*S1'*S0)+(D*S1*S0)+(C*S1*S0'))'", "B^A", "C^B^A"
                if (Internal.FFunction = Pad.FName) or (Internal.FFunction = '('+Pad.FName+')') then
                  // non-inverted
                else if (Internal.FFunction = Pad.FName+'''') or (Internal.FFunction = '('+Pad.FName+')''') then
                  Begin
                    // inverted
                    if not Inv then
                      WriteLn('Warning: Pad cell '+PadCell.FName+' inverts the signal but you didn''t use ''-inv''');
                    Inv := not Inv;
                  End
                else
                  raise Exception.Create('Cannot interpret function of pin '+Internal.FName+' '''+Internal.FFunction+'''');
              End
            else if FDir = dirOut then
              Begin
                // Pad: output, Internal: input
                if Pad.FFunction = '' then
                  raise Exception.Create('Undefined function for internal pin '+Pad.FName+' of pad cell '+PadCell.FName);
                // Examples: "0", "1", "A", "(A)", "(A)'", "((A*B)+C)'", "((A*S1'*S0')+(B*S1'*S0)+(D*S1*S0)+(C*S1*S0'))'", "B^A", "C^B^A"
                if (Pad.FFunction = Internal.FName) or (Pad.FFunction = '('+Internal.FName+')') then
                  // non-inverted
                else if (Pad.FFunction = Internal.FName+'''') or (Pad.FFunction = '('+Internal.FName+')''') then
                  Begin
                    // inverted
                    //if not Inv then
                    //  WriteLn('Warning: Pad cell '+PadCell.FName+' inverts the signal but you didn''t use ''-inv''');
                    Inv := not Inv;
                  End
                else if (Pad.FFunction = '0') or (Pad.FFunction = '1') then
                  Begin
                    // open drain and open source
                    if Pad.FThreeState = '' then
                      raise Exception.Create('Open drain or open source pad but missing attribute ''three-state''');
                    (* Open drain with non-inverting input:
                     *   Pad: function: "0", three_state: "A" ==> A='0' -> Pad = '0', A='1' -> Pad = 'Z'
                     * Open source with inverting input:
                     *   Pad: function: "1", three_state: "A" ==> A='0' -> Pad = '1', A='1' -> Pad = 'Z'
                     *)
                    if (Pad.FFunction = '0') and (Pad.FThreeState = Internal.FName) then
                      // open drain with non-inverting input
                    else
                      raise Exception.Create('Cannot handle this open-drain or -source pad cell '+PadCell.FName+' with function '+Pad.FFunction+' and three_state '+Pad.FThreeState);
                  End
                else
                  raise Exception.Create('Cannot interpret function of pin '+Pad.FName+' '''+Pad.FFunction+'''');
              End
            else
              raise Exception.Create('Port direction '+CPortDirectionVerilog[FDir]+' of '+FName+' not supported');
            if PortName = '' then PortName := FName;
            WriteLn('Instantiating pad cell ',PadCell.FName,' for ',ParentInst.FName,' ',CPortDirectionVerilog[FDir],' ',FName,' to chip top level ',CPortDirectionVerilog[FDir],' ',PortName);
            Port := FChip.AddPort(TPort.Create(PortName,FDir,FType));
            Signal := TSignal.Create(FName+'_s',FType);
            FChip.AddSignal(Signal);
            ParentInst.ConnectPort(FName,Signal);
            if Inv then
              Begin
                Signal2 := TSignal.Create(FName+'_s_n',FType);
                if FDir = dirIn then
                  FChip.AddAssignment(Signal,TValueOperatorNot.Create(Signal2))
                else
                  FChip.AddAssignment(Signal2,TValueOperatorNot.Create(Signal));
                FChip.AddSignal(Signal2);
                Signal := Signal2;
              End;
            if FType = TypeBit then
              Begin
                PadInst := TInstance.Create('Pad'+PortName,FPadModules[PadCell.FName]);
                FChip.AddInstance(PadInst);
                PadInst.ConnectPort(Pad.FName,Port);
                PadInst.ConnectPort(Internal.FName,Signal);
              End
            else
              Begin
                // array ports
                For J := 0 to FType.GetWidthInt-1 do
                  Begin
                    PadInst := TInstance.Create('Pad'+PortName+'_'+IntToStr(J),FPadModules[PadCell.FName]);
                    FChip.AddInstance(PadInst);
                    PadInst.ConnectPort(Pad.FName,TValueIndex.Create(Port,TValueInteger.Create(J)));
                    PadInst.ConnectPort(Internal.FName,TValueIndex.Create(Signal,TValueInteger.Create(J)));
                  End;
              End;
          End
      else if ChipPinType = cptDirect then
        With Signals.Data[I] as TPort do
          Begin
            if PortName = '' then PortName := FName;
            WriteLn('Connecting ',ParentInst.FName,' ',CPortDirectionVerilog[FDir],' ',FName,' to chip top level ',CPortDirectionVerilog[FDir],' ',PortName);
            Signal := FChip.AddPort(TPort.Create(PortName,FDir,FType));
            ParentInst.ConnectPort(FName,Signal);
          End
      else if ChipPinType = cptConst then
        With Signals.Data[I] as TPort do
          Begin
            if FDir <> dirIn then
              Begin
                WriteLn('Warning: Cannot set ',ParentInst.FName,' ',CPortDirectionVerilog[FDir],' ',FName,' with constant value');
                Continue;
              End;
            if FType = TypeBit then
              Value := TValueBit.Create(Chr(Ord('0') + (ConstValue and $01)))
            else
              Value := TValueVector.Create(FType.GetWidthInt,ConstValue);
            WriteLn('Setting ',ParentInst.FName,' ',CPortDirectionVerilog[FDir],' ',FName,' with constant ',Value.GetVHDLValue);
            ParentInst.ConnectPort(FName,Value);
          End
      else if ChipPinType = cptOpen then
        With Signals.Data[I] as TPort do
          Begin
            if FDir <> dirOut then
              Begin
                WriteLn('Warning: Cannot let ',ParentInst.FName,' ',CPortDirectionVerilog[FDir],' ',FName,' open');
                Continue;
              End;
            WriteLn('Letting ',ParentInst.FName,' ',CPortDirectionVerilog[FDir],' ',FName,' open');
            ParentInst.ConnectPort(FName,Nil);
          End
      else
        raise Exception.Create('Unsupported chip pin type');
        // cptDirectInOut, cptDirectOD: no signals in Signals --> we don't come here
    End;
  if ChipPinType = cptDirectInOut then
    Begin
      PortIn     := ParentInst.FModule.GetSignal(SigInSt,    [spPort]) as TPort;
      PortOut    := ParentInst.FModule.GetSignal(SigOutSt,   [spPort]) as TPort;
      PortEnable := ParentInst.FModule.GetSignal(SigEnableSt,[spPort]) as TPort;
      if not assigned(PortIn)     then raise Exception.Create('Invalid in port '+SigInSt);
      if not assigned(PortOut)    then raise Exception.Create('Invalid out port '+SigOutSt);
      if not assigned(PortEnable) then raise Exception.Create('Invalid enable port '+SigEnableSt);
      if PortIn.    FDir <> dirIn  then raise Exception.Create('In port '+SigInSt+' must be an input');
      if PortOut.   FDir <> dirOut then raise Exception.Create('Out port '+SigOutSt+' must be an output');
      if PortEnable.FDir <> dirOut then raise Exception.Create('Enable port '+SigEnableSt+' must be an output');
      if (PortIn.FType.GetWidthInt <> PortOut.FType.GetWidthInt) or (PortIn.FType.GetWidthInt <> PortEnable.FType.GetWidthInt) then
        raise Exception.Create('In, out and enable signals must have the same width');
      // TODO: also support one common single-bit enable signal for all in/out together
      if InvIn     then raise Exception.Create('Cannot handle inverted input');
      if InvOut    then raise Exception.Create('Cannot handle inverted output');
      if InvEnable then raise Exception.Create('Cannot handle inverted enable');
      WriteLn('Creating bidirectional driver ',PortName,' for input ',SigInSt,', output ',SigOutSt,', and enable ',SigEnableSt);
      Signal := FChip.AddPort(TPort.Create(PortName,dirInOut,PortIn.FType));
      ParentInst.ConnectPort(SigInSt,    FChip.AddSignal(TSignal.Create(SigInSt,    PortIn.    FType)));
      ParentInst.ConnectPort(SigOutSt,   FChip.AddSignal(TSignal.Create(SigOutSt,   PortOut.   FType)));
      ParentInst.ConnectPort(SigEnableSt,FChip.AddSignal(TSignal.Create(SigEnableSt,PortEnable.FType)));
      // assign In
      FChip.AddAssignment(FChip.FSignals[SigInSt],TValueOperatorFunction.Create('To_X01',Signal));
      // driver
      Process := TProcess.Create('InOut_'+PortName+'_Proc');
      FChip.AddProcess(Process);
      Process.AddSensitive(PortOut,0);
      Process.AddSensitive(PortEnable,1);
      if PortIn.FType = TypeBit then
        Begin
          Condition := TCondition.Create(TValueOperatorEqual.Create(PortEnable,TValueBit.Create('1')));
          Process.AddStatement(Condition);
          Condition.AddStatementThen(TAssignment.Create(Signal,PortOut));
          Condition.AddStatementElse(TAssignment.Create(Signal,TValueBit.Create('Z')));
        End
      else
        Begin
          Loop := TForLoop.Create('I',TRangeAttrib.Create(PortOut));
          Process.AddStatement(Loop);
          Condition := TCondition.Create(TValueOperatorEqual.Create(TValueIndex.Create(PortEnable,Loop.FLoopVar),TValueBit.Create('1')));
          Loop.AddStatement(Condition);
          Condition.AddStatementThen(TAssignment.Create(TValueIndex.Create(Signal,Loop.FLoopVar),TValueIndex.Create(PortOut,Loop.FLoopVar)));
          Condition.AddStatementElse(TAssignment.Create(TValueIndex.Create(Signal,Loop.FLoopVar),TValueBit.Create('Z')));
        End;
    End
  else if (ChipPinType = cptPadCell) and (Signals.Count = 0) then
    Begin
      if Inv then
        raise Exception.Create('-inv not supported for bidirectional pad cells');
      PortIn     := Nil;
      PortOut    := Nil;
      PortEnable := Nil;
      ValOut     := Nil;
      ValEnable  := Nil;
      // TODO: since I've added optional and constant out/enable connections,
      // this is a bit brittle and only a few combinations actually work
      if SigInSt > '' then
        Begin
          PortIn := ParentInst.FModule.GetSignal(SigInSt,[spPort]) as TPort;
          if not assigned(PortIn) then raise Exception.Create('Invalid in port '+SigInSt);
          if PortIn.FDir <> dirIn  then raise Exception.Create('In port '+SigInSt+' must be an input');
        End;
      PortOut := ParentInst.FModule.GetSignal(SigOutSt,[spPort]) as TPort;
      if assigned(PortOut) then
        Begin
          if PortOut.FDir <> dirOut then raise Exception.Create('Out port '+SigOutSt+' must be an output');
        End
      else
        Begin
          I := ParseSignalValue(SigOutSt,J);   // will raise an exception if the format is wrong
          if J <= 0 then
            raise Exception.Create('You have to specify the width of the value for -out');
          if J = 1 then  ValOut := TValueBit.Create(Chr(Ord('0')+I))
          else           ValOut := TValueVector.Create(J,I);
        End;
      PortEnable := ParentInst.FModule.GetSignal(SigEnableSt,[spPort]) as TPort;
      if assigned(PortEnable) then
        Begin
          if PortEnable.FDir <> dirOut then raise Exception.Create('Enable port '+SigEnableSt+' must be an output');
        End
      else
        Begin
          I := ParseSignalValue(SigEnableSt,J);   // will raise an exception if the format is wrong
          if J <= 0 then
            raise Exception.Create('You have to specify the width of the value for -enable');
          if J = 1 then ValEnable := TValueBit.Create(Chr(Ord('0')+I))
          else          ValEnable := TValueVector.Create(J,I);
        End;
      // TODO: check that {Port,Val}{In,Out,Enable} have the same width
      // TODO: also support one common single-bit enable signal for all in/out together
      PinIn     := Nil;
      PinOut    := Nil;
      PinEnable := Nil;
      // go through all pins and assign them to PinIn/Out/Enable
      For I := 0 to PadCell.FPins.Count-1 do
        With PadCell.FPins.Data[I] do
          Begin
            if FIsPad then
              Begin
                // have pad pin
                if FName <> Pad.FName then
                  raise Exception.Create('Error: Found another pad pin '+FName+' of pad cell '+PadCell.FName);
                if FDirection <> dirInOut then
                  raise Exception.Create('Pad cell '+PadCell.FName+' pad '+FName+' is an '+CPortDirectionVerilog[FDirection]+' but must be an inout');
                if FThreeState = '' then
                  raise Exception.Create('Bidirectional pad cell '+PadCell.FName+' pad '+FName+' doesn''t have a ''three_state'' attribute');
              End
            else if FDirection = dirOut then
              Begin
                // got output to internal signals
                if FFunction = '' then
                  raise Exception('Bidirectional pad cell '+PadCell.FName+' internal output '+FName+' has no ''function'' attribute');
                if (FFunction = Pad.FName) or (FFunction = '('+Pad.FName+')') then
                  // non-inverted
                else if (FFunction = Pad.FName+'''') or (FFunction = '('+Pad.FName+')''') then
                  Begin
                    // inverted
                    InvOut := not InvOut;
                  End
                else
                  raise Exception.Create('Cannot interpret function of pin '+Pad.FName+' '''+Pad.FFunction+'''');
                // the internal output is connected to the PortIn of the parent module
                if assigned(PinIn) then
                  raise Exception('Error: Bidirectional pad cell '+PadCell.FName+' has another output '+FName+' (first output: '+PinIn.FName+')');
                PinIn := PadCell.FPins.Data[I];
              End
            else if (FDirection = dirIn) and (Pos(FName,Pad.FThreeState) > 0) then
              Begin
                // got enable signal
                (* if the "PAD" pin has an attribute "three_state : "EN";",
                 * then the "PAD" pin is tristated (i.e. turned off, used
                 * as input) when "EN" is '1'. When "EN" = '0', the "PAD"
                 * is an output. *)
                if (Pad.FThreeState = FName) or (Pad.FThreeState = '('+FName+')') then
                  // non-inverted three_state --> this means inverted enable
                  InvEnable := not InvEnable
                else if (Pad.FThreeState = FName+'''') or (Pad.FThreeState = '('+FName+')''') then
                  // inverted three_state --> this means non-inverted enable
                else
                  raise Exception.Create('Cannot interpret three_state of pin '+Pad.FName+' '''+Pad.FThreeState+'''');
                if assigned(PinEnable) then
                  raise Exception('Error: Bidirectional pad cell '+PadCell.FName+' has another enable input '+FName+' (first input: '+PinEnable.FName+')');
                PinEnable := PadCell.FPins.Data[I];
              End
            else if FDirection = dirIn then
              Begin
                // got input from internal signals
                if (Pad.FFunction = FName) or (Pad.FFunction = '('+FName+')') then
                  // non-inverted
                else if (Pad.FFunction = FName+'''') or (Pad.FFunction = '('+FName+')''') then
                  Begin
                    // inverted
                    InvIn := not InvIn;
                  End
                else
                  raise Exception.Create('Cannot interpret function of pin '+Pad.FName+' '''+Pad.FFunction+'''');
                // the internal input is connected to the PortOut of the parent module
                if assigned(PinOut) then
                  raise Exception('Error: Bidirectional pad cell '+PadCell.FName+' has another input '+FName+' (first input: '+PinOut.FName+')');
                PinOut := PadCell.FPins.Data[I];
              End
            else
              raise Exception.Create('Strange pin '+FName+' of bidirectional pad cell '+PadCell.FName);
          End;
      // ok, now we should know all connections
      if not assigned(PinIn)     then raise Exception.Create('Bidirectional pad cell '+PadCell.FName+' doesn''t have internal signal input');
      if not assigned(PinOut)    then raise Exception.Create('Bidirectional pad cell '+PadCell.FName+' doesn''t have internal signal output');
      if not assigned(PinEnable) then raise Exception.Create('Bidirectional pad cell '+PadCell.FName+' doesn''t have internal enable');
      WriteLn('Instantiating pad cell ',PadCell.FName,' for pad ',PortName,' for input ',SigInSt,', output ',SigOutSt,', and enable ',SigEnableSt);
      Port := FChip.AddPort(TPort.Create(PortName,dirInOut,PortIn.FType));
      if assigned(PortIn) then
        Begin
          SigIn     := FChip.AddSignal(TSignal.Create(SigInSt+    '_s',PortIn.    FType));
          ParentInst.ConnectPort(SigInSt,    SigIn);
        End;
      if assigned(PortOut) then
        Begin
          SigOut    := FChip.AddSignal(TSignal.Create(SigOutSt+   '_s',PortOut.   FType));
          ParentInst.ConnectPort(SigOutSt,   SigOut);
        End;
      if assigned(PortEnable) then
        Begin
          SigEnable := FChip.AddSignal(TSignal.Create(SigEnableSt+'_s',PortEnable.FType));
          ParentInst.ConnectPort(SigEnableSt,SigEnable);
        End;
      if InvIn  then raise Exception.Create('Cannot handle inverted internal input');
      if InvOut then raise Exception.Create('Cannot handle inverted internal output');
      if InvEnable then
        Begin
          if assigned(PortEnable) then
            Begin
              Signal2 := FChip.AddSignal(TSignal.Create(SigEnableSt+'_s_n',PortEnable.FType));
              FChip.AddAssignment(Signal2,TValueOperatorNot.Create(SigEnable));
              SigEnable := Signal2;
            End
          else
            Begin
              raise Exception.Create('TODO: Cannot handle inverted constant enable value');
            End;
        End;

      // instantiate cell
      if PortIn.FType = TypeBit then
        Begin
          PadInst := TInstance.Create('Pad'+PortName,FPadModules[PadCell.FName]);
          FChip.AddInstance(PadInst);
          PadInst.ConnectPort(Pad.FName,Port);
          if assigned(PortIn)     then PadInst.ConnectPort(PinIn.    FName,SigIn);
          if assigned(PortOut)    then PadInst.ConnectPort(PinOut.   FName,SigOut)
          else                         PadInst.ConnectPort(PinOut.   FName,ValOut);
          if assigned(PortEnable) then PadInst.ConnectPort(PinEnable.FName,SigEnable)
          else                         PadInst.ConnectPort(PinEnable.FName,ValEnable);
        End
      else
        Begin
          // array ports
          For J := 0 to PortIn.FType.GetWidthInt-1 do
            Begin
              PadInst := TInstance.Create('Pad'+PortName+'_'+IntToStr(J),FPadModules[PadCell.FName]);
              FChip.AddInstance(PadInst);
              PadInst.ConnectPort(Pad.FName,TValueIndex.Create(Port,TValueInteger.Create(J)));
              PadInst.ConnectPort(PinIn.    FName,TValueIndex.Create(SigIn,TValueInteger.Create(J)));
              PadInst.ConnectPort(PinOut.   FName,TValueIndex.Create(SigOut,TValueInteger.Create(J)));
              PadInst.ConnectPort(PinEnable.FName,TValueIndex.Create(SigEnable,TValueInteger.Create(J)));
            End;
        End;
    End
  else if ChipPinType = cptDirectOD then
    Begin
      PortOut := ParentInst.FModule.GetSignal(SigOutSt,   [spPort]) as TPort;
      if not assigned(PortOut) then
        raise Exception.Create('Invalid out port '+SigOutSt);
      if PortOut.FDir <> dirOut then raise Exception.Create('Out port '+SigOutSt+' must be an output');
      if SigInSt > '' then
        Begin
          PortIn := ParentInst.FModule.GetSignal(SigInSt,    [spPort]) as TPort;
          if not assigned(PortIn) then
            raise Exception.Create('Invalid in port '+SigInSt);
          if PortIn.FDir <> dirIn  then raise Exception.Create('In port '+SigInSt+' must be an input');
          if PortIn.FType.GetWidthInt <> PortOut.FType.GetWidthInt then
            raise Exception.Create('In and out signals must have the same width');
        End;
      Write('Creating open-drain driver ',PortName,' for output ',SigOutSt);
      if SigInSt > '' then
        WriteLn(' and input ',SigInSt)
      else
        WriteLn;
      if SigInSt > '' then
        Begin
          Signal := FChip.AddPort(TPort.Create(PortName,dirInOut,PortOut.FType));
          ParentInst.ConnectPort(SigInSt,    FChip.AddSignal(TSignal.Create(SigInSt,    PortIn.    FType)));
          // assign In
          FChip.AddAssignment(FChip.FSignals[SigInSt],TValueOperatorFunction.Create('To_X01',Signal));
        End
      else
        Signal := FChip.AddPort(TPort.Create(PortName,dirOut,PortOut.FType));
      ParentInst.ConnectPort(SigOutSt,   FChip.AddSignal(TSignal.Create(SigOutSt,   PortOut.   FType)));
      // driver
      Process := TProcess.Create('OD_'+PortName+'_Proc');
      FChip.AddProcess(Process);
      Process.AddSensitive(PortOut,0);
      if PortOut.FType = TypeBit then
        Begin
          Condition := TCondition.Create(TValueOperatorEqual.Create(PortOut,TValueBit.Create('1')));
          Process.AddStatement(Condition);
          Condition.AddStatementThen(TAssignment.Create(Signal,TValueBit.Create('Z')));
          Condition.AddStatementElse(TAssignment.Create(Signal,TValueBit.Create('0')));
        End
      else
        Begin
          Loop := TForLoop.Create('I',TRangeAttrib.Create(PortOut));
          Process.AddStatement(Loop);
          Condition := TCondition.Create(TValueOperatorEqual.Create(TValueIndex.Create(PortOut,Loop.FLoopVar),TValueBit.Create('1')));
          Loop.AddStatement(Condition);
          Condition.AddStatementThen(TAssignment.Create(TValueIndex.Create(Signal,Loop.FLoopVar),TValueBit.Create('Z')));
          Condition.AddStatementElse(TAssignment.Create(TValueIndex.Create(Signal,Loop.FLoopVar),TValueBit.Create('0')));
        End;
    End;
End;

Type TWhenList   = specialize TFPGMap<String,TValueWhen>;
     TArrayItems = specialize TFPGMap<Integer,TSignal>;      // array port index --> signal
     TOutArray   = specialize TFPGMap<String,TArrayItems>;   // reconf.signal name --> array items
     TOutConcat  = specialize TFPGMap<String,TValueConcat>;  // reconf.signal name --> TValueConcat

(*ronn
*)
Procedure TFlowApp.GenerateMuxedModule(ObjC:Integer;ObjV:PPTcl_Object);
Var MM           : TModule;  // MuxedModule
    SelWidth     : Integer;
    Select       : TPort;
    AppIdx       : Integer;
    I,J,K        : Integer;
    App          : TReconfApp;
    AppInst      : TInstance;
    ValueWhen    : TValueWhen;
    Value        : TValue;
    OutWhen      : TWhenList;
    NumParamsIn  : TStringIntMap;
    NumParamsOut : TStringIntMap;

  Procedure AddReconfInput(PType:TType;RName:String;RDir:TPortDirection;RType:TType;Value:TValue);
  Begin
    if not MM.FPorts.Find(RName,J) then
      Begin
        // this reconf.sig wasn't used before
        MM.AddPort(TPort.Create(RName,RDir,RType));
      End;
    ValueWhen := TValueWhen.Create;
    MM.AddAssignment(Value,ValueWhen);
    ValueWhen.AddValue(MM.FPorts[RName],TValueOperatorEqual.Create(Select,TValueVector.Create(SelWidth,AppIdx)));
    if PType = TypeBit then
      Value := TValueBit.Create('0')
    else
      Value := TValueVector.Create(PType.GetWidthInt,StringOfChar('0',PType.GetWidthInt));
    ValueWhen.AddElse(Value);
  End;

  Procedure AddReconfOutput(RName:String;RDir:TPortDirection;RType:TType;Value:TValue);
  Begin
    if not MM.FPorts.Find(RName,J) then
      Begin
        // this reconf.sig wasn't used before
        MM.AddPort(TPort.Create(RName,RDir,RType));
        ValueWhen := TValueWhen.Create;
        OutWhen.Add(RName,ValueWhen);
        MM.AddAssignment(MM.FPorts[RName],ValueWhen);
      End;
    OutWhen[RName].AddValue(Value,TValueOperatorEqual.Create(Select,TValueVector.Create(SelWidth,AppIdx)));
  End;

  Procedure DoConnect(PName:String;PType:TType;RName:String;RDir:TPortDirection;RType:TType);
  Var Signal : TSignal;
  Begin
    Signal := MM.AddSignal(TSignal.Create(App.FName+'_'+PName,PType));
    AppInst.ConnectPort(PName,Signal);
    if RDir = dirIn then
      Begin
        AddReconfInput(PType,RName,RDir,RType,Signal);
      End
    else
      Begin
        AddReconfOutput(RName,RDir,RType,Signal);
      End;
  End;

Var ModuleName : String;
    FileName   : String;
    ObjI       : Integer;
    Signal     : TSignal;
    OutArray   : TOutArray;
    OutConcat  : TOutConcat;
Begin
  // generate_muxed_module [-module MuxedModule] -filename muxedmodule.vhd
  // defaults
  ModuleName := 'MuxedModule';
  Filename   := '';
  // parse parameters
  ObjI := 1;
  While ObjI < ObjC do
    Begin
      if (ObjV^[ObjI].AsString = '-module') and (ObjC > ObjI+1) then
        Begin
          Inc(ObjI);
          ModuleName := ObjV^[ObjI].AsString
        End
      else if (ObjV^[ObjI].AsString = '-filename') and (ObjC > ObjI+1) then
        Begin
          Inc(ObjI);
          Filename := ObjV^[ObjI].AsString
        End
      else
        raise Exception.Create('bad arg: ' + ObjV^[0].AsPChar + ' [-padcell xxx -connect EN ...|-direct|-direct_inout -in "name" -out "name" -enable "name"|-direct_od [-in "name"] -out "name"|-const "0"|-open|...] [-portname "portname"] [-regex regex]|name');
      Inc(ObjI);
    End;

  if FileName = '' then
    raise Exception.Create('You have to set the filename using -filename');

  // create module
  MM := TModule.Create(ModuleName);
  // Select_i input
  SelWidth := RoundUpLd(FReconfApps.FReconfApps.Count);
  Select := TPort.Create('Select_i',dirIn,TType.Create('std_logic_vector',dirDown,SelWidth-1,0));
  MM.AddPort(Select);
  // prepare for output MUXes
  OutWhen := TWhenList.Create;
  // prepare parameter collections
  NumParamsIn  := TStringIntMap.Create;
  NumParamsOut := TStringIntMap.Create;

  // all (Ex.)Apps
  For AppIdx := 0 to FReconfApps.FReconfApps.Count-1 do
    Begin
      App := FReconfApps.FReconfApps.Data[AppIdx];    // don't use "With" to avoid name conflicts
      // Instance
      if not assigned(App.FModule) then
        App.GenerateNetlist;
      AppInst := TInstance.Create(App.FName+'_1',App.FModule);
      MM.AddInstance(AppInst);
      // Signals
      For I := 0 to FReconfSignals.FConnTypes.Count-1 do
        Begin
          NumParamsIn [FReconfSignals.FConnTypes.Keys[I]] := 0;
          NumParamsOut[FReconfSignals.FConnTypes.Keys[I]] := 0;
        End;
      OutArray  := TOutArray.Create;
      OutConcat := TOutConcat.Create;
      For I := 0 to App.FDirectPorts.Count-1 do
        With App.FDirectPorts.Data[I] do
          Begin
            DoConnect(FName,FReconfSignal.GetSignal.FType,FReconfSignal.FName,FReconfSignal.GetDirection,FReconfSignal.GetSignal.FType);
          End;
      For I := 0 to App.FDynamicPorts.Count-1 do
        With App.FDynamicPorts.Data[I] do
          Begin
            With (FReconfSignal.FSigConn as TSigConnDyn).FConnTypeOptions do
              if FArray then
                Begin
                  // array port
                  if (FReconfSignal.FSigConn as TSigConnDyn).FConnType.FWidth <> 1 then
                    raise Exception.Create('TODO: Implement array port with vectors');
                  Signal := MM.AddSignal(TSignal.Create(App.FName+'_'+FName,(FReconfSignal.FSigConn as TSigConnDyn).FConnType.FType));
                  AppInst.ConnectPort(FName,Signal);
                  if FReconfSignal.GetDirection = dirIn then
                    Begin
                      if not MM.FPorts.Find(FReconfSignal.FName,J) then
                        Begin
                          // this reconf.sig wasn't used before
                          MM.AddPort(TPort.Create(FReconfSignal.FName,FReconfSignal.GetDirection,FReconfSignal.GetSignal.FType));
                        End;
                      ValueWhen := TValueWhen.Create;
                      MM.AddAssignment(Signal,ValueWhen);
                      ValueWhen.AddValue(TValueIndex.Create(MM.FPorts[FReconfSignal.FName],TValueInteger.Create(FIndex)),TValueOperatorEqual.Create(Select,TValueVector.Create(SelWidth,AppIdx)));
                      if (FReconfSignal.FSigConn as TSigConnDyn).FConnType.FType = TypeBit then
                        Value := TValueBit.Create('0')
                      else
                        Value := TValueVector.Create((FReconfSignal.FSigConn as TSigConnDyn).FConnType.FType.GetWidthInt,StringOfChar('0',(FReconfSignal.FSigConn as TSigConnDyn).FConnType.FType.GetWidthInt));
                      ValueWhen.AddElse(Value);
                    End
                  else
                    Begin
                      if not OutArray.Find(FReconfSignal.FName,J) then
                        Begin
                          OutArray. Add(FReconfSignal.FName,TArrayItems.Create);
                          OutConcat.Add(FReconfSignal.FName,TValueConcat.Create);
                          AddReconfOutput(FReconfSignal.FName,FReconfSignal.GetDirection,FReconfSignal.GetSignal.FType,OutConcat[FReconfSignal.FName]);
                        End;
                      OutArray[FReconfSignal.FName].Add(FIndex,Signal);
                    End;
                End
              else
               if (FPadLeftWidth >= 0) or (FPadRightWidth >= 0) then
                 Begin
                   // padded signals
                   Signal := MM.AddSignal(TSignal.Create(App.FName+'_'+FName,(FReconfSignal.FSigConn as TSigConnDyn).FConnType.FType));
                   AppInst.ConnectPort(FName,Signal);
                   if FReconfSignal.GetDirection = dirIn then
                     Begin
                       if not MM.FPorts.Find(FReconfSignal.FName,J) then
                         Begin
                           // this reconf.sig wasn't used before
                           MM.AddPort(TPort.Create(FReconfSignal.FName,FReconfSignal.GetDirection,FReconfSignal.GetSignal.FType));
                         End;
                       ValueWhen := TValueWhen.Create;
                       MM.AddAssignment(Signal,ValueWhen);
                       Value := TValueConcat.Create;
                       if FPadRightWidth >= 0 then
                         (Value as TValueConcat).Add(TValueVector.Create(FPadRightWidth,0));
                       (Value as TValueConcat).Add(MM.FPorts[FReconfSignal.FName]);
                       if FPadLeftWidth >= 0 then
                         (Value as TValueConcat).Add(TValueVector.Create(FPadLeftWidth,0));
                       ValueWhen.AddValue(Value,TValueOperatorEqual.Create(Select,TValueVector.Create(SelWidth,AppIdx)));
                       if (FReconfSignal.FSigConn as TSigConnDyn).FConnType.FType = TypeBit then
                         Value := TValueBit.Create('0')
                       else
                         Value := TValueVector.Create((FReconfSignal.FSigConn as TSigConnDyn).FConnType.FType.GetWidthInt,StringOfChar('0',(FReconfSignal.FSigConn as TSigConnDyn).FConnType.FType.GetWidthInt));
                       ValueWhen.AddElse(Value);
                     End
                   else
                     Begin
                       // the (Ex.)App. port is already padded, therefore we need a subset of its output
                       J := 0;
                       K := (FReconfSignal.FSigConn as TSigConnDyn).FConnType.FWidth-1;
                       if FPadRightWidth >= 0 then
                         J := FPadRightWidth;
                       if FPadLeftWidth >= 0 then
                         K := K - FPadLeftWidth;
                       Value := TValueIndex.Create(Signal,TRange.Create(dirDown,K,J));
                       AddReconfOutput(FReconfSignal.FName,FReconfSignal.GetDirection,FReconfSignal.GetSignal.FType,Value);
                     End;
                 End
              else
                DoConnect(FName,(FReconfSignal.FSigConn as TSigConnDyn).FConnType.FType,FReconfSignal.FName,FReconfSignal.GetDirection,FReconfSignal.GetSignal.FType);
          End;
      For I := 0 to App.FParamPorts.Count-1 do
        With App.FParamPorts.Data[I] do
          Begin
            if FDirection = dirIn then
              Begin
                DoConnect(FName,FConnType.FType,'Param'+FConnType.FName+'In'+IntToStr(NumParamsIn[FConnType.FName])+'_i',dirIn,FConnType.FType);
                NumParamsIn[FConnType.FName] := NumParamsIn[FConnType.FName] + 1;
              End
            else
              Begin
                DoConnect(FName,FConnType.FType,'Param'+FConnType.FName+'Out'+IntToStr(NumParamsOut[FConnType.FName])+'_o',dirIn,FConnType.FType);
                NumParamsOut[FConnType.FName] := NumParamsIn[FConnType.FName] + 1;
              End;
          End;
      For I := 0 to App.FConstantValues.Count-1 do
        With App.FConstantValues.Data[I] do
          Begin
            // only signals with connection/usage "dynamic" are set as assignment inside the (Ex.)App,
            // "config" and "param" are set outside of the application
            if FReconfSignal.FSigConn is TSigConnDyn then
              Begin
                DoConnect(FReconfSignal.GetPortName,(FReconfSignal.FSigConn as TSigConnDyn).FConnType.FType,FReconfSignal.FName,FReconfSignal.GetDirection,FReconfSignal.GetSignal.FType);
              End
            else
              Begin
                AddReconfOutput(FReconfSignal.FName,FReconfSignal.GetDirection,FReconfSignal.GetSignal.FType,GetValue);
              End;
          End;
      For I := 0 to OutArray.Count-1 do
        Begin
          For J := 0 to FReconfSignals[OutArray.Keys[I]].GetSignal.FType.GetWidthInt-1 do
            if OutArray.Data[I].Find(J,K) then
              OutConcat.Data[I].Add(OutArray.Data[I].Data[K])   // add as MSB
            else
              OutConcat.Data[I].Add(TValueBit.Create('0'));    // TODO: implement vector arrays
          OutArray.Data[I].Free;
        End;
      OutArray.Free;
      OutConcat.Free;
    End;
  // default values for outputs
  For I := 0 to OutWhen.Count-1 do
    Begin
      if FReconfSignals[OutWhen.Keys[I]].GetSignal.FType = TypeBit then
        Value := TValueBit.Create('0')
      else
        Value := TValueVector.Create(FReconfSignals[OutWhen.Keys[I]].GetSignal.FType.GetWidthInt,StringOfChar('0',FReconfSignals[OutWhen.Keys[I]].GetSignal.FType.GetWidthInt));
      OutWhen.Data[I].AddElse(Value);
    End;
  // write module
  WriteLn('Writing MUXed module ',ModuleName,' to ',Filename);
  FilePutContents(Filename,MM.GetVHDL);
  // done, clean up
  OutWhen.Free;
  NumParamsIn.Free;
  NumParamsOut.Free;
  MM.Free;
End;

(*****************************************************************************)
(***  Internal Worker Functions **********************************************)
(*****************************************************************************)

Procedure TFlowApp.CreatePeriphIntfOpenMSP430(ObjC:Integer;ObjV:PPTcl_Object);
Var I            : Integer;
    PerAddr      : String;
    PerDIn       : String;
    PerWr        : String;
    PerEn        : String;
Begin
  // defaults
  PerAddr  := ''; // unset
  PerDIn   := ''; // unset
  PerWr    := ''; // unset
  PerEn    := ''; // unset
  // parse parameters
  // create_peripheral_interface -type openmsp430 -peraddr sig -perdin sig -perwr sig -peren sig
  I := 3;  // first parameter after "-type openmsp430"
  While I < ObjC do
    Begin
      Case ObjV^[I].AsString of
        '-peraddr'  : Begin PerAddr  := ObjV^[I+1].AsString; Inc(I); End;
        '-perdin'   : Begin PerDIn   := ObjV^[I+1].AsString; Inc(I); End;
        '-perwr'    : Begin PerWr    := ObjV^[I+1].AsString; Inc(I); End;
        '-peren'    : Begin PerEn    := ObjV^[I+1].AsString; Inc(I); End;
      else
        raise Exception.Create('Invalid parameter '+ObjV^[I].AsString);
      End;
      Inc(I);
    End;

  // check that all parameters were set
  if PerAddr  = '' then raise Exception.Create('You have to set the PerAddr_i signal name');
  if PerDIn   = '' then raise Exception.Create('You have to set the PerDin_i signal name');
  if PerWr    = '' then raise Exception.Create('You have to set the PerWr_i signal name');
  if PerEn    = '' then raise Exception.Create('You have to set the PerEn_i signal name');

  FPeriphIntf := TPeriphIntfOpenMSP430.Create(
    FReconfModule.CheckParentSignal(PerAddr,'std_logic_vector',dirDown,13,0),
    FReconfModule.CheckParentSignal(PerDIn, 'std_logic_vector',dirDown,15,0),
    FReconfModule.CheckParentSignal(PerWr,  'std_logic_vector',dirDown, 1,0),
    FReconfModule.CheckParentSignal(PerEn,  TypeBit.FName,     dirDown,-1,-1)
  );
  FReconfModule.FPeriphIntf := FPeriphIntf;
End;

Procedure TFlowApp.CreatePeriphInstOpenMSP430(AName:String;ObjC:Integer;ObjV:PPTcl_Object);
Var I            : Integer;
    BaseAddr     : Integer;
    PerDOut      : String;
Begin
  // defaults
  BaseAddr := 0;  // unset
  PerDOut  := ''; // unset
  // parse parameters
  // create_peripheral_instance "CfgIntf" -baseaddr 0x0180 -perdout "CfgIntf_DOut_s"
  I := 2;  // first parameter after name
  While I < ObjC do
    Begin
      Case ObjV^[I].AsString of
        '-baseaddr' : Begin BaseAddr := ObjV^[I+1].AsInteger(FTCL); Inc(I); End;
        '-perdout'  : Begin PerDOut  := ObjV^[I+1].AsString; Inc(I); End;
      else
        raise Exception.Create('Invalid parameter '+ObjV^[I].AsString);
      End;
      Inc(I);
    End;

  // check that all parameters were set
  if BaseAddr = 0  then raise Exception.Create('You have to set the base address');
  if PerDOut  = '' then raise Exception.Create('You have to set the PerDOut_o signal name');

  FPeriphIntf.AddInstance(TPeriphInstOpenMSP430.Create(AName,FPeriphIntf,BaseAddr,
    FReconfModule.CheckParentSignal(PerDOut,'std_logic_vector',dirDown,15,0)
  ));
End;

Procedure TFlowApp.CreateConfigInterfaceOpenMSP430(APeriphInst:TPeriphInstOpenMSP430;ObjC:Integer;ObjV:PPTcl_Object);
Var I        : Integer;
    InstName : String;
Begin
  // defaults
  InstName := 'CfgIntf_0';
  // parse parameters
  // create_config_interface periph_inst [-instname name]
  I := 2;  // first parameter after "periph_inst"
  While I < ObjC do
    Begin
      Case ObjV^[I].AsString of
        '-instname' : Begin InstName := ObjV^[I+1].AsString; Inc(I); End;
      else
        raise Exception.Create('Invalid parameter '+ObjV^[I].AsString);
      End;
      Inc(I);
    End;

  FConfigInterface := TConfigInterfaceOpenMSP430.Create(APeriphInst,InstName);
  FReconfModule.FConfigInterface := FConfigInterface;
End;

Procedure TFlowApp.CreateParamInterfaceOpenMSP430(APeriphInst:TPeriphInstOpenMSP430;ObjC:Integer;ObjV:PPTcl_Object);
Var I        : Integer;
    InstName : String;
Begin
  // defaults
  InstName := 'ParamIntf_0';
  // parse parameters
  // create_param_interface periph_inst [-instname name]
  I := 2;  // first parameter after "periph_inst"
  While I < ObjC do
    Begin
      Case ObjV^[I].AsString of
        '-instname' : Begin InstName := ObjV^[I+1].AsString; Inc(I); End;
      else
        raise Exception.Create('Invalid parameter '+ObjV^[I].AsString);
      End;
      Inc(I);
    End;

  FParamInterface := TParamInterfaceOpenMSP430.Create(APeriphInst,InstName);
  FReconfModule.FParamInterface := FParamInterface;
End;


(*****************************************************************************)
(***  Main Program  **********************************************************)
(*****************************************************************************)

Var App         : TFlowApp;
    RunScripts  : Boolean;
    BatchMode   : Boolean;
    ExitStatus  : Integer;

Procedure Usage(ExitCode:Byte);
Begin
  WriteLn('flowproc');
  WriteLn;
  WriteLn('Usage: flowproc [-h|--help] [-b] [-f filename] [-c string] [-n]');
  WriteLn;
  WriteLn('  -h, --help   Print a usage information and exit.');
  WriteLn;
  WriteLn('  -b           Batch mode. flowproc will not wait for user input but quit');
  WriteLn('               directly after the startup scripts, the scripts given with -f');
  WriteLn('               parameters and the commands given with -c parameters were');
  WriteLn('               executed.');
  WriteLn;
  WriteLn('  -f filename  Execute the Tcl script in filename at program start.');
  WriteLn;
  WriteLn('  -c string    Execute the commands given in string.');
  WriteLn;
  WriteLn('  -n           Do not execute the start scripts (/etc/flowproc/... TODO )');
  WriteLn;
  Halt(ExitCode);
End;

(**
 * Local helper to execute TCL commands
 *
 * The TCL commands are evaluated by Tcl. Then the result is checked and
 * printed to the screen. If 'exit' was used, an exception is raised. This is
 * caught in the main program.
 *)
Procedure Eval(St:String);
Var Code : Integer;
Begin
  // execute command
  Code := App.FTCL.Eval(St);

  // handle result
  if (Code <> TCL_OK) then
    Write('Error: ');
  St := App.FTCL.GetStringResult;   // use 'St' to avoid additional variable
  if St > '' then
    WriteLn(St);

  // handle if 'exit' was called
  ExitStatus := App.FCmdLine.ExitStatus;
  if (ExitStatus >= 0) or (Code <> TCL_OK) then
    raise Exception.Create(St);
End;

Procedure EvalFile(St:String);
Var Code : Integer;
Begin
  // execute script file
  Code := App.FTCL.EvalFile(St);

  // handle result
  if (Code <> TCL_OK) then
    Write('Error: ');
  St := App.FTCL.GetStringResult;   // use 'St' to avoid additional variable
  if St > '' then
    WriteLn(St);

  // handle if 'exit' was called
  ExitStatus := App.FCmdLine.ExitStatus;
  if (ExitStatus >= 0) or (Code <> TCL_OK) then
    raise Exception.Create(St);
End;

(**
 * Parse the command line parameters
 *
 * The parameters are parsed in two phases. Phase 0 only sets internal variables
 * which influence the behavior of the program (-b, -n). In phase 1 the scripts
 * and commands specified with parameters -f and -c, respectively, are exeucuted.
 *
 * @param Phase
 *)
Procedure ParseParams(Phase:Integer);
Var I : Integer;
Begin
  I := 1;
  While I <= ParamCount do
    Begin
      if (ParamStr(I) = '-h') or (ParamStr(I) = '--help') then
        // help
        Usage(0)
      else if ParamStr(I) = '-b' then
        // switch to batch mode
        BatchMode := true
      else if ParamStr(I) = '-f' then
        Begin
          // execute script file
          Inc(I);
          if Phase = 1 then
            Begin
              EvalFile(ParamStr(I));
              Flush(Output);
            End;
        End
      else if ParamStr(I) = '-c' then
        Begin
          // execute command
          Inc(I);
          if Phase = 1 then
            Eval(ParamStr(I));
        End
      else if ParamStr(I) = '-n' then
        // don't run startup scripts
        RunScripts := false
      else
        // error
        Usage(1);
      Inc(I);
    End;
End;

Procedure ExecuteStartupScripts;
  Procedure EvalFileIfExists(Filename:String);
  Begin
    if FileExists(Filename) then
      Begin
        //WriteLn('Executing ',Filename);
        EvalFile(Filename);
        Flush(Output);
      End
    else
      //WriteLn('Cannot find ',Filename);
  End;
Var List : TStringList;
    St   : String;
Begin
  // run /etc/flowproc/flowprocrc
  EvalFileIfExists('/etc/flowproc/flowprocrc');
  // run /etc/flowproc/flowprocrc.d/*.tcl
  List := FileSearchGlobList('/etc/flowproc/flowprocrc.d/*.tcl');
  List.Sorted := true;  // automatically sorts the list
  for St in List do
    Begin
      //WriteLn('Executing ',St);
      EvalFile(St);
      Flush(Output);
    End;
  // run ~/.flowprocrc
  EvalFileIfExists(BaseUnix.fpGetEnv(PChar('HOME'))+'/.flowprocrc');
End;

Begin
  App := TFlowApp.Create;
  // initialize default values
  ExitStatus := 0;
  RunScripts := true;
  BatchMode  := false;
  // parse parameters to set variables
  ParseParams(0);
  try
    try
      // execute startup scripts
      if RunScripts then
        ExecuteStartupScripts;
      // parse parameters to execute scripts and commands given as parameters
      ParseParams(1);
      // show help and run command prompt
      if not BatchMode then
        Begin
          App.Help(0,Nil);
          Flush(Output);
          ExitStatus := App.Run;
        End;
    except
      on E : Exception do
        Begin
          // notify caller of this program that something went wrong
          if ExitStatus <= 0 then ExitStatus := 1;
          Flush(Output); Flush(ErrOutput); Flush(StdOut); Flush(StdErr);
          DumpExceptionBackTrace(StdErr);
          raise;
        End;
    End;
  finally
    // cleanup, finish
    App.Free;
    if ExitStatus < 0 then
      ExitStatus := 0;   // script finished but "exit" was never used
    Halt(ExitStatus);
  End;
End.

