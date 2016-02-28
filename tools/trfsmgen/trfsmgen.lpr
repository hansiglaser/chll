(*ronn
trfsmgen(1tr) -- Generate configuration bitstream for TR-FSMs
=============================================================

## SYNOPSYS

`trfsmgen` [`-h`|`--help`] [`-b`] [`-f` <filename>] [`-c` <string>] [`-n`] [`-s`]

## DESCRIPTION

`trfsmgen` is a command line tool to generate the configuration bitstream for
TR-FSM.

## USER INTERFACE

`trfsmgen` offers a convenient _command line_ interface. It uses _Tcl_ as
scripting language and command interpreter. Actually, every command you enter
is implemented as a Tcl command (but written in Pascal and compiled in the
executable). The Tcl programming language offers unlimited options to use,
customize and automate `trfsmgen`.

`trfsmgen` prints a prompt at the screen to ask the use to enter commands. To
wait for the user input, _GNU Readline_ is used. This offers comforable command
line editing, history and auto-completion.

For each Tcl command a dedicated `man`(1tr)ual page is provided. The
sub-extension `tr` is used. These are viewed with the built-in command `man`
from the `trfsmgen` command line.

To quit `trfsmgen` use `exit`(1tr) or type [Ctrl]-[D].

## OPTIONS

  * `-h`, `--help`:
    Print a usage information and exit.

  * `-b`:
    Batch mode. `trfsmgen` will not wait for user input but quit directly after
    the startup scripts, the scripts given with `-f` parameters and the commands
    given with `-c` parameters were executed.

  * `-f` <filename>:
    Execute the Tcl script in _filename_ at program start.

  * `-c` <string>:
    Execute the commands given in _string_.

  * `-n`:
    Do not execute the start scripts (/etc/trfsmgen/... TODO )

  * `-s`:
    Do not perform (slow) symbol lookup in stack trace)

## STARTUP SCRIPTS

`trfsmgen` searches and executes startup scripts at program start, which are
_/etc/trfsmgen/trfsmgenrc_, _/etc/trfsmgen/trfsmgen.d/*.tcl_ (in alphabetical
order) and _~/trfsmgenrc_ in that order. If the option `-n` is given, none of
these startup scripts are executed.

Next, if the option `-f` _filename_ is given, the Tcl script _filename_ is
executed. The Tcl commands supplied with the option `-c` _string_ are also
executed. These scripts and commands are executed in the order of the options
on command line.

Finally, the user is prompted to input his commands, unless the option `-b`
is given, which immediately quits the program after all scripts and commands
are executed.

## PRINCIPLE

The process is split into two independent parts. The first part is to specify
the behavior of the FSM using an _FSM definition object_. Secondly, a
_TR-FSM object_ is created. The FSM definition object is assigned to the
TR-FSM object, and finally the bitstream is generated.

## COMMANDS

     help [word]
     man [page]
     history
     exit
     create_fsm_definition name
     add_input fsmdef name
     add_output fsmdef name
     add_state fsmdef name
     add_transition fsmdef from to output input value input value ...
     set_outputs_registered fsmdef true|false
     get_name fsmdef
     get_input_count fsmdef
     get_output_count fsmdef
     get_state_count fsmdef
     get_transition_count fsmdef
     get_reset_state fsmdef
     read_kiss filename
     read_ilang filename module
     find_cells [-one] [-name] module -cell cell|-instance instance
     get_cell_name cell
     get_fsm cell
     rename_fsm fsmdef name
     rename_input fsmdef input name
     rename_output fsmdef output name
     rename_state fsmdef state name
     check_fsm fsmdef
     print_fsm fsmdef
     print_fsms multifsmdefs
     write_fsm fsmdef -format vhdl|verilog|ilang filename
     create_trfsm [-version version] in out state n0 n1 n2 ...
     print_trfsm trfsm|multitrfsm
     map_fsms trfsms fsms arrayname
     set_fsm_definition trfsm fsmdef
     map_input trfsm logical physical
     map_output trfsm logical physical
     create_trfsm_wrapper trfsm cellname
     insert_trfsm_wrapper cell fsmdef wrapper instname
     generate_bitstream trfsm
     print_bitstream bitstream [-trfsm]
     write_bitstream bitstream -format vhdl|verilog|c|text|modelsim|lec|formality [options] prefix filename
     write_encoding trfsm -format lec [options] filename
     write_module module -format vhdl|verilog|ilang filename
     write_trfsm_wrapper wrapper -format vhdl|verilog|ilang|flowtcl filename

## FILES

  * _/etc/trfsmgen/trfsmgenrc_:
    System-wide startup script

  * _/etc/trfsmgen/trfsmgenrc.d/*.tcl_:
    Base directory for more system-wide startup scripts

  * _~/.trfsmgenrc_:
    Personal startup script

  * _~/.trfsmgen_history_:
    stores the history of the commands entered at the prompt

## TODO

 - implement the commands rename_*
 - `read_kiss` man page: document input/output signal order
 - add a command to verify a TR-FSM with its FSM definition

## REFERENCES

`trfsmgen` uses the [GNU
Readline](http://cnswww.cns.cwru.edu/php/chet/readline/rltop.html) library with
[Pascal bindings and an object-oriented
wrapper](https://github.com/hansiglaser/pas-readline/).

[Tcl (Tool Command Language)](http://www.tcl.tk/) is also used with [Pascal
bindings and an object-oriented
wrapper](https://github.com/hansiglaser/pas-tcl/).

## BUILD INSTRUCTIONS

You need the FreePascal compiler <http://www.freepascal.org/> and GNU Make. To
build the man pages, you need "ronn" <https://github.com/rtomayko/ronn>
(Debian package "ruby-ronn").

    # make
    # ./trfsmgen

## SEE ALSO

help(1tr), man(1tr)

## AUTHOR

Johann Glaser <Johann.Glaser@gmx.at>

*)
Program TRFSMGen;

{$mode objfpc}{$H+}

Uses
  Classes,SysUtils,BaseUnix,StrUtils,
  RegExpr,
  HistoryOOP,ReadlineOOP,TCL,TclOOP,TclApp,
  Math,Utils,Tables,
  StackTrace,    // will hook its address resolution function to the System unit's SysBackTraceStr function
  Netlist,ILang,
  FSMDef,TRFSMSpec,Bitstream,TRFSMBitstream,TRFSM;

Type

  { TFSMDefinitionObj }

  TFSMDefinitionObj = class(TTCLObj)
  public
    class Function  GetTypePtr : PTcl_ObjType;          override; // return static information on this type
    Function  SetFromAny(Interp:PTcl_Interp) : Integer; override; // set value from any other representation
    Function  UpdateString : AnsiString;                override; // update string representation
    Procedure Copy(ASrc : TTclObj);                     override; // Copy Constructor
  protected
    FDef: TFSMDefinition;
  public
    Constructor Create(AName:String);
    Destructor  Destroy; override;
  End;

  { TModuleObj }

  TModuleObj = class(TTCLObj)
  public
    class Function  GetTypePtr : PTcl_ObjType;          override; // return static information on this type
    Function  SetFromAny(Interp:PTcl_Interp) : Integer; override; // set value from any other representation
    Function  UpdateString : AnsiString;                override; // update string representation
    Procedure Copy(ASrc : TTclObj);                     override; // Copy Constructor
  protected
    FModule : TModule;
  public
    Constructor Create(AModule:TModule);
    Destructor  Destroy; override;
  End;

  { TCellObj }

  TCellObj = class(TTCLObj)
  public
    class Function  GetTypePtr : PTcl_ObjType;          override; // return static information on this type
    Function  SetFromAny(Interp:PTcl_Interp) : Integer; override; // set value from any other representation
    Function  UpdateString : AnsiString;                override; // update string representation
    Procedure Copy(ASrc : TTclObj);                     override; // Copy Constructor
  protected
    FModuleObj : TModuleObj;
    FCell      : TInstance;
  public
    Constructor Create(AModuleObj:TModuleObj;ACell:TInstance);
    Destructor  Destroy; override;
  End;

  { TTRFSMObj }

  TTRFSMObj = class(TTCLObj)
  public
    class Function  GetTypePtr : PTcl_ObjType;          override; // return static information on this type
    Function  SetFromAny(Interp:PTcl_Interp) : Integer; override; // set value from any other representation
    Function  UpdateString : AnsiString;                override; // update string representation
    Procedure Copy(ASrc : TTclObj);                     override; // Copy Constructor
  protected
    FTRFSM: TTRFSM;
  public
    Constructor Create(AVersion:String;AInputs,AOutputs,AStateBits:Integer;ANumTRs:TDynIntegerArray);
    Destructor  Destroy; override;
  End;

  { TWrapperObj }

  TWrapperObj = class(TTCLObj)
  public
    class Function  GetTypePtr : PTcl_ObjType;          override; // return static information on this type
    Function  SetFromAny(Interp:PTcl_Interp) : Integer; override; // set value from any other representation
    Function  UpdateString : AnsiString;                override; // update string representation
    Procedure Copy(ASrc : TTclObj);                     override; // Copy Constructor
  protected
    FTRFSM           : TTRFSM;
    FWrapperModule   : TModule;
    FTRFSMInstance   : TInstance;
  public
    Constructor Create;
    Destructor  Destroy; override;
  End;

  { TBitstreamObj }

  TBitstreamObj = class(TTCLObj)
  public
    class Function  GetTypePtr : PTcl_ObjType;          override; // return static information on this type
    Function  SetFromAny(Interp:PTcl_Interp) : Integer; override; // set value from any other representation
    Function  UpdateString : AnsiString;                override; // update string representation
    Procedure Copy(ASrc : TTclObj);                     override; // Copy Constructor
  protected
    FBitstream : TBitstream;
  public
    Constructor Create(ABitstream:TBitstream);
    Destructor  Destroy; override;
  End;

  { TTRFSMGenApp }

  TTRFSMGenApp = class(TTCLApp)
    FYosysFSMModule : TModule;
    FTRFSMModule    : TModule;
    Constructor Create;
    Destructor Destroy; override;

    // common commands
    Procedure Help               (ObjC:Integer;ObjV:PPTcl_Object);
    Procedure CreateFsmDefinition(ObjC:Integer;ObjV:PPTcl_Object);
    Procedure AddInput           (ObjC:Integer;ObjV:PPTcl_Object);
    Procedure AddOutput          (ObjC:Integer;ObjV:PPTcl_Object);
    Procedure AddState           (ObjC:Integer;ObjV:PPTcl_Object);
    Procedure AddTransition      (ObjC:Integer;ObjV:PPTcl_Object);
    Procedure SetOutputsRegistered(ObjC:Integer;ObjV:PPTcl_Object);
    Procedure GetName            (ObjC:Integer;ObjV:PPTcl_Object);
    Procedure GetInputCount      (ObjC:Integer;ObjV:PPTcl_Object);
    Procedure GetOutputCount     (ObjC:Integer;ObjV:PPTcl_Object);
    Procedure GetStateCount      (ObjC:Integer;ObjV:PPTcl_Object);
    Procedure GetTransitionCount (ObjC:Integer;ObjV:PPTcl_Object);
    Procedure GetResetState      (ObjC:Integer;ObjV:PPTcl_Object);
    Procedure ReadKiss           (ObjC:Integer;ObjV:PPTcl_Object);
    Procedure ReadILang          (ObjC:Integer;ObjV:PPTcl_Object);
    Procedure FindCells          (ObjC:Integer;ObjV:PPTcl_Object);
    Procedure GetCellName        (ObjC:Integer;ObjV:PPTcl_Object);
    Procedure GetFSM             (ObjC:Integer;ObjV:PPTcl_Object);
    Procedure RenameFsm          (ObjC:Integer;ObjV:PPTcl_Object);
    Procedure RenameInput        (ObjC:Integer;ObjV:PPTcl_Object);
    Procedure RenameOutput       (ObjC:Integer;ObjV:PPTcl_Object);
    Procedure RenameState        (ObjC:Integer;ObjV:PPTcl_Object);
    Procedure CheckFsm           (ObjC:Integer;ObjV:PPTcl_Object);
    Procedure PrintFsm           (ObjC:Integer;ObjV:PPTcl_Object);
    Procedure PrintFsms          (ObjC:Integer;ObjV:PPTcl_Object);
    Procedure WriteFsm           (ObjC:Integer;ObjV:PPTcl_Object);
    Procedure CreateTrfsm        (ObjC:Integer;ObjV:PPTcl_Object);
    Procedure PrintTrfsm         (ObjC:Integer;ObjV:PPTcl_Object);
    Procedure MapFsms            (ObjC:Integer;ObjV:PPTcl_Object);
    Procedure SetFsmDefinition   (ObjC:Integer;ObjV:PPTcl_Object);
    Procedure MapInput           (ObjC:Integer;ObjV:PPTcl_Object);
    Procedure MapOutput          (ObjC:Integer;ObjV:PPTcl_Object);
    Procedure CreateTrfsmWrapper (ObjC:Integer;ObjV:PPTcl_Object);
    Procedure InsertTrfsmWrapper (ObjC:Integer;ObjV:PPTcl_Object);
    Procedure GenerateBitstream  (ObjC:Integer;ObjV:PPTcl_Object);
    Procedure PrintBitstream     (ObjC:Integer;ObjV:PPTcl_Object);
    Procedure WriteBitstream     (ObjC:Integer;ObjV:PPTcl_Object);
    Procedure WriteEncoding      (ObjC:Integer;ObjV:PPTcl_Object);
    Procedure WriteModule        (ObjC:Integer;ObjV:PPTcl_Object);
    Procedure WriteTrfsmWrapper  (ObjC:Integer;ObjV:PPTcl_Object);
  End;

{ TFSMDefinitionObj }

Class Function TFSMDefinitionObj.GetTypePtr:PTcl_ObjType;
Const
  ObjType : Tcl_ObjType = (
    Name             : 'fsm_definition';
    freeIntRepProc   : @FreeIntRepProc;
    dupIntRepProc    : @DupIntRepProc;
    updateStringProc : @UpdateStringProc;
    setFromAnyProc   : Nil;//@SetFromAny;
  );
Begin
  Result := @ObjType;
End;

Function TFSMDefinitionObj.SetFromAny(Interp:PTcl_Interp):Integer;
Begin
  Result := TCL_ERROR;
End;

Function TFSMDefinitionObj.UpdateString:AnsiString;
Begin
  Result := '_' + FDef.Name;
End;

Procedure TFSMDefinitionObj.Copy(ASrc:TTclObj);
Begin
  FDef := (ASrc as TFSMDefinitionObj).FDef;  // TODO: shouldn't we create a copy of this object?
End;

Constructor TFSMDefinitionObj.Create(AName:String);
Begin
  inherited Create;
  FDef := TFSMDefinition.Create(AName);
  InvalidateString;
End;

Destructor TFSMDefinitionObj.Destroy;
Begin
  FDef.Free;
  inherited Destroy;
End;

{ TModuleObj }

Class Function TModuleObj.GetTypePtr:PTcl_ObjType;
Const
  ObjType : Tcl_ObjType = (
    Name             : 'fsm_definition';
    freeIntRepProc   : @FreeIntRepProc;
    dupIntRepProc    : @DupIntRepProc;
    updateStringProc : @UpdateStringProc;
    setFromAnyProc   : Nil;//@SetFromAny;
  );
Begin
  Result := @ObjType;
End;

Function TModuleObj.SetFromAny(Interp:PTcl_Interp):Integer;
Begin
  Result := TCL_ERROR;
End;

Function TModuleObj.UpdateString:AnsiString;
Begin
  Result := '_' + FModule.FName;
End;

Procedure TModuleObj.Copy(ASrc:TTclObj);
Begin
  FModule := (ASrc as TModuleObj).FModule;  // TODO: shouldn't we create a copy of this object?
End;

Constructor TModuleObj.Create(AModule:TModule);
Begin
  inherited Create;
  FModule := AModule;
End;

Destructor TModuleObj.Destroy;
Begin
  FModule.Free;
  Inherited Destroy;
End;

{ TCellObj }

Class Function TCellObj.GetTypePtr:PTcl_ObjType;
Const
  ObjType : Tcl_ObjType = (
    Name             : 'cell';
    freeIntRepProc   : @FreeIntRepProc;
    dupIntRepProc    : @DupIntRepProc;
    updateStringProc : @UpdateStringProc;
    setFromAnyProc   : Nil;//@SetFromAny;
  );
Begin
  Result := @ObjType;
End;

Function TCellObj.SetFromAny(Interp:PTcl_Interp):Integer;
Begin
  Result := TCL_ERROR;
End;

Function TCellObj.UpdateString:AnsiString;
Begin
  Result := '_' + FCell.FName;
End;

Procedure TCellObj.Copy(ASrc:TTclObj);
Begin
  FModuleObj := (ASrc as TCellObj).FModuleObj;  // TODO: shouldn't we create a copy of this object?
  FCell      := (ASrc as TCellObj).FCell;       // TODO: shouldn't we create a copy of this object?
  FModuleObj.IncrRefCount;
End;

Constructor TCellObj.Create(AModuleObj:TModuleObj;ACell:TInstance);
Begin
  inherited Create;
  FModuleObj := AModuleObj;
  FCell      := ACell;
  FModuleObj.IncrRefCount;
  InvalidateStringRep;
End;

Destructor TCellObj.Destroy;
Begin
  FModuleObj.DecrRefCount;
  Inherited Destroy;
End;

{ TTRFSMObj }

Class Function TTRFSMObj.GetTypePtr:PTcl_ObjType;
Const
  ObjType : Tcl_ObjType = (
    Name             : 'trfsm';
    freeIntRepProc   : @FreeIntRepProc;
    dupIntRepProc    : @DupIntRepProc;
    updateStringProc : @UpdateStringProc;
    setFromAnyProc   : Nil;//@SetFromAny;
  );
Begin
  Result := @ObjType;
End;

Function TTRFSMObj.SetFromAny(Interp:PTcl_Interp):Integer;
Begin
  Result := TCL_ERROR;
End;

Function TTRFSMObj.UpdateString:AnsiString;
Begin
  Result := '_trfsm';
End;

Procedure TTRFSMObj.Copy(ASrc:TTclObj);
Begin
  FTRFSM := (ASrc as TTRFSMObj).FTRFSM;  // TODO: shouldn't we create a copy of this object?
End;

Constructor TTRFSMObj.Create(AVersion:String;AInputs,AOutputs,AStateBits:Integer;ANumTRs:TDynIntegerArray);
Var Spec : TTRFSMSpecification;
Begin
  inherited Create;
  Spec := TTRFSMSpecification.Create(AVersion,AInputs,AOutputs,AStateBits,ANumTRs);
  FTRFSM := TTRFSM.Create(Spec);
  InvalidateString;
End;

Destructor TTRFSMObj.Destroy;
Begin
  FTRFSM.Free;
  inherited Destroy;
End;

{ TWrapperObj }

Class Function TWrapperObj.GetTypePtr:PTcl_ObjType;
Const
  ObjType : Tcl_ObjType = (
    Name             : 'wrapper';
    freeIntRepProc   : @FreeIntRepProc;
    dupIntRepProc    : @DupIntRepProc;
    updateStringProc : @UpdateStringProc;
    setFromAnyProc   : Nil;//@SetFromAny;
  );
Begin
  Result := @ObjType;
End;

Function TWrapperObj.SetFromAny(Interp:PTcl_Interp):Integer;
Begin
  Result := TCL_ERROR;
End;

Function TWrapperObj.UpdateString:AnsiString;
Begin
  Result := '_wrapper';
End;

Procedure TWrapperObj.Copy(ASrc:TTclObj);
Begin

End;

Constructor TWrapperObj.Create;
Begin
  inherited Create;
End;

Destructor TWrapperObj.Destroy;
Begin
  inherited Destroy;
End;

{ TBitstreamObj }

Class Function TBitstreamObj.GetTypePtr:PTcl_ObjType;
Const
  ObjType : Tcl_ObjType = (
    Name             : 'bitstream';
    freeIntRepProc   : @FreeIntRepProc;
    dupIntRepProc    : @DupIntRepProc;
    updateStringProc : @UpdateStringProc;
    setFromAnyProc   : Nil;//@SetFromAny;
  );
Begin
  Result := @ObjType;
End;

Function TBitstreamObj.SetFromAny(Interp:PTcl_Interp):Integer;
Begin
  Result := TCL_ERROR;
End;

Function TBitstreamObj.UpdateString:AnsiString;
Begin
  Result := FBitstream.GetString;
End;

Procedure TBitstreamObj.Copy(ASrc:TTclObj);
Begin
  FBitstream := (ASrc as TBitstreamObj).FBitstream;  // TODO: shouldn't we create a copy of this object?
End;

Constructor TBitstreamObj.Create(ABitstream:TBitstream);
Begin
  inherited Create;
  FBitstream := ABitstream;
  InvalidateString;
End;

Destructor TBitstreamObj.Destroy;
Begin
  FBitstream.Free;
  Inherited Destroy;
End;

{ TTRFSMGenApp }

Constructor TTRFSMGenApp.Create;
Begin
  inherited Create('.trfsmgen_history','TRFSMGen');

  // Register/override in the Tcl engine our new functions
  // common commands
  FCmdLine.CreateCommandExit   ('exit');
  FCmdLine.CreateCommandHistory('history');
  FCmdLine.CreateCommandMan    ('man',FpGetCwd + '/man');
  // own commands
  FTCL.CreateObjCommand('help',                 @Self.Help,               nil);
  FTCL.CreateObjCommand('create_fsm_definition',@Self.CreateFsmDefinition,nil);
  FTCL.CreateObjCommand('add_input',            @Self.AddInput,           nil);
  FTCL.CreateObjCommand('add_output',           @Self.AddOutput,          nil);
  FTCL.CreateObjCommand('add_state',            @Self.AddState,           nil);
  FTCL.CreateObjCommand('add_transition',       @Self.AddTransition,      nil);
  FTCL.CreateObjCommand('set_outputs_registered',@Self.SetOutputsRegistered,nil);
  FTCL.CreateObjCommand('get_name',             @Self.GetName,            nil);
  FTCL.CreateObjCommand('get_input_count',      @Self.GetInputCount,      nil);
  FTCL.CreateObjCommand('get_output_count',     @Self.GetOutputCount,     nil);
  FTCL.CreateObjCommand('get_state_count',      @Self.GetStateCount,      nil);
  FTCL.CreateObjCommand('get_transition_count', @Self.GetTransitionCount, nil);
  FTCL.CreateObjCommand('get_reset_state',      @Self.GetResetState,      nil);
  FTCL.CreateObjCommand('read_kiss',            @Self.ReadKiss,           nil);
  FTCL.CreateObjCommand('read_ilang',           @Self.ReadILang,          nil);
  FTCL.CreateObjCommand('find_cells',           @Self.FindCells,          nil);
  FTCL.CreateObjCommand('get_cell_name',        @Self.GetCellName,        nil);
  FTCL.CreateObjCommand('get_fsm',              @Self.GetFsm,             nil);
  FTCL.CreateObjCommand('rename_fsm',           @Self.RenameFsm,          nil);
  FTCL.CreateObjCommand('rename_input',         @Self.RenameInput,        nil);
  FTCL.CreateObjCommand('rename_output',        @Self.RenameOutput,       nil);
  FTCL.CreateObjCommand('rename_state',         @Self.RenameState,        nil);
  FTCL.CreateObjCommand('check_fsm',            @Self.CheckFsm,           nil);
  FTCL.CreateObjCommand('print_fsm',            @Self.PrintFsm,           nil);
  FTCL.CreateObjCommand('print_fsms',           @Self.PrintFsms,          nil);
  FTCL.CreateObjCommand('write_fsm',            @Self.WriteFsm,           nil);
  FTCL.CreateObjCommand('create_trfsm',         @Self.CreateTrfsm,        nil);
  FTCL.CreateObjCommand('print_trfsm',          @Self.PrintTrfsm,         nil);
  FTCL.CreateObjCommand('map_fsms',             @Self.MapFsms,            nil);
  FTCL.CreateObjCommand('set_fsm_definition',   @Self.SetFsmDefinition,   nil);
  FTCL.CreateObjCommand('map_input',            @Self.MapInput,           nil);
  FTCL.CreateObjCommand('map_output',           @Self.MapOutput,          nil);
  FTCL.CreateObjCommand('create_trfsm_wrapper', @Self.CreateTrfsmWrapper, nil);
  FTCL.CreateObjCommand('insert_trfsm_wrapper', @Self.InsertTrfsmWrapper, nil);
  FTCL.CreateObjCommand('generate_bitstream',   @Self.GenerateBitstream,  nil);
  FTCL.CreateObjCommand('print_bitstream',      @Self.PrintBitstream,     nil);
  FTCL.CreateObjCommand('write_bitstream',      @Self.WriteBitstream,     nil);
  FTCL.CreateObjCommand('write_encoding',       @Self.WriteEncoding,      nil);
  FTCL.CreateObjCommand('write_module',         @Self.WriteModule,        nil);
  FTCL.CreateObjCommand('write_trfsm_wrapper',  @Self.WriteTrfsmWrapper,  nil);

  // Create template of Yosys' $fsm module
  FYosysFSMModule := TModule.Create('$fsm');
  FYosysFSMModule.AddGeneric(TGeneric.Create('NAME',           TypeString, Nil));
  FYosysFSMModule.AddGeneric(TGeneric.Create('CLK_POLARITY',   TypeBit,    TValueBit.Create('1')));
  FYosysFSMModule.AddGeneric(TGeneric.Create('ARST_POLARITY',  TypeBit,    TValueBit.Create('1')));
  FYosysFSMModule.AddGeneric(TGeneric.Create('CTRL_IN_WIDTH',  TypeInt,    TValueInteger.Create(1)));
  FYosysFSMModule.AddGeneric(TGeneric.Create('CTRL_OUT_WIDTH', TypeInt,    TValueInteger.Create(1)));
  FYosysFSMModule.AddGeneric(TGeneric.Create('STATE_BITS',     TypeInt,    TValueInteger.Create(1)));
  FYosysFSMModule.AddGeneric(TGeneric.Create('STATE_NUM',      TypeInt,    TValueInteger.Create(1)));
  FYosysFSMModule.AddGeneric(TGeneric.Create('STATE_NUM_LOG2', TypeInt,    TValueInteger.Create(1)));
  FYosysFSMModule.AddGeneric(TGeneric.Create('STATE_RST',      TypeInt,    TValueInteger.Create(0)));
  FYosysFSMModule.AddGeneric(TGeneric.Create('STATE_TABLE',    TypeBit,    TValueBit.Create('0')));
  FYosysFSMModule.AddGeneric(TGeneric.Create('TRANS_NUM',      TypeInt,    TValueInteger.Create(1)));
  FYosysFSMModule.AddGeneric(TGeneric.Create('TRANS_TABLE',    TType.Create('std_logic_vector',dirDown,3,0),TValueVector.Create(4,'0x0x')));
  FYosysFSMModule.AddPort(TPort.Create('CLK', dirIn,TypeBit));
  FYosysFSMModule.AddPort(TPort.Create('ARST',dirIn,TypeBit));
  FYosysFSMModule.AddPort(TPort.Create('CTRL_IN',dirIn,TType.Create('std_logic_vector',dirDown,
    TValueOperatorMinus.Create(
      FYosysFSMModule.FGenerics['CTRL_IN_WIDTH'],
      TValueInteger.Create(1)),
    TValueInteger.Create(0))));
  FYosysFSMModule.AddPort(TPort.Create('CTRL_OUT',dirOut,TType.Create('std_logic_vector',dirDown,
    TValueOperatorMinus.Create(
      FYosysFSMModule.FGenerics['CTRL_OUT_WIDTH'],
      TValueInteger.Create(1)),
    TValueInteger.Create(0))));

  FTRFSMModule := TModule.Create('TRFSM');
  FTRFSMModule.AddGeneric(TGeneric.Create('InputWidth', TypeInt,Nil));
  FTRFSMModule.AddGeneric(TGeneric.Create('OutputWidth',TypeInt,Nil));
  FTRFSMModule.AddGeneric(TGeneric.Create('StateWidth', TypeInt,Nil));
  FTRFSMModule.AddGeneric(TGeneric.Create('UseResetRow',TypeInt,Nil));
  FTRFSMModule.AddGeneric(TGeneric.Create('NumRows0',   TypeInt,Nil));
  FTRFSMModule.AddGeneric(TGeneric.Create('NumRows1',   TypeInt,Nil));
  FTRFSMModule.AddGeneric(TGeneric.Create('NumRows2',   TypeInt,Nil));
  FTRFSMModule.AddGeneric(TGeneric.Create('NumRows3',   TypeInt,Nil));
  FTRFSMModule.AddGeneric(TGeneric.Create('NumRows4',   TypeInt,Nil));
  FTRFSMModule.AddGeneric(TGeneric.Create('NumRows5',   TypeInt,Nil));
  FTRFSMModule.AddGeneric(TGeneric.Create('NumRows6',   TypeInt,Nil));
  FTRFSMModule.AddGeneric(TGeneric.Create('NumRows7',   TypeInt,Nil));
  FTRFSMModule.AddGeneric(TGeneric.Create('NumRows8',   TypeInt,Nil));
  FTRFSMModule.AddGeneric(TGeneric.Create('NumRows9',   TypeInt,Nil));
  FTRFSMModule.AddPort(TPort.Create('Reset_n_i',dirIn,TypeBit));
  FTRFSMModule.AddPort(TPort.Create('Clk_i',    dirIn,TypeBit));
  // IOs
  FTRFSMModule.AddPort(TPort.Create('Input_i',dirIn,TType.Create('std_logic_vector',dirDown,
    TValueOperatorMinus.Create(
      FTRFSMModule.FGenerics['InputWidth'],
      TValueInteger.Create(1)),
    TValueInteger.Create(0))));
  FTRFSMModule.AddPort(TPort.Create('Output_o',dirOut,TType.Create('std_logic_vector',dirDown,
    TValueOperatorMinus.Create(
      FTRFSMModule.FGenerics['OutputWidth'],
      TValueInteger.Create(1)),
    TValueInteger.Create(0))));
  // Configuration
  FTRFSMModule.AddPort(TPort.Create('CfgMode_i',    dirIn, TypeBit));
  FTRFSMModule.AddPort(TPort.Create('CfgClk_i',     dirIn, TypeBit));
  FTRFSMModule.AddPort(TPort.Create('CfgShift_i',   dirIn, TypeBit));
  FTRFSMModule.AddPort(TPort.Create('CfgDataIn_i',  dirIn, TypeBit));
  FTRFSMModule.AddPort(TPort.Create('CfgDataOut_o', dirOut,TypeBit));
  // Scan Chain
  FTRFSMModule.AddPort(TPort.Create('ScanEnable_i', dirIn, TypeBit));
  FTRFSMModule.AddPort(TPort.Create('ScanClk_i',    dirIn, TypeBit));
  FTRFSMModule.AddPort(TPort.Create('ScanDataIn_i', dirIn, TypeBit));
  FTRFSMModule.AddPort(TPort.Create('ScanDataOut_o',dirOut,TypeBit));
End;

Destructor TTRFSMGenApp.Destroy;
Begin
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
Procedure TTRFSMGenApp.Help(ObjC:Integer;ObjV:PPTcl_Object);
Begin
  if ObjC = 2 then // one argument -> "apropos"
    Begin
      FCmdLine.Apropos(ObjV^[1].AsPChar);
      Exit;
    End;
  WriteLn('TR-FSM Gen');
  WriteLn('  help [word]');
  WriteLn('  man [page]');
  WriteLn('  history');
  WriteLn('  exit');
  WriteLn('  create_fsm_definition name');
  WriteLn('  add_input fsmdef name');
  WriteLn('  add_output fsmdef name');
  WriteLn('  add_state fsmdef name');
  WriteLn('  add_transition fsmdef from to output input value input value ...');
  WriteLn('  set_outputs_registered fsmdef true|false');
  WriteLn('  get_name fsmdef');
  WriteLn('  get_input_count fsmdef');
  WriteLn('  get_output_count fsmdef');
  WriteLn('  get_state_count fsmdef');
  WriteLn('  get_transition_count fsmdef');
  WriteLn('  get_reset_state fsmdef');
  WriteLn('  read_kiss filename');
  WriteLn('  read_ilang filename module');
  WriteLn('  find_cells [-one] [-name] module -cell cell|-instance instance');
  WriteLn('  get_cell_name cell');
  WriteLn('  get_fsm cell');
  WriteLn('  rename_fsm fsmdef name');
  WriteLn('  rename_input fsmdef inpt name');
  WriteLn('  rename_output fsmdef output name');
  WriteLn('  rename_state fsmdef state name');
  WriteLn('  check_fsm fsmdef');
  WriteLn('  print_fsm fsmdef');
  WriteLn('  print_fsms multifsmdefs');
  WriteLn('  write_fsm fsmdef -format vhdl|verilog|ilang filename');
  WriteLn('  create_trfsm in out state n0 n1 n2 ...');
  WriteLn('  print_trfsm trfsm|multitrfsm');
  WriteLn('  map_fsms trfsms fsms arrayname');
  WriteLn('  set_fsm_definition trfsm fsmdef');
  WriteLn('  map_input trfsm logical physical');
  WriteLn('  map_output trfsm logical physical');
  WriteLn('  create_trfsm_wrapper trfsm cellname');
  WriteLn('  insert_trfsm_wrapper cell fsmdef wrapper instname');
  WriteLn('  generate_bitstream trfsm');
  WriteLn('  print_bitstream bitstream [-trfsm]');
  WriteLn('  write_bitstream bitstream -format vhdl|verilog|c|text|modelsim|lec|formality [options] prefix filename');
  WriteLn('  write_encoding trfsm -format lec [options] filename');
  WriteLn('  write_module module -format vhdl|verilog|ilang filename');
  WriteLn('  write_trfsm_wrapper wrapper -format vhdl|verilog|ilang|flowtcl filename');
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
_~/.trfsmgen_history_ on program exit and loaded on program start.

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

Procedure AssertObjType(Obj:TTCL_Object;ReqType:TTCLObjClass);
Begin
  if ReqType.IsMyType(Obj) then
    Exit;
  // wrong type, complain
  if PTcl_Obj(Obj)^.typePtr <> Nil then
    raise Exception.Create('Expected data type "'+ReqType.GetTypePtr^.name+'" but got "'+PTcl_Obj(Obj)^.typePtr^.name+'"')
  else
    raise Exception.Create('Expected data type "'+ReqType.GetTypePtr^.name+'" but got an unknown type');
End;

Function AssertParamsFsmDef(ObjC,Num:Integer;ObjV:PPTcl_Object;Params:String) : TFSMDefinitionObj;
Begin
  AssertParams(ObjC,Num,ObjV,Params);
  AssertObjType(ObjV^[1],TFSMDefinitionObj);
  Result := TFSMDefinitionObj.Get(ObjV^[1]) as TFSMDefinitionObj;
End;

(*ronn
create_fsm_definition(1tr) -- Create an FSM definition object
=============================================================

## SYNOPSYS

`create_fsm_definition` <name>

## DESCRIPTION

This command creates a new (and empty) FSM definition object. This is an
implementation independent container which solely specifies the input and
output signals of an FSM and its states and the transitions between them.

For each transition a specific input signal pattern is requied. This also
influences the output pattern.

Use the parameter <name> to specify the name of the FSM.

## RETURNS

On success, the function returns an FSM definition object. This is used by
other functions, which manipulate the definition.

## EXAMPLE

    set fsmdef [create_fsm_definition "MyFSM"]
    add_input  $fsmdef "ButtonA_i"
    add_input  $fsmdef "ButtonB_i"
    add_output $fsmdef "LED_o"
    add_output $fsmdef "Sound_o"

## SEE ALSO

`add_input`(1tr), `add_output`(1tr), `add_state`(1tr), `add_transition`(1tr),
`get_name`(1tr), `get_input_count`(1tr), `get_output_count`(1tr),
`get_state_count`(1tr), `get_transition_count`(1tr), `get_reset_state`(1tr),
`read_kiss`(1tr), `rename_fsm`(1tr), `rename_input`(1tr), `rename_output`(1tr),
`rename_state`(1tr), `check_fsm`(1tr), `print_fsm`(1tr)

*)
Procedure TTRFSMGenApp.CreateFsmDefinition(ObjC:Integer;ObjV:PPTcl_Object);
Var Obj : TTCLObj;
Begin
  AssertParams(ObjC,2,ObjV,'name');
  // create_fsm_definition name
  Obj := TFSMDefinitionObj.Create(ObjV^[1].AsPChar);
  FTCL.SetObjResult(Obj.TclObj);
End;

(*ronn
add_input(1tr) -- Add an input signal to an FSM definition object
=================================================================

## SYNPOSYS

`add_input` <fsmdef> <name>

## DESCRIPTION

To add an input signal to an FSM definition object, use the command `add_input`.
The first parameter <fsmdef> is the FSM definition object as returned by
`create_fsm_definition`(1tr). The second parameter <name> is a string given as
the name of the input.

Note that the order of the definition of inputs and outputs influences the
behavior of later commands like `add_transition`(1tr).

## SEE ALSO

`add_output`(1tr), `create_fsm_definition`(1tr), `add_transition`(1tr)

*)
Procedure TTRFSMGenApp.AddInput(ObjC:Integer;ObjV:PPTcl_Object);
Begin
  // add_input $fsmdef name
  AssertParamsFsmDef(ObjC,3,ObjV,'fsmdef name').FDef.AddInput(ObjV^[2].AsPChar);
End;

(*ronn
add_output(1tr) -- Add an output signal to an FSM definition object
===================================================================

## SYNPOSYS

`add_output` <fsmdef> <name>

## DESCRIPTION

To add an output signal to an FSM definition object, use the command `add_output`.
The first parameter <fsmdef> is the FSM definition object as returned by
`create_fsm_definition`(1tr). The second parameter <name> is a string given as
the name of the output.

## SEE ALSO

`add_input`(1tr), `create_fsm_definition`(1tr)

*)
Procedure TTRFSMGenApp.AddOutput(ObjC:Integer;ObjV:PPTcl_Object);
Begin
  // add_output $fsmdef name
  AssertParamsFsmDef(ObjC,3,ObjV,'fsmdef name').FDef.AddOutput(ObjV^[2].AsPChar);
End;

(*ronn
add_state(1tr) -- Add a state to an FSM definition object
=========================================================

## SYNPOSYS

`add_state` <fsmdef> <name>

## DESCRIPTION

Add all states of the FSM using the command `add_state`. The first parameter
<fsmdef> is the FSM definition object as returned by
`create_fsm_definition`(1tr). The second parameter <name> is a string given as
the name of the state.

## SEE ALSO

`create_fsm_definition`(1tr), `add_transition`(1tr)

*)
Procedure TTRFSMGenApp.AddState(ObjC:Integer;ObjV:PPTcl_Object);
Begin
  // add_state $fsmdef name
  AssertParamsFsmDef(ObjC,3,ObjV,'fsmdef name').FDef.AddState(ObjV^[2].AsPChar);
End;

(**
 * add_transition $fsmdef "Off" "Off" "00011" "Abc_i" 0 ...
 *)
(*ronn
add_transition(1tr) -- Add a transition to an FSM definition object
===================================================================

## SYNPOSYS

`add_transition` <fsmdef> <state> <nextstate> <output> <Input> <value> ...

## DESCRIPTION

Add transitions between states using the command `add_transition`. The first
parameter <fsmdef> is the FSM definition object as returned by
`create_fsm_definition`(1tr). The second parameter <state> is a string specifying
the state from which the transition is started. The next state after the
transition is given by <nextstate>.

The output signals during the transition is specifed as <output>. This is given
as a bit vector specified as a string, e.g. "11010" with the left-most character
(here: "1") specifying the value of the output which was first added with
`add_output`(1tr) and the last (here: "0") specifying the value of the output
which was last added.

The following parameters are pairs of an input signal name as added with
`add_input`(1tr) and its value. Only if all specified signals have the
specified value, the transition is taken.

## EXAMPLES

    add_transition $fsm "Off" "Sleep" "100010" "Enable" 1

This example adds a transition to the FSM definition object stored in `$fsm`
from the state `"Off"` to the state `"Sleep"`. This transition is taken only
of the input signal `"Enable"` has a value of `1`. While the condition is met,
the output signals are set to `"100010"`.

   add_transition $fsm "Sleep" "SensorQuery" "111010" "Enable" 1 "TimerOvfl" 1

In this example a transition from `"Sleep"` to `"SensorQuery"` with the output
signal values `"111010"` is created. This transition is taken only, if the input
signals `"Enable"` `"TimerOvfl"` both have a value of `1`.

## SEE ALSO

`create_fsm_definition`(1tr), `add_state`(1tr), `add_output`(1tr),
`add_input`(1tr)

*)
Procedure TTRFSMGenApp.AddTransition(ObjC:Integer;ObjV:PPTcl_Object);
Var Obj         : TFSMDefinitionObj;
    StFrom,StTo : String;
    Output      : String;
    Idx         : Integer;
    Inputs      : TSignalDefinition;
    Pattern     : TInputPattern;
    Patterns    : TInputPatterns;
Begin
  if (ObjC < 6) or ((ObjC-5) and 1 <> 0) then
    raise Exception.Create('bad # arg: ' + ObjV^[0].AsPChar + ' fsmdef from to output input value ...');
  AssertObjType(ObjV^[1],TFSMDefinitionObj);
  Obj := TFSMDefinitionObj.Get(ObjV^[1]) as TFSMDefinitionObj;
  StFrom := ObjV^[2].AsPChar;
  StTo   := ObjV^[3].AsPChar;
  Output := ObjV^[4].AsPChar;
  Idx := 5;
  SetLength(Inputs,0);
  repeat
    SetLength(Inputs,Length(Inputs)+1);
    Inputs[Length(Inputs)-1].Signal := ObjV^[Idx].AsPChar;
    Inputs[Length(Inputs)-1].Value  := ObjV^[Idx+1].AsInteger(FTCL);
    Idx := Idx + 2;
  until Idx >= ObjC;
  Pattern  := Obj.FDef.GetInputPattern(Inputs);
  Patterns := Obj.FDef.GetInputPatterns([Pattern]);

  Obj.FDef.Add(StFrom,Patterns,StTo,Output);
End;

(*ronn
set_outputs_registered (1tr) -- Specify whether the outputs of an FSM should be registered
==========================================================================================

## SYNPOSYS

`set_outputs_registered` <fsmdef> true|false

## DESCRIPTION

The FSM definition object is updated to state whether the outputs of the FSM
should be registered. This is only supported by TR-FSM version 'REG'.

## EXAMPLES

This example sets the outputs of `$fsm` as registered.

    set_outputs_registered $fsm true

## SEE ALSO

`create_trfsm`(1tr)

*)
Procedure TTRFSMGenApp.SetOutputsRegistered(ObjC:Integer;ObjV:PPTcl_Object);
Var Obj   : TFSMDefinitionObj;
    RegSt : String;
    Reg   : Boolean;
Begin
  // set_outputs_registered fsmdef true|false
  Obj := AssertParamsFsmDef(ObjC,3,ObjV,'fmsdef true|false');
  RegSt := ObjV^[2].AsPChar;
  if      RegSt = 'true'  then Reg := true
  else if RegSt = 'false' then Reg := false
  else
    raise Exception.Create('bad arg: '+RegSt);
  Obj.FDef.OutputRegistered := Reg;
End;

(*ronn
get_name(1tr) -- Query the name of an FSM definition object
===========================================================

## SYNPOSYS

`get_name` <fsmdef>

## DESCRIPTION

The name of an FSM definition object is returned.

## RETURNS

Returns the name of the supplied FSM definition object.

## EXAMPLES

This example queries the name of `$fsm` and assigns the value to the variable
`Name`.

    set Name [get_name $fsm]

## SEE ALSO

`create_fsm_definition`(1tr)

*)
Procedure TTRFSMGenApp.GetName(ObjC:Integer;ObjV:PPTcl_Object);
Var Obj : TFSMDefinitionObj;
Begin
  Obj := AssertParamsFsmDef(ObjC,2,ObjV,'fsmdef');
  FTCL.SetResult(Obj.FDef.Name);
End;

(*ronn
get_input_count(1tr) -- Query number of inputs of an FSM definition object
==========================================================================

## SYNPOSYS

`get_input_count` <fsmdef>

## DESCRIPTION

The number of inputs as defined in an FSM definition object is returned.

## RETURNS

Returns the number of inputs as defined in the supplied FSM definition object.

## EXAMPLES

This example queries the number of inputs of `$fsm` and assigns the value to
the variable `NumInputs`.

    set NumInputs [get_input_count $fsm]

## SEE ALSO

`create_fsm_definition`(1tr), `add_input`(1tr)

*)
Procedure TTRFSMGenApp.GetInputCount(ObjC:Integer;ObjV:PPTcl_Object);
Var Obj : TFSMDefinitionObj;
Begin
  Obj := AssertParamsFsmDef(ObjC,2,ObjV,'fsmdef');
  FTCL.SetObjResult(Obj.FDef.InputCount);
End;

(*ronn
get_output_count(1tr) -- Query number of outputs of an FSM definition object
============================================================================

## SYNPOSYS

`get_output_count` <fsmdef>

## DESCRIPTION

The number of outputs as defined in an FSM definition object is returned.

## RETURNS

Returns the number of outputs as defined in the supplied FSM definition object.

## EXAMPLES

This example queries the number of outputs of `$fsm` and assigns the value to
the variable `NumOutputs`.

    set NumOutputs [get_output_count $fsm]

## SEE ALSO

`create_fsm_definition`(1tr), `add_output`(1tr)

*)
Procedure TTRFSMGenApp.GetOutputCount(ObjC:Integer;ObjV:PPTcl_Object);
Var Obj : TFSMDefinitionObj;
Begin
  Obj := AssertParamsFsmDef(ObjC,2,ObjV,'fsmdef');
  FTCL.SetObjResult(Obj.FDef.OutputCount);
End;

(*ronn
get_state_count(1tr) -- Query number of states of an FSM definition object
==========================================================================

## SYNPOSYS

`get_state_count` <fsmdef>

## DESCRIPTION

The number of states as defined in an FSM definition object is returned.

## RETURNS

Returns the number of states as defined in the supplied FSM definition object.

## EXAMPLES

This example queries the number of states of `$fsm` and assigns the value to
the variable `NumStates`.

    set NumStates [get_state_count $fsm]

## SEE ALSO

`create_fsm_definition`(1tr), `add_state`(1tr)

*)
Procedure TTRFSMGenApp.GetStateCount(ObjC:Integer;ObjV:PPTcl_Object);
Var Obj : TFSMDefinitionObj;
Begin
  Obj := AssertParamsFsmDef(ObjC,2,ObjV,'fsmdef');
  FTCL.SetObjResult(Obj.FDef.StateCount);
End;

(*ronn
get_transition_count(1tr) -- Query number of transitions of an FSM definition object
====================================================================================

## SYNPOSYS

`get_transition_count` <fsmdef>

## DESCRIPTION

The number of transitions as defined in an FSM definition object is returned.

## RETURNS

Returns the number of transitions as defined in the supplied FSM definition
object.

## EXAMPLES

This example queries the number of transitions of `$fsm` and assigns the value
to the variable `NumTransitions`.

    set NumTransitions [get_transition_count $fsm]

## SEE ALSO

`create_fsm_definition`(1tr), `add_transition`(1tr)

*)
Procedure TTRFSMGenApp.GetTransitionCount(ObjC:Integer;ObjV:PPTcl_Object);
Var Obj : TFSMDefinitionObj;
Begin
  Obj := AssertParamsFsmDef(ObjC,2,ObjV,'fsmdef');
  FTCL.SetObjResult(Obj.FDef.TransitionCount);
End;

(*ronn
get_reset_state (1tr) -- Query reset state of an FSM definition object
======================================================================

## SYNPOSYS

`get_reset_state` <fsmdef>

## DESCRIPTION

The reset state as defined in an FSM definition object is returned.

## RETURNS

Returns the reset state as defined in the supplied FSM definition object. If
no reset state is defined, an empty string is returned.

## EXAMPLES

This example queries the reset state of `$fsm` and assigns the value to the
variable `ResetState`.

    set ResetState [get_reset_state $fsm]

## SEE ALSO

`create_fsm_definition`(1tr)

*)
Procedure TTRFSMGenApp.GetResetState(ObjC:Integer;ObjV:PPTcl_Object);
Var Obj : TFSMDefinitionObj;
Begin
  Obj := AssertParamsFsmDef(ObjC,2,ObjV,'fsmdef');
  FTCL.SetResult(Obj.FDef.ResetState);
End;

(*ronn
read_kiss(1tr) -- Create an FSM definition object from a KISS2 file
===================================================================

## SYNPOSYS

`read_kiss` <filename>

## DESCRIPTION

With `read_kiss` a KISS2 file is read and an FSM definition object is created.
The inputs, outputs, states and transitions given in the KISS2 file are set
in the FSM definition object as if `add_input`(1tr), `add_output`(1tr),
`add_state`(1tr) and `add_transition`(1tr) were used.

The FSM definition name is set to the <filename> as specified on the command
line prompt. Use `rename_fsm`(1tr) to assign a new name.

The input and output signal names are named `InputN` and `OutputN`,
respectively, where `N` is a number counting from 0 upwards. These names
are automatically created, because the KISS2 file doesn't specify them. You can
use `rename_input`(1tr) and `rename_output`(1tr) to assign new names. Note that
Input0 corresponds to the right-most (LSB) digit of the input pattern vector
of the KISS2 file. Analogously, Output0 corresponds to the right-most digit of
the output pattern vector.

The states are named as given in the KISS2 file in columns 2 and 3. A new state
is added every time an unknown state name is observed. Use `rename_state`(1tr)
to assign new state names.

## RETURNS

On success, the function returns an FSM definition object with inputs, outputs,
states and transitions setup as given in the KISS2 file.

## EXAMPLE

    set fsmdef [read_kiss "bigfsm.kiss2"]
    check_fsm $fsmdef
    print_fsm $fsmdef

## TODO

[Kat94] p. 461ff uses labels for inputs und outputs and has spaces within
input signal vectors

## SEE ALSO

`create_fsm_definition`(1tr), `rename_fsm`(1tr), `rename_input`(1tr),
`rename_output`(1tr), `rename_state`(1tr), `write_encoding`(1tr)

*)
Procedure TTRFSMGenApp.ReadKiss(ObjC:Integer;ObjV:PPTcl_Object);
Var Obj      : TFSMDefinitionObj;
    Filename : String;
    T        : Text;
    Row      : Integer;
    St       : String;
    Tok      : TDynStringArray;
    NumInputs      : Integer;
    NumOutputs     : Integer;
    NumStates      : Integer;
    NumTransitions : Integer;
    ResetState     : String;
    EndOfFile      : Boolean;
    ObjSetup       : Boolean; // false if AddInput/AddOutput was not yet used
    I              : Integer;
    InputPatterns  : TDynStringArray;
Begin
  AssertParams(ObjC,2,ObjV,'filename');
  Filename := ObjV^[1].AsPChar;
  if not FileExists(Filename) then
    raise Exception.Create('Couldn''t open '+Filename);

  Obj := TFSMDefinitionObj.Create(ObjV^[1].AsPChar);
  NumInputs      := -1;
  NumOutputs     := -1;
  NumStates      := -1;
  NumTransitions := -1;
  ResetState     := '';
  EndOfFile      := false;
  ObjSetup       := false;
  SetLength(InputPatterns,1);   // we assume one pattern per transition at a time
  Assign(T,Filename);
  Reset(T);
  Row := 0;
  try
    While not EOF(T) do
      Begin
        ReadLn(T,St);
        Inc(Row);
        St := Trim(St);
        if St = '' then continue;
        if EndOfFile then
          raise Exception.CreateFmt('%s:%d: More lines after .e statement.',[Filename,Row]);
        // tokenize string by whitespace
        Tok := Split(St);
        if (Length(Tok) = 2) and (Tok[0][1] = '.') then
          Begin
            // 2-token lines
            Case Tok[0][2] of
              'i' : NumInputs      := StrToInt(Tok[1]);
              'o' : NumOutputs     := StrToInt(Tok[1]);
              's' : NumStates      := StrToInt(Tok[1]);
              'p' : NumTransitions := StrToInt(Tok[1]);
              'r' : ResetState     := Tok[1];
            else
              raise Exception.CreateFmt('%s:%d: Unknown KISS2 parameter ''%s''',[Filename,Row,Tok[0][2]]);
            End;
          End
        else if (Length(Tok) = 1) and (Tok[0][1] = '.') then
          Begin
            // 1-token lines
            Case Tok[0][2] of
              'e' : EndOfFile      := true;
            else
              raise Exception.CreateFmt('%s:%d: Unknown KISS2 parameter ''%s''',[Filename,Row,Tok[0][2]]);
            End;
          End
        else if Length(Tok) = 4 then
          Begin
            // --01 s1 s2 00010
            //   Tok[0]: input pattern, LSB is right most character
            //   Tok[1]: start state
            //   Tok[2]: next state
            //   Tok[3]: output pattern, LSB is right most character
            if not ObjSetup then
              Begin
                WriteLn('i: ',NumInputs,', o: ',NumOutputs,', s:',NumStates,', p: ',NumTransitions);
                if NumInputs < 0 then
                  raise Exception.Create('Missing .i statement to specify number of inputs');
                if NumOutputs < 0 then
                  raise Exception.Create('Missing .o statement to specify number of outputs');
                if NumStates < 0 then
                  raise Exception.Create('Missing .s statement to specify number of states');
                if NumTransitions < 0 then
                  raise Exception.Create('Missing .p statement to specify number of transitions');
                For I := 0 to NumInputs-1 do
                  Obj.FDef.AddInput('Input'+IntToStr(I));
                For I := 0 to NumOutputs-1 do
                  Obj.FDef.AddOutput('Output'+IntToStr(I));
                ObjSetup := true;
              End;
            if Obj.FDef.TryGetState(Tok[1]) < 0 then
              Obj.FDef.AddState(Tok[1]);
            if Obj.FDef.TryGetState(Tok[2]) < 0 then
              Obj.FDef.AddState(Tok[2]);
            // GetInputPatterns() and Add() require the input/output pattern
            // string with the first (LSB) input/output as the first character,
            // but the KISS file provides it as the right most character,
            // therefore reverse the strings.
            InputPatterns[0] := ReverseString(Tok[0]);
            Obj.FDef.Add(Tok[1],Obj.FDef.GetInputPatterns([ReverseString(Tok[0])]),Tok[2],ReverseString(Tok[3]));
          End
        else
          Begin
            raise Exception.CreateFmt('%s:%d: Invalid syntax',[Filename,Row]);
          End;
      End;
  Finally
    Close(T);
  End;
  if Obj.FDef.StateCount <> NumStates then
    raise Exception.CreateFmt('The file specified %d states, but %d were found in its transition table',[NumStates,Obj.FDef.StateCount]);
  if Obj.FDef.TransitionCount <> NumTransitions then
    raise Exception.CreateFmt('The file specified %d transitions, but the transition table specified %d',[NumTransitions,Obj.FDef.TransitionCount]);
  if (ResetState > '') and (Obj.FDef.TryGetState(ResetState) < 0) then
    raise Exception.CreateFmt('The specified reset state ''%s'' was not used by the transistion',[ResetState]);
  Obj.FDef.ResetState := ResetState;

  WriteLn('Successfully read ',Row,' lines.');
  FTCL.SetObjResult(Obj.TclObj);
End;

(*ronn
TODO
*)
Procedure TTRFSMGenApp.ReadILang(ObjC:Integer;ObjV:PPTcl_Object);
Var Filename   : String;
    ModuleName : String;
    Modules    : TModuleList;
    Obj        : TModuleObj;
    I          : Integer;
Begin
  // read_ilang filename module
  AssertParams(ObjC,3,ObjV,'filename module');
  Filename := ObjV^[1].AsPChar;
  if not FileExists(Filename) then
    raise Exception.Create('Couldn''t open '+Filename);
  ModuleName := ObjV^[2].AsString;

  // read ILang file
  Modules := TModuleList.Create;
  Modules.Add(FYosysFSMModule.FName,FYosysFSMModule);  // seed with our knowledge of the Yosys $fsm cell
  Parse(Filename,Modules);
  if Modules.IndexOf(ModuleName) < 0 then
    raise Exception.Create('ILang file '''+Filename+''' doesn''t contain a module named '''+ModuleName+'''');

  Obj := TModuleObj.Create(Modules[ModuleName]);
  Modules.Free;

  WriteLn('Successfully imported module ''',ModuleName,''' from ''',Filename,'''.');
  For I := 0 to Obj.FModule.FInstances.Count-1 do
    if Obj.FModule.FInstances.Data[I].FModule.FName = '$fsm' then
      WriteLn('  FSM Cell: ',Obj.FModule.FInstances.Data[I].FName);

  FTCL.SetObjResult(Obj.TclObj);
End;

(*ronn
TODO
return T???Obj(s) or instance name(s) as string
*)
Procedure TTRFSMGenApp.FindCells(ObjC:Integer;ObjV:PPTcl_Object);
Var ObjI        : Integer;
    FindOne     : Boolean;
    ReturnNames : Boolean;
    ModuleObj   : TModuleObj;
    RegEx       : String;
    I           : Integer;
    Cells       : Array of TInstance;
    List        : PTcl_Obj;
Begin
  // find_cells [-one] [-name] module -cell cell|-instance instance
  if (ObjC < 4) or (ObjC > 6) then
    raise Exception.Create('bad # arg: ' + ObjV^[0].AsPChar + ' [-one] [-name] module -cell cell|-instance instance');

  // defaults
  FindOne     := false;
  ReturnNames := false;
  // parse parameters
  ObjI := 1;
  While true do
    Begin
      if ObjV^[ObjI].AsPChar[0] <> '-' then break;
      if ObjV^[ObjI].AsPChar = '-one' then
        FindOne := true
      else if ObjV^[ObjI].AsPChar = '-name' then
        ReturnNames := true
      else
        raise Exception.Create('bad arg '''+ObjV^[ObjI].AsString+''': ' + ObjV^[0].AsPChar + ' [-one] [-name] module -cell cell|-instance instance');
      Inc(ObjI);
    End;

  AssertObjType(ObjV^[ObjI],TModuleObj);
  ModuleObj := TModuleObj.Get(ObjV^[ObjI]) as TModuleObj;

  SetLength(Cells,0);
  // find instances of cell
  RegEx := '^'+ObjV^[ObjI+2].AsString+'$';
  if ObjV^[ObjI+1].AsPChar = '-cell' then
    Begin
      // filter by cell type
      For I := 0 to ModuleObj.FModule.FInstances.Count-1 do
        if ExecRegExpr(RegEx,ModuleObj.FModule.FInstances.Data[I].FModule.FName) then
          Begin
            SetLength(Cells,Length(Cells)+1);
            Cells[Length(Cells)-1] := ModuleObj.FModule.FInstances.Data[I];
          End;
    End
  else if ObjV^[ObjI+1].AsPChar = '-instance' then
    Begin
      // filter by instance name
      For I := 0 to ModuleObj.FModule.FInstances.Count-1 do
        if ExecRegExpr(RegEx,ModuleObj.FModule.FInstances.Keys[I]) then
          Begin
            SetLength(Cells,Length(Cells)+1);
            Cells[Length(Cells)-1] := ModuleObj.FModule.FInstances.Data[I];
          End;
    End
  else
    raise Exception.Create('bad arg: ' + ObjV^[0].AsPChar + ' [-one] [-name] module -cell cell|-instance instance');

  if FindOne and (Length(Cells) <> 1) then
    raise Exception.CreateFmt('Requested exactly one cell but found %d',[Length(Cells)]);

  if FindOne then
    Begin
      if ReturnNames then
        FTCL.SetResult(Cells[0].FName)
      else
        FTCL.SetObjResult(TCellObj.Create(ModuleObj,Cells[0]).TclObj);
    End
  else
    Begin
      List := FTCL.NewListObj(0,Nil);
      if ReturnNames then
        Begin
          For I := 0 to Length(Cells)-1 do
            FTCL.ListObjAppendElement(List,Cells[I].FName);
        End
      else
        Begin
          For I := 0 to Length(Cells)-1 do
            FTCL.ListObjAppendElement(List,TCellObj.Create(ModuleObj,Cells[I]).TclObj);
        End;
      FTCL.SetObjResult(List);
    End;
End;

(*ronn
TODO
*)
Procedure TTRFSMGenApp.GetCellName(ObjC:Integer;ObjV:PPTcl_Object);
Var CellObj : TCellObj;
Begin
  // get_cell_name cell
  AssertParams(ObjC,2,ObjV,'cell');

  AssertObjType(ObjV^[1],TCellObj);
  CellObj := TCellObj.Get(ObjV^[1]) as TCellObj;

  FTCL.SetResult(CellObj.FCell.FName);
End;

Function ExtractFSMDefinition(FSM:TInstance) : TFSMDefinition;
Var I            : Integer;
    C            : TValueConcat;
    S            : TSignal;
    StateBits    : Integer;
    StateNum     : Integer;
    StateTable   : String;
    StateNumLog2 : Integer;
    TransNum     : Integer;
    TransWidth   : Integer;
    TransTable   : String;
    TransSt      : String;
    TransState   : String;
    TransInputs  : String;
    TransNext    : String;
    TransOutputs : String;
Begin
  if FSM.FModule.FName <> '$fsm' then
    raise Exception.Create('FSM cell type '''+FSM.FModule.FName+''' must be $fsm');

  // name
  Result := TFSMDefinition.Create((FSM.FGenericValues['NAME'] as TValueString).FValue);
  // inputs
  if FSM.FConnections['CTRL_IN'] is TValueConcat then
    Begin
      C := FSM.FConnections['CTRL_IN'] as TValueConcat;    // port CTRL_IN is connected with a concatenation of the individual input signals
      if C.FValues.Count <> (FSM.FGenericValues['CTRL_IN_WIDTH'] as TValueInteger).FValue then
        raise Exception.CreateFmt('$fsm has CTRL_IN_WIDTH = %d, but the input CTRL_IN is concatenated of %d signals',[(FSM.FGenericValues['CTRL_IN_WIDTH'] as TValueInteger).FValue,C.FValues.Count]);
      For I := 0 to C.FValues.Count-1 do
        Begin
          S := C.FValues.Items[I] as TSignal;
          Result.AddInput(S.FName);
        End;
    End
  else if FSM.FConnections['CTRL_IN'] is TSignal then
    Begin
      if 1 <> (FSM.FGenericValues['CTRL_IN_WIDTH'] as TValueInteger).FValue then
        raise Exception.CreateFmt('$fsm has CTRL_IN_WIDTH = %d, but the input CTRL_IN is a single signals',[(FSM.FGenericValues['CTRL_IN_WIDTH'] as TValueInteger).FValue]);
      S := FSM.FConnections['CTRL_IN'] as TSignal;
      Result.AddInput(S.FName);
    End
  else
    raise Exception.Create('Data type of $fsm CTRL_IN '''+FSM.FConnections['CTRL_IN'].ClassName+''' not supported.');
  // outputs
  if FSM.FConnections['CTRL_OUT'] is TValueConcat then
    Begin
      C := FSM.FConnections['CTRL_OUT'] as TValueConcat;   // port CTRL_OUT is connected with a concatenation of the individual output signals
      if C.FValues.Count <> (FSM.FGenericValues['CTRL_OUT_WIDTH'] as TValueInteger).FValue then
        raise Exception.CreateFmt('$fsm has CTRL_OUT_WIDTH = %d, but the output CTRL_OUT is concatenated of %d signals',[(FSM.FGenericValues['CTRL_OUT_WIDTH'] as TValueInteger).FValue,C.FValues.Count]);
      For I := 0 to C.FValues.Count-1 do
        Begin
          S := C.FValues.Items[I] as TSignal;
          Result.AddOutput(S.FName);
        End;
    End
  else if FSM.FConnections['CTRL_OUT'] is TSignal then
    Begin
      if 1 <> (FSM.FGenericValues['CTRL_OUT_WIDTH'] as TValueInteger).FValue then
        raise Exception.CreateFmt('$fsm has CTRL_OUT_WIDTH = %d, but the output CTRL_OUT is a single signals',[(FSM.FGenericValues['CTRL_OUT_WIDTH'] as TValueInteger).FValue]);
      S := FSM.FConnections['CTRL_OUT'] as TSignal;
      Result.AddOutput(S.FName);
    End
  else
    raise Exception.Create('Data type of $fsm CTRL_OUT '''+FSM.FConnections['CTRL_OUT'].ClassName+''' not supported.');
  // states
  StateBits  := (FSM.FGenericValues['STATE_BITS']  as TValueInteger).FValue;
  StateNum   := (FSM.FGenericValues['STATE_NUM']   as TValueInteger).FValue;
  StateTable := (FSM.FGenericValues['STATE_TABLE'] as TValueVector).FValue;
  if Length(StateTable) <> StateBits*StateNum then
    raise Exception.CreateFmt('$fsm has STATE_BITS = %d, STATE_NUM = %d, but length of STATE_TABLE is %d instead of %d',[StateBits,StateNum,Length(StateTable),StateBits*StateNum]);
  For I := 0 to StateNum-1 do
    Result.AddState(Copy(StateTable,1+(StateNum-1-I)*StateBits,StateBits));
  // reset state
  I := (FSM.FGenericValues['STATE_RST'] as TValueInteger).FValue;
  Result.SetResetState(Copy(StateTable,1+(StateNum-1-I)*StateBits,StateBits));
  // transitions
  StateNumLog2 := (FSM.FGenericValues['STATE_NUM_LOG2']   as TValueInteger).FValue;
  TransWidth   := StateNumLog2 + Result.InputCount + StateNumLog2 + Result.OutputCount;
  TransNum     := (FSM.FGenericValues['TRANS_NUM']   as TValueInteger).FValue;
  if FSM.FGenericValues['TRANS_TABLE'] is TValueVector then
    TransTable   := (FSM.FGenericValues['TRANS_TABLE'] as TValueVector).FValue
  else if FSM.FGenericValues['TRANS_TABLE'] is TValueInteger then
    TransTable   := IntToBin((FSM.FGenericValues['TRANS_TABLE'] as TValueInteger).FValue,TransWidth*TransNum)
  else
    raise Exception.Create('Data type of $fsm TRANS_TABLE '''+FSM.FConnections['TRANS_TABLE'].ClassName+''' not supported.');
  if Length(TransTable) <> TransWidth*TransNum then
    raise Exception.CreateFmt('$fsm has TRANS_NUM = %d, but length of TRANS_TABLE is %d instead of %d',[TransNum,Length(TransTable),TransWidth*TransNum]);
  For I := 0 to TransNum-1 do
    Begin
      TransSt := Copy(TransTable,1+(TransNum-1-I)*TransWidth,TransWidth);
      TransState   := Copy(TransSt,1,StateNumLog2);
      TransInputs  := Copy(TransSt,1+StateNumLog2,Result.InputCount);
      TransNext    := Copy(TransSt,1+StateNumLog2+Result.InputCount,StateNumLog2);
      TransOutputs := Copy(TransSt,1+StateNumLog2+Result.InputCount+StateNumLog2,Result.OutputCount);
      Result.Add(BinToInt(TransState),Result.GetInputPatterns([ReverseString(TransInputs)]),BinToInt(TransNext),ReverseString(TransOutputs));
    End;
End;

(*ronn
TODO
*)
Procedure TTRFSMGenApp.GetFSM(ObjC:Integer;ObjV:PPTcl_Object);
Var CellObj   : TCellObj;
    FSM       : TInstance;
    FSMDef    : TFSMDefinition;
    Obj       : TFSMDefinitionObj;
Begin
  // get_fsm cell
  AssertParams(ObjC,2,ObjV,'cell');
  AssertObjType(ObjV^[1],TCellObj);
  CellObj := TCellObj.Get(ObjV^[1]) as TCellObj;
  FSM := CellObj.FCell;
  if FSM.FModule.FName <> '$fsm' then
    raise Exception.Create('Cell '+FSM.FName+' is an instance of '+FSM.FModule.FName+' but should be $fsm');

  FSMDef := ExtractFSMDefinition(FSM);

  Obj := TFSMDefinitionObj.Create('dummy');
  Obj.FDef.Free;
  Obj.FDef := FSMDef;
  Obj.InvalidateString;

  FTCL.SetObjResult(Obj.TclObj);
End;

Procedure TTRFSMGenApp.RenameFsm(ObjC:Integer;ObjV:PPTcl_Object);
Begin
  WriteLn('TODO: implement');
End;

Procedure TTRFSMGenApp.RenameInput(ObjC:Integer;ObjV:PPTcl_Object);
Begin
  WriteLn('TODO: implement');
End;

Procedure TTRFSMGenApp.RenameOutput(ObjC:Integer;ObjV:PPTcl_Object);
Begin
  WriteLn('TODO: implement');
End;

Procedure TTRFSMGenApp.RenameState(ObjC:Integer;ObjV:PPTcl_Object);
Begin
  WriteLn('TODO: implement');
End;

(*ronn
check_fsm(1tr) -- Check consistency of an FSM definition object
===============================================================

## SYNPOSYS

`check_fsm` <fsmdef>

## DESCRIPTION

Checks the consistency of the given FSM definition object <fsmdef>. The
errors are printed on the screen.

The following checks are performed:

 - Check that all transition's State, NextState, input and output patterns
   are valid
 - Check that every state except the first state (= reset state) has at least
   one transition to enter
 - Check that every state has at least one transition to leave (optional)
 - Check that every state's leaving transitions cover all input combinations

## SEE ALSO

`print_fsm`(1tr)

*)
Procedure TTRFSMGenApp.CheckFsm(ObjC:Integer;ObjV:PPTcl_Object);
Var Errors : TDynStringArray;
    I      : Integer;
Begin
  Errors := AssertParamsFsmDef(ObjC,2,ObjV,'fsmdef').FDef.Check;
  if Length(Errors) = 0 then
    Begin
      WriteLn('No errors found.');
      Exit;
    End;
  For I := 0 to Length(Errors)-1 do
    WriteLn(Errors[I]);
End;

(*ronn
print_fsm(1tr) -- Print human-readable information of an FSM definition object
==============================================================================

## SYNPOSYS

`print_fsm` <fsmdef>

## DESCRIPTION

Prints the FSM definition object <fsmdef> as a human readable table.

## SEE ALSO

`check_fsm`(1tr)

*)
Procedure TTRFSMGenApp.PrintFsm(ObjC:Integer;ObjV:PPTcl_Object);
Var Def : TFSMDefinition;
Begin
  Def := AssertParamsFsmDef(ObjC,2,ObjV,'fsmdef').FDef;
  WriteLn('FSM ''',Def.Name,''':');
  Def.Print;
End;

(*ronn
TODO
*)
Procedure TTRFSMGenApp.PrintFsms(ObjC:Integer;ObjV:PPTcl_Object);
Var AppList : PTcl_Obj;
    NumApps : Integer;
    NoApps  : Boolean;    // true if we got a list instead of an associative array
    AppIdx  : Integer;
    AppName : PChar;
    FSMList : PTcl_Obj;
    NumFSMs : Integer;
    FSMIdx  : Integer;
    FSMObj  : PTcl_Obj;
    FSM     : TFSMDefinitionObj;
    I       : Integer;
    Table   : TTable;
    TextT   : TTextTable;
    Row     : Integer;
    Observed : TDynIntegerArray;
Begin
  // print_fsms multifsmdefs
  if ObjC <> 2 then
    raise Exception.Create('bad # arg: ' + ObjV^[0].AsPChar + ' multifsmdefs');
  // ObjV^[1] is an associative array (key: application name) of lists of FsmDef objects
  // get array keys (there is no better way :-( )
  // Attention: this only works with Tcl_EvalEx (but not Tcl_Eval!) and only
  // before Tcl_GetStringResult!
  // First, assume we got an associative array. Retrieve a list of its keys.
  if FTCL.Eval('array names {'+ObjV^[1].AsString+'}') <> TCL_OK then
    raise Exception.Create('Error acquiring array keys from array $'+ObjV^[1].AsString);
  AppList := FTCL.GetObjResult;
  NumApps := FTCL.ListObjLength(AppList);
  if NumApps = 0 then
    Begin
      // Empty list of keys, most probably we didn't get an associative array
      // but a simple list of FSMDef objects.
      NumFSMs := FTCL.ListObjLength(ObjV^[1].PObj);
      if NumFSMs <= 0 then
        raise Exception.Create('Unknown or empty array $'+ObjV^[1].AsString);
      NoApps  := true;
      NumApps := 1;   // just to get into the for loop below
    End
  else
    NoApps := false;
  Table := TTable.Create;
  if not NoApps then
    Table.SetVal(0, 0,'App');
  Table.SetVal(0, 1,'FSM');
  Table.SetVal(0, 2,'In');
  Table.SetVal(0, 3,'Out');
  Table.SetVal(0, 4,'State');
  Table.SetVal(0, 5,'Bits');
  Table.SetVal(0, 6,'Trans.');
  Table.SetVal(0, 7,'n0');
  Table.SetVal(0, 8,'n1');
  Table.SetVal(0, 9,'n2');
  Table.SetVal(0,10,'n3');
  Table.SetVal(0,11,'n4');
  Table.SetVal(0,12,'n5');
  Table.SetVal(0,13,'n6');
  Table.SetVal(0,14,'n7');
  Table.SetVal(0,15,'n8');
  Table.SetVal(0,16,'n9');
  Row := 1;
  For AppIdx := 0 to NumApps-1 do
    Begin
      if not NoApps then
        Begin
          AppName := Tcl_GetString(FTCL.ListObjIndex(AppList,AppIdx));
          Table.SetVal(Row,0,AppName);
          FSMList := FTCL.GetVarEx(ObjV^[1].AsString,AppName);
        End
      else
        FSMList := ObjV^[1].PObj;
      NumFSMs := FTCL.ListObjLength(FSMList);
      For FSMIdx := 0 to NumFSMs-1 do
        Begin
          FSMObj := FTCL.ListObjIndex(FSMList,FSMIdx);
          //WriteLn(AppName,'  $',IntToHex(PtrUInt(FSMObj),16));
          //WriteLn('  $',IntToHex(PtrUInt(FSMObj^.typePtr),16));
          FSM := (TTCLObj.Get(FSMObj) as TFSMDefinitionObj);
          Table.SetVal(Row, 1,FSM.FDef.Name);
          Table.SetVal(Row, 2,FSM.FDef.InputCount);
          Table.SetVal(Row, 3,FSM.FDef.OutputCount);
          Table.SetVal(Row, 4,FSM.FDef.StateCount);
          Table.SetVal(Row, 5,RoundUpLd(FSM.FDef.StateCount));
          Table.SetVal(Row, 6,FSM.FDef.TransitionCount);
          Observed := FSM.FDef.GetObserved;
          For I := 0 to 9 do
            if I < Length(Observed) then
              Table.SetVal(Row,7+I,Observed[I]);
          Inc(Row);
        End;
    End;
  // add "min" row
  Table.AddAggregateRow([1..Row-1],[2..16],High(Integer),@Min);
  Table.SetVal(Row,0,'Min');
  // add "max" row
  Table.AddAggregateRow([1..Row-1],[2..16],Low(Integer),@Max);
  Table.SetVal(Row+1,0,'Max');

  // format table
  TextT := TTextTable.Create(Table);
  For I := 2 to 16 do
    TextT.SetJustifyCol(I,jjRight);
  TextT.SetColSep(2,' | ');
  TextT.SetColSep(7,' | ');
  TextT.SetRowSep(1,'-');
  TextT.SetRowSep(Row,'-');
  WriteLn(TextT.GetTable);
  TextT.Free;
  Table.Free;
End;

(*ronn
TODO
*)
Procedure TTRFSMGenApp.WriteFsm(ObjC:Integer;ObjV:PPTcl_Object);
Var FsmDef    : TFSMDefinitionObj;
    OrigEnc   : Boolean;
    Format    : String;
    Filename  : String;
    St        : String;
    I         : Integer;
    Module    : TModule;
    Instance  : TInstance;
Begin
  // write_fsm fsmdef -format vhdl|verilog|ilang filename
  if (ObjC < 5) or (ObjC > 6) then
    raise Exception.Create('bad # arg: ' + ObjV^[0].AsPChar + ' fsmdef [-origenc] -format vhdl|verilog|ilang filename');
  AssertObjType(ObjV^[1],TFSMDefinitionObj);
  FsmDef := TFSMDefinitionObj.Get(ObjV^[1]) as TFSMDefinitionObj;
  I := 2;
  OrigEnc := False;   // default
  if ObjV^[I].AsPChar = '-origenc' then
    Begin
      OrigEnc := True;
      Inc(I);
    End;
  if ObjV^[I].AsPChar <> '-format' then
    raise Exception.Create('bad arg: ' + ObjV^[0].AsPChar + ' module -format vhdl|verilog|ilang filename');
  Inc(I);
  Format   := ObjV^[I].AsPChar;
  Filename := ObjV^[ObjC-1-0].AsPChar;  // last argument
  Case Format of
    'verilog' : raise Exception.Create('TODO: implement Verilog format');
    'vhdl'    : raise Exception.Create('TODO: implement VHDL format');
    'ilang'   : ;
    else
      raise Exception.Create('bad arg: ' + ObjV^[0].AsPChar + ' module -format vhdl|verilog|ilang filename');
  End;

  Module := TModule.Create('fsm_module');
  Module.AddPort(TPort.Create('Clk_i',    dirIn,TypeBit));
  Module.AddPort(TPort.Create('Reset_n_i',dirIn,TypeBit));
  Module.AddPort(TPort.Create('Input_i',  dirIn, 'std_logic_vector',TRange.Create(dirDown,FsmDef.FDef.InputCount-1, 0)));
  Module.AddPort(TPort.Create('Output_o', dirOut,'std_logic_vector',TRange.Create(dirDown,FsmDef.FDef.OutputCount-1,0)));
  Instance := Module.AddInstance(TInstance.Create('$fsm_instance',FYosysFSMModule));
  Instance.SetGeneric('NAME',          TValueString.Create('\fsm_name'));
  Instance.SetGeneric('CLK_POLARITY',  TValueBit.Create('1'));
  Instance.SetGeneric('ARST_POLARITY', TValueBit.Create('1'));
  Instance.SetGeneric('CTRL_IN_WIDTH', TValueInteger.Create(FsmDef.FDef.InputCount));
  Instance.SetGeneric('CTRL_OUT_WIDTH',TValueInteger.Create(FsmDef.FDef.OutputCount));
  Instance.SetGeneric('STATE_BITS',    TValueInteger.Create(RoundUpLd(FsmDef.FDef.StateCount)));
  Instance.SetGeneric('STATE_NUM',     TValueInteger.Create(FsmDef.FDef.StateCount));
  Instance.SetGeneric('STATE_NUM_LOG2',TValueInteger.Create(RoundUpLd(FsmDef.FDef.StateCount)));
  Instance.SetGeneric('STATE_RST',     TValueInteger.Create(FsmDef.FDef.GetState(FsmDef.FDef.ResetState)));
  St := '';
  For I := FsmDef.FDef.StateCount-1 downto 0 do
    if OrigEnc then
      Begin
        if not StrConsistsOf(FsmDef.FDef.State[I],['0','1']) then
          raise Exception.Create('Can''t use original encoding of state '''+FsmDef.FDef.State[I]+'''. It must be a digital value of ''0''s and ''1''s.');
        St := St + FsmDef.FDef.State[I];
      End
    else
      St := St + IntToBin(I,RoundUpLd(FsmDef.FDef.StateCount));  // don't use FsmDef.FDef.State[I] because this can be arbitrary strings
  Instance.SetGeneric('STATE_TABLE',   TValueVector.Create(Length(St),St));  // Length(St) = StateCount*StateWidth
  Instance.SetGeneric('TRANS_NUM',     TValueInteger.Create(FsmDef.FDef.TransitionCount));
  St := '';
  For I := FsmDef.FDef.TransitionCount-1 downto 0 do
    Begin
      St := St + IntToBin(FsmDef.FDef.Transition[I].State,RoundUpLd(FsmDef.FDef.StateCount));   // this is an index into STATE_TABLE
      if Length(FsmDef.FDef.Transition[I].Input) <> 1 then
        raise Exception.CreateFmt('Can''t translate transition %d, because it has %d input patterns. Only 1 pattern allowed.',[I,Length(FsmDef.FDef.Transition[I].Input)]);
      St := St + ReverseString(FsmDef.FDef.Transition[I].Input[0]);
      St := St + IntToBin(FsmDef.FDef.Transition[I].NextState,RoundUpLd(FsmDef.FDef.StateCount));   // this is an index into STATE_TABLE
      St := St + ReverseString(FsmDef.FDef.Transition[I].Output);
    End;
  Instance.SetGeneric('TRANS_TABLE',   TValueVector.Create(Length(St),St));
  Instance.ConnectPort('CLK',     Module.FPorts['Clk_i']);
  Instance.ConnectPort('ARST',    Module.FPorts['Reset_n_i']);
  Instance.ConnectPort('CTRL_IN', Module.FPorts['Input_i']);
  Instance.ConnectPort('CTRL_OUT',Module.FPorts['Output_o']);

  St := Module.GetILang;
  Module.Free;

  FilePutContents(Filename,St);
End;

(*ronn
create_trfsm(1tr) -- Create a TR-FSM object
===========================================

## SYNPOSYS

`create_trfsm` [-version <version>] <num_inputs> <num_outputs> <state_bits> <num_TR0> <num_TR1> <num_TR2> ...

## DESCRIPTION

Create a TR-FSM object using `create_trfsm`. You have to specify the number of
input and output signals with the parameters <num_inputs> and <num_outputs>
respectively. The number of state bits is specified with the parameter
<state_bits>.

The number of transition rows with given widths is specified by the final
parameters. <num_TR0> specifies the number of transition rows with no inputs,
<num_TR1> specifies the number of transition rows with one input, and so on.

Note: It doesn't make sense to specify TRs with more inputs than the TR-FSM.
The last <num_TRx> should be > 0.

## VERSIONS

The original TR-FSM which was manufactured with the SNOPS1 test chip is denoted
by the version 'SNOPS1'. This is the default. Martin Schmölzer extended the
TR-FSM by a configurable output register. This is version 'REG'.

## RETURNS

On success, the function returns a TR-FSM object. This is used by other
functions, which manipulate the object.

## EXAMPLE

    set trfsm [create_trfsm 10 10 5 8 8 8 8 6 2]
    set_fsm_definition $trfsm $fsmdef

## SEE ALSO

`create_fsm_definition`(1tr), `set_fsm_definition`(1tr), `map_input`(1tr),
`map_output`(1tr), `generate_bitstream`(1tr), `set_outputs_registered`(1tr)

*)
Procedure TTRFSMGenApp.CreateTrfsm(ObjC:Integer;ObjV:PPTcl_Object);
Var Obj    : TTCLObj;
    Version: String;
    ParOfs : Integer;
    I      : Integer;
    NumTRs : TDynIntegerArray;
Begin
  if ObjC < 5 then
    raise Exception.Create('bad # arg: ' + ObjV^[0].AsPChar + ' [-version version] in out state n0 n1 n2 ...');
  // create_trfsm [-version version] in out state n0 n1 n2 ...
  Version := 'SNOPS1';
  ParOfs  := 1;
  if ObjV^[1].AsString = '-version' then
    Begin
      Version := ObjV^[2].AsString;
      ParOfs  := 3;
    End;
  SetLength(NumTRs,ObjC-(ParOfs+3));
  For I := ParOfs+3 to ObjC-1 do
    NumTRs[I-(ParOfs+3)] := ObjV^[I].AsInteger(FTCL);
  if Length(NumTRs)-1 > ObjV^[ParOfs].AsInteger(FTCL) then
    WriteLn('Warning: You have specified transition rows with more inputs than the TR-FSM.');
  Obj := TTRFSMObj.Create(Version,ObjV^[ParOfs].AsInteger(FTCL),ObjV^[ParOfs+1].AsInteger(FTCL),ObjV^[ParOfs+2].AsInteger(FTCL),NumTRs);
  FTCL.SetObjResult(Obj.TclObj);
End;

Function AssertParamsTRFSM(ObjC,Num:Integer;ObjV:PPTcl_Object;Params:String) : TTRFSMObj;
Begin
  AssertParams(ObjC,Num,ObjV,Params);
  AssertObjType(ObjV^[1],TTRFSMObj);
  Result := TTRFSMObj.Get(ObjV^[1]) as TTRFSMObj;
End;

(*ronn
TODO
*)
Procedure TTRFSMGenApp.PrintTrfsm(ObjC:Integer;ObjV:PPTcl_Object);
Var Obj   : TTcl_Object;
    Spec  : TTRFSMSpecification;
    I,J   : Integer;
    Table : TTable;
    TextT : TTextTable;
    Row   : Integer;
Begin
  // print_trfsm trfsm|multitrfsm
  AssertParams(ObjC,2,ObjV,'trfsm|multitrfsm');

  Obj := ObjV^[1];
  if not TTRFSMObj.IsMyType(Obj) and
     not ((Obj.PObj^.typePtr <> Nil) and (Obj.PObj^.typePtr^.name = 'list')) then
    raise Exception.Create('bad arg: ' + ObjV^[0].AsPChar + ' trfsm|multitrfsm');

  Table := TTable.Create;
  Table.SetVal(0, 1,'In');
  Table.SetVal(0, 2,'Out');
  Table.SetVal(0, 3,'State');
  Table.SetVal(0, 4,'n0');
  Table.SetVal(0, 5,'n1');
  Table.SetVal(0, 6,'n2');
  Table.SetVal(0, 7,'n3');
  Table.SetVal(0, 8,'n4');
  Table.SetVal(0, 9,'n5');
  Table.SetVal(0,10,'n6');
  Table.SetVal(0,11,'n7');
  Table.SetVal(0,12,'n8');
  Table.SetVal(0,13,'n9');
  Table.SetVal(0,14,'nTR');
  Row := 1;

  if TTRFSMObj.IsMyType(Obj) then
    Begin
      // single TR-FSM
      Spec := (TTRFSMObj.Get(Obj) as TTRFSMObj).FTRFSM.Specification;
      Table.SetVal(Row,1,Spec.Inputs);
      Table.SetVal(Row,2,Spec.Outputs);
      Table.SetVal(Row,3,Spec.StateBits);
      For I := 0 to Spec.MaxTRWidth do
        Table.SetVal(Row,I+4,Spec.NumTRs[I]);
      Table.SetVal(Row,14,Spec.TotalTRs);
    End
  else
    Begin
      // list of TR-FSM
      For J := 0 to FTCL.ListObjLength(Obj.PObj)-1 do
        Begin
          Spec := (TTRFSMObj.Get(FTCL.ListObjIndex(Obj.PObj,J)) as TTRFSMObj).FTRFSM.Specification;
          Table.SetVal(Row,1,Spec.Inputs);
          Table.SetVal(Row,2,Spec.Outputs);
          Table.SetVal(Row,3,Spec.StateBits);
          For I := 0 to Spec.MaxTRWidth do
            Table.SetVal(Row,I+4,Spec.NumTRs[I]);
          Table.SetVal(Row,14,Spec.TotalTRs);
          Inc(Row);
        End;
      // add "min" row
      Table.AddAggregateRow([1..Row-1],[1..14],High(Integer),@Min);
      Table.SetVal(Row,0,'Min');
      // add "max" row
      Table.AddAggregateRow([1..Row-1],[1..14],Low(Integer),@Max);
      Table.SetVal(Row+1,0,'Max');
    End;

  // format table
  TextT := TTextTable.Create(Table);
  For I := 1 to 14 do
    TextT.SetJustifyCol(I,jjRight);
  TextT.SetRowSep(1,'-');
  if not TTRFSMObj.IsMyType(Obj) then
    TextT.SetRowSep(Row,'-');
  WriteLn(TextT.GetTable);
  TextT.Free;
  Table.Free;
End;

(*ronn
TODO
*)
Procedure TTRFSMGenApp.MapFsms(ObjC:Integer;ObjV:PPTcl_Object);
Var TRFSMList : PTcl_Obj;
    FSMList   : PTcl_Obj;
    ArrayName : String;
    FSMIdx    : Integer;
    TRFSMIdx  : Integer;
    FSM       : TFSMDefinition;
    TRFSM     : TTRFSM;
    Placed    : Boolean;
    Used      : Array of Boolean;    // index = TRFSM index
Begin
  // map_fsms trfsms fsms arrayname
  AssertParams(ObjC,4,ObjV,'trfsms fsms arrayname');

  TRFSMList := ObjV^[1].PObj;
  if (TRFSMList^.typePtr = Nil) or (TRFSMList^.typePtr^.name <> 'list') or
     (FTCL.ListObjLength(TRFSMList) <= 0) then
    raise Exception.Create('First argument must be a list of TRFSM objects');
  For TRFSMIdx := 0 to FTCL.ListObjLength(TRFSMList)-1 do
    if (FTCL.ListObjIndex(TRFSMList,TRFSMIdx)^.typePtr = Nil) or (FTCL.ListObjIndex(TRFSMList,TRFSMIdx)^.typePtr^.name <> 'trfsm') then
      raise Exception.Create('First argument must be a list of TRFSM objects');

  FSMList := ObjV^[2].PObj;
  if (FSMList^.typePtr = Nil) or (FSMList^.typePtr^.name <> 'list') or
     (FTCL.ListObjLength(FSMList) <= 0) then
    raise Exception.Create('Second argument must be a list of FSM objects');
  For FSMIdx := 0 to FTCL.ListObjLength(FSMList)-1 do
    if (FTCL.ListObjIndex(FSMList,FSMIdx)^.typePtr = Nil) or (FTCL.ListObjIndex(FSMList,FSMIdx)^.typePtr^.name <> 'fsm_definition') then
      raise Exception.Create('Second argument must be a list of FSM objects');

  if FTCL.ListObjLength(FSMList) > FTCL.ListObjLength(TRFSMList) then
    raise Exception.CreateFmt('You supplied %d FSMs but only %d TRFSMs',[FTCL.ListObjLength(FSMList),FTCL.ListObjLength(TRFSMList)]);

  ArrayName := ObjV^[3].AsString;

  { Check for every FSM definition to which TR-FSM it fits and then do the
    assignment }
  SetLength(Used,FTCL.ListObjLength(TRFSMList));
  For TRFSMIdx := 0 to FTCL.ListObjLength(TRFSMList)-1 do
    Used[TRFSMIdx] := False;
  For FSMIdx := 0 to FTCL.ListObjLength(FSMList)-1 do
    Begin
      FSM := (TFSMDefinitionObj.Get(FTCL.ListObjIndex(FSMList,FSMIdx)) as TFSMDefinitionObj).FDef;
      Placed := false;
      // go through TR-FSMs and search for a fitting TR-FSM
      // Important: We have to check the TR-FSM instances in order of
      // increasing size!
      For TRFSMIdx := 0 to FTCL.ListObjLength(TRFSMList)-1 do
        Begin
          TRFSM := (TTRFSMObj.Get(FTCL.ListObjIndex(TRFSMList,TRFSMIdx)) as TTRFSMObj).FTRFSM;
          if (not Used[TRFSMIdx]) and TRFSM.CheckDefinition(FSM) then
            Begin
              // map FSM to TR-FSM instance
              FTCL.SetVar(ArrayName,IntToStr(FSMIdx),IntToStr(TRFSMIdx));
              Used[TRFSMIdx] := true;
              Placed := true;
              break;
            End;
        End;
      if not Placed then
        raise Exception.CreateFmt('Can''t place FSM "%s" to any TR-FSM because no free instance of sufficient size is available.',[FSM.Name]);
    End;
End;

(*ronn
set_fsm_definition(1tr) -- Assign an FSM definition object to a TR-FSM object
=============================================================================

## SYNPOSYS

`set_fsm_definition` <trfsm> <fsmdef>

## DESCRIPTION

This command assigns an FSM definition object (as created by
`create_fsm_definition`(1tr) or `read_kiss`(1tr)) to a TR-FSM object (as
created by `create_trfsm`(1tr).

An error is issued, if the TR-FSM does not have enough resources to implement
the given FSM. These resources include the number of inputs, outputs,
possible states available with the state bits and the maximum number of
transitions.

## EXAMPLE

    set trfsm [create_trfsm 10 10 5 8 8 8 8 6 2]
    set_fsm_definition $trfsm $fsmdef

## SEE ALSO

`create_fsm_definition`(1tr)

*)
Procedure TTRFSMGenApp.SetFsmDefinition(ObjC:Integer;ObjV:PPTcl_Object);
Var TRFSM  : TTRFSMObj;
    FsmDef : TFSMDefinitionObj;
Begin
  // set_fsm_definition trfsm fsmdef
  TRFSM  := AssertParamsTRFSM(ObjC,3,ObjV,'trfsm fsmdef');
  AssertObjType(ObjV^[2],TFSMDefinitionObj);
  FsmDef := TFSMDefinitionObj.Get(ObjV^[2]) as TFSMDefinitionObj;
  TRFSM.FTRFSM.SetDefinition(TFSMDefinition.Create(FsmDef.FDef));  // use copy-constructor, because TTRFSM.Destroy Free()s the FSM definition object
End;

(*ronn
map_input(1tr) -- Map a logical FSM input to its physical location
==================================================================

## SYNPOSYS

`map_input` <trfsm> <logical> <physical>

## DESCRIPTION

The FSM definition object specifies logical inputs given by names while the
TR-FSM has one input signal vector with multiple single-bit signals. The
mapping of logical inputs of the FSM definition to the bit position in the
TR-FSM input vector is specified using `map_input`. Use this command for each
input of your FSM.

## EXAMPLES

    map_input $trfsm "Enable"        9
    map_input $trfsm "TimerOvfl"     8
    map_input $trfsm "I2CReady"      7
    map_input $trfsm "DiffTooLarge"  6

## SEE ALSO

`map_output`(1tr)

*)
Procedure TTRFSMGenApp.MapInput(ObjC:Integer;ObjV:PPTcl_Object);
Var TRFSM : TTRFSMObj;
Begin
  TRFSM := AssertParamsTRFSM(ObjC,4,ObjV,'trfsm logical physical');
  // map_input trfsm input num
  TRFSM.FTRFSM.MapInput(ObjV^[2].AsPChar,ObjV^[3].AsInteger(FTCL));
End;

(*ronn
map_output(1tr) -- Map a logical FSM output to its physical location
====================================================================

## SYNPOSYS

`map_output` <trfsm> <logical> <physical>

## DESCRIPTION

The FSM definition object specifies logical outputs given by names while the
TR-FSM has one output signal vector with multiple single-bit signals. The
mapping of logical outputs of the FSM definition to the bit position in the
TR-FSM output vector is specified using `map_output`. Use this command for each
output of your FSM.

## EXAMPLES

    map_output $trfsm "TimerPreset"   9
    map_output $trfsm "TimerEn"       8
    map_output $trfsm "I2CStart"      7
    map_output $trfsm "StoreNewValue" 6
    map_output $trfsm "AddOrSub"      5
    map_output $trfsm "CpuIntr"       4

## SEE ALSO

`map_input`(1tr)

*)
Procedure TTRFSMGenApp.MapOutput(ObjC:Integer;ObjV:PPTcl_Object);
Var TRFSM : TTRFSMObj;
Begin
  TRFSM := AssertParamsTRFSM(ObjC,4,ObjV,'trfsm logical physical');
  // map_output trfsm output num
  TRFSM.FTRFSM.MapOutput(ObjV^[2].AsPChar,ObjV^[3].AsInteger(FTCL));
End;

(*ronn
TODO

 - erzeugt:
    - neues TModule das statt dem $fsm eingesetzt werden kann (TRFSM-Wrapper-Modul)
    - darin Instanz von TRFSM (dafür TModule basteln)
 - welche Eingangs-Daten braucht es
    - TTRFSM-Objekt (bzw. TCL TTRFSMObj)
    - Modul--Namen für TRFSM-Wrapper-Modul

*)
Procedure TTRFSMGenApp.CreateTrfsmWrapper(ObjC:Integer;ObjV:PPTcl_Object);
Var I            : Integer;
    WrapperObj   : TWrapperObj;
    WrapCellName : String;
    Signal       : TSignal;
    C            : TValueConcat;
Begin
  // create_trfsm_wrapper trfsm cellname
  AssertParams(ObjC,3,ObjV,'trfsm cellname');
  WrapperObj := TWrapperObj.Create;

  AssertObjType(ObjV^[1],TTRFSMObj);
  WrapperObj.FTRFSM := (TTRFSMObj.Get(ObjV^[1]) as TTRFSMObj).FTRFSM;

  WrapCellName := ObjV^[2].AsString;

  // create wrapper module
  WrapperObj.FWrapperModule := TModule.Create(WrapCellName);
  WrapperObj.FWrapperModule.AddPort(TPort.Create('Reset_n_i',dirIn, TypeBit));
  WrapperObj.FWrapperModule.AddPort(TPort.Create('Clk_i',    dirIn, TypeBit));
  For I := 0 to WrapperObj.FTRFSM.Specification.Inputs-1 do
    WrapperObj.FWrapperModule.AddPort(TPort.Create('In'+IntToStr(I)+'_i',  dirIn, TypeBit));
  For I := 0 to WrapperObj.FTRFSM.Specification.Outputs-1 do
    WrapperObj.FWrapperModule.AddPort(TPort.Create('Out'+IntToStr(I)+'_o', dirOut,TypeBit));
  // Configuration interface (it is not necessary to add attributes here, because these are automatically assigned in FlowProc's TReconfApp.CheckNetlistExtracted)
  WrapperObj.FWrapperModule.AddPort(TPort.Create('CfgMode_i',    dirIn, TypeBit));
  WrapperObj.FWrapperModule.AddPort(TPort.Create('CfgClk_i',     dirIn, TypeBit));
  WrapperObj.FWrapperModule.AddPort(TPort.Create('CfgShift_i',   dirIn, TypeBit));
  WrapperObj.FWrapperModule.AddPort(TPort.Create('CfgDataIn_i',  dirIn, TypeBit));
  WrapperObj.FWrapperModule.AddPort(TPort.Create('CfgDataOut_o', dirOut,TypeBit));

  // instantiate TR-FSM inside the wrapper module
  WrapperObj.FTRFSMInstance := TInstance.Create('TRFSM_1',FTRFSMModule);
  WrapperObj.FWrapperModule.AddInstance(WrapperObj.FTRFSMInstance);
  WrapperObj.FTRFSMInstance.SetGeneric('InputWidth', TValueInteger.Create(WrapperObj.FTRFSM.Specification.Inputs));
  WrapperObj.FTRFSMInstance.SetGeneric('OutputWidth',TValueInteger.Create(WrapperObj.FTRFSM.Specification.Outputs));
  WrapperObj.FTRFSMInstance.SetGeneric('StateWidth', TValueInteger.Create(WrapperObj.FTRFSM.Specification.StateBits));
  WrapperObj.FTRFSMInstance.SetGeneric('UseResetRow',TValueInteger.Create(0));
  For I := 0 to 9 do
    if I <= WrapperObj.FTRFSM.Specification.MaxTRWidth then
      WrapperObj.FTRFSMInstance.SetGeneric('NumRows'+IntToStr(I),TValueInteger.Create(WrapperObj.FTRFSM.Specification.NumTRs[I]))
    else
      WrapperObj.FTRFSMInstance.SetGeneric('NumRows'+IntToStr(I),TValueInteger.Create(0));
  // connect Reset_n_i and Clk_i
  WrapperObj.FTRFSMInstance.ConnectPort('Reset_n_i',WrapperObj.FWrapperModule.FPorts['Reset_n_i']);
  WrapperObj.FTRFSMInstance.ConnectPort('Clk_i',    WrapperObj.FWrapperModule.FPorts['Clk_i']);
  // connect inputs
  C := TValueConcat.Create;
  For I := 0 to WrapperObj.FTRFSM.Specification.Inputs-1 do
    C.Add(WrapperObj.FWrapperModule.FPorts['In'+IntToStr(I)+'_i']);
  Signal := WrapperObj.FWrapperModule.AddSignal(TSignal.Create('Input_s',TType.Create('std_logic_vector',dirDown,WrapperObj.FTRFSM.Specification.Inputs-1, 0)));
  WrapperObj.FWrapperModule.AddAssignment(Signal,C);
  WrapperObj.FTRFSMInstance.ConnectPort('Input_i',Signal);
  // connect outputs
  Signal := WrapperObj.FWrapperModule.AddSignal(TSignal.Create('Output_s',TType.Create('std_logic_vector',dirDown,WrapperObj.FTRFSM.Specification.Outputs-1, 0)));
  WrapperObj.FTRFSMInstance.ConnectPort('Output_o',Signal);
  For I := 0 to WrapperObj.FTRFSM.Specification.Outputs-1 do
    WrapperObj.FWrapperModule.AddAssignment(
      WrapperObj.FWrapperModule.FPorts['Out'+IntToStr(I)+'_o'],
      TValueIndex.Create(Signal,TValueInteger.Create(I)));
  // connect config interfaces
  WrapperObj.FTRFSMInstance.ConnectPort('CfgMode_i',   WrapperObj.FWrapperModule.FPorts['CfgMode_i']);
  WrapperObj.FTRFSMInstance.ConnectPort('CfgClk_i',    WrapperObj.FWrapperModule.FPorts['CfgClk_i']);
  WrapperObj.FTRFSMInstance.ConnectPort('CfgShift_i',  WrapperObj.FWrapperModule.FPorts['CfgShift_i']);
  WrapperObj.FTRFSMInstance.ConnectPort('CfgDataIn_i', WrapperObj.FWrapperModule.FPorts['CfgDataIn_i']);
  WrapperObj.FTRFSMInstance.ConnectPort('CfgDataOut_o',WrapperObj.FWrapperModule.FPorts['CfgDataOut_o']);
  // connect dummy signals to scan interfaces
  Signal := WrapperObj.FWrapperModule.AddSignal(TSignal.Create('ScanEnable_s',TypeBit));
    WrapperObj.FTRFSMInstance.ConnectPort('ScanEnable_i',Signal);
    WrapperObj.FWrapperModule.AddAssignment(Signal,TValueBit.Create('0'));
  Signal := WrapperObj.FWrapperModule.AddSignal(TSignal.Create('ScanClk_s',TypeBit));
    WrapperObj.FTRFSMInstance.ConnectPort('ScanClk_i',Signal);
    WrapperObj.FWrapperModule.AddAssignment(Signal,TValueBit.Create('0'));
  Signal := WrapperObj.FWrapperModule.AddSignal(TSignal.Create('ScanDataIn_s',TypeBit));
    WrapperObj.FTRFSMInstance.ConnectPort('ScanDataIn_i',Signal);
    WrapperObj.FWrapperModule.AddAssignment(Signal,TValueBit.Create('0'));
  Signal := WrapperObj.FWrapperModule.AddSignal(TSignal.Create('ScanDataOut_s',TypeBit));
    WrapperObj.FTRFSMInstance.ConnectPort('ScanDataOut_o',Signal);

  // final checks
  WrapperObj.FTRFSMInstance.CheckGenerics;
  WrapperObj.FTRFSMInstance.CheckConnections;

  // done
  FTCL.SetObjResult(WrapperObj.TclObj);
End;

(*ronn
TODO
*)
Procedure TTRFSMGenApp.InsertTrfsmWrapper(ObjC:Integer;ObjV:PPTcl_Object);
Var CellObj      : TCellObj;
    YosysFSM     : TInstance;
    Module       : TModule;
    I,J          : Integer;
    FSMDef       : TFSMDefinition;
    WrapperObj   : TWrapperObj;
    WrapperInstance : TInstance;
    WrapInstName : String;
    Signal       : TSignal;
    C            : TValueConcat;
Begin
  // insert_trfsm_wrapper cell fsmdef wrapper instname
  AssertParams(ObjC,5,ObjV,'cell fsmdef wrapper instname');

  AssertObjType(ObjV^[1],TCellObj);
  CellObj := TCellObj.Get(ObjV^[1]) as TCellObj;

  YosysFSM := CellObj.FCell;
  if YosysFSM.FModule.FName <> '$fsm' then
    raise Exception.Create('Cell '+YosysFSM.FName+' is an instance of '+YosysFSM.FModule.FName+' but should be $fsm');

  Module := CellObj.FModuleObj.FModule;

  AssertObjType(ObjV^[2],TFSMDefinitionObj);
  FSMDef := (TFSMDefinitionObj.Get(ObjV^[2]) as TFSMDefinitionObj).FDef;

  AssertObjType(ObjV^[3],TWrapperObj);
  WrapperObj := TWrapperObj.Get(ObjV^[3]) as TWrapperObj;

  WrapInstName := ObjV^[4].AsString;

  // set_fsm_definition
  WrapperObj.FTRFSM.SetDefinition(TFSMDefinition.Create(FSMDef));  // use copy-constructor, because TTRFSM.Destroy Free()s the FSM definition object
  // map_input
  if YosysFSM.FConnections['CTRL_IN'] is TValueConcat then
    Begin
      C := YosysFSM.FConnections['CTRL_IN'] as TValueConcat;    // port CTRL_IN is connected with a concatenation of the individual input signals
      For I := 0 to C.FValues.Count-1 do
        Begin
          Signal := C.FValues.Items[I] as TSignal;         // C.FValues.Items[0] is MSB, Count-1 is LSB
          WriteLn('Mapping signal to input  ',I:2,': ',Signal.FName);
          WrapperObj.FTRFSM.MapInput(Signal.FName,I);
        End;
    End
  else if YosysFSM.FConnections['CTRL_IN'] is TSignal then
    Begin
      I := 0;
      Signal := YosysFSM.FConnections['CTRL_IN'] as TSignal;
      WriteLn('Mapping signal to input  ',I:2,': ',Signal.FName);
      WrapperObj.FTRFSM.MapInput(Signal.FName,I);
    End
  else
    raise Exception.Create('Data type of $fsm CTRL_IN '''+YosysFSM.FConnections['CTRL_IN'].ClassName+''' not supported.');
  // map_output
  if YosysFSM.FConnections['CTRL_OUT'] is TValueConcat then
    Begin
      C := YosysFSM.FConnections['CTRL_OUT'] as TValueConcat;   // port CTRL_OUT is connected with a concatenation of the individual output signals
      For I := 0 to C.FValues.Count-1 do
        Begin
          Signal := C.FValues.Items[I] as TSignal;         // C.FValues.Items[0] is MSB, Count-1 is LSB
          WriteLn('Mapping signal to output ',I:2,': ',Signal.FName);
          WrapperObj.FTRFSM.MapOutput(Signal.FName,I);
        End;
    End
  else if YosysFSM.FConnections['CTRL_OUT'] is TSignal then
    Begin
      I := 0;
      Signal := YosysFSM.FConnections['CTRL_OUT'] as TSignal;
      WriteLn('Mapping signal to output ',I:2,': ',Signal.FName);
      WrapperObj.FTRFSM.MapOutput(Signal.FName,I);
    End
  else
    raise Exception.Create('Data type of $fsm CTRL_OUT '''+YosysFSM.FConnections['CTRL_OUT'].ClassName+''' not supported.');

  // create instance of wrapper module and replace Yosys' $fsm cell with it
  WrapperInstance := TInstance.Create(WrapInstName,WrapperObj.FWrapperModule);
  I := Module.FArchBody.IndexOfData(YosysFSM);  // position of "old" $fsm instance in FArchBody map
  J := Module.FArchBody.Keys[I];                // SortID of "old" $fsm
  Module.FArchBody.Delete(I);                   // remove "old" $fsm

  Module.AddInstance(WrapperInstance,J);        // put new wrapper at same SortID

  // connect wrapper ports
  WrapperInstance.ConnectPort('Reset_n_i',YosysFSM.FConnections['ARST']);
  WrapperInstance.ConnectPort('Clk_i',    YosysFSM.FConnections['CLK']);
  // connect inputs, use zeros if TR-FSM is wider
  if YosysFSM.FConnections['CTRL_IN'] is TValueConcat then
    Begin
      C := YosysFSM.FConnections['CTRL_IN'] as TValueConcat;    // port CTRL_IN is connected with a concatenation of the individual input signals
      For I := 0 to WrapperObj.FTRFSM.Specification.Inputs-1 do
        if I < FSMDef.InputCount then
          WrapperInstance.ConnectPort('In'+IntToStr(I)+'_i',C.FValues.Items[C.FValues.Count-1-I] as TSignal)          // C.FValues.Items[0] is MSB, Count-1 is LSB
        else
          WrapperInstance.ConnectPort('In'+IntToStr(I)+'_i',TValueBit.Create('0'));
    End
  else if YosysFSM.FConnections['CTRL_IN'] is TSignal then
    Begin
      For I := 0 to WrapperObj.FTRFSM.Specification.Inputs-1 do
        if I = 0 then
          WrapperInstance.ConnectPort('In'+IntToStr(I)+'_i',YosysFSM.FConnections['CTRL_IN'] as TSignal)
        else
          WrapperInstance.ConnectPort('In'+IntToStr(I)+'_i',TValueBit.Create('0'));
    End
  else
    raise Exception.Create('Data type of $fsm CTRL_IN '''+YosysFSM.FConnections['CTRL_IN'].ClassName+''' not supported.');
  // connect outputs, use dummy signals if TR-FSM is wider
  if YosysFSM.FConnections['CTRL_OUT'] is TValueConcat then
    Begin
      C := YosysFSM.FConnections['CTRL_OUT'] as TValueConcat;   // port CTRL_OUT is connected with a concatenation of the individual output signals
      For I := 0 to WrapperObj.FTRFSM.Specification.Outputs-1 do
        if I < FSMDef.OutputCount then
          WrapperInstance.ConnectPort('Out'+IntToStr(I)+'_o',C.FValues.Items[C.FValues.Count-1-I] as TSignal)          // C.FValues.Items[0] is MSB, Count-1 is LSB
        else
          Begin
            Signal := Module.AddSignal(TSignal.Create(WrapInstName+'_Out'+IntToStr(I)+'_s', TypeBit));   // prefix with wrapper instance name so in case of multiple wrappers within one module those signal names don't collide
            WrapperInstance.ConnectPort('Out'+IntToStr(I)+'_o',Signal);
          End;
    End
  else if YosysFSM.FConnections['CTRL_OUT'] is TSignal then
    Begin
      For I := 0 to WrapperObj.FTRFSM.Specification.Outputs-1 do
        if I = 0 then
          WrapperInstance.ConnectPort('Out'+IntToStr(I)+'_o',YosysFSM.FConnections['CTRL_OUT'] as TSignal)          // C.FValues.Items[0] is MSB, Count-1 is LSB
        else
          Begin
            Signal := Module.AddSignal(TSignal.Create(WrapInstName+'_Out'+IntToStr(I)+'_s', TypeBit));   // prefix with wrapper instance name so in case of multiple wrappers within one module those signal names don't collide
            WrapperInstance.ConnectPort('Out'+IntToStr(I)+'_o',Signal);
          End;
    End
  else
    raise Exception.Create('Data type of $fsm CTRL_OUT '''+YosysFSM.FConnections['CTRL_OUT'].ClassName+''' not supported.');
  // replace "old" $fsm instance
  I := Module.FInstances.IndexOf(YosysFSM.FName); // position of "old" $fsm instance in FInstances map
  Module.FInstances.Delete(I);
  // connect dummy signals to config interfaces
  Signal := Module.AddSignal(TSignal.Create(WrapInstName+'_CfgMode_s',TypeBit));
    WrapperInstance.ConnectPort('CfgMode_i',Signal);
    Module.AddAssignment(Signal,TValueBit.Create('0'));
  Signal := Module.AddSignal(TSignal.Create(WrapInstName+'_CfgClk_s',TypeBit));
    WrapperInstance.ConnectPort('CfgClk_i',Signal);
    Module.AddAssignment(Signal,TValueBit.Create('0'));
  Signal := Module.AddSignal(TSignal.Create(WrapInstName+'_CfgShift_s',TypeBit));
    WrapperInstance.ConnectPort('CfgShift_i',Signal);
    Module.AddAssignment(Signal,TValueBit.Create('0'));
  Signal := Module.AddSignal(TSignal.Create(WrapInstName+'_CfgDataIn_s',TypeBit));
    WrapperInstance.ConnectPort('CfgDataIn_i',Signal);
    Module.AddAssignment(Signal,TValueBit.Create('0'));
  Signal := Module.AddSignal(TSignal.Create(WrapInstName+'_CfgDataOut_s',TypeBit));
    WrapperInstance.ConnectPort('CfgDataOut_o',Signal);

  // final checks
  WrapperInstance.CheckGenerics;
  WrapperInstance.CheckConnections;
End;

(*ronn
generate_bitstream(1tr) -- Generate the bitstream of a TR-FSM
=============================================================

## SYNPOSYS

`generate_bitstream` <trfsm> [-statemap simple|random_state|random_binary] [-seed <n>]

## DESCRIPTION

The command `generate_bitstream` performs all necessary steps to generate the
configuration bitstream of a TR-FSM.

The optional parameter `-statemap` specifies which mapping algorithm from the
states of the FSM definition to the binary state vector of the TR-FSM is used.

 * **simple** mapping simply increments the binary value of the TR-FSM state
   vector and uses the FSM definition states in the given order. If the FSM
   definition object hast 5 states, these are mapped to the TR-FSM binary
   values 0 to 4 in the same order as given in the FSM definition object.

 * **random_state** mapping randomizes the FSM definition states and maps them
   to an increasing binary value of the TR-FSM state. If the FSM definition
   object has 5 states, these are mapped to randomly to the TR-FSM binary
   values 0 to 4.

 * **random_binary** mapping randomizes the binary TR-FSM state value and maps
   them to the FSM definition states. If the TR-FSM has a 4 bit wide state
   vector FSM definition object has 5 states, these are mapped to random
   TR-FSM state values of 0 to 15.

All mappings take special care if a reset state was specified. This is always
mapped to the TR-FSM binary state encoding 0.

The optional parameter `-seed` specified the random seed used by the random_*
state mappers. It defaults to 0.

## RETURNS

On success, the function returns a bitstream object. Use the functions
`print_bitstream`(1tr) and `write_bitstream`(1tr) with this object.

## EXAMPLE

    set bs [generate_bitstream $trfsm]
    print_bitstream $bs
    print_bitstream $bs -trfsm
    write_bitstream $bs -format vhdl    "SensorFSM" "/tmp/bitstream.vhd"
    write_bitstream $bs -format verilog "SensorFSM" "/tmp/bitstream.v"
    write_bitstream $bs -format c       "SensorFSM" "/tmp/bitstream.h"
    write_bitstream $bs -format text    "SensorFSM" "/tmp/bitstream.txt"

## SEE ALSO

`print_bitstream`(1tr), `write_bitstream`(1tr)

*)
Procedure TTRFSMGenApp.GenerateBitstream(ObjC:Integer;ObjV:PPTcl_Object);
Var TRFSM : TTRFSMObj;
    BS    : TBitstream;
    Obj   : TBitstreamObj;
Begin
  // generate_bitstream <trfsm> [-statemap simple|random_state|random_binary] [-seed <n>]
  if not (ObjC in [2,4,6]) then
    raise Exception.Create('bad # arg: ' + ObjV^[0].AsPChar + ' <trfsm> [-statemap simple|random_state|random_binary] [-seed <n>]');
  AssertObjType(ObjV^[1],TTRFSMObj);
  TRFSM := TTRFSMObj.Get(ObjV^[1]) as TTRFSMObj;
  if ObjC = 4 then
    Begin
      // -statemap simple|random_state|random_binary
      if ObjV^[2].AsPChar <> '-statemap' then
        raise Exception.Create('bad arg: ' + ObjV^[0].AsPChar + ' <trfsm> [-statemap simple|random_state|random_binary] [-seed <n>]');
      case ObjV^[3].AsString of
        'simple'        : TRFSM.FTRFSM.StateMapper := smSimple;
        'random_state'  : TRFSM.FTRFSM.StateMapper := smRandomState;
        'random_binary' : TRFSM.FTRFSM.StateMapper := smRandomBinary;
      else
        raise Exception.Create('bad arg: ' + ObjV^[0].AsPChar + ' <trfsm> [-statemap simple|random_state|random_binary] [-seed <n>]');
      End;
    End;
  if ObjC = 6 then
    Begin
      // -statemap simple|random_state|random_binary
      if ObjV^[4].AsPChar <> '-seed' then
        raise Exception.Create('bad arg: ' + ObjV^[0].AsPChar + ' <trfsm> [-statemap simple|random_state|random_binary] [-seed <n>]');
      TRFSM.FTRFSM.MapperSeed := ObjV^[5].AsInteger(FTCL);
    End;

  // generate_bitstream trfsm
  TRFSM.FTRFSM.Map;
  TRFSM.FTRFSM.PrintMapping;
  TRFSM.FTRFSM.GenerateBitstream;

  BS  := TRFSM.FTRFSM.GetBitstream(0);
  if not assigned(BS) then
    raise Exception.Create('Error during generation of the bitstream.');
  Obj := TBitstreamObj.Create(BS);
  FTCL.SetObjResult(Obj.TclObj);
End;

(*ronn
print_bitstream(1tr) -- Print a bitstream to the screen
=======================================================

## SYNPOSYS

`print_bitstream` <bitstream> [-trfsm]

## DESCRIPTION

Print the bitstream on screen as string of '0's and '1's. The first character
on screen is also the first bit which has to be shifted into the TR-FSM
configuration chain.

The optional parameter `-trfsm` groups the bits as stored in the individual
TR-FSM components (state selection gate, input switching matrix, input pattern
gate, next state register, output pattern gate). Note that in this
representation, the bits are printed reversed.

## EXAMPLE

See `generate_bitstream`(1tr) for an example.

## SEE ALSO

`generate_bitstream`(1tr), `write_bitstream`(1tr)

*)
Procedure TTRFSMGenApp.PrintBitstream(ObjC:Integer;ObjV:PPTcl_Object);
Var Bitstream : TBitstreamObj;
Begin
  // print_bitstream bitstream [-trfsm]
  if ObjC < 2 then
    raise Exception.Create('bad # arg: ' + ObjV^[0].AsPChar + ' bitstream [-trfsm]');
  AssertObjType(ObjV^[1],TBitstreamObj);
  Bitstream := TBitstreamObj.Get(ObjV^[1]) as TBitstreamObj;

  if ObjC = 3 then
    Begin
      if ObjV^[2].AsPChar = '-trfsm' then
        Begin
          if not (Bitstream.FBitstream is TTRFSMBitstream) then
            raise Exception.Create('Parameter -trfsm only valid for TRFSM bitstreams');
          WriteLn((Bitstream.FBitstream as TTRFSMBitstream).GetTRFSMString);
        End
      else
      raise Exception.Create('bad arg: ' + ObjV^[0].AsPChar + ' bitstream [-trfsm]');
    End
  else
    Begin
      WriteLn(Bitstream.FBitstream.GetString);
    End;
End;

Function AssertParamsBitstream(ObjC,Num:Integer;ObjV:PPTcl_Object;Params:String) : TBitstreamObj;
Begin
  AssertParams(ObjC,Num,ObjV,Params);
  AssertObjType(ObjV^[1],TBitstreamObj);
  Result := TBitstreamObj.Get(ObjV^[1]) as TBitstreamObj;
End;

(*ronn
write_bitstream(1tr) -- Write a bitstream to a file
===================================================

## SYNPOSYS

`write_bitstream` <bitstream> -format vhdl|verilog|c|text|modelsim|lec|formality [options] <prefix> <filename>

## DESCRIPTION

Save the bitstream to a file on disk. Multipe formats are supported, see below.

*Warning:* The destination file is overwritten without warning!

## FORMATS

 * **VHDL:** A VHDL file fragment is written, which declares two constants.
   The first constant `<prefix>Length` is an integer an specifies the number of
   configuration bits. The second constant `<prefix>Cfg` is a `std_logic_vector`
   with all configuration bits. Its bit 0 is to be shifted into the TR-FSM
   config chain first.

 * **Verilog:** A Verilog file fragment is written, which declares two
   `localparam`s. The file can be `'include`d from your testbench. The first
   localparam `<prefix>_SIZE` specifies the number of configuration bits. The
   second localparam `<prefix>_VAL` is a vector with all configuration bits.
   Its bit 0 is to be shifted into the TR-FSM config chain first.

 * **C:** A C file fragment is written. The declared variable uses the `struct
   TConfigChainData`, which is declared in cfgintf.h. This file should be
   #include'd before the C file fragments. Note that the `struct` uses the
   types `uint32_t` and `uint8_t`, therefore you should `#include <stdint.h>`.

   The bitstream is specified as initialized struct named `Cfg<prefix>`. Its
   field `Length` specifies the number of bits in the bitstream. The field
   `Data` is an array of bytes. These should be shifted into the TR-FSM
   config chain LSB first, starting with the byte at index 0.

 * **Text:** A text file is written with the bitstream as string of '0's and
   '1's. The first character in the file is also the first bit which has to be
   shifted into the TR-FSM configuration chain. This is identical to the output
   of `print_bitstream`(1tr).

 * **ModelSim:** A .do file for Mentor ModelSim/QuestaSim is created which
   performs `force -freeze` on all config bits of the TR-FSM. This is handy if
   you don't want to waste simulation time with shifting in the configuration
   bitstream or if you don't (yet) have a global configuration infrastructure
   to reach the TR-FSM's config interface. Use the `<prefix>` to specify the
   full path of the TR-FSM instance, e.g.
   '/sensorfsm_tb/DUT/OnlySensorFSM_1/TRFSM_1'.

 * **LEC:** A .do file for Cadence Conformal LEC Logical Equivalence Checking
   is created which performs `add instance constraint` on all config bits of
   the TR-FSM. This allows checking the equivalence of the TR-FSM design with
   a hand-coded state machine. Use the `<prefix>` to specify the full path to
   the TR-FSM instance, e.g. '/OnlySensorFSM_1/TRFSM_1'. Include the created
   file with `dofile <filename>` directly after all `read design` commands.
   The optional `options` are directly appended to each `add instance
   constraint` line.

 * **Formality:** A TCL file for Synopsys Formality is created which performs
   `set_constant` on all config bits of the TR-FSM. This allows checking the
   equivalence of the TR-FSM design with a hand-coded state machine. Use the
   `<prefix>` to specify the full path to the TR-FSM instance, e.g.
   'i:/WORK/SensorFSM/PureSensorFSM_1/TRFSM_1'. Use `source <filename>` after
   `set_top i:/WORK/...` but before `match`. Other Formality commands related
   to this script are `report_constants` and `remove_constant`.

## EXAMPLE

See `generate_bitstream`(1tr) for an example.

## SEE ALSO

`generate_bitstream`(1tr), `print_bitstream`(1tr), `write_encoding`(1tr)

*)
Procedure TTRFSMGenApp.WriteBitstream(ObjC:Integer;ObjV:PPTcl_Object);
Var Bitstream : TBitstreamObj;
    I         : Integer;
    Format    : String;
    Prefix    : String;
    St        : String;
    Filename  : String;
Begin
  // write_bitstream bitstream -format vhdl|verilog|c|text|modelsim|lec|formality [options] prefix filename
  if ObjC < 6 then
    raise Exception.Create('bad # arg: ' + ObjV^[0].AsPChar + ' bitstream -format vhdl|verilog|c|text|modelsim|lec|formality [options] prefix filename');
  AssertObjType(ObjV^[1],TBitstreamObj);
  Bitstream := TBitstreamObj.Get(ObjV^[1]) as TBitstreamObj;
  if ObjV^[2].AsPChar <> '-format' then
    raise Exception.Create('bad arg: ' + ObjV^[0].AsPChar + ' bitstream -format vhdl|verilog|c|text|modelsim|lec|formality [options] prefix filename');
  Format   := ObjV^[3].AsPChar;
  Prefix   := ObjV^[ObjC-1-1].AsPChar;  // second-last argument
  Filename := ObjV^[ObjC-1-0].AsPChar;  // last argument
  Case Format of
    'vhdl': Begin
              St := '  constant ' + Prefix + 'Length : integer := ' + IntToStr(Bitstream.FBitstream.Count) + ';' + ^J +
                    '  constant ' + Prefix + 'Cfg    : std_logic_vector(' + Prefix + 'Length-1 downto 0) := "' + Bitstream.FBitstream.GetStringReverse + '";' + ^J;
            End;
    'verilog' : Begin
              St := 'localparam ' + Prefix + '_SIZE = ' + IntToStr(Bitstream.FBitstream.Count) + ';' + ^J +
                    'localparam ' + Prefix + '_VAL  = ' + IntToStr(Bitstream.FBitstream.Count) + '''b' + Bitstream.FBitstream.GetStringReverse + ';' + ^J;
            End;
    'c':    Begin
              St := 'const TConfigChainData Cfg'+Prefix+' = {' + ^J +
                    '  Length: '+ IntToStr(Bitstream.FBitstream.Count) +',' + ^J +
                    '  Data: {' + ^J +
                    Indent(Bitstream.FBitstream.GetCArray,4) +  ^J +
                    '  }' + ^J +
                    '};' + ^J;
            End;
    'text': Begin
              St := Bitstream.FBitstream.GetString + ^J;
            End;
    'modelsim': Begin
              St := (Bitstream.FBitstream as TTRFSMBitstream).GetModelSimTCLString(Prefix);
            End;
    'lec': Begin
              // collect options
              St := '';
              For I := 4 to ObjC-1-1-1 do
                St := St + ' ' + ObjV^[I].AsPChar;
              // leading ' ' is required by GetLEC4TRFSM
              St := (Bitstream.FBitstream as TTRFSMBitstream).GetLECString(Prefix,St);
            End;
    'formality': Begin
              St := (Bitstream.FBitstream as TTRFSMBitstream).GetFormalityString(Prefix);
            End;
    else
      raise Exception.Create('bad arg: ' + ObjV^[0].AsPChar + ' bitstream -format vhdl|verilog|c|text|modelsim|lec|formality [options] filename');
  End;
  FilePutContents(Filename,St);
End;

(*
.fromstates SPI_FSM_State_reg[3] SPI_FSM_State_reg[2] SPI_FSM_State_reg[1] SPI_FSM_State_reg[0]
.tostates State_o_reg[4] State_o_reg[3] State_o_reg[2] State_o_reg[1] State_o_reg[0]
.begin
0000 00000
0001 00001
0010 00101
0011 00110
0100 00011
0101 00100
0110 00111
0111 01000
1000 00010
.end
*)
Function WriteLECEncoding(TRFSM:TTRFSM;FromRegName:String) : String;
  Function GetFromStateWidth : Integer;
  Var I : Integer;
  Begin
    Result := 0;
    For I := 0 to Length(TRFSM.StateMap)-1 do
      Result := Max(Result,Length(TRFSM.StateMap[I]));
  End;

Var I : Integer;
Begin
  // check if TRFSM is already mapped
  // TODO
  // From States
  Result := '.fromstates';
  For I := GetFromStateWidth-1 downto 0 do
    Result := Result + ' ' + FromRegName + '[' + IntToStr(I) + ']';
  Result := Result + ^J;
  // To States
  Result := Result + '.tostates';
  For I := TRFSM.Specification.StateBits-1 downto 0 do
    Result := Result + ' State_o_reg[' + IntToStr(I) + ']';
  Result := Result + ^J;
  // Encodings
  Result := Result + '.begin' + ^J;
  For I := 0 to Length(TRFSM.StateMap)-1 do
    Begin
      if TRFSM.StateMap[I] = '' then
        continue;
      Result := Result + TRFSM.StateMap[I] + ' ' + IntToBin(I,TRFSM.Specification.StateBits) + ^J;
    End;

  Result := Result + '.end' + ^J;
End;

(*ronn
write_encoding(1tr) -- Write FSM encoding to a file
===================================================

## SYNPOSYS

`write_encoding` <trfsm> -format lec [options] <filename>

## DESCRIPTION

Write out the TR-FSM encoding to a file on disk. Multipe formats are supported,
see below.

*Important:* This command only works with a mapped TR-FSM, i.e. it is only
valid after `generate_bitstream`(1tr) was executed.

*Warning:* The destination file is overwritten without warning!

## FORMATS

 * **LEC:** A file for Cadence Conformal LEC Logical Equivalence Checking
   is created which can be used with the command `read fsm encoding`. The file
   contains a mapping from the "original" state encoding as used by the FSM
   definition object to the actual state vector values of the TR-FSM.

   Note that the "original" state encoding is simply the name of the state
   as given to the `add_state`(1tr) or as found in a KISS file by
   `read_kiss`(1tr). Therefore it is necessary that the KISS export doesn't set
   custom state names (e.g. 's0', 's1', ...) but uses the original state
   encoding, e.g. as found in the synthesized Verilog file.

## EXAMPLE

The following example reads a KISS file, creates a TR-FSM and writes out the
bitstream for Cadence Conformal LEC as well as the FSM encoding information
file.

    set fsmdef [read_kiss "bigfsm.kiss2"]
    check_fsm $fsmdef
    print_fsm $fsmdef

    set trfsm [create_trfsm 10 10 5 5 5 5 5 5]
    set_fsm_definition $trfsm $fsmdef
    for { set i 0 } { $i < $NumInputs } { incr i } {
      map_input $trfsm Input$i $i
    }
    for { set i 0 } { $i < $NumOutputs } { incr i } {
      map_output $trfsm Output$i $i
    }
    set bs [generate_bitstream $trfsm]
    write_bitstream $bs -format lec -revised /TRFSM_1 bitstream-lec.do
    write_encoding $trfsm -format lec FSM_State_reg encoding-lec.txt

## SEE ALSO

`generate_bitstream`(1tr), `add_state`(1tr), `read_kiss`(1tr),
`write_bitstream`(1tr)

*)
Procedure TTRFSMGenApp.WriteEncoding(ObjC:Integer;ObjV:PPTcl_Object);
Var TRFSM    : TTRFSMObj;
    Format   : String;
    Filename : String;
    St       : String;
Begin
  // write_encoding <trfsm> -format lec [options] filename
  if ObjC < 5 then
    raise Exception.Create('bad # arg: ' + ObjV^[0].AsPChar + ' <trfsm> -format lec [options] filename');
  AssertObjType(ObjV^[1],TTRFSMObj);
  TRFSM := TTRFSMObj.Get(ObjV^[1]) as TTRFSMObj;
  if ObjV^[2].AsPChar <> '-format' then
    raise Exception.Create('bad arg: ' + ObjV^[0].AsPChar + ' <trfsm> -format lec [options] filename');
  Format   := ObjV^[3].AsPChar;
  Filename := ObjV^[ObjC-1-0].AsPChar;  // last argument
  Case Format of
    'lec' : Begin
        St := WriteLECEncoding(TRFSM.FTRFSM,ObjV^[4].AsPChar);
      End;
    else
      raise Exception.Create('bad arg: ' + ObjV^[0].AsPChar + ' <trfsm> -format lec [options] filename');
  End;
  FilePutContents(Filename,St);
End;

Procedure TTRFSMGenApp.WriteModule(ObjC:Integer;ObjV:PPTcl_Object);
Var ModuleObj : TModuleObj;
    Format    : String;
    Filename  : String;
    St        : String;
    Module    : TModule;
Begin
  // write_module module -format vhdl|verilog|ilang filename
  AssertParams(ObjC,5,ObjV,'module -format vhdl|verilog|ilang filename');
  AssertObjType(ObjV^[1],TModuleObj);
  ModuleObj := TModuleObj.Get(ObjV^[1]) as TModuleObj;
  if ObjV^[2].AsPChar <> '-format' then
    raise Exception.Create('bad arg: ' + ObjV^[0].AsPChar + ' module -format vhdl|verilog|ilang filename');
  Format   := ObjV^[3].AsPChar;
  Filename := ObjV^[ObjC-1-0].AsPChar;  // last argument
  Case Format of
    'verilog' : St := ModuleObj.FModule.WriteVerilogDeclaration;
    'vhdl'    : St := ModuleObj.FModule.GetVHDL;
    'ilang'   : Begin Module := Synthesize(ModuleObj.FModule); St := Module.GetILang; Module.Free; End;
    else
      raise Exception.Create('bad arg: ' + ObjV^[0].AsPChar + ' module -format vhdl|verilog|ilang filename');
  End;
  FilePutContents(Filename,St);
End;

Procedure TTRFSMGenApp.WriteTrfsmWrapper(ObjC:Integer;ObjV:PPTcl_Object);

  Function GetFlowTCL(WrapperObj:TWrapperObj) : String;
  Var I : Integer;
  Begin
    Result := '#!flow' + LineEnding;
    Result += '#' + LineEnding;
    Result += '# Setup TR-FSM wrapper cell '+WrapperObj.FWrapperModule.FName + LineEnding;
    Result += '#' + LineEnding;
    Result += '# Auto-generated by trfsmgen';
    if FTCL.Eval('info script') = TCL_OK then
      Result += ' in script ' + FTCL.GetStringResult;
    Result += LineEnding;
    Result += '#' + LineEnding;
    Result += LineEnding;
    Result += 'puts "################################################################################"' + LineEnding;
    Result += 'puts "## Setup TR-FSM Wrapper Cell '+WrapperObj.FWrapperModule.FName+'"' + LineEnding;
    Result += LineEnding;
    Result += 'create_cell "'+WrapperObj.FWrapperModule.FName+'"' + LineEnding;
    Result += LineEnding;
    Result += 'puts "## Adding ports"' + LineEnding;
    Result += 'cell_add_port "Reset_n_i" -map "Reset_n_i"' + LineEnding;
    Result += 'cell_add_port "Clk_i"     -map "Clk_i"' + LineEnding;
    Result += '# Configuration interface' + LineEnding;
    Result += 'cell_add_config_chain -length '+IntToStr(WrapperObj.FTRFSM.BitstreamLength)+' -mode "CfgMode_i" -clk "CfgClk_i" -shift "CfgShift_i" -datain "CfgDataIn_i" -dataout "CfgDataOut_o"' + LineEnding;
    Result += '# Inputs' + LineEnding;
    For I := 0 to WrapperObj.FTRFSM.Specification.Inputs-1 do
      Result += 'cell_add_port "In'+IntToStr(I)+'_i"      -in  -conntype "Bit"' + LineEnding;
    Result += '# Outputs' + LineEnding;
    For I := 0 to WrapperObj.FTRFSM.Specification.Outputs-1 do
      Result += 'cell_add_port "Out'+IntToStr(I)+'_o"     -out -conntype "Bit"' + LineEnding;
  End;

Var WrapperObj : TWrapperObj;
    Format     : String;
    Filename   : String;
    St         : String;
    Module     : TModule;
Begin
  // write_trfsm_wrapper wrapper -format vhdl|verilog|ilang|flowtcl filename
  AssertParams(ObjC,5,ObjV,'wrapper -format vhdl|verilog|ilang|flowtcl filename');
  AssertObjType(ObjV^[1],TWrapperObj);
  WrapperObj := TWrapperObj.Get(ObjV^[1]) as TWrapperObj;
  if ObjV^[2].AsPChar <> '-format' then
    raise Exception.Create('bad arg: ' + ObjV^[0].AsPChar + ' wrapper -format vhdl|verilog|ilang|flowtcl filename');
  Format   := ObjV^[3].AsPChar;
  Filename := ObjV^[ObjC-1-0].AsPChar;  // last argument
  Case Format of
    'verilog' : St := WrapperObj.FWrapperModule.WriteVerilogDeclaration;
    'vhdl'    : St := WrapperObj.FWrapperModule.GetVHDL;
    'ilang'   : Begin Module := Synthesize(WrapperObj.FWrapperModule); St := Module.GetILang; Module.Free; End;
    'flowtcl' : St := GetFlowTCL(WrapperObj);
  else
    raise Exception.Create('bad arg: ' + ObjV^[0].AsPChar + ' wrapper -format vhdl|verilog|ilang|flowtcl filename');
  End;
  FilePutContents(Filename,St);
End;

(*****************************************************************************)
(***  Main Program  **********************************************************)
(*****************************************************************************)

Var TRFSMGenApp : TTRFSMGenApp;
    RunScripts  : Boolean;
    BatchMode   : Boolean;
    SymLookup   : Boolean;
    ExitStatus  : Integer;

Procedure Usage(ExitCode:Byte);
Begin
  WriteLn('TRFSMGen');
  WriteLn;
  WriteLn('Usage: trfsmgen [-h|--help] [-b] [-f filename] [-c string] [-n] [-s]');
  WriteLn;
  WriteLn('  -h, --help   Print a usage information and exit.');
  WriteLn;
  WriteLn('  -b           Batch mode. trfsmgen will not wait for user input but quit');
  WriteLn('               directly after the startup scripts, the scripts given with -f');
  WriteLn('               parameters and the commands given with -c parameters were');
  WriteLn('               executed.');
  WriteLn;
  WriteLn('  -f filename  Execute the Tcl script in filename at program start.');
  WriteLn;
  WriteLn('  -c string    Execute the commands given in string.');
  WriteLn;
  WriteLn('  -n           Do not execute the start scripts (/etc/trfsmgen/... TODO )');
  WriteLn;
  WriteLn('  -s           Do not perform (slow) symbol lookup for stack trace.');
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
  Code := TRFSMGenApp.FTCL.Eval(St);

  // handle result
  if (Code <> TCL_OK) then
    Write('Error: ');
  St := TRFSMGenApp.FTCL.GetStringResult;   // use 'St' to avoid additional variable
  if St > '' then
    WriteLn(St);

  // handle if 'exit' was called
  ExitStatus := TRFSMGenApp.FCmdLine.ExitStatus;
  if (ExitStatus > 0) or (Code <> TCL_OK) then
    raise Exception.Create(St);
End;

Procedure EvalFile(St:String);
Var Code : Integer;
Begin
  // execute script file
  Code := TRFSMGenApp.FTCL.EvalFile(St);

  // handle result
  if (Code <> TCL_OK) then
    Write('Error: ');
  St := TRFSMGenApp.FTCL.GetStringResult;   // use 'St' to avoid additional variable
  if St > '' then
    WriteLn(St);

  // handle if 'exit' was called
  ExitStatus := TRFSMGenApp.FCmdLine.ExitStatus;
  if (ExitStatus > 0) or (Code <> TCL_OK) then
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
      else if ParamStr(I) = '-s' then
        // don't do symbol lookup at stack trace
        SymLookup := false
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
  // run /etc/trfsmgen/trfsmgenrc
  EvalFileIfExists('/etc/trfsmgen/trfsmgenrc');
  // run /etc/trfsmgen/trfsmgenrc.d/*.tcl
  List := FileSearchGlobList('/etc/trfsmgen/trfsmgenrc.d/*.tcl');
  List.Sorted := true;  // automatically sorts the list
  for St in List do
    Begin
      //WriteLn('Executing ',St);
      EvalFile(St);
      Flush(Output);
    End;
  // run ~/.trfsmgenrc
  EvalFileIfExists(fpGetEnv('HOME')+'/.trfsmgenrc');
End;

Begin
  TRFSMGenApp := TTRFSMGenApp.Create;
  // initialize default values
  ExitStatus := 0;
  RunScripts := true;
  SymLookup  := true;
  BatchMode  := false;
  // parse parameters to set variables
  ParseParams(0);
  // handle settings
  if SymLookup then
    SetupStackTrace;
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
          TRFSMGenApp.Help(0,Nil);
          Flush(Output);
          ExitStatus := TRFSMGenApp.Run;
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
    TRFSMGenApp.Free;
    if ExitStatus < 0 then
      ExitStatus := 0;   // script finished but "exit" was never used
    Halt(ExitStatus);
  End;
End.

