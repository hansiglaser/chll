Unit ReconfCell;

{$mode objfpc}{$H+}

Interface

Uses
  Classes, SysUtils, FGL, ReconfSignals, ReconfModule, Netlist, Utils;

Type

  { TDynamicPort }

  TDynamicPort = class
    FName         : String;
    FDir          : TPortDirection;
    FConnType     : TConnType;
    Constructor Create(AName:String;ADir:TPortDirection;AConnType:TConnType);
    Function ToString:ansistring;override;
  End;
  TDynamicPorts = specialize TFPGMap<String,TDynamicPort>;

  { TDirectPort }

  TDirectPort = class
    FName         : String;
    FReconfSignal : TReconfSignalBase;  // only with FSigConn is TSigConnDirect
    Constructor Create(AName:String;AReconfSignal:TReconfSignalBase);
    Function ToString:ansistring;override;
  End;
  TDirectPorts = specialize TFPGMap<String,TDirectPort>;

  { TConfigPort }

  TConfigPort = class
    FName  : String;
    FWidth : Integer;
    Constructor Create(AName:String;AWidth:Integer);
    Function ToString:ansistring;override;
  End;
  TConfigPorts = specialize TFPGMap<String,TConfigPort>;

  { TConfigChain }

  TConfigChain = class
    FChainLen   : Integer;
    FCfgMode    : String;
    FCfgClk     : String;
    FCfgShift   : String;
    FCfgDataIn  : String;
    FCfgDataOut : String;
    Constructor Create;
  End;

  { TReconfCell }

  TReconfCell = class
    FName           : String;
    FReconfModule   : TReconfModule;
    // ports going via bit MUX
    FDynamicPorts   : TDynamicPorts;
    // ports going directly from ReconfModule to this cell
    FDirectPorts    : TDirectPorts;
    // ports coming from InterSynth's config vector
    FConfigPorts    : TConfigPorts;
    // internal config chain (only 0 or 1, therefore no array/map)
    FConfigChain    : TConfigChain;
    // Netlist objects (internally constructed)
    FModule         : TModule;
    FTestbench      : TModule;
    FInstance       : TInstance;
    // Netlist after synthesis (created by Yosys)
    FYosysNetlist   : TModule;  // probably with instances, which point to the module, so all objects are linked together
    Constructor Create(AName:String;AReconfModule:TReconfModule);
    Destructor Destroy; override;
    Function AddDynamicPort(AName:String;ADir:TPortDirection;AConnType:TConnType):TDynamicPort;
    Function AddDirectPort (AName:String;AReconfSignal:TReconfSignalBase) : TDirectPort;
    Function AddConfigPort (AName:String;AWidth:Integer) : TConfigPort;

    Function HasPort(AName:String) : Boolean;
    Procedure Show;
    Procedure GenerateNetlist;
    Function CheckNetlist:Integer;
  End;

  TReconfCellsList = specialize TFPGMap<String,TReconfCell>;

  { TReconfCells }

  TReconfCells = class
    FReconfCells : TReconfCellsList;
    Constructor Create;
    Destructor Destroy; override;
    Function GetCells : TModuleList;
    Function HaveConfigChains : Boolean;
  End;


Implementation

{ TDynamicPort }

Constructor TDynamicPort.Create(AName:String;ADir:TPortDirection;AConnType:TConnType);
Begin
  inherited Create;
  FName     := AName;
  FDir      := ADir;
  FConnType := AConnType;
End;

Function TDynamicPort.ToString:ansistring;
Begin
  Result := FName + ' (conntype: ' + FConnType.FName + ')';
End;

{ TDirectPort }

Constructor TDirectPort.Create(AName:String;AReconfSignal:TReconfSignalBase);
Begin
  inherited Create;
  FName         := AName;
  FReconfSignal := AReconfSignal;
End;

Function TDirectPort.ToString:ansistring;
Begin
  Result := FName + ' (' + FReconfSignal.FName + ')';
End;

{ TConfigPort }

Constructor TConfigPort.Create(AName:String;AWidth:Integer);
Begin
  inherited Create;
  FName  := AName;
  FWidth := AWidth;
End;

Function TConfigPort.ToString:ansistring;
Begin
  Result := FName + ' (cfg:' + IntToStr(FWidth) + ')';
End;

{ TConfigChain }

Constructor TConfigChain.Create;
Begin
  inherited Create;
  // set defaults
  FChainLen   := -1;   // unset
  FCfgMode    := 'CfgMode_i';
  FCfgClk     := 'CfgClk_i';
  FCfgShift   := 'CfgShift_i';
  FCfgDataIn  := 'CfgDataIn_i';
  FCfgDataOut := 'CfgDataOut_o';
End;

{ TReconfCell }

Constructor TReconfCell.Create(AName:String;AReconfModule:TReconfModule);
Begin
  inherited Create;
  FName           := AName;
  FReconfModule   := AReconfModule;
  FDynamicPorts   := TDynamicPorts.Create;
  FDirectPorts    := TDirectPorts. Create;
  FConfigPorts    := TConfigPorts. Create;
  FConfigChain    := Nil;   // unset
End;

Destructor TReconfCell.Destroy;
Begin
  FDynamicPorts.Free;
  FDirectPorts.Free;
  FConfigPorts.Free;
  FConfigChain.Free;  // this can handle Nil
  Inherited Destroy;
End;

Function TReconfCell.AddDynamicPort(AName:String;ADir:TPortDirection;AConnType:TConnType):TDynamicPort;
Begin
  Result := TDynamicPort.Create(AName,ADir,AConnType);
  FDynamicPorts.Add(AName,Result);
End;

Function TReconfCell.AddDirectPort(AName:String;AReconfSignal:TReconfSignalBase):TDirectPort;
Begin
  Result := TDirectPort.Create(AName,AReconfSignal);
  FDirectPorts.Add(AName,Result);
End;

Function TReconfCell.AddConfigPort(AName:String;AWidth:Integer):TConfigPort;
Begin
  Result := TConfigPort.Create(AName,AWidth);
  FConfigPorts.Add(AName,Result);
End;

Function TReconfCell.HasPort(AName:String):Boolean;
Begin
  if FDynamicPorts.     IndexOf(AName) >= 0 then Exit(true);
  if FDirectPorts.      IndexOf(AName) >= 0 then Exit(true);
  if FConfigPorts.      IndexOf(AName) >= 0 then Exit(true);
  Result := False;
End;

Procedure TReconfCell.Show;
Var I : Integer;
Begin
  WriteLn('Cell ''',FName,''':');
  WriteLn('  Dynamic Ports: ',FDynamicPorts.Count);
  For I := 0 to FDynamicPorts.Count-1 do
    WriteLn('    ',FDynamicPorts.Data[I].ToString);
  WriteLn('  Direct Ports: ',FDirectPorts.Count);
  For I := 0 to FDirectPorts.Count-1 do
    WriteLn('    ',FDirectPorts.Data[I].ToString);
  WriteLn('  Config Ports: ',FConfigPorts.Count);
  For I := 0 to FConfigPorts.Count-1 do
    WriteLn('    ',FConfigPorts.Data[I].ToString);
  if assigned(FConfigChain) then
    With FConfigChain do
      WriteLn('  Config Chain: mode: ',FCfgMode,', clk: ',FCfgClk,', shift: ',FCfgShift,', data in: ',FCfgDataIn,', data out: ',FCfgDataOut)
  else
    WriteLn('  no Config Chain');
End;

Procedure TReconfCell.GenerateNetlist;
Var I            : Integer;
    Port         : TPort;
    Assignment   : TAssignment;
    Signal       : TSignal;
    StimProc     : TProcess;
    Value        : TValue;
    Reset        : TSignal;
    Clock        : TSignal;
    ClkPeriode   : TConstant;
    Statement    : TStatement;
Begin
  if assigned(FModule) then
    raise Exception.Create('The netlist was already generated');

  // Create module of application
  FModule := TModule.Create(FName);
  FModule.FArchitectureName := 'rtl';
  FModule.FEntityAttrDecls.Add('intersynth_port',    TAttribute.Create('intersynth_port',    'string'));
  FModule.FEntityAttrDecls.Add('intersynth_conntype',TAttribute.Create('intersynth_conntype','string'));
  FModule.FEntityAttrDecls.Add('intersynth_param',   TAttribute.Create('intersynth_param',   'string'));
  FModule.FEntityAttrDecls.Add('intersynth_config',  TAttribute.Create('intersynth_config',  'integer'));
  FModule.FEntityAttrDecls.Add('intersynth_cfgchain',TAttribute.Create('intersynth_cfgchain','string'));
  For I := 0 to FDirectPorts.Count-1 do
    With FDirectPorts.Data[I] do
      Begin
        Port := FModule.AddPort(TPort.Create(FName,FReconfSignal.GetDirection,FReconfSignal.GetSignal.FType));
        Port.FAttributes.Add(FModule.FEntityAttrDecls['intersynth_port'],TValueString.Create(FReconfSignal.FName));
      End;
  For I := 0 to FDynamicPorts.Count-1 do
    With FDynamicPorts.Data[I] do
      Begin
        Port := FModule.AddPort(TPort.Create(FName,FDir,FConnType.FType));
        Port.FAttributes.Add(FModule.FEntityAttrDecls['intersynth_conntype'],TValueString.Create(FConnType.FName));
      End;
  For I := 0 to FConfigPorts.Count-1 do
    With FConfigPorts.Data[I] do
      Begin
        Port := FModule.AddPort(TPort.Create(FName,dirIn,TType.Create('std_logic_vector',dirDown,FWidth-1,0)));
        Port.FAttributes.Add(FModule.FEntityAttrDecls['intersynth_config'],TValueInteger.Create(FWidth));
      End;
  if assigned(FConfigChain) then
    With FConfigChain Do
      Begin
        Port := FModule.AddPort(TPort.Create(FCfgMode,   dirIn, TypeBit));
        Port.FAttributes.Add(FModule.FEntityAttrDecls['intersynth_cfgchain'],TValueString.Create('mode'));
        Port := FModule.AddPort(TPort.Create(FCfgClk,    dirIn, TypeBit));
        Port.FAttributes.Add(FModule.FEntityAttrDecls['intersynth_cfgchain'],TValueString.Create('clk'));
        Port := FModule.AddPort(TPort.Create(FCfgShift,  dirIn, TypeBit));
        Port.FAttributes.Add(FModule.FEntityAttrDecls['intersynth_cfgchain'],TValueString.Create('shift'));
        Port := FModule.AddPort(TPort.Create(FCfgDataIn, dirIn, TypeBit));
        Port.FAttributes.Add(FModule.FEntityAttrDecls['intersynth_cfgchain'],TValueString.Create('datain'));
        Port := FModule.AddPort(TPort.Create(FCfgDataOut,dirOut,TypeBit));
        Port.FAttributes.Add(FModule.FEntityAttrDecls['intersynth_cfgchain'],TValueString.Create('dataout'));
      End;

  // Create testbench for cell
  FTestbench := TModule.Create(FName+'_tb');
  FTestbench.FArchitectureName := 'behavior';
  FInstance := TInstance.Create('DUT',FModule);
  FTestbench.AddInstance(FInstance);
  StimProc := TProcess.Create('StimulusProc');
  Reset := Nil;
  Clock := Nil;
  For I := 0 to FDirectPorts.Count-1 do
    With FDirectPorts.Data[I] do
      Begin
        Signal := FTestbench.AddSignal(TSignal.Create(FName,FReconfSignal.GetSignal.FType));
        if FReconfSignal.GetSignal.FName = FReconfModule.FParentReset.FName then  // compare signal name, not reconf.signal name!
          Begin
            Signal.FComment := 'Reset';
            Signal.FDefault := TValueBit.Create('0');
            Reset := Signal;
          End
        else if FReconfSignal.GetSignal.FName = FReconfModule.FParentClock.FName then  // compare signal name, not reconf.signal name!
          Begin
            Signal.FComment := 'Clock';
            Signal.FDefault := TValueBit.Create('1');   // so that rising edges are synchronous to multiples of ClkPeriode
            Clock := Signal;
          End
        else if FReconfSignal.GetDirection = dirIn then
          Begin
            if FReconfSignal.GetSignal.FType = TypeBit then
              Value := TValueBit.Create('0')
            else
              Value := TValueVector.Create(FReconfSignal.GetSignal.FType.GetWidthInt,StringOfChar('0',FReconfSignal.GetSignal.FType.GetWidthInt));
            StimProc.AddStatement(TAssignment.Create(Signal,Value));
          End;
        FInstance.ConnectPort(FName,Signal);
      End;
  For I := 0 to FDynamicPorts.Count-1 do
    With FDynamicPorts.Data[I] do
      Begin
        Signal := FTestbench.AddSignal(TSignal.Create(FName,FConnType.FType));
        FInstance.ConnectPort(FName,Signal);
        if FDir = dirIn then
          Begin
            if FConnType.FWidth = 1 then
              Value := TValueBit.Create('0')
            else
              Value := TValueVector.Create(FConnType.FWidth,StringOfChar('0',FConnType.FWidth));
            StimProc.AddStatement(TAssignment.Create(Signal,Value));
          End;
      End;
  For I := 0 to FConfigPorts.Count-1 do
    With FConfigPorts.Data[I] do
      Begin
        Signal := FTestbench.AddSignal(TSignal.Create('Cfg'+FName,TType.Create('std_logic_vector',dirDown,FWidth-1,0)));
        FInstance.ConnectPort(FName,Signal);
        Value := TValueVector.Create(FWidth,StringOfChar('0',FWidth));
        StimProc.AddStatement(TAssignment.Create(Signal,Value));
      End;
  // define constant for clock periode
  if assigned(Clock) then
    ClkPeriode := FTestbench.AddSignal(TConstant.Create('ClkPeriode',TypeTime,TValueTime.Create(10.0,tuNS))) as TConstant;
  // check constant values of dynamic signals coming out of the application
  StimProc.AddStatement(TEmptyLine.Create);
  // first we need a short "wait" to update the simulaton values
  if assigned(Clock) then
    Statement := StimProc.AddStatement(TWait.Create(wtFor,TValueOperatorTimes.Create(TValueFloat.Create(0.1),ClkPeriode)))
  else
    Statement := StimProc.AddStatement(TWait.Create(wtFor,TValueTime.Create(1.0,tuNS)));
  // deassert reset
  if assigned(Reset) then
    Begin
      StimProc.AddStatement(TEmptyLine.Create);
      // wait a bit before deasserting Reset
      if assigned(Clock) then
        StimProc.AddStatement(TWait.Create(wtFor,TValueOperatorTimes.Create(TValueFloat.Create(2.2),ClkPeriode)))
      else
        StimProc.AddStatement(TWait.Create(wtFor,TValueTime.Create(22.0,tuNS)));
      // deassert Reset
      Assignment := TAssignment.Create(Reset,TValueBit.Create('1'));
      Assignment.FComment := 'deassert Reset';
      StimProc.AddStatement(Assignment);
    End;
  if assigned(Clock) then
    Begin
      Assignment := FTestbench.AddAssignment(Clock,TValueOperatorNot.Create(Clock));
      Assignment.FDelay := TValueOperatorTimes.Create(ClkPeriode,TValueFloat.Create(0.5));
      Assignment.FComment := 'Generate clock signal';
    End;
  // add some space
  StimProc.AddStatement(TEmptyLine.Create);
  // end of simulation
  Statement := TAssertion.Create(Nil,'### Simulation Finished ###',svFailure);
  Statement.FComment := 'End of simulation';
  StimProc.AddStatement(Statement);
  StimProc.AddStatement(TWait.Create);  // infinite wait
  FTestbench.AddProcess(StimProc);
End;

Function TReconfCell.CheckNetlist : Integer;
Var Errors : TDynStringArray;
Begin
  if not assigned(FYosysNetlist) then
    raise Exception.Create('No netlist imported.');

  // generate our own netlist as reference (if necessary)
  if not assigned(FModule) then
    GenerateNetlist;

  // compare
  Errors := FModule.Compare(FYosysNetlist,[mcName,mcMorePorts,mcLessPorts,mcLessPortAttrs]);

  // error handling
  For Result := 0 to Length(Errors)-1 do   // misuse 'Result'
    WriteLn(Errors[Result]);
  Result := Length(Errors);
End;

{ TReconfCells }

Constructor TReconfCells.Create;
Begin
  inherited Create;
  FReconfCells := TReconfCellsList.Create;
End;

Destructor TReconfCells.Destroy;
Begin
  FReconfCells.Free;
  inherited Destroy;
End;

Function TReconfCells.GetCells:TModuleList;
Var I : Integer;
Begin
  Result := TModuleList.Create;
  For I := 0 to FReconfCells.Count-1 do
    With FReconfCells.Data[I] do
      Begin
        if not assigned(FModule) then
          GenerateNetlist;
        Result.Add(FModule.FName,FModule);
      End;
End;

Function TReconfCells.HaveConfigChains:Boolean;
Var I : Integer;
Begin
  Result := false;
  For I := 0 to FReconfCells.Count-1 do
    if assigned(FReconfCells.Data[I].FConfigChain) then
      Exit(True);
End;

End.

