Unit ReconfApp;

{$mode objfpc}{$H+}

Interface

Uses
  Classes, SysUtils, Math, FGL, StrUtils, ReconfSignals, ReconfModule, ReconfCell, Netlist, Utils, ConfigIntf, ParamIntf;

Type

  { TDynamicPort }

  TDynamicPort = class
    FName         : String;
    FReconfSignal : TReconfSignalBase;  // only with FSigConn is TSigConnDyn
    FIndex        : Integer;   // specifies the LSB for Array-Signals, LSB + App-Signal-Width must not exceed the Reconf.Signal-Width, LSB must be a multiple of App-Signal-Width; -1 if not an array signal
    Constructor Create(AName:String;ASignal:TReconfSignalBase);
    Function ToString:ansistring;override;
    Procedure Check(ASigConn:TSigConnDyn);
  End;
  TDynamicPorts = specialize TFPGMap<String,TDynamicPort>;

  { TParamPort }

  TParamPort = class
    FName        : String;
    FConnType    : TConnType;
    FDirection   : TPortDirection;   // only dirIn and dirOut are allowed
    FHaveDefault : Boolean;
    FDefault     : Integer;  // TODO: should be a better type which supports more than 32 bits
    FMapping     : Integer;  // instance number of ParamIn/Out cell in InterSynth result
    Constructor Create(AName : String;AConnType : TConnType;ADirection : TPortDirection;AHaveDefault : Boolean;ADefault : Integer);
    Function ToString:ansistring;override;
  End;
  TParamPorts = specialize TFPGMap<String, TParamPort>;

  { TDirectPort }

  TDirectPort = class
    FName         : String;
    FReconfSignal : TReconfSignalBase;  // only with FSigConn is TSigConnDirect
    // TODO: more parameters
    Constructor Create(AName:String;ASignal:TReconfSignalBase);
    Function ToString:ansistring;override;
  End;
  TDirectPorts = specialize TFPGMap<String,TDirectPort>;

  { TConstantValue }

  TConstantValue = class
    FReconfSignal : TReconfSignalBase;  // only with FSigConn is TSigConnDynamic, TSigConnConfig or TSigConnParam
    FValue        : Integer;   // TODO: should be a better type which supports more than 32 bits
    Constructor Create(AReconfSignal:TReconfSignalBase;AValue:Integer);
    Function ToString:ansistring;override;
    Function GetValue : TValue;
  End;
  TConstantValues = specialize TFPGMap<String,TConstantValue>;

  TReconfModuleNetlistWrapApp = class;

  { TArraySignalInfo }

  TArraySignalInfo = class
    FReconfSignal : TReconfSignalBase;
    FWidth        : Integer;   // copy of (FReconfSignal.FSigConn as TSigConnDyn).FConnType.FWidth
    FBitmap       : Cardinal;  // '1' for index-positions which are already used
    Constructor Create(AReconfSignal:TReconfSignalBase);
    // convert array index to bit position
    Function IndexToLow  (AIndex:Integer):Integer;
    Function IndexToHigh (AIndex:Integer):Integer;
    Function IndexToRange(AIndex:Integer):TRange;
    // convert bit position to array index
    Function CheckLow(ALow:Integer):Boolean;   // true if ok
    Function CheckWidth(AWidth:Integer):Boolean;
    Function CheckWidth(AHigh,ALow:Integer):Boolean;
    Function LowToIndex(ALow:Integer):Integer;
    // usage
    Function IsUsed (AIndex:Integer):Boolean;   // true if used
    Function SetUsed(AIndex:Integer):Boolean;   // true if it was not used before
  End;
  TArraySignalInfos = specialize TFPGMap<String,TArraySignalInfo>;

  TCellUsage = specialize TFPGMap<String,Integer>;

  TReconfModuleWrapper = class
    FWrapper : TModule;
  End;
  TReconfModuleWrapperLEC = class (TReconfModuleWrapper)
    FLECSetup   : String;
    FLECMapping : String;
  End;

  TReconfModuleWrapperTarget = (rmtVHDL2008, rmtLEC);

  { TReconfApp }

  TReconfApp = class
    FName : String;
    FReconfModule   : TReconfModule;
    // reconf signals with connection/usage "dynamic"
    FDynamicPorts   : TDynamicPorts;
    FArraySignalInfos : TArraySignalInfos;   // store which indices of an array signal are used by this application
    // ports which are parameters
    FParamPorts     : TParamPorts;
    // reconf signals with connection/usage "direct"
    FDirectPorts    : TDirectPorts;
    // reconf signals with connection/usage "dynamic", "config" and "param"
    FConstantValues : TConstantValues;
    // internally constructed
    FParameters     : TParameters;
    // Netlist objects (internally constructed)
    FModule         : TModule;
    FTestbench      : TModule;
    FInstance       : TInstance;
    FISWrapper      : TModule;    // identical entity with FModule but wraps the InterSynth module
    FRMWrapper      : Array[TReconfModuleWrapperTarget] of TReconfModuleWrapper;    // identical entity with FModule but wraps the ReconfModule
    // Firmware
    FDriverHeaderFilename : String;
    // ReconfModule Wrapper
    FWrapper        : TReconfModuleNetlistWrapApp;
    // Netlist after synthesis (created by Yosys)
    FYosysNetlist   : TModule;  // probably with instances, which point to the module, so all objects are linked together
    // bitstream for InterSynth module
    FInterSynthBitdata : String;    // initialized to Length=0, when reading "netlist" by TInterSynthHandler.ReadConfig set to string of proper count of 'X', when it reads "bitdata bits" the according section is replaced by the given information

    Constructor Create(AName:String;AReconfModule:TReconfModule);   // damn circular unit references
    Destructor Destroy; override;

    Function AddDynamicPort(AName:String;ASignal:TReconfSignalBase;AIndex:Integer=-1):TDynamicPort;
    Function AddParamPort    (AName:String;AConnType:TConnType;ADirection:TPortDirection;AHaveDefault:Boolean;ADefault:Integer):TParamPort;
    Function AddDirectPort   (AName:String;ASignal:TReconfSignalBase) : TDirectPort;
    Function AddConstantValue(ASignal:TReconfSignalBase;AValue:Integer) : TConstantValue;
    Function HasPort(AName:String) : Boolean;
    Procedure Show;
    Procedure GenerateNetlist;
    Procedure GenerateInterSynthWrapper(AISHandler:TObject;AInterSynthTypeName,AInterSynthInstName:String);   // damn circular unit reference
    Procedure GenerateReconfModuleWrapper(ATarget:TReconfModuleWrapperTarget;ANetlist:TReconfModuleNetlist;AISHandler:TObject;AReconfModuleInstName:String);   // damn circular unit reference
    Function CheckNetlist:Integer;
    Function CheckNetlistExtracted(ACellLib:TReconfCells;AIgnoreCells:TFPGStringList):Integer;
    Function GetUsage(ACellLib:TReconfCells):TCellUsage;
    Function GetUsage:TCellUsage;
    Procedure SetupParameters;
    Function GetInterSynthInstance(AISHandler:TObject;AInstance:String):String;
    Function GetReconfSignalsBitstream : String;
  End;
  TReconfAppsList = specialize TFPGMap<String,TReconfApp>;

  { TReconfApps }

  TReconfApps = class
    FReconfApps : TReconfAppsList;
    Constructor Create;
    Destructor Destroy; override;
  End;

  { TReconfModuleNetlistWrapApp }

  TReconfModuleNetlistWrapApp = class(TReconfModuleNetlist)
    FReconfApp   : TReconfApp;
    FAppInstance : TInstance;
    FParameters  : TAddressedParameters;

    Constructor Create(AReconfModule:TReconfModule;AReconfigTypeName,AReconfigInstName:String;AReconfApp:TReconfApp;AAppInstName:String;AParamInterface:TParamInterface);
    Destructor  Destroy; override;
  End;

Implementation
Uses InterSynthHandler;

{ TDynamicPort }

Constructor TDynamicPort.Create(AName:String;ASignal:TReconfSignalBase);
Begin
  inherited Create;
  FName := AName;
  FIndex := -1;   // set this after Create() if ASignal.FSigConn is a TSigConnDyn and its FConnTypeOptions.FArray is true
  FReconfSignal := ASignal;
End;

Function TDynamicPort.ToString:ansistring;
Begin
  Result := FName + ' (' + FReconfSignal.FName;
  if FIndex >= 0 then
    Begin
      if (FReconfSignal.FSigConn as TSigConnDyn).FConnType.FWidth = 1 then
        Result += '(' + IntToStr(FIndex) + ')'
      else
        Result += '(' + IntToStr(FIndex*((FReconfSignal.FSigConn as TSigConnDyn).FConnType.FWidth+1)-1)+':'+IntToStr(FIndex*(FReconfSignal.FSigConn as TSigConnDyn).FConnType.FWidth) + ')';
    End;
  Result += ')';
End;

Procedure TDynamicPort.Check(ASigConn:TSigConnDyn);
Begin
  // check for consistency with FConnTypeOptions
  if ASigConn.FConnTypeOptions.FArray then
    Begin
      if FIndex < 0 then
        raise Exception.Create('Array signal index must be set');
      if (FIndex mod ASigConn.FConnType.FWidth) <> 0 then
        raise Exception.Create('Array signal index must be an integer multiple of the connection type '+ASigConn.FConnType.FName+' width '+IntToStr(ASigConn.FConnType.FWidth));
    End;
End;

{ TParamPort }

Constructor TParamPort.Create(AName:String;AConnType:TConnType;ADirection:TPortDirection;AHaveDefault:Boolean;ADefault:Integer);
Begin
  inherited Create;
  FName        := AName;
  FConnType    := AConnType;
  FDirection   := ADirection;
  FHaveDefault := AHaveDefault;
  FDefault     := ADefault;
  FMapping     := -1;   // unset
End;

Function TParamPort.ToString:ansistring;
Begin
  Result := FName + ' (conntype: '+FConnType.FName + ' ('+IntToStr(FConnType.FWidth)+') ' +
            CPortDirectionVerilog[FDirection];
  if FHaveDefault then
    Begin
      Result += ' default = ';
      if FConnType.FWidth = 1 then
        Result += '''' + IntToBin(FDefault,1) + ''''
      else
        Result += '"' + IntToBin(FDefault,FConnType.FWidth) + '"';
      Result += ' = $' + IntToHex(FDefault,(FConnType.FWidth+3) shr 2);
    End;
  Result += ')'
End;

{ TDirectPort }

Constructor TDirectPort.Create(AName:String;ASignal:TReconfSignalBase);
Begin
  inherited Create;
  FName         := AName;
  FReconfSignal := ASignal;
End;

Function TDirectPort.ToString:ansistring;
Begin
  Result := FName + ' (' + FReconfSignal.FName + ')';
End;

{ TConstantValue }

Constructor TConstantValue.Create(AReconfSignal:TReconfSignalBase;AValue:Integer);
Begin
  inherited Create;
  FReconfSignal := AReconfSignal;
  FValue        := AValue;
End;

Function TConstantValue.ToString:ansistring;
Begin
  Result := FReconfSignal.FName + ' := ';
  if FReconfSignal.GetSignal.FType.GetWidthInt = 1 then
    Result += '''' + IntToBin(FValue,1) + ''''
  else
    Result += '"' + IntToBin(FValue,FReconfSignal.GetSignal.FType.GetWidthInt) + '"';
End;

Function TConstantValue.GetValue:TValue;
Begin
  if FReconfSignal.GetSignal.FType = TypeBit then
    Result := TValueBit.Create(Chr(Ord('0')+FValue))
  else
    Result := TValueVector.Create(FReconfSignal.GetSignal.FType.GetWidthInt,FValue);
End;

{ TArraySignalInfo }

Constructor TArraySignalInfo.Create(AReconfSignal:TReconfSignalBase);
Begin
  inherited Create;
  if not (AReconfSignal.FSigConn is TSigConnDyn) then
    raise Exception.Create('Signal '+AReconfSignal.FName+' is not specified as dynamic');
  if not (AReconfSignal.FSigConn as TSigConnDyn).FConnTypeOptions.FArray then
    raise Exception.Create('Signal '+AReconfSignal.FName+' is not specified as array signal');
  FReconfSignal := AReconfSignal;
  FWidth        := (FReconfSignal.FSigConn as TSigConnDyn).FConnType.FWidth;
  FBitmap       := 0;
End;

Function TArraySignalInfo.IndexToLow(AIndex:Integer):Integer;
Begin
  Result := FWidth * AIndex;
End;

Function TArraySignalInfo.IndexToHigh(AIndex:Integer):Integer;
Begin
  Result := FWidth * AIndex + FWidth-1;
End;

Function TArraySignalInfo.IndexToRange(AIndex:Integer):TRange;
Begin
  Result := TRange.Create(dirDown,FWidth * AIndex + FWidth-1, FWidth * AIndex);
End;

Function TArraySignalInfo.CheckLow(ALow:Integer):Boolean;
Begin
  Result := ((ALow mod FWidth) = 0);
End;

Function TArraySignalInfo.CheckWidth(AWidth:Integer):Boolean;
Begin
  Result := (AWidth = FWidth);
End;

Function TArraySignalInfo.CheckWidth(AHigh,ALow:Integer):Boolean;
Begin
  Result := ((AHigh-ALow+1) = FWidth);
End;

Function TArraySignalInfo.LowToIndex(ALow:Integer):Integer;
Begin
  if not CheckLow(ALow) then
    raise Exception.CreateFmt('Bit position %d is not an integer multiple of connection type width %d',[ALow,FWidth]);
  Result := ALow div FWidth;
End;

Function TArraySignalInfo.IsUsed(AIndex:Integer):Boolean;
Begin
  Result := ((FBitmap and (1 shl AIndex)) <> 0);
End;

Function TArraySignalInfo.SetUsed(AIndex:Integer):Boolean;
Begin
  if IsUsed(AIndex) then
    Exit(false);
  Result := true;
  FBitmap := FBitmap or (1 shl AIndex);
End;

{ TReconfApp }

Constructor TReconfApp.Create(AName:String;AReconfModule:TReconfModule);
Begin
  inherited Create;
  FName := AName;
  FReconfModule   := AReconfModule;
  FDynamicPorts   := TDynamicPorts.     Create;
  FArraySignalInfos := TArraySignalInfos.Create;
  FDirectPorts    := TDirectPorts.      Create;
  FParamPorts     := TParamPorts.       Create;
  FParamPorts.Sorted := true;
  FParamPorts.Duplicates := dupError;
  FConstantValues := TConstantValues.Create;
End;

Destructor TReconfApp.Destroy;
Begin
  FDynamicPorts.Free;
  FArraySignalInfos.Free;
  FDirectPorts.Free;
  FParamPorts.Free;
  FConstantValues.Free;
  Inherited Destroy;
End;

Function TReconfApp.AddDynamicPort(AName:String;ASignal:TReconfSignalBase;AIndex:Integer=-1):TDynamicPort;
Begin
  Result := TDynamicPort.Create(AName,ASignal);
  Result.FIndex := AIndex;
  FDynamicPorts.Add(AName,Result);
  if (ASignal.FSigConn is TSigConnDyn) and ((ASignal.FSigConn as TSigConnDyn).FConnTypeOptions.FArray) then
    Begin
      if AIndex < 0 then
        raise Exception.Create('Reconf.signal '+ASignal.FName+' is an array signal, so you have to specify an index for '+AName);
      if FArraySignalInfos.IndexOf(ASignal.GetPortName) < 0 then
        FArraySignalInfos.Add(ASignal.FName,TArraySignalInfo.Create(ASignal));
      if not FArraySignalInfos[ASignal.FName].SetUsed(AIndex) then
        raise Exception.CreateFmt('This index position %d of %s is already used for another port',[AIndex,ASignal.FName]);
    End
  else
    if AIndex >= 0 then
      raise Exception.Create('Reconf.signal '+ASignal.FName+' is no array signal, therefore no index allowed for '+AName);
End;

Function TReconfApp.AddParamPort(AName:String;AConnType:TConnType;ADirection:TPortDirection;AHaveDefault:Boolean;ADefault:Integer):TParamPort;
Begin
  Result := TParamPort.Create(AName,AConnType,ADirection,AHaveDefault,ADefault);
  FParamPorts.Add(AName,Result);
End;

Function TReconfApp.AddDirectPort(AName:String;ASignal:TReconfSignalBase):TDirectPort;
Begin
  Result := TDirectPort.Create(AName,ASignal);
  FDirectPorts.Add(AName,Result);
End;

Function TReconfApp.AddConstantValue(ASignal:TReconfSignalBase;AValue:Integer):TConstantValue;
Begin
  Result := TConstantValue.Create(ASignal,AValue);
  FConstantValues.Add(ASignal.FName,Result);
End;

Function TReconfApp.HasPort(AName:String):Boolean;
Begin
  if FDynamicPorts.     IndexOf(AName) >= 0 then Exit(true);
  if FDirectPorts.      IndexOf(AName) >= 0 then Exit(true);
  if FParamPorts.       IndexOf(AName) >= 0 then Exit(true);
  Result := False;
End;

Procedure TReconfApp.Show;
Var I : Integer;
Begin
  WriteLn('Application ''',FName,''':');
  WriteLn('  Dynamic Ports: ',FDynamicPorts.Count);
  For I := 0 to FDynamicPorts.Count-1 do
    WriteLn('    ',FDynamicPorts.Data[I].ToString);
  WriteLn('  Direct Ports: ',FDirectPorts.Count);
  For I := 0 to FDirectPorts.Count-1 do
    WriteLn('    ',FDirectPorts.Data[I].ToString);
  WriteLn('  Parameters: ',FParamPorts.Count);
  For I := 0 to FParamPorts.Count-1 do
    WriteLn('    ',FParamPorts.Data[I].ToString);
  WriteLn('  Constants: ',FConstantValues.Count);
  For I := 0 to FConstantValues.Count-1 do
    WriteLn('    ',FConstantValues.Data[I].ToString);
End;

Procedure TReconfApp.GenerateNetlist;
Var I : Integer;
    Port : TPort;
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
  For I := 0 to FDirectPorts.Count-1 do
    With FDirectPorts.Data[I] do
      Begin
        Port := FModule.AddPort(TPort.Create(FName,FReconfSignal.GetDirection,FReconfSignal.GetSignal.FType));
        Port.FAttributes.Add(FModule.FEntityAttrDecls['intersynth_port'],TValueString.Create(FReconfSignal.FName));
      End;
  For I := 0 to FDynamicPorts.Count-1 do
    With FDynamicPorts.Data[I] do
      Begin
        Port := FModule.AddPort(TPort.Create(FName,FReconfSignal.GetDirection,(FReconfSignal.FSigConn as TSigConnDyn).FConnType.FType));
        Port.FAttributes.Add(FModule.FEntityAttrDecls['intersynth_port'],TValueString.Create(FReconfSignal.FName));
        Port.FAttributes.Add(FModule.FEntityAttrDecls['intersynth_conntype'],TValueString.Create((FReconfSignal.FSigConn as TSigConnDyn).FConnType.FName));
      End;
  For I := 0 to FParamPorts.Count-1 do
    With FParamPorts.Data[I] do
      Begin
        Port := FModule.AddPort(TPort.Create(FName,FDirection,FConnType.FType));
        Port.FAttributes.Add(FModule.FEntityAttrDecls['intersynth_param'],TValueString.Create(FName));
        Port.FAttributes.Add(FModule.FEntityAttrDecls['intersynth_conntype'],TValueString.Create(FConnType.FName));
      End;
  For I := 0 to FConstantValues.Count-1 do
    With FConstantValues.Data[I] do
      Begin
        if not (FReconfSignal.FSigConn is TSigConnDyn) then
          Continue;
        // only signals with connection/usage "dynamic" are set as assignment, "config" and "param" are set outside of the application
        Port := FModule.AddPort(TPort.Create(FReconfSignal.GetPortName,FReconfSignal.GetDirection,FReconfSignal.GetSignal.FType));
        Port.FAttributes.Add(FModule.FEntityAttrDecls['intersynth_port'],TValueString.Create(FReconfSignal.FName));
        Port.FAttributes.Add(FModule.FEntityAttrDecls['intersynth_conntype'],TValueString.Create((FReconfSignal.FSigConn as TSigConnDyn).FConnType.FName));
        if FReconfSignal.GetSignal.FType = TypeBit then
          Assignment := FModule.AddAssignment(Port,TValueBit.Create(Chr(Ord('0')+FValue)))
        else
          Assignment := FModule.AddAssignment(Port,TValueVector.Create(FReconfSignal.GetSignal.FType.GetWidthInt,FValue));
        Assignment.FComment := 'constant value for dynamic signal';
      End;

  // Create testbench for application
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
        Signal := FTestbench.AddSignal(TSignal.Create(FName,(FReconfSignal.FSigConn as TSigConnDyn).FConnType.FType));
        FInstance.ConnectPort(FName,Signal);
        if FReconfSignal.GetDirection = dirIn then
          Begin
            if (FReconfSignal.FSigConn as TSigConnDyn).FConnType.FWidth = 1 then
              Value := TValueBit.Create('0')
            else
              Value := TValueVector.Create(FReconfSignal.GetSignal.FType.GetWidthInt,StringOfChar('0',FReconfSignal.GetSignal.FType.GetWidthInt));
            StimProc.AddStatement(TAssignment.Create(Signal,Value));
          End;
      End;
  For I := 0 to FParamPorts.Count-1 do
    With FParamPorts.Data[I] do
      Begin
        if FConnType.FWidth = 1 then
          Signal := FTestbench.AddSignal(TSignal.Create(FName,TypeBit))
        else
          Signal := FTestbench.AddSignal(TSignal.Create(FName,'std_logic_vector',dirDown,TValueInteger.Create(FConnType.FWidth-1),TValueInteger.Create(0)));
        FInstance.ConnectPort(FName,Signal);
        if (FDirection = dirIn) and FHaveDefault then
          Begin
            if FConnType.FWidth = 1 then
              Value := TValueBit.Create(Chr(Ord('0')+FDefault))
            else
              Value := TValueVector.Create(FConnType.FWidth,IntToBin(FDefault,FConnType.FWidth));
            StimProc.AddStatement(TAssignment.Create(Signal,Value));
          End;
      End;
  For I := 0 to FConstantValues.Count-1 do
    With FConstantValues.Data[I] do
      Begin
        // all ports with any connection/usage get a signal, so the user can
        // add other instances, e.g. of the SPI master (but don't forget to
        // remove the initialization from the stimulus process!)
        if FReconfSignal.FSigConn is TSigConnDyn then
          Begin
            // dynamic signals come out of the application --> check with an
            // assertion, but do that below after a small "wait"
          End
        else
          Begin
            // signals with connection/usage "const", "config" or "param" and a
            // constant value get these assigned in the architecture body (and
            // not the process, because these values should not be changed!)
            Signal := FTestbench.AddSignal(TSignal.Create(FReconfSignal.GetPortName,FReconfSignal.GetSignal.FType));
            if FReconfSignal.GetSignal.FType = TypeBit then
              Value := TValueBit.Create(Chr(Ord('0')+FValue))
            else
              Value := TValueVector.Create(FReconfSignal.GetSignal.FType.GetWidthInt,FValue);
            Assignment := FTestbench.AddAssignment(Signal,Value);
            Assignment.FComment := 'constant value for reconfig signal';
          End;
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
  Statement.FComment := 'Check constant values of dynamic signals coming out of the application modules';
  // then check with assertions
  For I := 0 to FConstantValues.Count-1 do
    With FConstantValues.Data[I] do
      if FReconfSignal.FSigConn is TSigConnDyn then
        Begin
          // dynamic signals come out of the application --> check with an
          // assertion
          Signal := FTestbench.AddSignal(TSignal.Create(FReconfSignal.GetPortName,FReconfSignal.GetSignal.FType));
          if FReconfSignal.GetSignal.FType = TypeBit then
            Value := TValueBit.Create(Chr(Ord('0')+FValue))
          else
            Value := TValueVector.Create(FReconfSignal.GetSignal.FType.GetWidthInt,FValue);
          FInstance.ConnectPort(FReconfSignal.GetPortName,Signal);
          StimProc.AddStatement(TAssertion.Create(TValueOperatorEqual.Create(Signal,Value),
            'Dynamic reconf.signal '+FName+' should have constant value '+Value.GetVHDLValue,svFailure));
          // Attention: The above message can generate wrong VHDL, if the
          // signal is a vector. In this case, the VHDL code gets
          //   assert MySig = "0000" report "Dynamic ... constant value "0000"" severity failure
          // i.e. double double-qotes.
          // TODO: find out if VHDL supports quoted strings, otherwise create
          // a string in the above if-then-else without the quotes.
        End;
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

Procedure TReconfApp.GenerateInterSynthWrapper(AISHandler:TObject;AInterSynthTypeName,AInterSynthInstName:String);
Var ISHandler   : TInterSynthHandler;
    ISModule    : TModule;
    ISInstance  : TInstance;
    I,J         : Integer;
    St          : String;
    Value       : TValue;
    ParamInMask : TStringIntMap;
    Concat      : TValueConcat;
Begin
  // generate our own netlist as reference (if necessary)
  if not assigned(FModule) then
    GenerateNetlist;

  if Length(FInterSynthBitdata) = 0 then
    raise Exception.Create('No config data for InterSynth module available');
  For I := 0 to FParamPorts.Count-1 do
    With FParamPorts.Data[I] do
      if FMapping < 0 then
        raise Exception.Create('No mapping for parameter '+FName+' available');

  if assigned(FISWrapper) then
    raise Exception.Create('You already generated an InterSynth module wrapper');

  // what we need here: ISModule, mapping of ParamIn/Out ports, bitdata

  ISHandler := TInterSynthHandler(AISHandler);
  ISModule  := ISHandler.GenerateModule(AInterSynthTypeName);

  // copy FModule
  FISWrapper := FModule.Clone;
  FISWrapper.FArchitectureName := 'WrapInterSynth';
  // remove all assignments which are used to set constant values to
  // connection/usage = dynamic signals, these are now coming from the
  // InterSynth module
  While FISWrapper.FAssignments.Count > 0 do
    Begin
      FISWrapper.FArchBody.Delete(FISWrapper.FArchBody.IndexOfData(FISWrapper.FAssignments[0]));
      FISWrapper.FAssignments.Delete(0);
      // don't Free assignments, because these are still used by FModule
    End;

  ISInstance := TInstance.Create(AInterSynthInstName,ISModule);
  FISWrapper.AddInstance(ISInstance);

  // connect connection/usage direct reconf. signals
  For I := 0 to FDirectPorts.Count-1 do
    With FDirectPorts.Data[I] do
      Begin
        //WriteLn('Connecting direct InterSynth module port ',FReconfSignal.GetPortName,' to app. module port ',FDirectPorts.Data[I].FName);
        ISInstance.ConnectPort(FReconfSignal.GetPortName,FModule.FPorts[FReconfSignal.GetPortName]);
        // TODO: handle array ports
      End;
  // connect connection/usage dynamic reconf. signals
  For I := 0 to FDynamicPorts.Count-1 do
    With FDynamicPorts.Data[I] do
      Begin
        //WriteLn('Connecting dynamic InterSynth module port ',FReconfSignal.GetPortName,' to app. module port ',FDynamicPorts.Data[I].FName);
        if not (FReconfSignal.FSigConn as TSigConnDyn).FConnTypeOptions.FArray then
          ISInstance.ConnectPort(FReconfSignal.GetPortName,FModule.FPorts[FDynamicPorts.Data[I].FName])
        else
          ISInstance.ConnectPort(FReconfSignal.GetPortName+'_'+IntToStr(FIndex div (FReconfSignal.FSigConn as TSigConnDyn).FConnType.FWidth),FModule.FPorts[FName])
      End;

  // connect parameters for application
  // first generate signals for each param port of the InterSynth module
  ParamInMask := TStringIntMap.Create;
  For I := 0 to FReconfModule.FReconfSignals.FConnTypes.Count-1 do
    With FReconfModule.FReconfSignals.FConnTypes do
      Begin
        if ISHandler.FParamInCount[Data[I].FName] > 0 then
          Begin
            Value := FISWrapper.AddSignal(TSignal.Create('ParamIn_'+Data[I].FName+'_s', 'std_logic_vector', dirDown, Data[I].FWidth*ISHandler.FReadParamInCount[Data[I].FName]-1, 0));
            ISInstance.ConnectPort('ParamIn_'+Data[I].FName+'_i', Value);
            ParamInMask.Add(Data[I].FName,0);
          End;
        if ISHandler.FParamOutCount[Data[I].FName] > 0 then
          Begin
            Value := FISWrapper.AddSignal(TSignal.Create('ParamOut_'+Data[I].FName+'_s', 'std_logic_vector', dirDown, Data[I].FWidth*ISHandler.FReadParamOutCount[Data[I].FName]-1, 0));
            ISInstance.ConnectPort('ParamOut_'+Data[I].FName+'_o', Value);
          End;
      End;
  // then connect all application module ports with that signals
  For I := 0 to FParamPorts.Count-1 do
    With FParamPorts.Data[I] do
      Begin
        //WriteLn('Connecting param port ',FName,' to InterSynth module param port index ',FMapping);
        if FDirection = dirIn then
          Begin
            ParamInMask[FConnType.FName] := ParamInMask[FConnType.FName] or (1 shl FMapping);
            FISWrapper.AddAssignment(
              TValueIndex.Create(FISWrapper.FSignals['ParamIn_'+FConnType.FName+'_s'],
                TRange.Create(dirDown,(FMapping+1)*FConnType.FWidth-1,FMapping*FConnType.FWidth)),
              FISWrapper.FPorts[FName]);
          End
        else
          Begin
            FISWrapper.AddAssignment(
              FISWrapper.FPorts[FName],
              TValueIndex.Create(FISWrapper.FSignals['ParamOut_'+FConnType.FName+'_s'],
                TRange.Create(dirDown,(FMapping+1)*FConnType.FWidth-1,FMapping*FConnType.FWidth)));
          End;
      End;
  // finally set all unused param inputs to 0
  For I := 0 to FReconfModule.FReconfSignals.FConnTypes.Count-1 do
    With FReconfModule.FReconfSignals.FConnTypes do
      Begin
        if ISHandler.FReadParamInCount.IndexOf(Data[I].FName) < 0 then
          Continue;   // no ParamIn cells for that conn.type
        For J := 0 to ISHandler.FReadParamInCount[Data[I].FName]-1 do
          if ParamInMask[Data[I].FName] and (1 shl J) = 0 then
            Begin
              if Data[I].FType = TypeBit then
                Value := TValueBit.Create('0')
              else if Data[I].FType.FName = 'std_logic_vector' then
                Value := TValueVector.Create(Data[I].FType.FRange.GetWidthInt,0)
              else
                raise Exception.Create('Can''t handle type '+Data[I].FType.FName+' of connection type '+Data[I].FName);
              FISWrapper.AddAssignment(
                TValueIndex.Create(FISWrapper.FSignals['ParamIn_'+Data[I].FName+'_s'],
                  TRange.Create(dirDown,(J+1)*Data[I].FWidth-1,J*Data[I].FWidth)),
                Value);
            End;
      End;
  ParamInMask.Free;

  // connect dynamic signals with constant value
  For I := 0 to FConstantValues.Count-1 do
    With FConstantValues.Data[I] do
      Begin
        if not (FReconfSignal.FSigConn is TSigConnDyn) then
          Continue;
        // only signals with connection/usage "dynamic" are set as assignment, "config" and "param" are set outside of the application
        if not (FReconfSignal.FSigConn as TSigConnDyn).FConnTypeOptions.FArray then
          ISInstance.ConnectPort(FReconfSignal.GetPortName,FModule.FPorts[FReconfSignal.GetPortName])
        else
          Begin
            // Array port
            if ((FReconfSignal.FSigConn as TSigConnDyn).FConnTypeOptions.FPadLeftWidth >= 0) or
               ((FReconfSignal.FSigConn as TSigConnDyn).FConnTypeOptions.FPadRightWidth >= 0) then
              raise Exception.Create('TODO: Implement padding for array ports for constant values');
            // constant values are always outputs: create individual signals for
            // each element (=port of the InterSynth module), assign a
            // concatenation to the wrapper module output
            Concat := TValueConcat.Create;
            For J := 0 to (FReconfSignal.GetSignal.FType.GetWidthInt div (FReconfSignal.FSigConn as TSigConnDyn).FConnType.FWidth)-1 do
              Begin
                St := FReconfSignal.GetPortName + '_' + IntToStr(J);
                Value := FISWrapper.AddSignal(TSignal.Create(St+'_s',(FReconfSignal.FSigConn as TSigConnDyn).FConnType.FType));
                Concat.Add(Value);  // add as MSB!
                ISInstance.ConnectPort(St,Value);
              End;
            FISWrapper.AddAssignment(FModule.FPorts[FReconfSignal.GetPortName],Concat);
          End;
      End;

  // connect "bitdata" input
  Value := FISWrapper.AddSignal(TSignal.Create('BitData_s','std_logic_vector',dirDown,Length(FInterSynthBitdata)-1,0));
  ISInstance.ConnectPort('bitdata',Value);
  FISWrapper.AddAssignment(Value,TValueVector.Create(Length(FInterSynthBitdata), ReverseString(FInterSynthBitdata)));

  // connect signals of internal config chains
  if ISHandler.FReconfCells.HaveConfigChains then
    Begin
      ISInstance.ConnectPort('CfgMode_i',TValueBit.Create('0'));
      For I := 0 to ISHandler.FReconfCells.FReconfCells.Count-1 do
        With ISHandler.FReconfCells.FReconfCells.Data[I] do
          Begin
            if not assigned(FConfigChain) then continue;
            if ISHandler.FReadCellCount[FName] = 1 then
              ISInstance.ConnectPort('CfgClk_'+FName+'_i',TValueBit.Create('0'))
            else
              ISInstance.ConnectPort('CfgClk_'+FName+'_i',TValueVector.Create(ISHandler.FReadCellCount[FName],0))
          End;
      For I := 0 to ISHandler.FReconfCells.FReconfCells.Count-1 do
        With ISHandler.FReconfCells.FReconfCells.Data[I] do
          Begin
            if not assigned(FConfigChain) then continue;
            if ISHandler.FReadCellCount[FName] = 1 then
              ISInstance.ConnectPort('CfgShift_'+FName+'_i',TValueBit.Create('0'))
            else
              ISInstance.ConnectPort('CfgShift_'+FName+'_i',TValueVector.Create(ISHandler.FReadCellCount[FName],0))
          End;
      ISInstance.ConnectPort('CfgDataIn_i',TValueBit.Create('0'));
      For I := 0 to ISHandler.FReconfCells.FReconfCells.Count-1 do
        With ISHandler.FReconfCells.FReconfCells.Data[I] do
          Begin
            if not assigned(FConfigChain) then continue;
            if ISHandler.FReadCellCount[FName] = 1 then
              Value := TSignal.Create('CfgDataOut_'+FName+'_s',TypeBit)
            else
              Value := TSignal.Create('CfgDataOut_'+FName+'_s',TType.Create('std_logic_vector',dirDown,ISHandler.FReadCellCount[FName]-1,0));
            FISWrapper.AddSignal(Value as TSignal);
            ISInstance.ConnectPort('CfgDataOut_'+FName+'_o',Value);
          End;
    End;

  // connect unused inputs and outputs of InterSynth module
  For I := 0 to ISModule.FPorts.Count-1 do
    Begin
      if ISInstance.FConnections.IndexOf(ISModule.FPorts.Data[I].FName) >= 0 then
        continue;
      if ISModule.FPorts.Data[I].FDir = dirIn then
        Begin
          if ISModule.FPorts.Data[I].FType = TypeBit then
            Value := TValueBit.Create('0')
          else if ISModule.FPorts.Data[I].FType.FName = 'std_logic_vector' then
            Value := TValueVector.Create(ISModule.FPorts.Data[I].FType.FRange.GetWidthInt,0)
          else
            raise Exception.Create('Can''t handle type '+ISModule.FPorts.Data[I].FType.FName+' of port '+ISModule.FPorts.Data[I].FName);
          ISInstance.ConnectPort(ISModule.FPorts.Keys[I],Value);
        End
      else
        Begin
          Value := FISWrapper.AddSignal(TSignal.Create(ISModule.FPorts.Keys[I]+'_s',ISModule.FPorts.Data[I].FType));
          ISInstance.ConnectPort(ISModule.FPorts.Keys[I],Value);
        End;
    End;
End;

Type TInputPortions     = specialize TFPGMap<Integer,TValue>;          // key: index into signal vector, data: value
     TInputPortionsList = specialize TFPGMap<String,TInputPortions>;   // key: input port name, data: list of portions

Procedure TReconfApp.GenerateReconfModuleWrapper(ATarget:TReconfModuleWrapperTarget;ANetlist:TReconfModuleNetlist;AISHandler:TObject;AReconfModuleInstName:String);
Var ISHandler   : TInterSynthHandler;
    RMWrapper   : TModule;
    RMModule    : TModule;
    RMInstance  : TInstance;
    I,J         : Integer;
    St          : String;
    Signal      : TSignal;
    Value       : TValue;
    ParamInMask : TStringIntMap;
    Concat      : TValueConcat;
    InputPortions : TInputPortionsList;
    IP            : TInputPortions;
    RS            : TReconfSignalBase;
    PL, PR        : Integer;
    LECWrapper    : TReconfModuleWrapperLEC;   // just a shortcut variable
Begin
  if not assigned(ANetlist) then
    raise Exception.Create('Reconf. module netlist missing');
  // generate our own netlist as reference (if necessary)
  if not assigned(FModule) then
    GenerateNetlist;

  // check that InterSynth results were read in
  if Length(FInterSynthBitdata) = 0 then
    raise Exception.Create('No config data for InterSynth module available');
  For I := 0 to FParamPorts.Count-1 do
    With FParamPorts.Data[I] do
      if FMapping < 0 then
        raise Exception.Create('No mapping for parameter '+FName+' available');

  // create wrapper container
  if assigned(FRMWrapper[ATarget]) then
    raise Exception.Create('You already generated an reconf.module wrapper');
  Case ATarget of
    rmtVHDL2008 : Begin FRMWrapper[ATarget] := TReconfModuleWrapper.Create; End;
    rmtLEC      : Begin LECWrapper          := TReconfModuleWrapperLEC.Create; FRMWrapper[ATarget] := LECWrapper; End;
  else
    raise Exception.Create('Invalid Target');
  End;

  // what we need here: RMModule, mapping of ParamIn/Out ports, bitdata

  ISHandler := TInterSynthHandler(AISHandler);
  RMModule  := ANetlist.FModule;

  // copy FModule
  RMWrapper := FModule.Clone;
  FRMWrapper[ATarget].FWrapper := RMWrapper;
  RMWrapper.FArchitectureName := 'WrapReconfModule';
  // remove all assignments which are used to set constant values to
  // connection/usage = dynamic signals, these are now coming from the
  // InterSynth module
  While RMWrapper.FAssignments.Count > 0 do
    Begin
      RMWrapper.FArchBody.Delete(RMWrapper.FArchBody.IndexOfData(RMWrapper.FAssignments[0]));
      RMWrapper.FAssignments.Delete(0);
      // don't Free assignments, because these are still used by FModule
    End;

  // add instance of reconf.module
  RMInstance := TInstance.Create(AReconfModuleInstName,RMModule);
  RMWrapper.AddInstance(RMInstance);

  InputPortions := TInputPortionsList.Create;

  // connect connection/usage direct reconf. signals
  For I := 0 to FDirectPorts.Count-1 do
    With FDirectPorts.Data[I] do
      Begin
        WriteLn('Connecting direct reconf. module port ',FReconfSignal.GetPortName,' to app. module port ',FDirectPorts.Data[I].FName);
        RMInstance.ConnectPort(FReconfSignal.GetPortName,FModule.FPorts[FReconfSignal.GetPortName]);
        // TODO: handle array ports
      End;
  // connect connection/usage dynamic reconf. signals
  For I := 0 to FDynamicPorts.Count-1 do
    With FDynamicPorts.Data[I] do
      Begin
        if (FReconfSignal.FSigConn as TSigConnDyn).FConnTypeOptions.FArray then
          Begin
            // array port: create a signal with the full width plus an assignment for the app. module port
            WriteLn('Connecting dynamic reconf. module port ',FReconfSignal.GetPortName,'(',FIndex,') to app. module port ',FDynamicPorts.Data[I].FName);
            if RMInstance.FConnections.IndexOf(FReconfSignal.GetPortName) < 0 then
              Begin
                // create signal if it doesn't yet exist
                St := TrimSignalPostfix(FReconfSignal.GetPortName) + '_s';
                Value := RMWrapper.AddSignal(TSignal.Create(St,FReconfSignal.GetSignal.FType));
                RMInstance.ConnectPort(FReconfSignal.GetPortName,Value);
              End
            else
              Value := RMInstance.FConnections[FReconfSignal.GetPortName];
            if FReconfSignal.GetDirection = dirOut then
              Begin
                // output
                if (FReconfSignal.FSigConn as TSigConnDyn).FConnType.FType = TypeBit then
                  RMWrapper.AddAssignment(RMWrapper.FPorts[FDynamicPorts.Data[I].FName],TValueIndex.Create(Value,TValueInteger.Create(FIndex)))
                else
                  RMWrapper.AddAssignment(RMWrapper.FPorts[FDynamicPorts.Data[I].FName],TValueIndex.Create(Value,TRange.Create(dirDown,FIndex+(FReconfSignal.FSigConn as TSigConnDyn).FConnType.FWidth-1,FIndex)))
              End
            else
              Begin
                // input, thats complicated because we can't assign to indexed
                // portions of a vector, therefore we need a concatenation, but
                // its elements will only be known at the end of that function
                IP := TInputPortions.Create;
                IP.Sorted     := true;
                IP.Duplicates := dupError;
                InputPortions.Add(FReconfSignal.FName,IP);
                IP.Add(FIndex,RMWrapper.FPorts[FDynamicPorts.Data[I].FName]);
              End;
          End
        else if ((FReconfSignal.FSigConn as TSigConnDyn).FConnTypeOptions.FPadLeftWidth >= 0) or
                ((FReconfSignal.FSigConn as TSigConnDyn).FConnTypeOptions.FPadRightWidth >= 0) then
          Begin
            // RMInstance' port is smaller than FModule's port, e.g.
            // the reconf.signal I2C_ReadCount_o (which is also a port of RMInstance)
            // is 4 bits, but the (Ex.)App's I2C_ReadCount_o is conntype "Byte" (8 bits).
            WriteLn('Connecting dynamic reconf. module port ',FReconfSignal.GetPortName,' (width: ',FReconfSignal.GetSignal.FType.GetWidthInt,') to app. module port ',FDynamicPorts.Data[I].FName,' (width: ',(FReconfSignal.FSigConn as TSigConnDyn).FConnType.FWidth,')');
            if FReconfSignal.GetDirection = dirOut then
              With (FReconfSignal.FSigConn as TSigConnDyn).FConnTypeOptions do
                if (FPadLeftWidth >= 0) or (FPadRightWidth >= 0) then
                  Begin
                    // use an intermediate signal of proper width of the reconf.signal
                    St := TrimSignalPostfix(FReconfSignal.GetPortName);
                    Signal := TSignal.Create(St+'_s',FReconfSignal.GetSignal.FType);
                    RMWrapper.AddSignal(Signal);
                    RMInstance.ConnectPort(FReconfSignal.GetPortName,Signal);
                    // pad reconf.signal to desired width
                    Concat := TValueConcat.Create;
                    if FPadRightWidth > 0 then
                      Concat.Add(TValueVector.Create(FPadRightWidth,FPadRight));  // add as MSB
                    Concat.Add(Signal);  // add as MSB
                    if FPadLeftWidth > 0 then
                      Concat.Add(TValueVector.Create(FPadLeftWidth,FPadLeft));  // add as MSB
                    // assign to output port
                    RMWrapper.AddAssignment(RMWrapper.FPorts[FReconfSignal.GetPortName],Concat);
                  End
                else
                  raise Exception.Create('This signal must have padding')
            else
              With (FReconfSignal.FSigConn as TSigConnDyn).FConnTypeOptions do
                if (FPadLeftWidth >= 0) or (FPadRightWidth >= 0) then
                  Begin
                    // use an intermediate signal of proper width of the reconf.signal
                    St := TrimSignalPostfix(FReconfSignal.GetPortName);
                    Signal := TSignal.Create(St+'_s',FReconfSignal.GetSignal.FType);
                    RMWrapper.AddSignal(Signal);
                    RMInstance.ConnectPort(FReconfSignal.GetPortName,Signal);
                    // connect reconf.signal with indexed range of this signal
                    // TODO: here we need a range to select from the full conntype width. The current
                    // design doesn't specify a range but padding (which, preciesly, is only applicable
                    // to input signals, not outputs). Here we just use the width of the left and right
                    // padding as hints for the range. :-)
                    // check range
                    PL := Max(0,FPadLeftWidth);
                    PR := Max(0,FPadRightWidth);
                    if (PL+FReconfSignal.GetSignal.FType.GetWidthInt+PR <> (FReconfSignal.FSigConn as TSigConnDyn).FConnType.FWidth) then
                      raise Exception.CreateFmt('This signal must have correct padding: PadLeft (%d) + width (%d) + PadRight (%d) should be conntype width (%d)',
                        [PL,FReconfSignal.GetSignal.FType.GetWidthInt,PR,(FReconfSignal.FSigConn as TSigConnDyn).FConnType.FWidth]);
                    Value := TRange.Create(dirDown,(FReconfSignal.FSigConn as TSigConnDyn).FConnType.FWidth-PL-1,PR);
                    Value := TValueIndex.Create(RMWrapper.FPorts[FReconfSignal.GetPortName],Value);
                    // assign
                    RMWrapper.AddAssignment(Signal,Value);
                    // for LEC we have to set the unused input bits to constant '0'
                    if ATarget = rmtLEC then
                      Begin
                        LECWrapper.FLECSetup += '// unused input bits for padded signal ' + FDynamicPorts.Data[I].FName + LineEnding;
                        For J := 0 to PR-1 do
                          LECWrapper.FLECSetup += 'add pin constraint 0 ' + FDynamicPorts.Data[I].FName + '['+IntToStr(J)+']' + LineEnding;
                        For J := (FReconfSignal.FSigConn as TSigConnDyn).FConnType.FWidth-PL to (FReconfSignal.FSigConn as TSigConnDyn).FConnType.FWidth-1 do
                          LECWrapper.FLECSetup += 'add pin constraint 0 ' + FDynamicPorts.Data[I].FName + '['+IntToStr(J)+']' + LineEnding;
                      End;
                  End
                else
                  raise Exception.Create('This signal must have padding');
          End
        else
          Begin
            WriteLn('Connecting dynamic reconf. module port ',FReconfSignal.GetPortName,' to app. module port ',FDynamicPorts.Data[I].FName);
            RMInstance.ConnectPort(FReconfSignal.GetPortName,FModule.FPorts[FDynamicPorts.Data[I].FName])
          End
      End;

  // connect parameters for application
  // first prepare a mask for all ParamIn signals
  ParamInMask := TStringIntMap.Create;
  For I := 0 to FReconfModule.FReconfSignals.FConnTypes.Count-1 do
    With FReconfModule.FReconfSignals.FConnTypes do
      if ISHandler.FParamInCount[Data[I].FName] > 0 then
        ParamInMask.Add(Data[I].FName,0);
  // then connect all application module ports with internal signals using VHDL'2008 external names
  For I := 0 to FParamPorts.Count-1 do
    With FParamPorts.Data[I] do
      Begin
        // Directly connect with external name reference. We can't use aliases
        // here, because those would be declared in the architecture header and
        // therefore before the instance of the reconf.module (which is in the
        // architecture body). ModelSim would complain "Target of the external
        // name must be elaborated before the external name is evaluated.".
        WriteLn('Connecting param port ',FName,' to InterSynth module param port index ',FMapping);
        if FDirection = dirIn then
          Begin
            ParamInMask[FConnType.FName] := ParamInMask[FConnType.FName] or (1 shl FMapping);
            Case ATarget of
              rmtVHDL2008 : Begin
                RMWrapper.AddAssignment(
                  TExtName.Create(TSignal.Create(AReconfModuleInstName+'.'+'ParamIn_'+FConnType.FName+'_'+IntToStr(FMapping)+'_s',FConnType.FType)),
                  RMWrapper.FPorts[FName]);
              End;
              rmtLEC : Begin
                LECWrapper.FLECSetup += '// ' + FName + LineEnding;
                For J := 0 to FConnType.FWidth-1 do
                  Begin
                    St := AReconfModuleInstName+'/ParamIn_'+FConnType.FName+'_'+IntToStr(FMapping)+'_s['+IntToStr(J)+']';
                    LECWrapper.FLECSetup   += 'add ignored input ' + FName + '['+IntToStr(J)+'] -revised' + LineEnding;
                    LECWrapper.FLECSetup   += 'add primary input ' + St + ' -net -cut -revised' + LineEnding;
                    LECWrapper.FLECMapping += 'add mapped points '+FName+'['+IntToStr(J)+'] ' + St + LineEnding;
                  End;
              End;
            End;
          End
        else
          Begin
            Case ATarget of
              rmtVHDL2008 : Begin
                RMWrapper.AddAssignment(
                  RMWrapper.FPorts[FName],
                  TExtName.Create(TSignal.Create(AReconfModuleInstName+'.'+'ParamOut_'+FConnType.FName+'_'+IntToStr(FMapping)+'_s',FConnType.FType)));
              End;
              rmtLEC : Begin
                LECWrapper.FLECSetup += '// ' + FName + LineEnding;
                For J := 0 to FConnType.FWidth-1 do
                  Begin
                    St := AReconfModuleInstName+'/ParamOut_'+FConnType.FName+'_'+IntToStr(FMapping)+'_s['+IntToStr(J)+']';
                    LECWrapper.FLECSetup   += 'add ignored output ' + FName + '['+IntToStr(J)+'] -revised' + LineEnding;
                    LECWrapper.FLECSetup   += 'add primary output ' + St + ' -revised' + LineEnding;
                    LECWrapper.FLECMapping += 'add mapped points '+FName+'['+IntToStr(J)+'] ' + St + LineEnding;
                  End;
              End;
            End;
          End;
      End;
  // finally set all unused param inputs to 0
  For I := 0 to FReconfModule.FReconfSignals.FConnTypes.Count-1 do
    With FReconfModule.FReconfSignals.FConnTypes do
      Begin
        if ISHandler.FReadParamInCount.IndexOf(Data[I].FName) < 0 then
          Continue;   // no ParamIn cells for that conn.type
        For J := 0 to ISHandler.FReadParamInCount[Data[I].FName]-1 do
          if ParamInMask[Data[I].FName] and (1 shl J) = 0 then
            Begin
              WriteLn('Connecting unused ParamIn ',Data[I].FName,' ',J,' with default value');
              Case ATarget of
                rmtVHDL2008 : Begin
                  if Data[I].FType = TypeBit then
                    Value := TValueBit.Create('0')
                  else if Data[I].FType.FName = 'std_logic_vector' then
                    Value := TValueVector.Create(Data[I].FType.FRange.GetWidthInt,0)
                  else
                    raise Exception.Create('Can''t handle type '+Data[I].FType.FName+' of connection type '+Data[I].FName);
                  RMWrapper.AddAssignment(
                    TExtName.Create(TSignal.Create(AReconfModuleInstName+'.'+'ParamIn_'+Data[I].FName+'_'+IntToStr(J)+'_s',Data[I].FType)),
                    Value);
                End;
                rmtLEC : Begin
                  LECWrapper.FLECSetup += '// unused ParamIn_' + Data[I].FName + '['+IntToStr(J)+']' + LineEnding;
                  For PL := 0 to Data[I].FWidth-1 do
                    Begin
                      St := AReconfModuleInstName+'/ParamOutReg_ParamIn_'+Data[I].FName+'_'+IntToStr(J)+'/Param_o_reg['+IntToStr(PL)+']';
                      LECWrapper.FLECSetup += 'add instance constraint 0 ' + St + ' -revised' + LineEnding;
                    End;
                End;
              End;
            End;
      End;
  ParamInMask.Free;

  // connect dynamic signals with constant value
  For I := 0 to FConstantValues.Count-1 do
    With FConstantValues.Data[I] do
      Begin
        // In this list we have reconf.signals with connection/usage "dynamic",
        // "config" and "param". The (Ex.)App module only has ports for
        // "dynamic" signals, therefore only those are forwarded.
        // only signals with connection/usage "dynamic" are set as assignment
        // inside the (Ex.)App. module, "config" and "param" are set outside of the application
        // constant values with connection/usage "config" will be handled by GetReconfSignalsBitstream
        // constant values with connection/usage "param" will be handled below
        if not (FReconfSignal.FSigConn is TSigConnDyn) then
          Continue;
        WriteLn('Connecting dynamic reconf.signal with constant value ',FReconfSignal.GetPortName);
        if (FReconfSignal.FSigConn as TSigConnDyn).FConnTypeOptions.FArray then
          Begin
            // Array port
            // constant values are always outputs
            RMInstance.ConnectPort(FReconfSignal.GetPortName,FModule.FPorts[FReconfSignal.GetPortName])
          End
        else if ((FReconfSignal.FSigConn as TSigConnDyn).FConnTypeOptions.FPadLeftWidth >= 0) or
                ((FReconfSignal.FSigConn as TSigConnDyn).FConnTypeOptions.FPadRightWidth >= 0) then
          Begin
            raise Exception.Create('TODO: Implement padding for array ports for constant values');
          End
        else
          Begin
            RMInstance.ConnectPort(FReconfSignal.GetPortName,FModule.FPorts[FReconfSignal.GetPortName])
          End;
      End;

  // connect reconfig.signals with connection/usage "param"
  For I := 0 to FReconfModule.FReconfSignals.FReconfSignals.Count-1 do
    With FReconfModule.FReconfSignals.FReconfSignals.Data[I] do
      Begin
        if not (FSigConn is TSigConnParam) then
          continue;
        if GetDirection = dirIn then   // only set parameters going out of the Reconf.Module
          continue;
        // The (Ex.)App module doesn't access these reconf.signals, these have
        // to be set by the firmware before.
        // In the testbench, the ParamOutReg instances are disconnected using
        // a special VHDL configuration, therefore here we have to add drivers
        // for the reconf.module's "param" reconf.signal outputs
        if FConstantValues.IndexOf(FName) < 0 then
          Begin
            J := 0;
            WriteLn('Connecting reconf.signal with connection/usage "param" "',FName,'" with default value ',IntToBin(J,GetSignal.FType.GetWidthInt));
          End
        else
          Begin
            J := FConstantValues[FName].FValue;
            WriteLn('Connecting reconf.signal with connection/usage "param" "',FName,'" with constant value ',IntToBin(J,GetSignal.FType.GetWidthInt));
          End;
        Case ATarget of
          rmtVHDL2008 : Begin
            if GetSignal.FType = TypeBit then
              Value := TValueBit.Create(Chr(Ord('0')+J))
            else
              Value := TValueVector.Create(GetSignal.FType.GetWidthInt,J);
            // add assignment
            RMWrapper.AddAssignment(
              TExtName.Create(TSignal.Create(AReconfModuleInstName+'.'+GetPortName,GetSignal.FType)),
              Value);
            // QuestaSim 10.0 still complains
            //   Warning: (vsim-8684) No drivers exist on out port /adt7310_tb/DUT/MyReconfigLogic_0/SPC_LengthTimeout_o(15 downto 0), and its initial value is not used.
            //   Therefore, simulation behavior may occur that is not in compliance with
            //   the VHDL standard as the initial values come from the base signal /adt7310_tb/DUT/SPC_LengthTimeout_s(15 downto 0).
            //           Region: /adt7310_tb
            // although after the first simulation step, the value of e.g.
            // /adt7310_tb/DUT/SPC_LengthTimeout_s clearly is "0000..00", i.e. the
            // assigment worked. It doesn't complain about the disconnected
            // "ParamIn_xxx_n_s" signals. Currently the ParamOutReg.Param_o ports
            // for these connection/usage "param" reconf.signals are directly
            // connected (i.e. port map) to the ReconfModule output ports. If there
            // is an intermediate signal (i.e. port map to a signal, assignment of
            // this signal to the output port), QuestaSim doesn't complain any more.
            // Save the time, this could be cleaned up later (or wait for a newer
            // version of QuestaSim).
          End;
          rmtLEC : Begin
            For PL := 0 to GetSignal.FType.GetWidthInt-1 do
              Begin
                St := AReconfModuleInstName+'/ParamOutReg_'+FName+'/Param_o_reg['+IntToStr(PL)+']';
                LECWrapper.FLECSetup += 'add instance constraint '+Chr(Ord('0') + ((J shr PL) and $01)) +' ' + St + ' -revised' + LineEnding;
              End;
          End;
        End;
      End;

  // TODO: add "assert"s to check the values of the constant "config" and "param" signals

  // The "bitdata" config register will get its values using the bitstream in
  // FInterSynthBitdata, which is saved in various formats in TFlowApp.WriteBitstream.

  // The bitstream for the config register for the reconf.signals with
  // connection/usage "config" is generated with GetReconfSignalsBitstream and
  // also saved to files in TFlowApp.WriteBitstream.

  // assign concatenations of signals and default values to reconf.signal inputs
  // of connection/usage "dynamic"
  //  FReconfModule.FReconfSignals.ListReconfSignals(Nil,Nil);
  For I := 0 to InputPortions.Count-1 do
    Begin
      St := InputPortions.Keys[I];   // PortName
      IP := InputPortions.Data[I];
      RS := FReconfModule.FReconfSignals.FReconfSignals[St];
      Concat := TValueConcat.Create;
      For J := 0 to ((RS.GetSignal.FType.GetWidthInt div (RS.FSigConn as TSigConnDyn).FConnType.FWidth)-1) do
        Begin
          if IP.IndexOf(J) >= 0 then
            Begin
              //WriteLn('  Adding ',IP[J].GetVHDLValue,' at index ',J);
              Concat.Add(IP[J]);    // add as MSB
            End
          else
            Begin
              //WriteLn('  Adding 0 at index ',J);
              if (RS.FSigConn as TSigConnDyn).FConnType.FWidth = 1 then
                Concat.Add(TValueBit.Create('0'))    // add as MSB
              else
                Concat.Add(TValueVector.Create((RS.FSigConn as TSigConnDyn).FConnType.FWidth,0));   // add as MSB
            End;
        End;
      RMWrapper.AddAssignment(RMWrapper.FSignals[TrimSignalPostfix(St)+'_s'],Concat);
      InputPortions.Data[I].Free;
    End;
  InputPortions.Free;

  // connect unused inputs and outputs of reconf. module (including reconf.signals and peripheral interface to CfgIntf and ParamIntf)
  For I := 0 to RMModule.FPorts.Count-1 do
    Begin
      if RMInstance.FConnections.IndexOf(RMModule.FPorts.Data[I].FName) >= 0 then
        continue;
      //WriteLn('Connecting unused port ',RMModule.FPorts.Data[I].FName);
      if RMModule.FPorts.Data[I].FDir = dirIn then
        Begin
          if RMModule.FPorts.Data[I].FType = TypeBit then
            Value := TValueBit.Create('0')
          else if RMModule.FPorts.Data[I].FType.FName = 'std_logic_vector' then
            Value := TValueVector.Create(RMModule.FPorts.Data[I].FType.FRange.GetWidthInt,0)
          else
            raise Exception.Create('Can''t handle type '+RMModule.FPorts.Data[I].FType.FName+' of port '+RMModule.FPorts.Data[I].FName);
          RMInstance.ConnectPort(RMModule.FPorts.Keys[I],Value);
        End
      else
        Begin
          Value := RMWrapper.AddSignal(TSignal.Create(TrimSignalPostfix(RMModule.FPorts.Keys[I])+'_s',RMModule.FPorts.Data[I].FType));
          RMInstance.ConnectPort(RMModule.FPorts.Keys[I],Value);
        End;
    End;

  // for LEC we also have to disable the CfgIntf and ParamIntf
  if ATarget = rmtLEC then
    Begin
      LECWrapper.FLECSetup += '// deactivate effects of CfgIntf' + LineEnding;
      For I := 0 to (FReconfModule.FConfigRegisters as TAddressedConfigRegisters).FNumRegisters-1 do
        LECWrapper.FLECSetup += 'add instance constraint 0 ' + AReconfModuleInstName+'/'+(FReconfModule.FConfigInterface as TConfigInterface).FInstanceName+'/CfgShift_reg['+IntToStr(I)+'] -revised' + LineEnding;
      LECWrapper.FLECSetup += 'add instance constraint 0 ' + AReconfModuleInstName+'/'+(FReconfModule.FConfigInterface as TConfigInterface).FInstanceName+'/CfgMode_reg -revised' + LineEnding;
      LECWrapper.FLECSetup += '// deactivate effects of ParamIntf' + LineEnding;
      LECWrapper.FLECSetup += 'add instance constraint 0 ' + AReconfModuleInstName+'/'+(FReconfModule.FParamInterface as TParamInterface).FInstanceName+'/ParamWr_o_reg -revised' + LineEnding;
    End;
End;

Function TReconfApp.CheckNetlist : Integer;
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

Function TReconfApp.CheckNetlistExtracted(ACellLib:TReconfCells;AIgnoreCells:TFPGStringList):Integer;

Function MyError(Msg:String) : Boolean;  // return value is always true so that we can write "if MyError('blah') then continue;"
Begin
  WriteLn(Msg);
  Inc(CheckNetlistExtracted);  // error counter
  Result := true;
End;

Function CopyAttribs(ASrc,ADst:TSignal;AllowDstAttr:Boolean) : Integer;
Var I         : Integer;
    AttrName  : String;
    AttrValue : TAttributeValue;
Begin
  //WriteLn('Copy attributes from signal ',ASrc.GetVerilogValue,' (',ASrc.ClassName,') to signal ',ADst.GetVerilogValue,' (',ADst.ClassName,')');
  Result := 0;
  For I := 0 to ASrc.FAttributes.Count-1 do
    Begin
      AttrName  := ASrc.FAttributes.FAttrValList.Keys[I];
      AttrValue := ASrc.FAttributes.FAttrValList.Data[I];
      if Pos('intersynth_',AttrName) <> 1 then Continue;
      if ADst.FAttributes.FAttrValList.IndexOf(AttrName) >= 0 then
        if AllowDstAttr then
          continue
        else
          MyError('Signal '+ASrc.GetVerilogValue+' already has attribute '+AttrName+' ('+ADst.FAttributes.FAttrValList[AttrName].GetVerilog+')');
      //WriteLn('Adding attribute ',AttrValue.GetVerilog,' from signal ',ASrc.GetVerilogValue,' to signal ',ADst.GetVerilogValue);
      ADst.FAttributes.FAttrValList.Add(AttrName,AttrValue);   // just use the same instance of AttrValue, we will not modify anything here
      Inc(Result);
    End;
End;

Function CheckAttribs(SigA,SigB:TSignal) : Integer;
Var I          : Integer;
    AttrName   : String;
    AttrValueA : TValue;
    AttrValueB : TValue;
Begin
  //WriteLn('Comparing attributes from signal ',SigA.GetVerilogValue,' (',SigA.ClassName,') to signal ',SigA.GetVerilogValue,' (',SigA.ClassName,')');
  Result := 0;
  For I := 0 to SigA.FAttributes.Count-1 do
    Begin
      Inc(Result);
      AttrName := SigA.FAttributes.FAttrValList.Keys[I];
      if Pos('intersynth_',AttrName) <> 1 then Continue;
      if SigB.FAttributes.FAttrValList.IndexOf(AttrName) < 0 then
        MyError('Signal '+SigA.GetVerilogValue+' has attribute '+AttrName+' ('+SigB.FAttributes.FAttrValList[AttrName].GetVerilog+') but signal '+SigB.GetVerilogValue+' doesn''t');
      AttrValueA := SigA.FAttributes.FAttrValList.Data[I].FValue;
      AttrValueB := SigB.FAttributes.FAttrValList[AttrName].FValue;
      if AttrValueA.ClassType <> AttrValueB.ClassType then
        MyError('Attribute '+AttrName+' of signal '+SigA.GetVerilogValue+' has a different value class ('+AttrValueA.ClassName+') than of signal '+SigB.GetVerilogValue+' ('+AttrValueB.ClassName+')');
      if AttrValueA is TValueInteger then
        Begin
          if (AttrValueA as TValueInteger).FValue <> (AttrValueB as TValueInteger).FValue then
            MyError('Attribute '+AttrName+' values of signals '+SigA.GetVerilogValue+' and '+SigB.GetVerilogValue+' differ ('+IntToStr((AttrValueA as TValueInteger).FValue)+' vs. '+IntToStr((AttrValueB as TValueInteger).FValue)+')');
        End
      else if AttrValueA is TValueString then
        Begin
          if (AttrValueA as TValueString).FValue <> (AttrValueB as TValueString).FValue then
            MyError('Attribute '+AttrName+' values of signals '+SigA.GetVerilogValue+' and '+SigB.GetVerilogValue+' differ ('+(AttrValueA as TValueString).FValue+' vs. '+(AttrValueB as TValueString).FValue+')');
        End
      else
        MyError('Can''t compare value class ('+AttrValueA.ClassName+') of attribute '+AttrName+' of signals '+SigA.GetVerilogValue+' and '+SigB.GetVerilogValue);

      //WriteLn('Attribute '+AttrName+' values of signals '+SigA.GetVerilogValue+' and '+SigB.GetVerilogValue+' match ('+(AttrValueA as TValueString).FValue+')');
      Dec(Result);
    End;
End;

Function CheckAttribsConst(SigA:TSignal;SigB:TValue) : Integer;
Var Width      : Integer;
    ConnTypeSt : String;
    ConnType   : TConnType;
    PortSt     : String;
    ArrayPort  : Boolean;
    ConfigWidth : Integer;
Begin
  Result := 1;
  if SigB is TValueBit then Width := 1
  else if SigB is TValueVector then Width := (SigB as TValueVector).FWidth
  else
    raise Exception.Create('Can''t handle '+SigB.ClassName);

  if SigA.FAttributes.FAttrValList.IndexOf('intersynth_conntype') >= 0 then
    Begin
      // signal with constant value driven from a CONST cell via the MUX tree
      if SigA.FAttributes.FAttrValList['intersynth_conntype'].FValue.ClassType <> TValueString then
        if MyError('Signal '+SigA.GetVerilogValue+' with constant driver '+SigB.GetVerilogValue+' has an attribute intersynth_conntype value ('+SigA.FAttributes.FAttrValList['intersynth_conntype'].FValue.GetVerilogValue+') of type '+SigA.FAttributes.FAttrValList['intersynth_conntype'].FValue.ClassName+', only string is allowed') then Exit;

      ConnTypeSt := (SigA.FAttributes.FAttrValList['intersynth_conntype'].FValue as TValueString).FValue;

      if FReconfModule.FReconfSignals.FConnTypes.IndexOf(ConnTypeSt) < 0 then
        if MyError('Signal '+SigA.GetVerilogValue+' with constant driver '+SigB.GetVerilogValue+' intersynth_conntype '''+ConnTypeSt+''' is unknown') then Exit;

      ConnType := FReconfModule.FReconfSignals.FConnTypes[ConnTypeSt];

      ArrayPort := false;
      if SigA.FAttributes.FAttrValList.IndexOf('intersynth_port') >= 0 then
        Begin
          // constant value and an 'intersynth_port' attribute: this must be a top level output of this app.
          PortSt := (SigA.FAttributes.FAttrValList['intersynth_port'].FValue as TValueString).FValue;
          if (FReconfModule.FReconfSignals.FReconfSignals[PortSt].FSigConn as TSigConnDyn).FConnTypeOptions.FArray then
            ArrayPort := true;
          if ((FReconfModule.FReconfSignals.FReconfSignals[PortSt].FSigConn as TSigConnDyn).FConnTypeOptions.FPadLeftWidth >= 0) or
             ((FReconfModule.FReconfSignals.FReconfSignals[PortSt].FSigConn as TSigConnDyn).FConnTypeOptions.FPadRightWidth >= 0) then
            raise Exception.Create('TODO: Implement padding for array ports');
          if FConstantValues.IndexOf(PortSt) >= 0 then
            Begin
              // check that the value is correct
              if ((SigB is TValueBit)    and (FConstantValues[PortSt].FValue <> BinToInt((SigB as TValueBit).   FValue))) or
                 ((SigB is TValueVector) and (FConstantValues[PortSt].FValue <> BinToInt((SigB as TValueVector).FValue))) then
                if MyError('Constant value for port '+SigA.GetVerilogValue+' is '+SigB.GetVerilogValue+' but should be '+IntToStr(SigA.FType.GetWidthInt)+'''b'+IntToBin(FConstantValues[PortSt].FValue,SigA.FType.GetWidthInt)) then Exit;
            End;
        End;
      if not ArrayPort and (ConnType.FWidth <> Width) then
        Begin
          if MyError('Signal '+SigA.GetVerilogValue+' connection type '''+ConnTypeSt+''' requires a width of '+IntToStr(ConnType.FWidth)+', but the constant driver '+SigB.GetVerilogValue+' has wrong width of '+IntToStr(Width)) then Exit;
        End
      else if ArrayPort and ((Width mod ConnType.FWidth) <> 0) then
        Begin
          if MyError('Array signal '+SigA.GetVerilogValue+' connection type '''+ConnTypeSt+''' has width '+IntToStr(ConnType.FWidth)+', but the constant driver '+SigB.GetVerilogValue+' width '+IntToStr(Width)+' is not an integer multiple of that.') then Exit
        End;
    End
  else if SigA.FAttributes.FAttrValList.IndexOf('intersynth_config') >= 0 then
    Begin
      // signal with constant value set by configuration
      ConfigWidth := (SigA.FAttributes.FAttrValList['intersynth_config'].FValue as TValueInteger).FValue;
      if ConfigWidth <> Width then
        if MyError('Signal '+SigA.GetVerilogValue+' with constant driver '+SigB.GetVerilogValue+' has a width of '+IntToStr(Width)+', but the config value has a width of '+IntToStr(ConfigWidth)) then Exit;
    End
  else if SigA.FAttributes.FAttrValList.IndexOf('intersynth_cfgchain') >= 0 then
    Begin
      // just ignore this assignment of a constant driver to a signal with an "intersynth_cfgchain" attribute
    End
  else
    if MyError('Signal '+SigA.GetVerilogValue+' with constant driver '+SigB.GetVerilogValue+' doesn''t have an intersynth_conntype or intersynth_config attribute') then Exit;

  Result := 0;    // no error found
End;

Var I,P     : Integer;
    SIM     : TStringIntMap;
    Value   : TValue;
    Port    : String;
    Changes : Boolean;
Begin
  if not assigned(FYosysNetlist) then
    raise Exception.Create('No netlist imported.');

  // generate our own netlist as reference (if necessary)
  if not assigned(FModule) then
    GenerateNetlist;

  Result := 0;   // reset error counter

  // check that no non-instance things are around
  For I := 0 to FYosysNetlist.FAssignments.Count-1 do
    With FYosysNetlist.FAssignments[I] do
      Begin
        // ok, some assignments are ok
        if not (FDest is TPort) and not (FDest is TSignal) then
          MyError('Application has assignment ('+GetVerilog+') to '+FDest.ClassName+' (only port and signal allowed)');
        if not (FValue is TValueBit) and not (FValue is TValueVector) and not (FValue is TPort) and not (FValue is TSignal) then
          MyError('Application has assignment ('+GetVerilog+') from '+FValue.ClassName+' (only constant, port and signal allowed)');
      End;
  if FYosysNetlist.FProcesses.Count > 0 then
    MyError('Application has '+IntToStr(FYosysNetlist.FProcesses.Count)+' processes which are not allowed');

  // check that all cell instances are from the cell library
  SIM := TStringIntMap.Create;
  For I := 0 to FYosysNetlist.FInstances.Count-1 do
    With FYosysNetlist.FInstances.Data[I] do
      Begin
        if AIgnoreCells.IndexOf(FModule.FName) >= 0 then
          continue;
        if ACellLib.FReconfCells.IndexOf(FModule.FName) < 0 then
          if SIM.IndexOf(FModule.FName) < 0 then
            SIM.Add(FModule.FName,1)
          else
            SIM[FModule.FName] := SIM[FModule.FName] + 1;
      End;
  if SIM.Count > 0 then
    For I := 0 to SIM.Count-1 do
      MyError('Application uses '+IntToStr(SIM.Data[I])+' instances of module '+SIM.Keys[I]+' which is not part of the cell library');
  SIM.Clear;

  // check that all cell instance ports are connected with signals or constants (no concat or index)
  For I := 0 to FYosysNetlist.FInstances.Count-1 do
    With FYosysNetlist.FInstances.Data[I] do
      Begin
        if AIgnoreCells.IndexOf(FModule.FName) >= 0 then
          continue;
        P := CheckConnections;
        if P > 0 then
          MyError('Cell '+FModule.FName+' instance '+FName+' has '+IntToStr(P)+' unconnected ports');
        For P := 0 to FConnections.Count-1 do
          Begin
            Value := FConnections.Data[P];
            if not (Value is TValueBit) and not (Value is TValueVector) and not (Value is TPort) and not (Value is TSignal) then
              MyError('Cell '+FModule.FName+' instance '+FName+' port '+FConnections.Keys[P]+' is connected to a '+Value.ClassName+': '+Value.GetVerilogValue+' (only constant, port and signal allowed)');
          End;
      End;
  (*
   * Check conntypes and port usages
   *)
  // assignments: copy port attributes to signal attributes
  For I := 0 to FYosysNetlist.FAssignments.Count-1 do
    With FYosysNetlist.FAssignments[I] do
      Begin
        if (FDest.ClassType = TSignal) and (FValue.ClassType = TPort) then    // "FDest is TSignal" is true for descendants too (TPort)
          CopyAttribs(FValue as TSignal,FDest as TPort,False)
        else if (FDest.ClassType = TPort) and (FValue.ClassType = TSignal) then
          CopyAttribs(FDest as TPort,FValue as TSignal,False)
        else if (FDest is TSignal) and (FValue.ClassType = TValueBit) then
          //CopyAttribs(FDest as TPort,FValue as TSignal,False)
        else if (FDest is TSignal) and (FValue.ClassType = TValueVector) then
          //CopyAttribs(FDest as TPort,FValue as TSignal,False)
        else if (FDest.ClassType = TSignal) and (FValue.ClassType = TSignal) then
          // ignore here, is done in the next step below
        else if (FDest.ClassType = TPort) and (FValue.ClassType = TPort) then
          // ignore here
        else
          MyError('Assignment '''+GetVerilog+''' not supported: '+FDest.ClassName+' <= '+FValue.ClassName);
      End;
  // propagate attributes from signals to further signals
  repeat
    Changes := false;
    For I := 0 to FYosysNetlist.FAssignments.Count-1 do
      With FYosysNetlist.FAssignments[I] do
        Begin
          if (FDest.ClassType = TSignal) and (FValue.ClassType = TSignal) then
            Begin
              if CopyAttribs(FDest as TSignal,FValue as TSignal,True) > 0 then
                Changes := True;
              if CopyAttribs(FValue as TSignal,FDest as TSignal,True) > 0 then
                Changes := True;
            End;
        End;
  Until not Changes;
  // now all signals have connection types propagated from module ports, but
  // there might be intermediate signals just between internal cell instances
  // --> set attributes from cell ports
  repeat
    Changes := false;
    For I := 0 to FYosysNetlist.FInstances.Count-1 do
      With FYosysNetlist.FInstances.Data[I] do
        Begin
          if AIgnoreCells.IndexOf(FModule.FName) >= 0 then
            continue;
          For P := 0 to FConnections.Count-1 do
            Begin
              Port  := FConnections.Keys[P];
              Value := FConnections.Data[P];
              if Value.ClassType <> TSignal then
                continue;
              if (Value as TSignal).FAttributes.CountRegEx('^intersynth_') = 0 then
                Begin
                  if FModule.FPorts[Port].FAttributes.CountRegEx('^intersynth_') = 0 then
                    MyError('Cell '+FName+' ('+FModule.FName+') port '+Port+' is connected to signal '+(Value as TSignal).FName+' without (propagated) InterSynth attributes, but doesn''t have InterSynth attributes itself');
                  if CopyAttribs(FModule.FPorts[Port],Value as TSignal,False) > 0 then
                    Changes := true;
                End;
            End;
        End;
  Until not Changes;
  // now all signals have attributes propagated from module ports or from cell
  // ports and there shouldn't be any signals without attributes
  // --> check attributes
  For I := 0 to FYosysNetlist.FInstances.Count-1 do
    With FYosysNetlist.FInstances.Data[I] do
      Begin
        if AIgnoreCells.IndexOf(FModule.FName) >= 0 then
          continue;
        For P := 0 to FConnections.Count-1 do
          Begin
            Port  := FConnections.Keys[P];
            Value := FConnections.Data[P];
            if Value is TSignal then   // this is true for TSignal or TPort
              Begin
                if (Value as TSignal).FAttributes.CountRegEx('^intersynth_') > 0 then
                  CheckAttribs(FModule.FPorts[Port],Value as TSignal)
                else if Value.ClassType = TPort then
                  MyError('Cell '+FName+' ('+FModule.FName+') port '+Port+' is connected to top module port '+(Value as TPort).FName+', but this doesn''t have InterSynth attributes')
                else
                  MyError('Cell '+FName+' ('+FModule.FName+') port '+Port+' doesn''t have any InterSynth attributes');
              End
            else if (Value is TValueBit) or (Value is TValueVector) then
              Begin
                CheckAttribsConst(FModule.FPorts[Port],Value);
              End
            else
              MyError('Cell '+FModule.FName+' instance '+FName+' port '+Port+' is connected to a '+Value.ClassName+': '+Value.GetVerilogValue+' (only constant, port and signal allowed)');
          End;
      End;
  // assignments: check ConnType when connected to a constant
  For I := 0 to FYosysNetlist.FAssignments.Count-1 do
    With FYosysNetlist.FAssignments[I] do
      Begin
        if (FDest.ClassType = TSignal) and (FValue.ClassType = TPort) then    // "FDest is TSignal" is true for descendants too (TPort)
          // ignore here
        else if (FDest.ClassType = TPort) and (FValue.ClassType = TSignal) then
          // ignore here
        else if (FDest is TSignal) and (FValue.ClassType = TValueBit) then
          CheckAttribsConst(FDest as TSignal,FValue)
        else if (FDest is TSignal) and (FValue.ClassType = TValueVector) then
          CheckAttribsConst(FDest as TSignal,FValue)
        else if (FDest.ClassType = TSignal) and (FValue.ClassType = TSignal) then
          // ignore here
        else if (FDest.ClassType = TPort) and (FValue.ClassType = TPort) then
          // ignore here
        else
          MyError('Assignment '''+GetVerilog+''' not supported: '+FDest.ClassName+' <= '+FValue.ClassName);
      End;
End;

Function TReconfApp.GetUsage(ACellLib:TReconfCells):TCellUsage;
Var I,J : Integer;
Begin
  // initialize map
  Result := TCellUsage.Create;
  Result.Add('$fsm',0);
  For I := 0 to ACellLib.FReconfCells.Count-1 do
    Result.Add(ACellLib.FReconfCells.Keys[I],0);
  // count cell instances
  For I := 0 to FYosysNetlist.FInstances.Count-1 do
    With FYosysNetlist.FInstances.Data[I] do
      Begin
        J := Result.IndexOf(FModule.FName);
        if J < 0 then
          raise Exception.Create('Module '+FModule.FName+' of cell instance '+FName+' is not in cell library. Did you run CheckNetlistExtracted?');
        Result.Data[J] := Result.Data[J] + 1;
      End;
End;

Function TReconfApp.GetUsage:TCellUsage;
Var I,J : Integer;
Begin
  // initialize map
  Result := TCellUsage.Create;
  // count cell instances
  For I := 0 to FYosysNetlist.FInstances.Count-1 do
    With FYosysNetlist.FInstances.Data[I] do
      Begin
        J := Result.IndexOf(FModule.FName);
        if J < 0 then
          Result.Add(FModule.FName,1)
        else
          Result.Data[J] := Result.Data[J] + 1;
      End;
End;

Procedure TReconfApp.SetupParameters;
Var I : Integer;
Begin
  FParameters := TParameters.Create;
  // simplest method: iterate over params (separately for in and out), and assign
  // ascending addresses
  For I := 0 to FParamPorts.Count-1 do
    With FParamPorts.Data[I] do
      Begin
        if FDirection = dirIn then
          Begin
            // input into app. --> write param cell
            FParameters.Add(FName,TParameter.Create(FName,pdWrite,FConnType.FType,FHaveDefault,FDefault));
          End
        else
          Begin
            // output from app. --> read param cell
            FParameters.Add(FName,TParameter.Create(FName,pdRead,FConnType.FType,FHaveDefault,FDefault));
          End;
      End;
End;

Function TReconfApp.GetInterSynthInstance(AISHandler:TObject;AInstance:String):String;
Var ISHandler : TInterSynthHandler;
Begin
  if FYosysNetlist.FInstances.IndexOf(AInstance) < 0 then
    raise Exception.Create('No instance '+AInstance+' in application '+FName);
  ISHandler := TInterSynthHandler(AISHandler);
  With FYosysNetlist.FInstances[AInstance] do
    Result := ISHandler.GetCellInstance(FModule.FName,ISHandler.GetNodeMap(Self.FName,FName));
End;

Function TReconfApp.GetReconfSignalsBitstream:String;
Var CR : TChunkedConfigRegister;
    I  : Integer;
    Ch : TConfigChunk;
    V  : Integer;
    St : String;
    P  : Integer;
Begin
  CR := TChunkedConfigRegister(FReconfModule.FReconfSignals.FConfigRegister);
  if not assigned(CR) then
    raise Exception.Create('No config register for reconf.signals defined. Use TReconfSignals.SetupConfigAndParam!');
  Result := StringOfChar('0',CR.FLength);
  For I := 0 to FReconfModule.FReconfSignals.FReconfSignals.Count-1 do
    With FReconfModule.FReconfSignals.FReconfSignals.Data[I] do
      Begin
        if not (FSigConn is TSigConnConfig) then
          continue;
        Ch := CR.FChunks[GetPortName];
        if Ch.FLength <> GetSignal.FType.GetWidthInt then
          raise Exception.Create('Internal Error: Length of config chunk '+Ch.FName+' ('+IntToStr(Ch.FLength)+') doesn''t match width of reconf.signal '+GetPortName+' ('+IntToStr(GetSignal.FType.GetWidthInt)+')');
        if FConstantValues.IndexOf(FName) < 0 then
          V := Ch.FDefault
        else
          Begin
            V := FConstantValues[FName].FValue;
          End;
        St := IntToBin(V,Ch.FLength);
        // bit index = char index-1, i.e. the string prints reversed to a VHDL vector
        For P := 1 to Ch.FLength do
          Result[Ch.FAddress+P] := St[Ch.FLength-P+1];
      End;
End;

{ TReconfApps }

Constructor TReconfApps.Create;
Begin
  inherited Create;
  FReconfApps := TReconfAppsList.Create;
End;

Destructor TReconfApps.Destroy;
Begin
  FReconfApps.Free;
  Inherited Destroy;
End;

{ TReconfModuleNetlistWrapApp }

Constructor TReconfModuleNetlistWrapApp.Create(AReconfModule:TReconfModule;AReconfigTypeName,AReconfigInstName:String;AReconfApp:TReconfApp;AAppInstName:String;AParamInterface:TParamInterface);
Var I,J,K        : Integer;
    ConnParams   : TConnectedParameters;
    Signal       : TSignal;
    Index        : TValue;
    Value        : TValue;
    St           : String;
    Instance     : TInstance;
    PL,PR        : Integer;
Begin
  inherited Create(AReconfModule,AReconfigTypeName,AReconfigInstName);
  FReconfApp := AReconfApp;
  // here we have the reconf.module with instances of the CfgIntf and ParamIntf, plus an instance of it

  if not assigned(FReconfApp.FModule) then
    FReconfApp.GenerateNetlist;   // TReconfApp still builds the netlist itself, there is no dedicated TXyzNetlist class yet

  if not assigned(FReconfApp.FParameters) then
    FReconfApp.SetupParameters;

  FModule.FArchitectureName := 'Wrap'+FReconfApp.FName;

  // add instance of this app in the wrapping module
  FAppInstance := TInstance.Create(AAppInstName,FReconfApp.FModule);
  FModule.AddInstance(FAppInstance);

  // assign parameter addresses
  FParameters := TAddressedParameters.Create(AParamInterface);
  FParameters.Add(TParameters(FReconfModule.FReconfSignals.FParameters));   // reconf.signals
  FParameters.Add(FReconfApp.FParameters);                                  // (Ex.)App. parameters
  FParameters.AssignAddresses;

  // parameter infrastructure
  ConnParams := TConnectedParameters.Create(TParamInterface(AParamInterface),Self,FParameters);

  // iterate over all reconf signals
  For I := 0 to FReconfModule.FReconfSignals.FReconfSignals.Count-1 do
    With FReconfModule.FReconfSignals.FReconfSignals.Data[I] do
      Begin
        if FSigConn is TSigConnConst then
          Begin
            // connection/usage: constant
            FModule.AddAssignment(FModule.FPorts[GetPortName],(FSigConn as TSigConnConst).GetValue);
            // Note: TReconfSignal.GetSignal returns a TSignal or TPort of the FParent, but the ReconfModule has a port of a derived name
          End
        else if FSigConn is TSigConnConfig then
          Begin
            // connection/usage: config
            // here we make it simple: just add an assignment instead of
            // instantiating a config register and filling config values
            J := FReconfApp.FConstantValues.IndexOf(FName);    // reconf.signal name, not port name!
            if J >= 0 then
              Value := FReconfApp.FConstantValues.Data[J].GetValue
            else
              Begin
                if GetSignal.FType = TypeBit then
                  Value := TValueBit.Create('0')
                else
                  Value := TValueVector.Create(GetSignal.FType.GetWidthInt,0);   // default
              End;
            FModule.AddAssignment(FModule.FPorts[GetPortName],Value);
          End
        else if FSigConn is TSigConnParam then
          Begin
            // connection/usage: param
            // assign source/destination signal
            ConnParams.Add(TParameterData.Create(FParameters.FParameters[FName],FModule.FPorts[GetPortName]));
            // the rest is handled by ConnParams after this loop
          End
        else if FSigConn is TSigConnDirect then
          Begin
            // connection/usage: direct
            // search current TReconfSignal in list of direct ports
            K := -1;
            For J := 0 to FReconfApp.FDirectPorts.Count-1 do
              if FReconfApp.FDirectPorts.Data[J].FReconfSignal = FReconfModule.FReconfSignals.FReconfSignals.Data[I] then
                K := J;
            if K >= 0 then
              Begin
                // yes, this reconf signal is used by the app
                FAppInstance.ConnectPort(FReconfApp.FDirectPorts.Keys[K],FModule.FPorts[GetPortName]);
                Continue;
              End;
            // unused reconf signal
            if GetDirection = dirIn then
              Continue;
            // output --> set a default value
            if GetSignal.FType = TypeBit then
              Value := TValueBit.Create('0')
            else
              Value := TValueVector.Create(GetSignal.FType.GetWidthInt,0);   // default
            FModule.AddAssignment(FModule.FPorts[GetPortName],Value);
            // this may not be used to set constant values
          End
        else if FSigConn is TSigConnDyn then
          Begin
            // connection/usage: dynamic
            // search current TReconfSignal in list of dynamic ports
            K := 0;
            For J := 0 to FReconfApp.FDynamicPorts.Count-1 do
              if FReconfApp.FDynamicPorts.Data[J].FReconfSignal = FReconfModule.FReconfSignals.FReconfSignals.Data[I] then
                Begin
                  // yes, this reconf signal is used by the app
                  WriteLn('Connecting app. port ',FReconfApp.FDynamicPorts.Keys[J],' with dynamic reconf.signal ',GetPortName,': ',(FSigConn as TSigConnDyn).FConnType.FName,' ',(FSigConn as TSigConnDyn).FConnTypeOptions.ToString);
                  FReconfApp.FDynamicPorts.Data[J].Check(FSigConn as TSigConnDyn);
                  if (FSigConn as TSigConnDyn).FConnType.FWidth < GetSignal.FType.GetWidthInt then
                    Begin
                      // width of signal in app. is smaller than the reconf.signal
                      //WriteLn('Conntype width ',(FSigConn as TSigConnDyn).FConnType.FWidth,' < signal width ',GetSignal.FType.GetWidthInt);
                      if (FSigConn as TSigConnDyn).FConnTypeOptions.FArray then
                        Begin
                          // reconf.signal is an array signal --> use a sub-range
                          if (FSigConn as TSigConnDyn).FConnType.FWidth = 1 then
                            Index := TValueInteger.Create(FReconfApp.FDynamicPorts.Data[J].FIndex)
                          else
                            With FReconfApp.FDynamicPorts.Data[J], (FSigConn as TSigConnDyn).FConnType do
                              Index := TRange.Create(dirDown,(FIndex+1)*FWidth-1,FIndex*FWidth);
                          Value := TValueIndex.Create(FModule.FPorts[GetPortName], Index);
                          // connect
                          FAppInstance.ConnectPort(FReconfApp.FDynamicPorts.Keys[J], Value);
                        End
                      else
                        raise Exception.Create('This signal must be an array port');
                    End
                  else if (FSigConn as TSigConnDyn).FConnType.FWidth > GetSignal.FType.GetWidthInt then
                    Begin
                      // width of signal in app. (conntype) is larger than the reconf.signal
                      //WriteLn('Conntype width ',(FSigConn as TSigConnDyn).FConnType.FWidth,' > signal width ',GetSignal.FType.GetWidthInt);
                      if GetDirection = dirOut then
                        With (FSigConn as TSigConnDyn).FConnTypeOptions do
                          if (FPadLeftWidth >= 0) or (FPadRightWidth >= 0) then
                            Begin
                              // check range
                              PL := Max(0,FPadLeftWidth);
                              PR := Max(0,FPadRightWidth);
                              if (PL+GetSignal.FType.GetWidthInt+PR <> (FSigConn as TSigConnDyn).FConnType.FWidth) then
                                raise Exception.CreateFmt('This signal must have correct padding: PadLeft (%d) + width (%d) + PadRight (%d) should be conntype width (%d)',
                                  [PL,GetSignal.FType.GetWidthInt,PR,(FSigConn as TSigConnDyn).FConnType.FWidth]);
                              // use an intermediate signal of full conntype width for the instance
                              St := TrimSignalPostfix(GetPortName);
                              Signal := TSignal.Create(St+'_s',(FSigConn as TSigConnDyn).FConnType.FType);
                              FModule.AddSignal(Signal);
                              FAppInstance.ConnectPort(FReconfApp.FDynamicPorts.Keys[J],Signal);
                              // connect reconf.signal with indexed range of this signal
                              // TODO: here we need a range to select from the full conntype width. The current
                              // design doesn't specify a range but padding (which, preciesly, is only applicable
                              // to input signals, not outputs). Here we just use the width of the left and right
                              // padding as hints for the range. :-)
                              Value := TRange.Create(dirDown,(FSigConn as TSigConnDyn).FConnType.FWidth-PL-1,PR);
                              Value := TValueIndex.Create(Signal,Value);
                              FModule.AddAssignment(FModule.FPorts[GetPortName],Value);
                            End
                          else
                            raise Exception.Create('This signal must have padding')
                      else
                        With (FSigConn as TSigConnDyn).FConnTypeOptions do
                          if (FPadLeftWidth >= 0) or (FPadRightWidth >= 0) then
                            Begin
                              // pad reconf.signal to desired width
                              Value := TValueConcat.Create;
                              if FPadRightWidth > 0 then
                                TValueConcat(Value).Add(TValueVector.Create(FPadRightWidth,FPadRight));  // add as MSB
                              TValueConcat(Value).Add(FModule.FPorts[GetPortName]);  // add as MSB
                              if FPadLeftWidth > 0 then
                                TValueConcat(Value).Add(TValueVector.Create(FPadLeftWidth,FPadLeft));  // add as MSB
                              (* Directly connecting a TValueConcat to an instance port is not allowed.
                               * See also TReconfModuleNetlistWithInterSynth.Create
                               *
                               * ModelSim in VHDL'87 mode complains:
                               *   Actual (infix expression) for formal "AdcValue_i" is not a signal.
                               *   Value associated with 'AdcValue_i' does not have a static name.
                               * ModelSim in VHDL'93 and VHDL'2002 mode complains:
                               *   Use of non globally static actual (infix expression) of formal "AdcValue_i" requires VHDL 2008.
                               *)
                              St := TrimSignalPostfix(GetPortName);
                              Signal := TSignal.Create(St+'_s',(FSigConn as TSigConnDyn).FConnType.FType);
                              FModule.AddSignal(Signal);
                              FModule.AddAssignment(Signal,Value);
                              // connect
                              FAppInstance.ConnectPort(FReconfApp.FDynamicPorts.Keys[J],Signal);
                            End
                          else
                            raise Exception.Create('This signal must have padding');
                    End
                  else
                    Begin
                      // width as specified by connection type and width of the reconf.signal are identical
                      // check for internal consistency
                      With (FSigConn as TSigConnDyn).FConnTypeOptions do
                        if FArray or (FPadLeftWidth <> -1) or (FPadRightWidth <> -1) then
                          raise Exception.CreateFmt('Strange, width %d of dynamic reconf. signal %s matches '+
                                                    'the width %d defined by the connection type %s, but the '+
                                                    'connection type options suggest an array port or padding: %s',
                                                    [GetSignal.FType.GetWidthInt,GetPortName,
                                                     (FSigConn as TSigConnDyn).FConnType.FWidth,(FSigConn as TSigConnDyn).FConnType.FName,
                                                     (FSigConn as TSigConnDyn).FConnTypeOptions.ToString]);
                      // connect
                      FAppInstance.ConnectPort(FReconfApp.FDynamicPorts.Keys[J],FModule.FPorts[GetPortName]);
                    End;
                  // use as counter that this signal was used or at least an array element of it, for which no default value has to be assigned
                  Inc(K);
                End;
            // unused reconf signal?
            if (GetSignal.FType.GetWidthInt <= (FSigConn as TSigConnDyn).FConnType.FWidth) then
              Begin
                // Signal width and ConnType width are identical
                if      K = 1 then Continue
                else if K > 1 then raise Exception.CreateFmt('Strange, signal width %d and conntype width %d are identical, but this signal is used multiple times (%d)',[GetSignal.FType.GetWidthInt,(FSigConn as TSigConnDyn).FConnType.FWidth,K]);
              End
            else if (GetSignal.FType.GetWidthInt > (FSigConn as TSigConnDyn).FConnType.FWidth) then
              Begin
                // Signal is wider than conntype --> multiple instances
                if K >= (GetSignal.FType.GetWidthInt div (FSigConn as TSigConnDyn).FConnType.FWidth) then
                  Continue;
              End
            else
              Begin
                // Signal width is less than conntype --> padding
                if      K = 1 then Continue
                else if K > 1 then raise Exception.CreateFmt('Strange, signal width %d is smaller than conntype width %d, but this signal is used multiple times (%d)',[GetSignal.FType.GetWidthInt,(FSigConn as TSigConnDyn).FConnType.FWidth,K]);
              End;
            // input into ReconfModule?
            if GetDirection = dirIn then
              Continue;
            // output --> set a value, first see if we have a given value
            J := FReconfApp.FConstantValues.IndexOf(GetPortName);
            if J >= 0 then
              Begin
                // the constant value is set inside the reconf. app. module, so connect it
                FAppInstance.ConnectPort(FReconfApp.FConstantValues.Keys[J],FModule.FPorts[GetPortName]);
              End
            else
              Begin
                if (FSigConn as TSigConnDyn).FConnTypeOptions.FArray then
                  Begin
                    For J := 0 to (GetSignal.FType.GetWidthInt div (FSigConn as TSigConnDyn).FConnType.FWidth)-1 do
                      Begin
                        if (FReconfApp.FArraySignalInfos.IndexOf(FName) >= 0) and FReconfApp.FArraySignalInfos[FName].IsUsed(J) then
                          Continue;   // used
                        if (FSigConn as TSigConnDyn).FConnType.FType = TypeBit then
                          Begin
                            Index := TValueInteger.Create(J);
                            Value := TValueBit.Create('0');
                          End
                        else
                          Begin
                            Index := FReconfApp.FArraySignalInfos[FName].IndexToRange(J);
                            Value := TValueVector.Create((FSigConn as TSigConnDyn).FConnType.FWidth,0);   // default
                          End;
                        FModule.AddAssignment(TValueIndex.Create(FModule.FPorts[GetPortName],Index),Value);
                      End;
                  End
                else
                  Begin
                    // set in architecture
                    if GetSignal.FType = TypeBit then
                      Value := TValueBit.Create('0')
                    else
                      Value := TValueVector.Create(GetSignal.FType.GetWidthInt,0);   // default
                    FModule.AddAssignment(FModule.FPorts[GetPortName],Value);
                  End;
              End;
          End
        else
          raise Exception.Create('Unknown signal connection '+FSigConn.ClassName);
      End;
  // iterate over all params of the app, create and connect signals and update FParameters
  For I := 0 to FReconfApp.FParamPorts.Count-1 do
    With FReconfApp.FParamPorts.Data[I] do
      Begin
        St := TrimSignalPostfix(FName);
        Signal := TSignal.Create(St+'_s',FConnType.FType);
        FModule.AddSignal(Signal);
        FAppInstance.ConnectPort(FName,Signal);
        ConnParams.Add(TParameterData.Create(FParameters.FParameters[FName],Signal));
      End;

  // config interface: it is unused, but we have to set all generics and connect
  // all ports, so we do some dummy stuff here
  Instance := FModule.FInstances[(FReconfModule.FConfigInterface as TConfigInterface).FInstanceName];
  Instance.SetGeneric('NumCfgs',TValueInteger.Create(1));
  Instance.ConnectPort('CfgClk_o',    FModule.AddSignal(TSignal.Create('CfgClk_s',    TType.Create('std_logic_vector',dirDown,0,0))));
  Instance.ConnectPort('CfgMode_o',   FModule.AddSignal(TSignal.Create('CfgMode_s',   TypeBit)));
  Instance.ConnectPort('CfgShift_o',  FModule.AddSignal(TSignal.Create('CfgShift_s',  TType.Create('std_logic_vector',dirDown,0,0))));
  Instance.ConnectPort('CfgDataOut_o',FModule.AddSignal(TSignal.Create('CfgDataOut_s',TypeBit)));
  Signal := FModule.AddSignal(TSignal.Create('CfgDataIn_s', TType.Create('std_logic_vector',dirDown,0,0)));
  Instance.ConnectPort('CfgDataIn_i', Signal);
  FModule.AddAssignment(Signal,TValueVector.Create(1,0)).FComment := 'just a fixed value for the config interface';
  Instance.CheckGenerics;
  Instance.CheckConnections;

  // add parameterization infrastructure (param out cells, ...)
  // param interface
  WriteLn('Connecting ParamIntf');
  ConnParams.ConnectParamIntf;
  WriteLn('Creating param registers');
  ConnParams.CreateParameters;
  WriteLn('Connecting param data signals');
  ConnParams.ConnectData;
  WriteLn('Connecting param control signals');
  ConnParams.ConnectControl;

  FAppInstance.CheckConnections;
  FAppInstance.CheckGenerics;
End;

Destructor TReconfModuleNetlistWrapApp.Destroy;
Begin
  FParameters.Free;
  // TODO: more to Free() here
  Inherited Destroy;
End;

End.

