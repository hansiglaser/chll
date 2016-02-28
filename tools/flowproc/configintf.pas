Unit ConfigIntf;

{$mode objfpc}{$H+}

Interface

Uses
  Classes, SysUtils, FGL, Netlist, ReconfModule, PeriphIntf, Utils;

Type

  { TConfigInterface }

  TConfigInterface = class(IPeriphInst)
    FInstanceName : String;
    FCfgRegModule : TModule;
    Constructor Create(AInstanceName:String);
    Destructor  Destroy; override;
    Procedure AddToNetlist(ANetlist:TReconfModuleNetlist); virtual; abstract;
  End;

  { TConfigInterfaceOpenMSP430 }

  TConfigInterfaceOpenMSP430 = class(TConfigInterface)
    FPeriphInst : TPeriphInstOpenMSP430;
    Constructor Create(APeriphInst:TPeriphInstOpenMSP430;AInstanceName:String);
    Destructor  Destroy; override;
    Procedure AddToNetlist(ANetlist:TReconfModuleNetlist); override;
  End;

  (*** Config Register Base Information **************************************)

  { TConfigRegister }

  TConfigRegister = class    // base class
    FName       : String;
    FLength     : Integer;
    FDefault    : Integer;   // TODO: we need something wider here!
  protected
    Constructor Create(AName:String;ALength:Integer;ADefault:Integer);
  End;

  { TInternalConfigRegister }

  TInternalConfigRegister = class(TConfigRegister)
    Constructor Create(AName:String;ALength:Integer;ADefault:Integer);
  End;

  { TExternalConfigRegister }

  TExternalConfigRegister = class(TConfigRegister)
    Constructor Create(AName:String;ALength:Integer;ADefault:Integer);
  End;

  { TConfigChunk }

  TConfigChunk = class
    FName    : String;
    FLength  : Integer;
    FDefault : Integer;
    FAddress : Integer;
    Constructor Create(AName:String;ALength:Integer;ADefault:Integer);
  End;

  TConfigChunks = specialize TFPGMap<String,TConfigChunk>;

  { TChunkedConfigRegister }

  TChunkedConfigRegister = class(TExternalConfigRegister)
    FChunks : TConfigChunks;
    Constructor Create(AName:String);
    Destructor Destroy; override;
    Procedure Add(AChunk:TConfigChunk);
    Procedure AssignAddresses;
    Procedure List;
  End;

  TConfigRegisters = specialize TFPGMap<String,TConfigRegister>;

  (*** Addressed Config Registers ********************************************)

  { TAddressedConfigRegister }

  TAddressedConfigRegister = class
    FConfigRegister : TConfigRegister;
    FAddress        : Integer;
    Constructor Create(AConfigRegister:TConfigRegister);
  End;

  TAddressedConfigRegisterList       = specialize TFPGMap<String,TAddressedConfigRegister>;
  TAddressedConfigRegisterSortedList = specialize TFPGMap<Integer,TAddressedConfigRegister>;    // key: address

  { TAddressedConfigRegisters }

  TAddressedConfigRegisters = class
    FConfigRegisters       : TAddressedConfigRegisterList;
    FSortedConfigRegisters : TAddressedConfigRegisterSortedList;
    FNumRegisters          : Integer;
    Constructor Create;
    Destructor  Destroy; override;
    Procedure Add(AConfigRegister:TConfigRegister);
    Procedure Add(AConfigRegisters:TConfigRegisters);
    Procedure AssignAddresses;
    Procedure List;
  End;

  (*** Connected Config Registers: Data **************************************)

  TConfigRegisterControlBase = class;

  { TConfigRegisterData }

  TConfigRegisterData = class
    FConfigRegister : TAddressedConfigRegister;
    Constructor Create(AConfigRegister:TAddressedConfigRegister);
    Function CreateConfigRegister(AConfigInterface:TConfigInterface;ANetlist:TReconfModuleNetlist) : TConfigRegisterControlBase; virtual; abstract;
    Procedure Connect(AConfigInterface:TConfigInterface;ANetlist:TReconfModuleNetlist); virtual; abstract;
  End;

  { TInternalConfigRegisterData }

  TInternalConfigRegisterData = class(TConfigRegisterData)
    Constructor Create(AConfigRegister:TAddressedConfigRegister);
    Function CreateConfigRegister(AConfigInterface:TConfigInterface;ANetlist:TReconfModuleNetlist):TConfigRegisterControlBase; override;
    Procedure Connect(AConfigInterface:TConfigInterface;ANetlist:TReconfModuleNetlist); override;
  End;

  { TExternalConfigRegisterData }

  TExternalConfigRegisterData = class(TConfigRegisterData)
    FSignal   : TSignal;
    FInstance : TInstance;
    Constructor Create(AConfigRegister:TAddressedConfigRegister;ASignal:TSignal);
    Function CreateConfigRegister(AConfigInterface:TConfigInterface;ANetlist:TReconfModuleNetlist):TConfigRegisterControlBase; override;
    Procedure Connect(AConfigInterface:TConfigInterface;ANetlist:TReconfModuleNetlist); override;
  End;

  TChunkSignals = specialize TFPGMap<String,TSignal>;    // key: TConfigChunk.FName

  { TChunkedConfigRegisterData }

  TChunkedConfigRegisterData = class(TExternalConfigRegisterData)
    FSignals : TChunkSignals;
    Constructor Create(AConfigRegister:TAddressedConfigRegister);
    Destructor Destroy; override;
    Procedure SetSignal(AChunk:String;ASignal:TSignal);
    Function CheckSignals(Out Errors:String):Boolean;
    Procedure Connect(AConfigInterface:TConfigInterface;ANetlist:TReconfModuleNetlist); override;
  End;

  (*** Connected Config Registers: Control ***********************************)

  { TConfigRegisterControlBase }

  TConfigRegisterControlBase = class   // base class
    Procedure Connect(AConfigInterface:TConfigInterface;ANetlist:TReconfModuleNetlist); virtual; abstract;
  End;

  TConfigRegisterControl = class(TConfigRegisterControlBase)
    FConfigRegister : TConfigRegisterData;
    FInstance       : TInstance;
    FCfgMode        : String;
    FCfgClk         : String;
    FCfgShift       : String;
    FCfgDataIn      : String;
    FCfgDataOut     : String;
    Constructor Create(AConfigRegister:TConfigRegisterData;AInstance:TInstance;ACfgMode,ACfgClk,ACfgShift,ACfgDataIn,ACfgDataOut:String);
    Procedure Connect(AConfigInterface:TConfigInterface;ANetlist:TReconfModuleNetlist); override;
  End;

  { TConfigRegisterControlGroupInstance }

  TConfigRegisterControlGroupInstance = class
    FConfigRegister : TConfigRegisterData;
    FInstance       : TInstance;
    FCfgClk         : String;
    FCfgShift       : String;
    FCfgDataOut     : String;
    Constructor Create(AConfigRegister:TConfigRegisterData;AInstance:TInstance;ACfgClk,ACfgShift,ACfgDataOut:String);
    Procedure Connect(AConfigInterface:TConfigInterface;ANetlist:TReconfModuleNetlist);
  End;

  TConfigRegisterControlGroupInstances = specialize TFPGMap<String,TConfigRegisterControlGroupInstance>;

  { TConfigRegisterControlGroup }

  TConfigRegisterControlGroup = class(TConfigRegisterControlBase)
    FInstance        : TInstance;
    FCfgMode         : String;
    FCfgDataIn       : String;
    FConfigRegisters : TConfigRegisterControlGroupInstances;
    Constructor Create(AInstance:TInstance;ACfgMode,ACfgDataIn:String);
    Destructor Destroy; override;
    Procedure Add(AConfigRegister:TConfigRegisterData;ACfgClk,ACfgShift,ACfgDataOut:String);
    Procedure Connect(AConfigInterface:TConfigInterface;ANetlist:TReconfModuleNetlist); override;
  End;

  (*** Connected Config Registers: Common ************************************)

  TConfigRegisterControls = specialize TFPGObjectList<TConfigRegisterControlBase>;
  TConfigRegisterDatas    = specialize TFPGObjectList<TConfigRegisterData>;

  { TConnectedConfigRegisters }

  TConnectedConfigRegisters = class
    FConfigInterface : TConfigInterface;
    FNetlist         : TReconfModuleNetlist;
    FConfigRegisters : TAddressedConfigRegisters;
    FControl         : TConfigRegisterControls;
    FData            : TConfigRegisterDatas;
    Constructor Create(AConfigInterface:TConfigInterface;ANetlist:TReconfModuleNetlist;AConfigRegisters:TAddressedConfigRegisters);
    Destructor  Destroy; override;

    Procedure Add(AControl:TConfigRegisterControlBase);
    Procedure Add(AData:TConfigRegisterData);
    Function  CheckData(Out Errors:String):Boolean;

    Procedure ConnectCfgIntf;
    Procedure CreateConfigRegisters;
    Procedure ConnectData;
    Procedure ConnectControl;
  End;

Implementation

{ TConfigInterface }

Constructor TConfigInterface.Create(AInstanceName:String);
Begin
  inherited Create;
  FInstanceName := AInstanceName;

  // create config register module
  FCfgRegModule := TModule.Create('ConfigRegister');
  // Generics
  FCfgRegModule.AddGeneric(TGeneric.Create('Width', TypeInt,TValueInteger.Create(1)),0);
  // Ports
  FCfgRegModule.AddPort(TPort.Create('Reset_n_i',   dirIn, TypeBit));
  FCfgRegModule.AddPort(TPort.Create('Output_o',    dirOut,TType.Create('std_logic_vector',
    TRange.Create(dirDown,
      TValueOperatorMinus.Create(FCfgRegModule.FGenerics['Width'],TValueInteger.Create(1)),
      TValueInteger.Create(0)))));
  // Config Interface
  FCfgRegModule.AddPort(TPort.Create('CfgMode_i',   dirIn, TypeBit));
  FCfgRegModule.AddPort(TPort.Create('CfgClk_i',    dirIn, TypeBit));
  FCfgRegModule.AddPort(TPort.Create('CfgShift_i',  dirIn, TypeBit));
  FCfgRegModule.AddPort(TPort.Create('CfgDataIn_i', dirIn, TypeBit));
  FCfgRegModule.AddPort(TPort.Create('CfgDataOut_o',dirOut,TypeBit));
End;

Destructor TConfigInterface.Destroy;
Begin
  FCfgRegModule.Free;
  Inherited Destroy;
End;

{ TConfigInterfaceOpenMSP430 }

Constructor TConfigInterfaceOpenMSP430.Create(APeriphInst:TPeriphInstOpenMSP430;AInstanceName:String);
Begin
  inherited Create(AInstanceName);
  FPeriphInst := APeriphInst;
  FPeriphInst.FInst := Self;
End;

Destructor TConfigInterfaceOpenMSP430.Destroy;
Begin
  Inherited Destroy;
End;

Procedure TConfigInterfaceOpenMSP430.AddToNetlist(ANetlist:TReconfModuleNetlist);
Var Module       : TModule;
    Instance     : TInstance;
    RangeNumCfgs : TRange;
Begin
  //// Create Module /////////////////////////////////////////////////////////
  Module := TModule.Create('CfgIntf');
  // Generics
  Module.AddGeneric(TGeneric.Create('NumCfgs', TypeInt,TValueInteger.Create(    3)),0);
  Module.FGenerics['NumCfgs'].FComment := 'Number of configuration chains';
  Module.AddGeneric(TGeneric.Create('BaseAddr',TypeInt,TValueIntegerHex.Create($0180)),1);
  // Ports
  Module.AddPort(TPort.Create('Reset_n_i',    dirIn, TypeBit));
  Module.AddPort(TPort.Create('Clk_i',        dirIn, TypeBit));
  // OpenMSP430 Interface
  Module.AddPort(TPort.Create('PerAddr_i',    dirIn, TType.Create('std_logic_vector',dirDown,13,0)));
  Module.AddPort(TPort.Create('PerDIn_i',     dirIn, TType.Create('std_logic_vector',dirDown,15,0)));
  Module.AddPort(TPort.Create('PerDOut_o',    dirOut,TType.Create('std_logic_vector',dirDown,15,0)));
  Module.AddPort(TPort.Create('PerWr_i',      dirIn, TType.Create('std_logic_vector',dirDown, 1,0)));
  Module.AddPort(TPort.Create('PerEn_i',      dirIn, TypeBit));
  Module.FPorts['PerAddr_i'].FComment := 'OpenMSP430 Interface';
  // Config Interface
  RangeNumCfgs := TRange.Create(dirDown,TValueOperatorMinus.Create(Module.FGenerics['NumCfgs'],TValueInteger.Create(1)),TValueInteger.Create(0));
  Module.AddPort(TPort.Create('CfgClk_o',    dirOut,TType.Create('std_logic_vector',RangeNumCfgs)),19);
  Module.AddPort(TPort.Create('CfgMode_o',   dirOut,TypeBit),                                      20);
  Module.AddPort(TPort.Create('CfgShift_o',  dirOut,TType.Create('std_logic_vector',RangeNumCfgs)),21);
  Module.AddPort(TPort.Create('CfgDataOut_o',dirOut,TypeBit),                                      22);
  Module.AddPort(TPort.Create('CfgDataIn_i', dirIn, TType.Create('std_logic_vector',RangeNumCfgs)),23);

  // signals for config bits and connections to the instance will be done in finish_reconf_module

  //// Create Instance ///////////////////////////////////////////////////////
  Instance := TInstance.Create(FInstanceName,Module);
  // generic "NumCfgs" is set in finish_reconf_module
  Instance.SetGeneric('BaseAddr',TValueIntegerHex.Create(FPeriphInst.FBaseAddr));

  Instance.ConnectPort('Reset_n_i',ANetlist.FResetPort);
  Instance.ConnectPort('Clk_i',    ANetlist.FClockPort);
  // Peripheral Interface to CPU
  Instance.ConnectPort('PerAddr_i',FPeriphInst.GetPerAddr(ANetlist));
  Instance.ConnectPort('PerDIn_i', FPeriphInst.GetPerDIn (ANetlist));
  Instance.ConnectPort('PerDOut_o',FPeriphInst.GetPerDOut(ANetlist));
  Instance.ConnectPort('PerWr_i',  FPeriphInst.GetPerWr  (ANetlist));
  Instance.ConnectPort('PerEn_i',  FPeriphInst.GetPerEn  (ANetlist));

  Instance.CheckConnections;   // CfgClk_o, CfgDataIn_i, CfgDataOut_o, CfgMode_o, CfgShift_o
  Instance.CheckGenerics;      // "NumCfgs"

  ANetlist.FModule.AddInstance(Instance);
  ANetlist.FModule.FInstances[FInstanceName].FComment := 'Configuration Interface';
End;

(*** Config Register Base Information ****************************************)

{ TConfigRegister }

Constructor TConfigRegister.Create(AName:String;ALength:Integer;ADefault:Integer);
Begin
  inherited Create;
  FName    := AName;
  FLength  := ALength;
  FDefault := ADefault;
End;

{ TInternalConfigRegister }

Constructor TInternalConfigRegister.Create(AName:String;ALength:Integer;ADefault:Integer);
Begin
  inherited Create(AName,ALength,ADefault);
End;

{ TExternalConfigRegister }

Constructor TExternalConfigRegister.Create(AName:String;ALength:Integer;ADefault:Integer);
Begin
  inherited Create(AName,ALength,ADefault);
End;

{ TConfigChunk }

Constructor TConfigChunk.Create(AName:String;ALength:Integer;ADefault:Integer);
Begin
  inherited Create;
  FName    := AName;
  FLength  := ALength;
  FDefault := ADefault;
  FAddress := -1;   // unset
End;

{ TChunkedConfigRegister }

Constructor TChunkedConfigRegister.Create(AName:String);
Begin
  inherited Create(AName,-1,-1);   // the default isn't really used, because each chunk has its own default
  FChunks := TConfigChunks.Create;
End;

Destructor TChunkedConfigRegister.Destroy;
Var I : Integer;
Begin
  For I := 0 to FChunks.Count-1 do
    FChunks.Data[I].Free;
  FChunks.Free;
  Inherited Destroy;
End;

Procedure TChunkedConfigRegister.Add(AChunk:TConfigChunk);
Begin
  if FLength >= 0 then
    raise Exception.Create('Addresses were already assigned, you can''t add more chunks');
  FChunks.Add(AChunk.FName,AChunk);
End;

Procedure TChunkedConfigRegister.AssignAddresses;
Var I : Integer;
Begin
  if FLength >= 0 then
    raise Exception.Create('Addresses were already assigned');
  FLength := 0;
  For I := 0 to FChunks.Count-1 do
    Begin
      FChunks.Data[I].FAddress := FLength;
      Inc(FLength,FChunks.Data[I].FLength);
    End;
End;

Procedure TChunkedConfigRegister.List;
Var I : Integer;
Begin
  if FLength < 0 then
    Begin
      // addresses not yet assigned
      WriteLn('  Chunks of ',FName,':');
      For I := 0 to FChunks.Count-1 do
        WriteLn('    ',FChunks.Keys[I],' (width: ',FChunks.Data[I].FLength,')');
    End
  else
    Begin
      WriteLn('  Chunks of ',FName,' (width: ',FLength,'):');
      For I := 0 to FChunks.Count-1 do
        WriteLn('    ',(FChunks.Data[I].FAddress+FChunks.Data[I].FLength-1):4,':',FChunks.Data[I].FAddress:4,': ',FChunks.Keys[I],' (width: ',FChunks.Data[I].FLength,')');
    End;
End;

(*** Addressed Config Registers **********************************************)

{ TAddressedConfigRegister }

Constructor TAddressedConfigRegister.Create(AConfigRegister:TConfigRegister);
Begin
  inherited Create;
  FConfigRegister := AConfigRegister;
  FAddress        := -1;   // unset
End;

{ TAddressedConfigRegisters }

Constructor TAddressedConfigRegisters.Create;
Begin
  inherited Create;
  FConfigRegisters       := TAddressedConfigRegisterList.Create;
  FSortedConfigRegisters := Nil;   // set by AssignAddresses
  FNumRegisters          := -1;    // set by AssignAddresses
End;

Destructor TAddressedConfigRegisters.Destroy;
Var I : Integer;
Begin
  For I := 0 to FConfigRegisters.Count-1 do
    FConfigRegisters.Data[I].Free;
  FConfigRegisters.Free;
  FSortedConfigRegisters.Free;
  Inherited Destroy;
End;

Procedure TAddressedConfigRegisters.Add(AConfigRegister:TConfigRegister);
Begin
  if FConfigRegisters.IndexOf(AConfigRegister.FName) >= 0 then
    raise Exception.Create('Can''t add this parameter, because another parameter named '''+AConfigRegister.FName+''' already exists.');
  FConfigRegisters.Add(AConfigRegister.FName,TAddressedConfigRegister.Create(AConfigRegister));
End;

Procedure TAddressedConfigRegisters.Add(AConfigRegisters:TConfigRegisters);
Var I : Integer;
Begin
  if assigned(FSortedConfigRegisters) then
    raise Exception.Create('Addresses already assigned, you can''t add more config registers');
  For I := 0 to AConfigRegisters.Count-1 do
    Add(AConfigRegisters.Data[I]);
End;

Procedure TAddressedConfigRegisters.AssignAddresses;
Var I : Integer;
    A : Integer;
Begin
  if assigned(FSortedConfigRegisters) then
    raise Exception.Create('Addresses already assigned');

  FSortedConfigRegisters := TAddressedConfigRegisterSortedList.Create;
  FSortedConfigRegisters.Sorted     := true;
  FSortedConfigRegisters.Duplicates := dupError;

  A := 0;
  For I := 0 to FConfigRegisters.Count-1 do
    Begin
      FConfigRegisters.Data[I].FAddress := A;
      FSortedConfigRegisters.Add(A,FConfigRegisters.Data[I]);
      Inc(A);
    End;
  FNumRegisters := A;
End;

Procedure TAddressedConfigRegisters.List;
Var I : Integer;
Begin
  WriteLn('  Config Registers:');
  if assigned(FSortedConfigRegisters) then
    Begin
      For I := 0 to FSortedConfigRegisters.Count-1 do
        With FSortedConfigRegisters.Data[I],FConfigRegister do
          WriteLn('    $',IntToHex(FAddress,2),'  ',FName,StringOfChar(' ',20-Length(FName)),' : ',FLength);
    End
  else
    Begin
      For I := 0 to FConfigRegisters.Count-1 do
        With FConfigRegisters.Data[I],FConfigRegister do
          WriteLn('    ',FName,StringOfChar(' ',20-Length(FName)),' : ',FLength);
    End;

  // list all TChunkedConfigRegister;
  For I := 0 to FConfigRegisters.Count-1 do
    if FConfigRegisters.Data[I].FConfigRegister is TChunkedConfigRegister then
      (FConfigRegisters.Data[I].FConfigRegister as TChunkedConfigRegister).List;
End;

(*** Connected Config Registers: Data ****************************************)

{ TConfigRegisterData }

Constructor TConfigRegisterData.Create(AConfigRegister:TAddressedConfigRegister);
Begin
  inherited Create;
  FConfigRegister := AConfigRegister;
End;

{ TInternalConfigRegisterData }

Constructor TInternalConfigRegisterData.Create(AConfigRegister:TAddressedConfigRegister);
Begin
  if not (AConfigRegister.FConfigRegister is TInternalConfigRegister) then
    raise Exception.Create('TInternalConfigRegisterData only works with TInternalConfigRegister, but not '+AConfigRegister.FConfigRegister.ClassName);
  inherited Create(AConfigRegister);
End;

Function TInternalConfigRegisterData.CreateConfigRegister(AConfigInterface:TConfigInterface;ANetlist:TReconfModuleNetlist):TConfigRegisterControlBase;
Begin
  // do nothing
  Result := Nil;
End;

Procedure TInternalConfigRegisterData.Connect(AConfigInterface:TConfigInterface;ANetlist:TReconfModuleNetlist);
Begin
  // do nothing
End;

{ TExternalConfigRegisterData }

Constructor TExternalConfigRegisterData.Create(AConfigRegister:TAddressedConfigRegister;ASignal:TSignal);
Begin
  if not (AConfigRegister.FConfigRegister is TExternalConfigRegister) then
    raise Exception.Create('TExternalConfigRegisterData only works with TExternalConfigRegister, but not '+AConfigRegister.FConfigRegister.ClassName);
  inherited Create(AConfigRegister);
  FSignal := ASignal;
End;

Function TExternalConfigRegisterData.CreateConfigRegister(AConfigInterface:TConfigInterface;ANetlist:TReconfModuleNetlist):TConfigRegisterControlBase;
Var Signal : TSignal;
Begin
  // create instance
  WriteLn('Creating config register ',FConfigRegister.FConfigRegister.FName,' (width ',FConfigRegister.FConfigRegister.FLength,') at ',IntToStr(FConfigRegister.FAddress));
  FInstance := TInstance.Create('CfgReg'+FConfigRegister.FConfigRegister.FName,AConfigInterface.FCfgRegModule);
  ANetlist.FModule.AddInstance(FInstance);
  FInstance.SetGeneric('Width',TValueInteger.Create(FConfigRegister.FConfigRegister.FLength));

  // connect Reset_n_i
  FInstance.ConnectPort('Reset_n_i',ANetlist.FResetPort);
  // also create its control interface
  Result := TConfigRegisterControl.Create(Self,FInstance,'CfgMode_i','CfgClk_i','CfgShift_i','CfgDataIn_i','CfgDataOut_o');
End;

Procedure TExternalConfigRegisterData.Connect(AConfigInterface:TConfigInterface;ANetlist:TReconfModuleNetlist);
Begin
  // connect FSignal to Output_o
  FInstance.ConnectPort('Output_o',FSignal);
End;

{ TChunkedConfigRegisterData }

Constructor TChunkedConfigRegisterData.Create(AConfigRegister:TAddressedConfigRegister);
Begin
  if not (AConfigRegister.FConfigRegister is TChunkedConfigRegister) then
    raise Exception.Create('TChunkedConfigRegisterData only works with TChunkedConfigRegister, but not '+AConfigRegister.FConfigRegister.ClassName);
  inherited Create(AConfigRegister, Nil);   // the signal will be created later
  FSignals := TChunkSignals.Create;
End;

Destructor TChunkedConfigRegisterData.Destroy;
Begin
  FSignals.Free;
  Inherited Destroy;
End;

Procedure TChunkedConfigRegisterData.SetSignal(AChunk:String;ASignal:TSignal);
Begin
  if FSignals.IndexOf(AChunk) >= 0 then
    raise Exception.Create('Chunk '+AChunk+' already has a signal assigned ('+FSignals[AChunk].GetVHDLDeclaration+')');
  if (FConfigRegister.FConfigRegister as TChunkedConfigRegister).FChunks.IndexOf(AChunk) < 0 then
    raise Exception.Create('Unknown chunk '+AChunk+' in config register '+FConfigRegister.FConfigRegister.FName);
  FSignals.Add(AChunk,ASignal);
End;

// result: true if errors, false if everything is ok
Function TChunkedConfigRegisterData.CheckSignals(Out Errors:String):Boolean;
Var I   : Integer;
    CCR : TChunkedConfigRegister;
Begin
  CCR := FConfigRegister.FConfigRegister as TChunkedConfigRegister;
  Errors := '';
  For I := 0 to CCR.FChunks.Count-1 do
    if FSignals.IndexOf(CCR.FChunks.Keys[I]) < 0 then
      Errors += 'Missing signal for chunk '+CCR.FChunks.Keys[I] + LineEnding;
  Result := (Length(Errors) > 0);
End;

Procedure TChunkedConfigRegisterData.Connect(AConfigInterface:TConfigInterface;ANetlist:TReconfModuleNetlist);
Var I      : Integer;
    Chunks : TConfigChunks;
    Addr   : Integer;
    Length : Integer;
Begin
  // create signal for the whole config bit vector
  FSignal := TSignal.Create('Cfg'+FConfigRegister.FConfigRegister.FName+'_s',TType.Create('std_logic_vector',dirDown,FConfigRegister.FConfigRegister.FLength-1,0));
  ANetlist.FModule.AddSignal(FSignal);
  // connect ranges of FSignal to chunk signals
  Chunks := (FConfigRegister.FConfigRegister as TChunkedConfigRegister).FChunks;
  For I := 0 to FSignals.Count-1 do
    Begin
      Addr   := Chunks[FSignals.Keys[I]].FAddress;
      Length := Chunks[FSignals.Keys[I]].FLength;
      if FSignals.Data[I].FType = TypeBit then
        ANetlist.FModule.AddAssignment(FSignals.Data[I],TValueIndex.Create(FSignal,TValueInteger.Create(Addr)))
      else if FSignals.Data[I].FType.FName = 'std_logic_vector' then
        ANetlist.FModule.AddAssignment(FSignals.Data[I],TValueIndex.Create(FSignal,TRange.Create(dirDown,Addr+Length-1,Addr)))
      else
        raise Exception.Create('Unknown type '''+FSignals.Data[I].FType.GetVHDL+''' of signal '''+FSignals.Data[I].FName+'''');
    End;
  // continue
  inherited Connect(AConfigInterface,ANetlist);
End;

(*** Connected Config Registers: Control *************************************)

{ TConfigRegisterControl }

Constructor TConfigRegisterControl.Create(AConfigRegister:TConfigRegisterData;AInstance:TInstance;ACfgMode,ACfgClk,ACfgShift,ACfgDataIn,ACfgDataOut:String);
Begin
  inherited Create;
  FConfigRegister := AConfigRegister;
  FInstance       := AInstance;
  FCfgMode        := ACfgMode;
  FCfgClk         := ACfgClk;
  FCfgShift       := ACfgShift;
  FCfgDataIn      := ACfgDataIn;
  FCfgDataOut     := ACfgDataOut;
End;

Procedure TConfigRegisterControl.Connect(AConfigInterface:TConfigInterface;ANetlist:TReconfModuleNetlist);
Begin
  // use signals created by TConnectedConfigRegisters.ConnectCfgIntf
  FInstance.ConnectPort(FCfgMode,   ANetlist.FModule.FSignals['CfgMode_s']);
  FInstance.ConnectPort(FCfgClk,    TValueIndex.Create(ANetlist.FModule.FSignals['CfgClk_s'],   TValueInteger.Create(FConfigRegister.FConfigRegister.FAddress)));
  FInstance.ConnectPort(FCfgShift,  TValueIndex.Create(ANetlist.FModule.FSignals['CfgShift_s'], TValueInteger.Create(FConfigRegister.FConfigRegister.FAddress)));
  FInstance.ConnectPort(FCfgDataIn, ANetlist.FModule.FSignals['CfgDataOut_s']);
  FInstance.ConnectPort(FCfgDataOut,TValueIndex.Create(ANetlist.FModule.FSignals['CfgDataIn_s'],TValueInteger.Create(FConfigRegister.FConfigRegister.FAddress)));

  FInstance.CheckConnections;
  FInstance.CheckGenerics;
End;

{ TConfigRegisterControlGroupInstance }

Constructor TConfigRegisterControlGroupInstance.Create(AConfigRegister:TConfigRegisterData;AInstance:TInstance;ACfgClk,ACfgShift,ACfgDataOut:String);
Begin
  inherited Create;
  FConfigRegister := AConfigRegister;
  FInstance       := AInstance;
  FCfgClk         := ACfgClk;
  FCfgShift       := ACfgShift;
  FCfgDataOut     := ACfgDataOut;
End;

Procedure TConfigRegisterControlGroupInstance.Connect(AConfigInterface:TConfigInterface;ANetlist:TReconfModuleNetlist);
Var I : Integer;
Begin
  FInstance.ConnectPort(FCfgClk,    TValueIndex.Create(ANetlist.FModule.FSignals['CfgClk_s'],   TValueInteger.Create(FConfigRegister.FConfigRegister.FAddress)));
  FInstance.ConnectPort(FCfgShift,  TValueIndex.Create(ANetlist.FModule.FSignals['CfgShift_s'], TValueInteger.Create(FConfigRegister.FConfigRegister.FAddress)));
  FInstance.ConnectPort(FCfgDataOut,TValueIndex.Create(ANetlist.FModule.FSignals['CfgDataIn_s'],TValueInteger.Create(FConfigRegister.FConfigRegister.FAddress)));
End;

{ TConfigRegisterControlGroup }

Constructor TConfigRegisterControlGroup.Create(AInstance:TInstance;ACfgMode,ACfgDataIn:String);
Begin
  inherited Create;
  FInstance        := AInstance;
  FCfgMode         := ACfgMode;
  FCfgDataIn       := ACfgDataIn;
  FConfigRegisters := TConfigRegisterControlGroupInstances.Create;
  FConfigRegisters.Sorted     := true;
  FConfigRegisters.Duplicates := dupError;
End;

Destructor TConfigRegisterControlGroup.Destroy;
Var I : Integer;
Begin
  For I := 0 to FConfigRegisters.Count-1 do
    FConfigRegisters.Data[I].Free;
  FConfigRegisters.Free;
  Inherited Destroy;
End;

Procedure TConfigRegisterControlGroup.Add(AConfigRegister:TConfigRegisterData;ACfgClk,ACfgShift,ACfgDataOut:String);
Begin
  FConfigRegisters.Add(AConfigRegister.FConfigRegister.FConfigRegister.FName,
    TConfigRegisterControlGroupInstance.Create(AConfigRegister,FInstance,ACfgClk,ACfgShift,ACfgDataOut));
End;

Procedure TConfigRegisterControlGroup.Connect(AConfigInterface:TConfigInterface;ANetlist:TReconfModuleNetlist);
Var I : Integer;
Begin
  // common ports
  FInstance.ConnectPort(FCfgMode,   ANetlist.FModule.FSignals['CfgMode_s']);
  FInstance.ConnectPort(FCfgDataIn, ANetlist.FModule.FSignals['CfgDataOut_s']);
  // individual ports
  For I := 0 to FConfigRegisters.Count-1 do
    FConfigRegisters.Data[I].Connect(AConfigInterface,ANetlist);
End;

(*** Connected Config Registers: Common **************************************)

{ TConnectedConfigRegisters }

Constructor TConnectedConfigRegisters.Create(AConfigInterface:TConfigInterface;ANetlist:TReconfModuleNetlist;AConfigRegisters:TAddressedConfigRegisters);
Begin
  inherited Create;
  FConfigInterface := AConfigInterface;
  FNetlist         := ANetlist;
  FConfigRegisters := AConfigRegisters;
  FControl         := TConfigRegisterControls.Create;
  FData            := TConfigRegisterDatas.   Create;
End;

Destructor TConnectedConfigRegisters.Destroy;
Begin
  FControl.Free;
  FData.Free;
  Inherited Destroy;
End;

Procedure TConnectedConfigRegisters.Add(AControl:TConfigRegisterControlBase);
Var I : Integer;
Begin
  FControl.Add(AControl);
  // add "Data" inside control group
  if AControl is TConfigRegisterControlGroup then
    With AControl as TConfigRegisterControlGroup do
      For I := 0 to FConfigRegisters.Count-1 do
        With FConfigRegisters.Data[I] do
          Self.Add(FConfigRegister);
End;

Procedure TConnectedConfigRegisters.Add(AData:TConfigRegisterData);
Begin
  FData.Add(AData);
End;

Function TConnectedConfigRegisters.CheckData(Out Errors:String):Boolean;
Var I : Integer;
    L : TStringIntMap;
Begin
  Errors := '';

  // first, collect a list of all data objects
  L := TStringIntMap.Create;
  For I := 0 to FData.Count-1 do
    With FData.Items[I] do
      L.Add(FConfigRegister.FConfigRegister.FName,0);

  // second, mark all data objects for which there are parameters
  For I := 0 to FConfigRegisters.FConfigRegisters.Count-1 do
    With FConfigRegisters.FConfigRegisters.Data[I],FConfigRegister do
      Begin
        if L.IndexOf(FName) < 0 then
          Errors += 'Missing TConfigRegisterData for config register ''' + FName + '''' + LineEnding
        else
          L[FName] := L[FName] + 1;
      End;

  // finally
  For I := 0 to L.Count-1 do
    if L.Data[I] = 0 then
      Errors += 'Superfluous TConfigRegisterData for unknown config register ''' + L.Keys[I] + '''' + LineEnding
    else if L.Data[I] > 1 then
      Errors += 'Config register ''' + L.Keys[I] + ''' occured ' + IntToStr(L.Data[I]) + ' times' + LineEnding;
  L.Free;

  Result := (Length(Errors) > 0);
End;

Procedure TConnectedConfigRegisters.ConnectCfgIntf;
Var St       : String;
    Instance : TInstance;
Begin
  Instance := FNetlist.FModule.FInstances[(FNetlist.FReconfModule.FConfigInterface as TConfigInterface).FInstanceName];
  if not assigned(Instance) then
    raise Exception.Create('Before adding the config registers to the netlist, you have to add the config interface to the netlist');
  if CheckData(St) then
    raise Exception.Create(St);

  // set generic
  Instance.SetGeneric('NumCfgs',TValueInteger.Create(FConfigRegisters.FNumRegisters));
  // use same signal names as TConfigRegisterControl*
  Instance.ConnectPort('CfgClk_o',    FNetlist.FModule.AddSignal(TSignal.Create('CfgClk_s',    TType.Create('std_logic_vector',dirDown,FConfigRegisters.FNumRegisters-1,0))));
  Instance.ConnectPort('CfgMode_o',   FNetlist.FModule.AddSignal(TSignal.Create('CfgMode_s',   TypeBit)));
  Instance.ConnectPort('CfgShift_o',  FNetlist.FModule.AddSignal(TSignal.Create('CfgShift_s',  TType.Create('std_logic_vector',dirDown,FConfigRegisters.FNumRegisters-1,0))));
  Instance.ConnectPort('CfgDataOut_o',FNetlist.FModule.AddSignal(TSignal.Create('CfgDataOut_s',TypeBit)));
  Instance.ConnectPort('CfgDataIn_i', FNetlist.FModule.AddSignal(TSignal.Create('CfgDataIn_s', TType.Create('std_logic_vector',dirDown,FConfigRegisters.FNumRegisters-1,0))));

  Instance.CheckGenerics;
  Instance.CheckConnections;
End;

Procedure TConnectedConfigRegisters.CreateConfigRegisters;
Var I : Integer;
    C : TConfigRegisterControlBase;
Begin
  For I := 0 to FData.Count-1 do
    Begin
      C := FData[I].CreateConfigRegister(FConfigInterface,FNetlist);
      if assigned(C) then
        Add(C);
    End;
End;

Procedure TConnectedConfigRegisters.ConnectData;
Var I : Integer;
Begin
  For I := 0 to FData.Count-1 do
    FData[I].Connect(FConfigInterface,FNetlist);
End;

Procedure TConnectedConfigRegisters.ConnectControl;
Var I : Integer;
Begin
  For I := 0 to FControl.Count-1 do
    FControl[I].Connect(FConfigInterface,FNetlist);
End;

End.

