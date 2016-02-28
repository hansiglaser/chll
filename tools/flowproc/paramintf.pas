Unit ParamIntf;

{$mode objfpc}{$H+}

Interface

Uses
  Classes, SysUtils, FGL, Netlist, ReconfModule, PeriphIntf, Utils;

Type

  { TParamInterface }

  TParamInterface = class(IPeriphInst)
    FInstanceName : String;
    FParamOutRegModule : TModule;
    Constructor Create(AInstanceName:String);
    Destructor  Destroy; override;
    Procedure AddToNetlist(ANetlist:TReconfModuleNetlist); virtual; abstract;
  protected
    class Function GetAddrRange(AType:TType) : Integer; virtual; abstract;
  End;

  { TParamInterfaceOpenMSP430 }

  TParamInterfaceOpenMSP430 = class(TParamInterface)
    FPeriphInst : TPeriphInstOpenMSP430;
    Constructor Create(APeriphInst:TPeriphInstOpenMSP430;AInstanceName:String);
    Destructor  Destroy; override;
    Procedure AddToNetlist(ANetlist:TReconfModuleNetlist); override;
  protected
    class Function GetAddrRange(AType:TType) : Integer; override;
  End;

  (*** Parameter Base Information ********************************************)

  TParamDir = (pdWrite,pdRead);      // Write: CPU --> Reconf.Module, Read: Reconf.Module --> CPU

Const
  CParamDir : Array[TParamDir] of String = ('Wr','Rd');

Type

  { TParameter }

  TParameter = class
    FName        : String;
    FDir         : TParamDir;
    FType        : TType;
    FHaveDefault : Boolean;
    FDefault     : Integer;   // TODO: should be a better type which supports more than 32 bits
    Constructor Create(AName:String;ADir:TParamDir;AType:TType;AHaveDefault:Boolean;ADefault:Integer);
  End;

  TParameters = specialize TFPGMap<String,TParameter>;

  (*** Addressed Parameter ***************************************************)

  { TAddressedParameter }

  TAddressedParameter = class
    FParameter : TParameter;
    FAddress   : Integer;
    FAddrRange : Integer;   // 1..n, number of addresses used by this parameter
    Constructor Create(AParameter:TParameter);
  End;

  TAddressedParameterList       = specialize TFPGMap<String,TAddressedParameter>;
  TAddressedParameterSortedList = specialize TFPGMap<Integer,TAddressedParameter>;    // key: address

  { TAddressedParameters }

  TAddressedParameters = class
    FParamInterface : TParamInterface;
    FParameters     : TAddressedParameterList;
    FRdParameters   : TAddressedParameterSortedList;
    FWrParameters   : TAddressedParameterSortedList;
    FNextWrAddr     : Integer;
    FNextRdAddr     : Integer;
    Constructor Create(AParamInterface:TParamInterface);
    Destructor  Destroy; override;
    Procedure Add(AParameter:TParameter);
    Procedure Add(AParameters:TParameters);
    Procedure AssignAddresses;
    Procedure List;
  End;

  (*** Connected Parameter: Data *********************************************)

  TParameterControl = class;

  { TParameterData }

  TParameterData = class
    FParameter : TAddressedParameter;
    FSignal    : TSignal;   // destination of pdWrite, source of pdRead
    FInstance  : TInstance;
    Constructor Create(AParameter:TAddressedParameter;ASignal:TSignal);
    Function CreateParameter(AParamInterface:TParamInterface;ANetlist:TReconfModuleNetlist;AWrAddrWidth:Integer):TParameterControl; virtual;
    Procedure Connect(AParamInterface:TParamInterface;ANetlist:TReconfModuleNetlist); virtual;
  End;

  (*** Connected Parameter: Control ******************************************)

  { TParameterControl }

  TParameterControl = class
    FParameter   : TParameterData;
    FInstance    : TInstance;
    FEnable      : String;
    FParamWrData : String;
    Constructor Create(AParameter:TParameterData;AInstance:TInstance;AEnable,AParamWrData:String);
    Procedure Connect(AParamInterface:TParamInterface;ANetlist:TReconfModuleNetlist;AWrAddrWidth:Integer);
  End;

  (*** Connected Parameter: Common *******************************************)

  TParameterControls = specialize TFPGObjectList<TParameterControl>;
  TParameterDatas    = specialize TFPGObjectList<TParameterData>;

  { TConnectedParameters }

  TConnectedParameters = class
    FParamInterface : TParamInterface;
    FNetlist        : TReconfModuleNetlist;
    FParameters     : TAddressedParameters;
    FControl        : TParameterControls;
    FData           : TParameterDatas;
    FWrAddrWidth    : Integer;
    FRdAddrWidth    : Integer;
    Constructor Create(AParamInterface:TParamInterface;ANetlist:TReconfModuleNetlist;AParameters:TAddressedParameters);
    Destructor  Destroy; override;

    Procedure Add(AControl:TParameterControl);
    Procedure Add(AData:TParameterData);
    Function  CheckData(Out Errors:String) : Boolean;

    Procedure ConnectParamIntf;
    Procedure CreateParameters;
    Procedure ConnectData;
    Procedure ConnectControl;
  End;

Implementation

{ TParamInterface }

Constructor TParamInterface.Create(AInstanceName:String);
Var WidthType : TType;
Begin
  inherited Create;
  FInstanceName := AInstanceName;

  // create ParamOut register module
  FParamOutRegModule := TModule.Create('ParamOutReg');
  // Generics
  FParamOutRegModule.AddGeneric(TGeneric.Create('Width', TypeInt,TValueInteger.Create(16)));
  WidthType := TType.Create('std_logic_vector',dirDown,TValueOperatorMinus.Create(FParamOutRegModule.FGenerics['Width'],TValueInteger.Create(1)),TValueInteger.Create(0));
  // Ports
  FParamOutRegModule.AddPort(TPort.Create('Reset_n_i',    dirIn, TypeBit));
  FParamOutRegModule.AddPort(TPort.Create('Clk_i',        dirIn, TypeBit));
  // Interface to ParamIntf
  FParamOutRegModule.AddPort(TPort.Create('Enable_i',     dirIn, TypeBit));
  FParamOutRegModule.AddPort(TPort.Create('ParamWrData_i',dirIn, WidthType));
  // output
  FParamOutRegModule.AddPort(TPort.Create('Param_o',      dirOut,WidthType));
End;

Destructor TParamInterface.Destroy;
Begin
  FParamOutRegModule.Free;
  Inherited Destroy;
End;

{ TParamInterfaceOpenMSP430 }

Constructor TParamInterfaceOpenMSP430.Create(APeriphInst:TPeriphInstOpenMSP430;AInstanceName:String);
Begin
  inherited Create(AInstanceName);
  FPeriphInst := APeriphInst;
  FPeriphInst.FInst := Self;
End;

Destructor TParamInterfaceOpenMSP430.Destroy;
Begin
  Inherited Destroy;
End;

Procedure TParamInterfaceOpenMSP430.AddToNetlist(ANetlist:TReconfModuleNetlist);
Var Module   : TModule;
    Instance : TInstance;
    Range    : TRange;
Begin
  //// Create Module /////////////////////////////////////////////////////////
  Module := TModule.Create('ParamIntf');
  // Generics
  Module.AddGeneric(TGeneric.Create('WrAddrWidth', TType.Create('integer range 1 to 15'),TValueInteger.Create(4)));
  Module.AddGeneric(TGeneric.Create('RdAddrWidth', TType.Create('integer range 1 to 15'),TValueInteger.Create(4)));
  Module.AddGeneric(TGeneric.Create('BaseAddr',TypeInt,TValueIntegerHex.Create($0180)));
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
  // Param Out
  Range := TRange.Create(dirDown,TValueOperatorMinus.Create(Module.FGenerics['WrAddrWidth'],TValueInteger.Create(1)),TValueInteger.Create(0));
  Module.AddPort(TPort.Create('ParamWrAddr_o',dirOut,TType.Create('std_logic_vector',Range)));
  Module.AddPort(TPort.Create('ParamWrData_o',dirOut,TType.Create('std_logic_vector',dirDown,15,0)));
  Module.AddPort(TPort.Create('ParamWr_o',    dirOut,TypeBit));
  Module.FPorts['ParamWrAddr_o'].FComment := 'Param Out';
  // Param In
  Range := TRange.Create(dirDown,TValueOperatorMinus.Create(Module.FGenerics['RdAddrWidth'],TValueInteger.Create(1)),TValueInteger.Create(0));
  Module.AddPort(TPort.Create('ParamRdAddr_o',dirOut,TType.Create('std_logic_vector',Range)));
  Module.AddPort(TPort.Create('ParamRdData_i',dirIn, TType.Create('std_logic_vector',dirDown,15,0)));
  Module.FPorts['ParamRdAddr_o'].FComment := 'Param In';

  //// Create Instance ///////////////////////////////////////////////////////
  Instance := TInstance.Create(FInstanceName,Module);
  // generics "Wr/RdAddrWidth" are set in finish_reconf_module
  Instance.SetGeneric('BaseAddr',TValueIntegerHex.Create(FPeriphInst.FBaseAddr));

  Instance.ConnectPort('Reset_n_i',ANetlist.FResetPort);
  Instance.ConnectPort('Clk_i',    ANetlist.FClockPort);
  // Peripheral Interface to CPU
  Instance.ConnectPort('PerAddr_i',FPeriphInst.GetPerAddr(ANetlist));
  Instance.ConnectPort('PerDIn_i', FPeriphInst.GetPerDIn (ANetlist));
  Instance.ConnectPort('PerDOut_o',FPeriphInst.GetPerDOut(ANetlist));
  Instance.ConnectPort('PerWr_i',  FPeriphInst.GetPerWr  (ANetlist));
  Instance.ConnectPort('PerEn_i',  FPeriphInst.GetPerEn  (ANetlist));

  Instance.CheckConnections;  // Param{Wr,Rd}Addr_o, Param{Wr,Rd}Data_[oi], ParamWr_o
  Instance.CheckGenerics;     // "Wr/RdAddrWidth" are not yet set

  ANetlist.FModule.AddInstance(Instance);
  ANetlist.FModule.FInstances[FInstanceName].FComment := 'Parameterization Interface';
End;

class Function TParamInterfaceOpenMSP430.GetAddrRange(AType:TType):Integer;
Begin
  Result := (AType.GetWidthInt + 15) shr 4;   // 1..16 --> 1, 17..32 --> 2, ...
End;

(*** Parameter Base Information **********************************************)

{ TParameter }

Constructor TParameter.Create(AName:String;ADir:TParamDir;AType:TType;AHaveDefault:Boolean;ADefault:Integer);
Begin
  inherited Create;
  FName        := AName;
  FDir         := ADir;
  FType        := AType;
  FHaveDefault := AHaveDefault;
  FDefault     := ADefault;
End;

(*** Addressed Parameter *****************************************************)

{ TAddressedParameter }

Constructor TAddressedParameter.Create(AParameter:TParameter);
Begin
  FParameter := AParameter;
  FAddress   := -1;   // unset
  FAddrRange := -1;
End;

{ TAddressedParameters }

Constructor TAddressedParameters.Create(AParamInterface:TParamInterface);
Begin
  inherited Create;
  FParamInterface := AParamInterface;
  FParameters     := TAddressedParameterList.Create;
  FRdParameters   := Nil;   // set by AssignAddresses
  FWrParameters   := Nil;   // set by AssignAddresses
  FNextWrAddr     := -1;    // set by AssignAddresses
  FNextRdAddr     := -1;    // set by AssignAddresses
End;

Destructor TAddressedParameters.Destroy;
Var I : Integer;
Begin
  For I := 0 to FParameters.Count-1 do
    FParameters.Data[I].Free;
  FParameters.Free;
  FRdParameters.Free;
  FWrParameters.Free;
  Inherited Destroy;
End;

Procedure TAddressedParameters.Add(AParameter:TParameter);
Begin
  if FParameters.IndexOf(AParameter.FName) >= 0 then
    raise Exception.Create('Can''t add this parameter, because another parameter named '''+AParameter.FName+''' already exists.');
  FParameters.Add(AParameter.FName,TAddressedParameter.Create(AParameter));
End;

Procedure TAddressedParameters.Add(AParameters:TParameters);
Var I : Integer;
Begin
  if assigned(FRdParameters) then
    raise Exception.Create('Addresses already assigned, you can''t add more config registers');
  For I := 0 to AParameters.Count-1 do
    Add(AParameters.Data[I]);
End;

Procedure TAddressedParameters.AssignAddresses;
Var I : Integer;
Begin
  if assigned(FRdParameters) then
    raise Exception.Create('Addresses already assigned');

  FRdParameters := TAddressedParameterSortedList.Create;
  FWrParameters := TAddressedParameterSortedList.Create;
  FRdParameters.Sorted     := true;
  FWrParameters.Sorted     := true;
  FRdParameters.Duplicates := dupError;
  FWrParameters.Duplicates := dupError;

  FNextRdAddr := 0;
  FNextWrAddr := 0;
  For I := 0 to FParameters.Count-1 do
    With FParameters.Data[I] do
      Begin
        FAddrRange := FParamInterface.GetAddrRange(FParameter.FType);
        if FParameter.FDir = pdWrite then
          Begin
            FAddress := FNextWrAddr;
            Inc(FNextWrAddr,FAddrRange);
            FWrParameters.Add(FAddress,FParameters.Data[I]);
          End
        else
          Begin
            FAddress := FNextRdAddr;
            Inc(FNextRdAddr,FAddrRange);
            FRdParameters.Add(FAddress,FParameters.Data[I]);
          End;
      End;
End;

Procedure TAddressedParameters.List;
Var I : Integer;
Begin
  WriteLn('  Parameters:');
  if assigned(FRdParameters) then
    Begin
      WriteLn('    ParamRd:');
      For I := 0 to FRdParameters.Count-1 do
        With FRdParameters.Data[I],FParameter do
          WriteLn('    $',IntToHex(FAddress,2),'  ',FName,StringOfChar(' ',40-Length(FName)),' : ',FType.GetVHDL);
      WriteLn('    ParamWr:');
      For I := 0 to FWrParameters.Count-1 do
        With FWrParameters.Data[I],FParameter do
          WriteLn('    $',IntToHex(FAddress,2),'  ',FName,StringOfChar(' ',40-Length(FName)),' : ',FType.GetVHDL);
    End
  else
    Begin
      For I := 0 to FParameters.Count-1 do
        With FParameters.Data[I],FParameter do
          WriteLn('    ',FName,StringOfChar(' ',40-Length(FName)),' ',CParamDir[FDir],' : ',FType.GetVHDL);
    End;
End;

(*** Connected Parameter: Data ***********************************************)

{ TParameterData }

Constructor TParameterData.Create(AParameter:TAddressedParameter;ASignal:TSignal);
Begin
  inherited Create;
  FParameter := AParameter;
  FSignal    := ASignal;
End;

Function TParameterData.CreateParameter(AParamInterface:TParamInterface;ANetlist:TReconfModuleNetlist;AWrAddrWidth:Integer):TParameterControl;
Var Name : String;
Begin
  if FParameter.FParameter.FDir = pdWrite then
    Begin
      // place a param write cell
      WriteLn('Creating param write cell for parameter ',FParameter.FParameter.FName,' : ',FParameter.FParameter.FType.GetVHDL,' at $',IntToHex(FParameter.FAddress,2));
      // create instance
      Name := TrimSignalPostfix(FParameter.FParameter.FName);
      FInstance := TInstance.Create('ParamOutReg_'+Name, AParamInterface.FParamOutRegModule);
      ANetlist.FModule.AddInstance(FInstance);
      if FSignal.FType.GetWidthInt = 16 then
        FInstance.SetGeneric('Width',TValueInteger.Create(16))
      else if FSignal.FType.GetWidthInt < 16 then
        FInstance.SetGeneric('Width',TValueInteger.Create(FSignal.FType.GetWidthInt))
      else
        // TODO: special handling if FSignal.FType.GetWidthInt > 16: multiple instances
        raise Exception.Create('TODO: Implement write parameters wider than 16 bits');
      FInstance.ConnectPort('Reset_n_i',    ANetlist.FResetPort);
      FInstance.ConnectPort('Clk_i',        ANetlist.FClockPort);
      // also create its control interface
      Result := TParameterControl.Create(Self,FInstance,'Enable_i','ParamWrData_i');
    End
  else
    Begin
      // place a param read cell (it is just a signal assignment)
      WriteLn('Creating param read cell for parameter ',FParameter.FParameter.FName,' : ',FParameter.FParameter.FType.GetVHDL,' at $',IntToHex(FParameter.FAddress,2));
      Result := Nil;
    End;
End;

Procedure TParameterData.Connect(AParamInterface:TParamInterface;ANetlist:TReconfModuleNetlist);
Var Signal : TSignal;
    Concat : TValueConcat;
Begin
  if FParameter.FParameter.FDir = pdWrite then
    Begin
      // connect FSignal to Param_o
      if FSignal.FType.GetWidthInt = 16 then
        FInstance.ConnectPort('Param_o',FSignal)
      else if FSignal.FType.GetWidthInt < 16 then
        Begin
          if FSignal.FType = TypeBit then
            Begin
              Signal := TSignal.Create(TrimSignalPostfix(FSignal.FName)+'_s','std_logic_vector',dirDown,0,0);
              ANetlist.FModule.AddSignal(Signal);
              FInstance.ConnectPort('Param_o',Signal);
              ANetlist.FModule.AddAssignment(FSignal,TValueIndex.Create(Signal,TValueInteger.Create(0)));
            End
          else
            FInstance.ConnectPort('Param_o',FSignal)
        End
      else
        // TODO: special handling if FSignal.FType.GetWidthInt > 16: multiple ParamOutReg
        raise Exception.Create('TODO: Implement param write of width > 16 for '+FSignal.FName);
    End
  else
    Begin
      if FSignal.FType.GetWidthInt = 16 then
        ANetlist.FModule.AddAssignment(
          TValueIndex.Create(ANetlist.FModule.FSignals['Params_s'],TValueInteger.Create(FParameter.FAddress)),
          FSignal).FComment := 'Address $'+IntToHex(FParameter.FAddress,2)
      else if FSignal.FType.GetWidthInt < 16 then
        Begin
          // special handling if FSignal.FType.GetWidthInt < 16: concat with (others => '0')
          Concat := TValueConcat.Create;
          Concat.Add(FSignal);
          Concat.Add(TValueVector.Create(16 - FSignal.FType.GetWidthInt,0));   // add as MSB
          ANetlist.FModule.AddAssignment(
            TValueIndex.Create(ANetlist.FModule.FSignals['Params_s'],TValueInteger.Create(FParameter.FAddress)),
            Concat
            ).FComment := 'Address $'+IntToHex(FParameter.FAddress,2)
        End
      else
        // TODO: special handling if FSignal.FType.GetWidthInt > 16: multiple assignments
        raise Exception.Create('TODO: Implement param read of width > 16 for '+FSignal.FName);
    End;
End;

(*** Connected Parameter: Control ********************************************)

{ TParameterControl }

Constructor TParameterControl.Create(AParameter:TParameterData;AInstance:TInstance;AEnable,AParamWrData:String);
Begin
  inherited Create;
  FParameter   := AParameter;
  FInstance    := AInstance;
  FEnable      := AEnable;
  FParamWrData := AParamWrData;
End;

Procedure TParameterControl.Connect(AParamInterface:TParamInterface;ANetlist:TReconfModuleNetlist;AWrAddrWidth:Integer);
Var Name     : String;
    Enable   : TSignal;
    When     : TValueWhen;
    Width    : Integer;
Begin
  // use signals created by TConnectedParameters.ConnectParamIntf
  Name := TrimSignalPostfix(FParameter.FParameter.FParameter.FName);
  // write-enable signal
  Enable := ANetlist.FModule.AddSignal(TSignal.Create('Param'+Name+'Enable_s',TypeBit));
  When := TValueWhen.Create;
  When.AddValue(
    ANetlist.FModule.FSignals['ParamWr_s'],
    TValueOperatorEqual.Create(
      ANetlist.FModule.FSignals['ParamWrAddr_s'],
      TValueVector.Create(AWrAddrWidth,FParameter.FParameter.FAddress)
    ));
  When.AddElse(TValueBit.Create('0'));
  ANetlist.FModule.AddAssignment(Enable, When).FComment := 'Address $'+IntToHex(FParameter.FParameter.FAddress,2);

  FInstance.ConnectPort('Enable_i',     Enable);
  Width := (FInstance.FGenericValues['Width'] as TValueInteger).FValue;
  if Width = 16 then
    FInstance.ConnectPort('ParamWrData_i',ANetlist.FModule.FSignals['ParamWrData_s'])
  else if Width < 16 then
    FInstance.ConnectPort('ParamWrData_i',TValueIndex.Create(ANetlist.FModule.FSignals['ParamWrData_s'],TRange.Create(dirDown,Width-1,0)))
  else
    raise Exception.Create('TODO: Implement param write of width > 16 for '+Name);

  FInstance.CheckConnections;
  FInstance.CheckGenerics;
End;

(*** Connected Parameter: Common *********************************************)

{ TConnectedParameters }

Constructor TConnectedParameters.Create(AParamInterface:TParamInterface;ANetlist:TReconfModuleNetlist;AParameters:TAddressedParameters);
Begin
  inherited Create;
  FParamInterface := AParamInterface;
  FNetlist        := ANetlist;
  FParameters     := AParameters;
  FControl        := TParameterControls.Create;
  FData           := TParameterDatas.   Create;
  FRdAddrWidth    := -1;
  FWrAddrWidth    := -1;
End;

Destructor TConnectedParameters.Destroy;
Begin
  FControl.Free;
  FData.Free;
  Inherited Destroy;
End;

Procedure TConnectedParameters.Add(AControl:TParameterControl);
Begin
  FControl.Add(AControl);
End;

Procedure TConnectedParameters.Add(AData:TParameterData);
Begin
  FData.Add(AData);
End;

Function TConnectedParameters.CheckData(Out Errors:String):Boolean;
Var I : Integer;
    L : TStringIntMap;
Begin
  Errors := '';

  // first, collect a list of all data objects
  L := TStringIntMap.Create;
  For I := 0 to FData.Count-1 do
    With FData.Items[I] do
      L.Add(FParameter.FParameter.FName,0);

  // second, mark all data objects for which there are parameters
  For I := 0 to FParameters.FParameters.Count-1 do
    With FParameters.FParameters.Data[I],FParameter do
      Begin
        if L.IndexOf(FName) < 0 then
          Errors += 'Missing TParameterData for parameter ''' + FName + '''' + LineEnding;
        L[FName] := L[FName] + 1;
      End;

  // finally
  For I := 0 to L.Count-1 do
    if L.Data[I] = 0 then
      Errors += 'Superfluous TParameterData for unknown parameter ''' + L.Keys[I] + '''' + LineEnding
    else if L.Data[I] > 1 then
      Errors += 'Parameter ''' + L.Keys[I] + ''' occured ' + IntToStr(L.Data[I]) + ' times' + LineEnding;
  L.Free;

  Result := (Length(Errors) > 0);
End;

Procedure TConnectedParameters.ConnectParamIntf;
Var St          : String;
    Instance    : TInstance;
    WrAddrRange : Integer;
    RdAddrRange : Integer;
    SignalType  : TType;
    RdArrayType : TTypeDeclArray;
    RdArray     : TSignal;
    Assignment  : TAssignment;
Begin
  Instance := FNetlist.FModule.FInstances[(FNetlist.FReconfModule.FParamInterface as TParamInterface).FInstanceName];
  if not assigned(Instance) then
    raise Exception.Create('Before adding the parameters to the netlist, you have to add the param interface to the netlist');
  if FWrAddrWidth >= 0 then
    raise Exception.Create('You already added the parameters to this netlist');
  if CheckData(St) then
    raise Exception.Create(St);

  // calculate address signal width
  WrAddrRange := FParameters.FNextWrAddr;
  RdAddrRange := FParameters.FNextRdAddr;
  // TODO: special handling if WrAddrRange = 0 or RdAddrRange = 0
  FWrAddrWidth := RoundUpLd(WrAddrRange-1);    // RoundUpLd returns 3 for 7 and 4 for 8 --> reduce WrAddrRange by 1
  if RdAddrRange <= 1 then
    FRdAddrWidth := 1
  else
    FRdAddrWidth := RoundUpLd(RdAddrRange-1);

  // set generics
  Instance.SetGeneric('WrAddrWidth',TValueInteger.Create(FWrAddrWidth));
  Instance.SetGeneric('RdAddrWidth',TValueInteger.Create(FRdAddrWidth));
  Instance.FComment := 'Parameterization Interface: '+IntToStr(WrAddrRange)+' write addresses, '+IntToStr(RdAddrRange)+' read addresses';
  // use same signal names as TParameterControl.Connect
  // Param Out
  FNetlist.FModule.AddSignal(TSignal.Create('ParamWrAddr_s',TType.Create('std_logic_vector',dirDown,FWrAddrWidth-1,0)));
  FNetlist.FModule.AddSignal(TSignal.Create('ParamWrData_s',Instance.FModule.FPorts['ParamWrData_o'].FType));
  FNetlist.FModule.AddSignal(TSignal.Create('ParamWr_s',    Instance.FModule.FPorts['ParamWr_o'].FType));
  Instance.ConnectPort('ParamWrAddr_o', FNetlist.FModule.FSignals['ParamWrAddr_s']);
  Instance.ConnectPort('ParamWrData_o', FNetlist.FModule.FSignals['ParamWrData_s']);
  Instance.ConnectPort('ParamWr_o',     FNetlist.FModule.FSignals['ParamWr_s']);
  // Param In
  FNetlist.FModule.AddSignal(TSignal.Create('ParamRdAddr_s',TType.Create('std_logic_vector',dirDown,FRdAddrWidth-1,0)));
  FNetlist.FModule.AddSignal(TSignal.Create('ParamRdData_s',Instance.FModule.FPorts['ParamRdData_i'].FType));
  Instance.ConnectPort('ParamRdAddr_o', FNetlist.FModule.FSignals['ParamRdAddr_s']);
  Instance.ConnectPort('ParamRdData_i', FNetlist.FModule.FSignals['ParamRdData_s']);

  // add param read infrastructure
  SignalType := TType.Create('std_logic_vector',dirDown,15,0);
  RdArrayType := TTypeDeclArray.Create('Params_t',dirUp,0,RdAddrRange-1,SignalType);
  RdArray     := TSignal.Create('Params_s',RdArrayType.FType);
  FNetlist.FModule.AddType  (RdArrayType);
  FNetlist.FModule.AddSignal(RdArray);
  Assignment := FNetlist.FModule.AddAssignment(
    FNetlist.FModule.FSignals['ParamRdData_s'],
    TValueIndex.Create(
      RdArray,
      TValueOperatorFunction.Create('to_integer',
        TValueOperatorFunction.Create('unsigned',
          FNetlist.FModule.FSignals['ParamRdAddr_s']))));
  Assignment.FComment := 'Param read address decoder' + LineEnding +
                         'Synthesis: Accept undefined behavior if ParamRdAddr_s >= NumParams and' + LineEnding +
                         '  hope that the synthesis optimizes the MUX' + LineEnding +
                         'Simulation: ModelSim complains "Fatal: (vsim-3421) Value x is out of range' + LineEnding +
                         '  0 to n.", even during param write cycles, because ParamRdAddr has the' + LineEnding +
                         '  source as ParamWrAddr. Use the parameter "-noindexcheck" during' + LineEnding +
                         '  compilation ("vcom"). Simulation works fine then, but ModelSim generates' + LineEnding +
                         '  numerous "INTERNAL ERROR"s to stdout, which seem harmless.';

  Instance.CheckGenerics;
  Instance.CheckConnections;
End;

Procedure TConnectedParameters.CreateParameters;
Var I : Integer;
    C : TParameterControl;
Begin
  For I := 0 to FData.Count-1 do
    Begin
      C := FData[I].CreateParameter(FParamInterface,FNetlist,FWrAddrWidth);
      if assigned(C) then
        Add(C);
    End;
End;

Procedure TConnectedParameters.ConnectData;
Var I : Integer;
Begin
  For I := 0 to FData.Count-1 do
    FData[I].Connect(FParamInterface,FNetlist);
End;

Procedure TConnectedParameters.ConnectControl;
Var I : Integer;
Begin
  For I := 0 to FControl.Count-1 do
    FControl[I].Connect(FParamInterface,FNetlist,FWrAddrWidth);
End;

End.

