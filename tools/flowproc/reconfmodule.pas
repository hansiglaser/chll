Unit ReconfModule;

{$mode objfpc}{$H+}

Interface

Uses
  Classes, SysUtils, Math, Netlist, ReconfSignals, Utils;

Type

  TReconfModuleNetlist = class;
  TReconfModuleNetlistWithInterSynth = class;

  { TReconfModule }

  TReconfModule = class
    FParent             : TModule;    // parent module where we get instantiated
    FReconfigTypeName   : String;
    FReconfigInstName   : String;
    FInterSynthTypeName : String;
    FInterSynthInstName : String;
    FParentReset     : TSignal;    // could be a port, but also be a signal, e.g Reset_s = ~Reset_n_i;
    FParentClock     : TSignal;
    FResetName       : String;
    FClockName       : String;
    FReconfSignals   : TReconfSignals;
    FPeriphIntf      : TObject;    // TPeriphIntf;              // damn circular unit reference
    FConfigInterface : TObject;    // TConfigInterface;         // damn circular unit reference
    FParamInterface  : TObject;    // TParamInterface;          // damn circular unit reference
    FConfigRegisters : TObject;    // TAddressedConfigRegisters // damn circular unit refeference
    FParameters      : TObject;    // TAddressedParameters      // damn circular unit refeference
    FPreliminary     : TReconfModuleNetlist;
    FNetlist         : TReconfModuleNetlistWithInterSynth;
  public
    Constructor Create(AParent:TModule;AReconfigTypeName,AReconfigInstName,AInterSynthTypeName,AInterSynthInstName:String);
    Procedure AddReconfSignals(AReconfSignals:TReconfSignals);

    Function CheckParentSignal(AName,AType:String;ADir:TRangeDirection;ALeft,ARight:Integer) : TSignal;
  End;

  { TReconfModuleNetlist }

  TReconfModuleNetlist = class
  private
    Procedure AddReconfSignal(ASignal:TReconfSignalBase;Const AData:Pointer);
  public
    FReconfModule : TReconfModule;
    FModule       : TModule;                 // itself
    FInstance     : TInstance;               // instance of itself in its parent
    FResetPort    : TPort;                   // port of ReconfModule, connected to FReconfModule.FParentReset
    FClockPort    : TPort;                   // port of ReconfModule, connected to FReconfModule.FParentClock
    Constructor Create(AReconfModule:TReconfModule;AReconfigTypeName,AReconfigInstName:String);
  End;

  { TReconfModuleNetlistWithInterSynth }

  TReconfModuleNetlistWithInterSynth = class(TReconfModuleNetlist)
  private
    Procedure AddReconfSignalToInterSynth(ASignal:TReconfSignalBase;Const AData:Pointer);
  public
    FISModule   : TModule;
    FISInstance : TInstance;
    Constructor Create(AReconfModule:TReconfModule;AInterSynthHandler:TObject;AReconfigTypeName,AReconfigInstName,AInterSynthTypeName,AInterSynthInstName:String;AConfigInterface:TObject;AParamInterface:TObject);
  End;

Implementation
Uses PeriphIntf, ConfigIntf, ParamIntf, InterSynthHandler;

{ TReconfModule }

Constructor TReconfModule.Create(AParent:TModule;AReconfigTypeName,AReconfigInstName,AInterSynthTypeName,AInterSynthInstName:String);
Begin
  inherited Create;

  FParent             := AParent;
  FReconfigTypeName   := AReconfigTypeName;
  FReconfigInstName   := AReconfigInstName;
  FInterSynthTypeName := AInterSynthTypeName;
  FInterSynthInstName := AInterSynthInstName;
  // FReconfSignals will be set by AddReconfSignals

End;

Procedure TReconfModule.AddReconfSignals(AReconfSignals:TReconfSignals);
Begin
  FReconfSignals := AReconfSignals;
End;

Function TReconfModule.CheckParentSignal(AName,AType:String;ADir:TRangeDirection;ALeft,ARight:Integer):TSignal;
Begin
  if FParent.FSignals.IndexOf(AName) < 0 then
    raise Exception.Create('Signal '+AName+' doesn''t exist in the parent module');
  Result := FParent.FSignals[AName];
  if Result.FType.FName <> AType then
    raise Exception.Create('Signal '+AName+' of parent module has wrong type ('+Result.FType.FName+', should be '+AType);
  if ALeft <> -1 then
    With Result.FType.FRange do
      if (FDirection <> ADir) or ((FLeft as TValueInteger).FValue <> ALeft) or ((FRight as TValueInteger).FValue <> ARight) then
        raise Exception.CreateFmt('Signal %s of parent module has wrong range (%s, should be %d %s %d',[AName,Result.FType.GetVHDL,ALeft,CRangeDirectionVHDL[ADir],ARight]);
End;

{ TReconfModuleNetlist }

Constructor TReconfModuleNetlist.Create(AReconfModule:TReconfModule;AReconfigTypeName,AReconfigInstName:String);
Begin
  inherited Create;
  FReconfModule := AReconfModule;

  // module and an instance in its parent
  FModule   := TModule.Create(AReconfigTypeName);
  FInstance := TInstance.Create(AReconfigInstName,FModule);
  //FReconfModule.FParent.AddInstance(FInstance,1000);  // don't do that, because we might have multiple instances of TReconfModuleNetlist
  // reset
  FResetPort := TPort.Create(FReconfModule.FResetName,dirIn,TypeBit);
  FModule.AddPort(FResetPort,0);
  FInstance.ConnectPort(FReconfModule.FResetName,FReconfModule.FParentReset);
  // clock
  FClockPort := TPort.Create(FReconfModule.FClockName,Netlist.dirIn,TypeBit);
  FModule.AddPort(FClockPort,1);
  FInstance.ConnectPort(FReconfModule.FClockName,FReconfModule.FParentClock);
  // reconfig signals
  FReconfModule.FReconfSignals.Foreach(Nil,Nil,@AddReconfSignal,Nil);
  // peripheral interface, recursively also adds the param and config interface
  (FReconfModule.FPeriphIntf as TPeriphIntf).AddToNetlist(Self);
End;

Procedure TReconfModuleNetlist.AddReconfSignal(ASignal:TReconfSignalBase;const AData:Pointer);
Var PortName   : String;
    TheType    : TType;
    ModulePort : TPort;
    ParentSig  : TSignal;
Begin
  PortName := ASignal.GetPortName;
  TheType  := ASignal.GetSignal.FType;
  // Don't add Reset or Clock port once again (was already added in Create).
  // TODO: The criteria whether we are dealing with one of these two signals
  // should be better (via FReconfInstance.FConnections) than just a comparison
  // of names!
  if (ASignal.GetSignal.FName = FReconfModule.FParentReset.FName) or (ASignal.GetSignal.FName = FReconfModule.FParentClock.FName) then  // compare signal name, not reconf.signal name!
    Exit;
  // amend netlist
  ModulePort := TPort.Create(PortName,ASignal.GetDirection,TheType);
  FModule.AddPort(ModulePort{,1000 + I});
  ParentSig := ASignal.GetSignal;
  FInstance.ConnectPort(PortName,ParentSig);
End;

{ TReconfModuleNetlistWithInterSynth }

Constructor TReconfModuleNetlistWithInterSynth.Create(AReconfModule:TReconfModule;AInterSynthHandler:TObject;AReconfigTypeName,AReconfigInstName,AInterSynthTypeName,AInterSynthInstName:String;AConfigInterface:TObject;AParamInterface:TObject);
Var ISH : TInterSynthHandler;
    ConfigRegisters : TAddressedConfigRegisters;
    ConnCfgRegs     : TConnectedConfigRegisters;
    ChunkedCfgReg   : TChunkedConfigRegisterData;
    Parameters      : TAddressedParameters;
    ConnParams      : TConnectedParameters;
    I,J,K        : Integer;
    Signal       : TSignal;
    Index        : TValue;
    Value        : TValue;
    CC           : TValueConcat;
    St           : String;
    Instance     : TInstance;
    PL,PR        : Integer;
    SigConn      : TSigConnDyn;
Begin
  inherited Create(AReconfModule,AReconfigTypeName,AReconfigInstName);
  // here we have the reconf.module with instances of the CfgIntf and ParamIntf, plus an instance of it

  FModule.FArchitectureName := 'struct';

  // add instance of the InterSynth module
  ISH := TInterSynthHandler(AInterSynthHandler);
  FISModule   := ISH.GenerateModule(AInterSynthTypeName);    // get full InterSynth module
  FISInstance := TInstance.Create(AInterSynthInstName,FISModule);
  FModule.AddInstance(FISInstance);

  // config infrastructure
  ConfigRegisters := TAddressedConfigRegisters(FReconfModule.FConfigRegisters);
  ConnCfgRegs := TConnectedConfigRegisters.Create(TConfigInterface(AConfigInterface),Self,ConfigRegisters);
  // create config register for connection/usage "Config" reconf.signals
  ChunkedCfgReg := TChunkedConfigRegisterData.Create(ConfigRegisters.FConfigRegisters['ReconfSignals']);
  ConnCfgRegs.Add(ChunkedCfgReg);
  // create config register and connect to "bitdata" input
  Signal := FModule.AddSignal(TSignal.Create('BitData_s','std_logic_vector',dirDown,ISH.FReadBitdataSize-1,0));
  FISInstance.ConnectPort('bitdata',Signal);
  ConnCfgRegs.Add(TExternalConfigRegisterData.Create(ConfigRegisters.FConfigRegisters['bitdata'],Signal));
  // create config register control side for internal config registers
  ConnCfgRegs.Add(TConfigRegisterControl(ISH.GenerateConfigRegisterControlGroup(ConfigRegisters,FISInstance)));

  // parameter infrastructure
  Parameters := TAddressedParameters(FReconfModule.FParameters);
  ConnParams := TConnectedParameters.Create(TParamInterface(AParamInterface),Self,Parameters);

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
            // assign source/destination signal
            ChunkedCfgReg.SetSignal(GetPortName,FModule.FPorts[GetPortName]);
            // the rest is handled by ConnCfgRegs after this loop
          End
        else if FSigConn is TSigConnParam then
          Begin
            // connection/usage: param
            // assign source/destination signal
            ConnParams.Add(TParameterData.Create(Parameters.FParameters[FName],FModule.FPorts[GetPortName]));
            // the rest is handled by ConnParams after this loop
          End
        else if FSigConn is TSigConnDirect then
          Begin
            // connection/usage: direct
            FISInstance.ConnectPort(GetPortName,FModule.FPorts[GetPortName]);
          End
        else if FSigConn is TSigConnDyn then
          Begin
            // connection/usage: dynamic
            SigConn := FSigConn as TSigConnDyn;
            if SigConn.FConnTypeOptions.FArray then
              Begin
                // array signal: split in chunk signals for InterSynth module
                For J := 0 to (GetSignal.FType.GetWidthInt div SigConn.FConnType.FWidth)-1 do
                  Begin
                    if SigConn.FConnType.FWidth = 1 then
                      FISInstance.ConnectPort(GetPortName+'_'+IntToStr(J),TValueIndex.Create(FModule.FPorts[GetPortName],TValueInteger.Create(J)))
                    else
                      FISInstance.ConnectPort(GetPortName+'_'+IntToStr(J),TValueIndex.Create(FModule.FPorts[GetPortName],TRange.Create(dirDown,(J+1)*SigConn.FConnType.FWidth-1,J*SigConn.FConnType.FWidth)));
                    // TODO: probably this index-thing only works for inputs, but not for outputs
                  End;
              End
            else if (SigConn.FConnTypeOptions.FPadLeftWidth >= 0) or (SigConn.FConnTypeOptions.FPadRightWidth >= 0) then
              With (FSigConn as TSigConnDyn).FConnTypeOptions do
                Begin
                  // padded signal
                  if GetDirection = dirOut then
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
                      FISInstance.ConnectPort(GetPortName,Signal);
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
                    Begin
                      // pad reconf.signal to desired width
                      Value := TValueConcat.Create;
                      if FPadRightWidth > 0 then
                        TValueConcat(Value).Add(TValueVector.Create(FPadRightWidth,FPadRight));  // add as MSB
                      TValueConcat(Value).Add(FModule.FPorts[GetPortName]);  // add as MSB
                      if FPadLeftWidth > 0 then
                        TValueConcat(Value).Add(TValueVector.Create(FPadLeftWidth,FPadLeft));  // add as MSB

                      (* Directly connecting a TValueConcat to an instance port is not allowed.
                       * See also TReconfModuleNetlistWrapApp.Create
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
                      FISInstance.ConnectPort(GetPortName,Signal);
                    End
              End
            else
              Begin
                // 1:1 match
                FISInstance.ConnectPort(GetPortName,FModule.FPorts[GetPortName]);
              End;
          End
        else
          raise Exception.Create('Unknown signal connection '+FSigConn.ClassName);
      End;

  if ChunkedCfgReg.CheckSignals(St) then
    raise Exception.Create(St);

  // add params of the InterSynth module to ConnCfgRegs
  // parameter name: "Param<In|Out>_<conntype>_<index>"
  // parameter port: "Param<In|Out>_<conntype>_<i|o>"        (ParamIn_Bit_i, ParamOut_Bit_o, ...)
  For I := 0 to FReconfModule.FReconfSignals.FConnTypes.Count-1 do
    With FReconfModule.FReconfSignals.FConnTypes.Data[I] do
      Begin
        if (ISH.FReadParamInCount.IndexOf(FName) >= 0) and (ISH.FReadParamInCount[FName] > 0) then
          Begin
            // parameter into the InterSynth module
            Signal := TSignal.Create('ParamIn_'+FName+'_s',TType.Create('std_logic_vector',dirDown,FWidth*ISH.FReadParamInCount[FName]-1,0));
            FModule.AddSignal(Signal);
            FISInstance.ConnectPort('ParamIn_'+FName+'_i',Signal);
            Value := Signal;
            CC := TValueConcat.Create;
            For J := 0 to ISH.FReadParamInCount[FName]-1 do
              Begin
                Signal := TSignal.Create('ParamIn_'+FName+'_'+IntToStr(J)+'_s',FType);
                FModule.AddSignal(Signal);
                CC.Add(Signal);   // add as MSB
                ConnParams.Add(TParameterData.Create(Parameters.FParameters['ParamIn_'+FName+'_'+IntToStr(J)],Signal));
              End;
            FModule.AddAssignment(Value,CC);
          End;
        if (ISH.FReadParamOutCount.IndexOf(FName) >= 0) and (ISH.FReadParamOutCount[FName] > 0) then
          Begin
            Signal := TSignal.Create('ParamOut_'+FName+'_s',TType.Create('std_logic_vector',dirDown,FWidth*ISH.FReadParamOutCount[FName]-1,0));
            FModule.AddSignal(Signal);
            FISInstance.ConnectPort('ParamOut_'+FName+'_o',Signal);
            Value := Signal;
            For J := 0 to ISH.FReadParamOutCount[FName]-1 do
              Begin
                Signal := TSignal.Create('ParamOut_'+FName+'_'+IntToStr(J)+'_s',FType);
                FModule.AddSignal(Signal);
                FModule.AddAssignment(Signal,TValueIndex.Create(Value,TRange.Create(dirDown,(J+1)*FType.GetWidthInt-1,J*FType.GetWidthInt)));
                ConnParams.Add(TParameterData.Create(Parameters.FParameters['ParamOut_'+FName+'_'+IntToStr(J)],Signal));
              End;
          End;
      End;

  // config interface
  WriteLn('Connecting CfgIntf');
  ConnCfgRegs.ConnectCfgIntf;         // create signals "CfgMode_s", ..., set generic "NumRegs"
  WriteLn('Creating config registers');
  ConnCfgRegs.CreateConfigRegisters;
  WriteLn('Connecting config bitstream signals');
  ConnCfgRegs.ConnectData;
  WriteLn('Connecting config control signals');
  ConnCfgRegs.ConnectControl;

  // param interface
  WriteLn('Connecting ParamIntf');
  ConnParams.ConnectParamIntf;
  WriteLn('Creating param registers');
  ConnParams.CreateParameters;
  WriteLn('Connecting param data signals');
  ConnParams.ConnectData;
  WriteLn('Connecting param control signals');
  ConnParams.ConnectControl;

  FISInstance.CheckConnections;
  FISInstance.CheckGenerics;
End;

Procedure TReconfModuleNetlistWithInterSynth.AddReconfSignalToInterSynth(ASignal:TReconfSignalBase;Const AData:Pointer);
Var PortName   : String;
    TheType    : TType;
    SigConn    : TSigConnDyn;
    I          : Integer;
Begin
  PortName := ASignal.GetPortName;
  if ASignal.FSigConn is TSigConnDyn then
    TheType := (ASignal.FSigConn as TSigConnDyn).FConnType.FType
  else
    TheType  := ASignal.GetSignal.FType;
  // Don't add Reset or Clock port once again (was already added in Create).
  // TODO: The criteria whether we are dealing with one of these two signals
  // should be better (via FReconfInstance.FConnections) than just a comparison
  // of names!
  if (ASignal.GetSignal.FName = FResetPort.FName) or (ASignal.GetSignal.FName = FClockPort.FName) then  // compare signal name, not reconf.signal name!
    Exit;
  // amend netlist
  if ASignal.FSigConn is TSigConnDyn then
    Begin
      SigConn := ASignal.FSigConn as TSigConnDyn;
      if not SigConn.FConnTypeOptions.FArray then
        Begin
          FISInstance.ConnectPort(PortName,FModule.FPorts[PortName]);
        End
      else
        Begin
          // array signal: split in single-bit signals for InterSynth module
          For I := 0 to (ASignal.GetSignal.FType.GetWidthInt div SigConn.FConnType.FWidth)-1 do
            Begin
              if SigConn.FConnType.FWidth = 1 then
                FISInstance.ConnectPort(PortName+'_'+IntToStr(I),TValueIndex.Create(FModule.FPorts[PortName],TValueInteger.Create(I)))
              else
                FISInstance.ConnectPort(PortName+'_'+IntToStr(I),TValueIndex.Create(FModule.FPorts[PortName],TRange.Create(dirDown,(I+1)*SigConn.FConnType.FWidth-1,I*SigConn.FConnType.FWidth)));
              // TODO: probably this index-thing only works for inputs, but not for outputs
            End;
        End;
    End
  else if ASignal.FSigConn is TSigConnDirect then
    Begin
      FISInstance.ConnectPort(PortName,FModule.FPorts[PortName]);
      // TODO: special handling of direct connection array signals
    End;
  // other usages/connections are not connected to the InterSynth module
End;

End.

