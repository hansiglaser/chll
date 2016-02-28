Unit InterSynthHandler;

{$mode objfpc}{$H+}

Interface

Uses
  Classes, SysUtils, StrUtils, FGL,
  Netlist, Utils, Tables,
  ReconfSignals, ReconfApp, ReconfCell, ConfigIntf, ParamIntf;

Type

  { TConstCellType }

  TConstCellType = class
    FConnType : TConnType;
    FName     : String;
    FModule   : TModule;
    Constructor Create(AConnType:TConnType);
    Destructor  Destroy; override;
    Function GetCellType : String;
  End;
  TConstCellTypes = specialize TFPGMap<String,TConstCellType>;

  { TConstCellInstance }

  TConstCellInstance = class
    FType     : TConstCellType;
    FInstance : String;
    FValue    : String;
    Constructor Create(AType:TConstCellType;AInstance,AValue:String);
    Function GetNode : String;
  End;
  TConstCellInstances = specialize TFPGMap<String,TConstCellInstance>;

  { TInterSynthHandler }

  TInterSynthHandler = class
    // references to global information
    FReconfSignals     : TReconfSignals;
    FReconfApps        : TReconfApps;
    FReconfCells       : TReconfCells;
    // internally generated stuff
    FConstCellTypes    : TConstCellTypes;
    FParamInCount      : TStringIntMap;      // key: conntype, value: total sum of param in  for this conntype for all apps
    FParamOutCount     : TStringIntMap;      // key: conntype, value: total sum of param out for this conntype for all apps
    // lists to check for completeness
    FReadConnTypes     : TConnTypes;
    FReadSigCells      : TReconfSignalList;  // "celltype Cell(In|Out|InOut)<signal><postfix> ..."
    FReadSigPorts      : TReconfSignalList;  // "(input|output) <signal><postfix> ..."
    FReadSigCellArr    : TStringIntMap;      // bit field for every index
    FReadSigPortArr    : TStringIntMap;
    FReadParamInCells  : TConnTypes;
    FReadParamInPorts  : TConnTypes;
    FReadParamOutCells : TConnTypes;
    FReadParamOutPorts : TConnTypes;
    FReadConstCells    : TConnTypes;
    FReadCells         : TReconfCellsList;
    FReadCellPorts     : TStringIntMap;      // better would be a list
    FReadDirectSigs    : TReconfSignalList;  // index: 'cell.port', value: TReconfSignalBase
    FReadCfgPorts      : TStringIntMap;      // better would be a list
    FReadPortCount     : TStringIntMap;      // key: celltype, value: cell count
    FReadCellCount     : TStringIntMap;      // key: celltype, value: cell count
    FReadConstCount    : TStringIntMap;      // key: conntype, value: cell count
    FReadParamInCount  : TStringIntMap;      // key: conntype, value: cell count
    FReadParamOutCount : TStringIntMap;      // key: conntype, value: cell count
    FReadNodeMap       : TStringIntMap;      // key: 'app.instance', value: cellnum
    FReadNodeInstance  : TStringStringMap;
    FReadBitdataSize   : Integer;
    // config and param infrastructure
    FConfigRegisters   : TConfigRegisters;
    FParameters        : TParameters;
    Constructor Create(AReconfSignals:TReconfSignals;AReconfApps:TReconfApps;AReconfCells:TReconfCells);
    Destructor  Destroy; override;
    Procedure Prepare;
    Procedure WriteConnTypes(Var T:Text);
    Procedure WriteDynPorts (Var T:Text);
    Procedure WriteCellTypes(Var T:Text);
    Procedure WriteNetlist  (Var T:Text;AApp:String);
    Procedure WriteNetlists (Var T:Text);
    Procedure WriteStdCells (Var T:Text);    // Verilog output!
    Procedure ReadCommands(AFilename:String);
    Procedure ReadConfig(AFilename:String);
    Function  CheckMissing:Integer;
    Procedure ShowInfo(Cols:TStringIntMap;Table:TTable;TextT:TTextTable);
    Function  GetCellInstance(ACell:String;ANum:Integer):String;
    Function  GetNodeMap(AApp,AInstance:String):Integer;
    Function  GenerateModule(AName:String):TModule;
    Procedure SetupConfigRegisters;
    Function  GenerateConfigRegisterControlGroup(AConfigRegisters:TAddressedConfigRegisters;AISInstance:TInstance):TConfigRegisterControlGroup;
    Procedure SetupParameters;
    Function CheckUsage(AApp:TReconfApp;AIgnore:TFPGStringList):Integer;
  Private
    Procedure AddReconfSignal(ASignal:TReconfSignalBase;Const AData:Pointer);
    Procedure SetupConstCellTypes;
  End;

Implementation

{ TConstCell }

Constructor TConstCellType.Create(AConnType:TConnType);
Begin
  inherited Create;
  FConnType := AConnType;
  FName     := 'CONST_'+FConnType.FName;
  FModule   := TModule.Create(FName);
  FModule.AddPort(TPort.Create('Value_o',   dirOut,FConnType.FType));
  FModule.AddPort(TPort.Create('CfgValue_i',dirIn, FConnType.FType));
  FModule.AddAssignment(FModule.FPorts['Value_o'],FModule.FPorts['CfgValue_i']);
End;

Destructor TConstCellType.Destroy;
Begin
  FModule.Free;
  Inherited Destroy;
End;

Function TConstCellType.GetCellType:String;
Begin
  Result := 'celltype CONST_'+FConnType.FName+' '+FConnType.FName+' *Value_o cfg:'+IntToStr(FConnType.FWidth)+' CfgValue_i';
End;

{ TConstCellInstance }

Constructor TConstCellInstance.Create(AType:TConstCellType;AInstance,AValue:String);
Begin
  inherited Create;
  FType     := AType;
  FInstance := AInstance;
  FValue    := AValue;
End;

Function TConstCellInstance.GetNode:String;
Begin
  Result := 'node Inst'+FInstance+' CONST_'+FType.FConnType.FName+' Value_o '+FInstance+' CfgValue_i '''+FValue;
End;

{ TInterSynthHandler }

Constructor TInterSynthHandler.Create(AReconfSignals:TReconfSignals;AReconfApps:TReconfApps;AReconfCells:TReconfCells);
Begin
  inherited Create;
  FReconfSignals     := AReconfSignals;
  FReconfApps        := AReconfApps;
  FReconfCells       := AReconfCells;
  // internally generated stuff
  FConstCellTypes    := TConstCellTypes.Create;
  FParamInCount      := TStringIntMap.Create;
  FParamOutCount     := TStringIntMap.Create;
  // places to store what we have read in
  FReadConnTypes     := TConnTypes.Create;
  FReadSigCells      := TReconfSignalList.Create;
  FReadSigPorts      := TReconfSignalList.Create;
  FReadSigCellArr    := TStringIntMap.Create;
  FReadSigPortArr    := TStringIntMap.Create;
  FReadDirectSigs    := TReconfSignalList.Create;
  FReadParamInCells  := TConnTypes.Create;
  FReadParamInPorts  := TConnTypes.Create;
  FReadParamOutCells := TConnTypes.Create;
  FReadParamOutPorts := TConnTypes.Create;
  FReadConstCells    := TConnTypes.Create;
  FReadCells         := TReconfCellsList.Create;
  FReadCellPorts     := TStringIntMap.Create;      // better would be a list
  FReadCellCount     := TStringIntMap.Create;
  FReadCfgPorts      := TStringIntMap.Create;      // better would be a list
  FReadPortCount     := TStringIntMap.Create;
  FReadConstCount    := TStringIntMap.Create;
  FReadParamInCount  := TStringIntMap.Create;
  FReadParamOutCount := TStringIntMap.Create;
  FReadNodeMap       := TStringIntMap.Create;
  FReadNodeInstance  := TStringStringMap.Create;
  FReadBitdataSize   := -1;   // unset

  FReadConnTypes.    Duplicates := dupError;
  FReadSigCells.     Duplicates := dupError;
  FReadSigPorts.     Duplicates := dupError;
  FReadSigCellArr.   Duplicates := dupError;
  FReadSigPortArr.   Duplicates := dupError;
  FReadDirectSigs.   Duplicates := dupError;
  FReadParamInCells. Duplicates := dupError; 
  FReadParamInPorts. Duplicates := dupError; 
  FReadParamOutCells.Duplicates := dupError; 
  FReadParamOutPorts.Duplicates := dupError;
  FReadConstCells.   Duplicates := dupError;
  FReadCells.        Duplicates := dupError;
  FReadCellPorts.    Duplicates := dupError;
  FReadCellCount.    Duplicates := dupError;
  FReadCfgPorts.     Duplicates := dupError;
  FReadPortCount.    Duplicates := dupError;
  FReadConstCount.   Duplicates := dupError;
  FReadParamInCount. Duplicates := dupError;
  FReadParamOutCount.Duplicates := dupError;
  FReadNodeMap.      Duplicates := dupError;
  FReadNodeInstance. Duplicates := dupError;
  FReadConnTypes.    Sorted := true;
  FReadSigCells.     Sorted := true;
  FReadSigPorts.     Sorted := true;
  FReadSigCellArr.   Sorted := true;
  FReadSigPortArr.   Sorted := true;
  FReadDirectSigs.   Sorted := true;
  FReadParamInCells. Sorted := true; 
  FReadParamInPorts. Sorted := true; 
  FReadParamOutCells.Sorted := true; 
  FReadParamOutPorts.Sorted := true;
  FReadConstCells.   Sorted := true;
  FReadCells.        Sorted := true;
  FReadCellPorts.    Sorted := true;
  FReadCellCount.    Sorted := true;
  FReadCfgPorts.     Sorted := true;
  FReadPortCount.    Sorted := true;
  FReadConstCount.   Sorted := true;
  FReadParamInCount. Sorted := true;
  FReadParamOutCount.Sorted := true;
  FReadNodeMap.      Sorted := true;
  FReadNodeInstance. Sorted := true;
End;

Destructor TInterSynthHandler.Destroy;
Begin
  FConstCellTypes.Free;
  FParamInCount.Free;
  FParamOutCount.Free;
  FReadConnTypes.Free;
  FReadSigCells.Free;
  FReadSigPorts.Free;
  FReadSigCellArr.Free;
  FReadSigPortArr.Free;
  FReadParamInCells.Free;
  FReadParamInPorts.Free;
  FReadParamOutCells.Free;
  FReadParamOutPorts.Free;
  FReadConstCells.Free;
  FReadCells.Free;
  FReadCellPorts.Free;
  FReadDirectSigs.Free;
  FReadCfgPorts.Free;
  FReadPortCount.Free;
  FReadCellCount.Free;
  FReadConstCount.Free;
  FReadParamInCount.Free;
  FReadParamOutCount.Free;
  FReadNodeMap.Free;
  FReadNodeInstance.Free;
  FConfigRegisters.Free;
  FParameters.Free;
  Inherited Destroy;
End;

Procedure TInterSynthHandler.Prepare;
Var I,J : Integer;
Begin
  // determine total number of ParamIn/Out cells for this conntype
  For I := 0 to FReconfSignals.FConnTypes.Count-1 do   // misuse I
    With FReconfSignals.FConnTypes.Data[I] do
      if FParamInCount.IndexOf(FName) < 0 then
        Begin
          FParamInCount. Add(FName,0);
          FParamOutCount.Add(FName,0);
        End
      else
        Begin
          FParamInCount [FName] := 0;
          FParamOutCount[FName] := 0;
        End;
  For I := 0 to FReconfApps.FReconfApps.Count-1 do
    With FReconfApps.FReconfApps.Data[I] do
      For J := 0 to FParamPorts.Count-1 do
        With FParamPorts.Data[J] do
          if FDirection = dirIn then
            FParamInCount [FConnType.FName] := FParamInCount [FConnType.FName] + 1
          else
            FParamOutCount[FConnType.FName] := FParamOutCount[FConnType.FName] + 1;
End;

Const CPortDirectionPrefix : Array[TPortDirection] of String = ('*UNKNOWN*','In','Out','InOut');

Procedure TInterSynthHandler.WriteConnTypes(Var T:Text);
Var I : Integer;
Begin
  // first check that all connection types have their tree parameters set
  For I := 0 to FReconfSignals.FConnTypes.Count-1 do
    With FReconfSignals.FConnTypes.Data[I] do
      if FTrees = 0 then
        raise Exception.Create('Connection type '''+FName+''' doesn''t have tree parameters. Use set_conntype_tree');
  WriteLn(T,'### Connection Types');
  For I := 0 to FReconfSignals.FConnTypes.Count-1 do
    With FReconfSignals.FConnTypes.Data[I] do
      if FTrees < 0 then
        WriteLn(T,'conntype ',FName,' ',FWidth)
      else
        WriteLn(T,'conntype ',FName,' ',FWidth,' ',FTrees,' ',FloatToStr(FCost));
End;

Procedure TInterSynthHandler.WriteDynPorts(Var T:Text);
Var I,J      : Integer;
    SigConn  : TSigConnDyn;
    CellName : String;
Begin
  WriteLn(T,'### Dynamic Ports');
  For I := 0 to FReconfSignals.FReconfSignals.Count-1 do
    With FReconfSignals.FReconfSignals.Data[I] Do
      Begin
        if not (FSigConn is TSigConnDyn) then
          Continue;
        SigConn := (FSigConn as TSigConnDyn);
        if not SigConn.FConnTypeOptions.FArray then
          Begin
            CellName := 'Cell' + CPortDirectionPrefix[GetDirection] + GetPortName;
            WriteLn(T,'celltype !',CellName,' ',SigConn.FConnType.FName,' ',Select('*','',GetDirection = dirIn),'PORT');
            // celltype [!]<name> [ { <conntype> [@|*]<port> | cfg:<bit-width> <cfgport> } .. ]
            //   '!' prefix of name: this cell should not be instantiated in the HDL module
            //   '*' prefix of port: output
            //   '@' prefix of port: feedback input, i.e. can be connected directly to an output of the very same cell
            WriteLn(T,Select('input','output',GetDirection = dirIn),' ',GetPortName,' ',
              SigConn.FConnType.FWidth,' ',
              CellName,' PORT');
            // input <name> [*]<bit-width> [ <celltype> { <port>| .<signal> } .. ]
            //   '*' prefix of bit-width: a separate signal is routed to each cell
            //   <celltype> <port> syntax: signal goes to interconnect
            //   <celltype> .<signal> syntax: connection goes directly to newly created port <signal> on all cell instances
            // problem: InterSynth only instantiates input cells, if they are
            // used --> workaround: force a minimum of 1
            if GetDirection = dirIn then
              WriteLn(T,'headroom cellup ',CellName,' min 1');
          End
        else
          Begin
            For J := 0 to (GetSignal.FType.GetWidthInt div SigConn.FConnType.FWidth)-1 do
              Begin
                CellName := 'Cell' + CPortDirectionPrefix[GetDirection] + GetPortName + '_' + IntToStr(J);
                WriteLn(T,'celltype !',CellName,' ',SigConn.FConnType.FName,' ',Select('*','',GetDirection = dirIn),'PORT');
                WriteLn(T,Select('input','output',GetDirection = dirIn),' ',GetPortName,'_',IntToStr(J),' ',
                  SigConn.FConnType.FWidth,' ',
                  CellName,' PORT');
                if GetDirection = dirIn then
                  WriteLn(T,'headroom cellup ',CellName,' min 1');
              End;
          End;
      End;
End;

Procedure TInterSynthHandler.WriteCellTypes(Var T:Text);
Var CellIdx,SigIdx,PortIdx : Integer;
Begin
  Prepare;
  WriteLn(T,'### Cell Types');
  For CellIdx := 0 to FReconfCells.FReconfCells.Count-1 do
    With FReconfCells.FReconfCells.Data[CellIdx] do
      Begin
        Write(T,'celltype ',FName);
        // celltype [!]<name> [ { <conntype> [@|*]<port> | cfg:<bit-width> <cfgport> } .. ]
        //   '!' prefix of name: this cell should not be instantiated in the HDL module
        //   '*' prefix of port: output
        //   '@' prefix of port: feedback input, i.e. can be connected directly to an output of the very same cell
        For PortIdx := 0 to FDynamicPorts.Count-1 do
          With FDynamicPorts.Data[PortIdx] do
            Write(T,' ',FConnType.FName,' ',Select('','*',FDir = dirIn),FDynamicPorts.Data[PortIdx].FName);
        // 'cfg:<bit-width> <cfgport>'
        For PortIdx := 0 to FConfigPorts.Count-1 do
          With FConfigPorts.Data[PortIdx] do
            Write(T,' cfg:',FWidth,' ',FConfigPorts.Data[PortIdx].FName);
        WriteLn(T);
      End;
  WriteLn(T,'# Special Cell Types for Constant Values');
  if FConstCellTypes.Count = 0 then
    SetupConstCellTypes;
  For CellIdx := 0 to FConstCellTypes.Count-1 do
    WriteLn(T,FConstCellTypes.Data[CellIdx].GetCellType);
  WriteLn(T,'# Special Cell Types for Param Values');
  WriteLn('Example applications usage of parameters');
  For CellIdx := 0 to FReconfSignals.FConnTypes.Count-1 do   // misuse CellIdx
    WriteLn('  ',FReconfSignals.FConnTypes.Data[CellIdx].FName,StringOfChar(' ',8-Length(FReconfSignals.FConnTypes.Data[CellIdx].FName)),'  in: ',FParamInCount[FReconfSignals.FConnTypes.Data[CellIdx].FName]:2,'  out: ',FParamOutCount[FReconfSignals.FConnTypes.Data[CellIdx].FName]:2);

  For CellIdx := 0 to FReconfSignals.FConnTypes.Count-1 do
    With FReconfSignals.FConnTypes.Data[CellIdx] do
      Begin
        if FParamInCount[FName] > 0 then
          Begin
            WriteLn(T,'celltype !CellParamIn_',FName,' ',FName,' *PORT');
            WriteLn(T,'input ParamIn_',FName,'_i *',FWidth,' CellParamIn_',FName,' PORT');
          End;
        if FParamOutCount[FName] > 0 then
          Begin
            WriteLn(T,'celltype !CellParamOut_',FName,' ',FName,' PORT');
            WriteLn(T,'output ParamOut_',FName,'_o *',FWidth,' CellParamOut_',FName,' PORT');
          End;
      End;

  WriteLn(T);
  WriteLn(T,'### Direct Ports');
  For SigIdx := 0 to FReconfSignals.FReconfSignals.Count-1 do
    With FReconfSignals.FReconfSignals.Data[SigIdx] Do
      Begin
        if not (FSigConn is TSigConnDirect) then
          Continue;
        Write(T,Select('input','output',GetDirection = dirIn),' ',GetPortName,' ',
          Select('*','',false{TODO: array signals!}),GetSignal.FType.GetWidthInt);
        // input <name> [*]<bit-width> [ <celltype> { <port>| .<signal> } .. ]
        //   '*' prefix of bit-width: a separate signal is routed to each cell
        //   <celltype> <port> syntax: signal goes to interconnect
        //   <celltype> .<signal> syntax: connection goes directly to newly created port <signal> on all cell instances
        For CellIdx := 0 to FReconfCells.FReconfCells.Count-1 do
          With FReconfCells.FReconfCells.Data[CellIdx] do
            Begin
              For PortIdx := 0 to FDirectPorts.Count-1 do
                Begin
                  if FDirectPorts.Data[PortIdx].FReconfSignal <> FReconfSignals.FReconfSignals.Data[SigIdx] then
                    Continue;
                  Write(T,' ',FReconfCells.FReconfCells.Data[CellIdx].FName,' .',FDirectPorts.Data[PortIdx].FName);
                End;
            End;
        WriteLn(T);
      End;

  if FReconfCells.HaveConfigChains then
    Begin
      WriteLn(T);
      WriteLn(T,'### Config Chains');
      Write(T,'input CfgMode_i 1');
      For CellIdx := 0 to FReconfCells.FReconfCells.Count-1 do
        With FReconfCells.FReconfCells.Data[CellIdx] do
          if assigned(FConfigChain) then
            Write(T,' ',FName,' .',FConfigChain.FCfgMode);
      WriteLn(T);
      For CellIdx := 0 to FReconfCells.FReconfCells.Count-1 do
        With FReconfCells.FReconfCells.Data[CellIdx] do
          if assigned(FConfigChain) then
            WriteLn(T,'input CfgClk_',FName,'_i *1 ',FName,' .',FConfigChain.FCfgClk);
      WriteLn(T);
      For CellIdx := 0 to FReconfCells.FReconfCells.Count-1 do
        With FReconfCells.FReconfCells.Data[CellIdx] do
          if assigned(FConfigChain) then
            WriteLn(T,'input CfgShift_',FName,'_i *1 ',FName,' .',FConfigChain.FCfgShift);    // only one celltype and port possible
      Write(T,'input CfgDataIn_i 1');
      For CellIdx := 0 to FReconfCells.FReconfCells.Count-1 do
        With FReconfCells.FReconfCells.Data[CellIdx] do
          if assigned(FConfigChain) then
            Write(T,' ',FName,' .',FConfigChain.FCfgDataIn);
      WriteLn(T);
      For CellIdx := 0 to FReconfCells.FReconfCells.Count-1 do
        With FReconfCells.FReconfCells.Data[CellIdx] do
          if assigned(FConfigChain) then
            WriteLn(T,'output CfgDataOut_',FName,'_o *1 ',FName,' .',FConfigChain.FCfgDataOut);
    End;
End;

Procedure TInterSynthHandler.WriteNetlist(Var T:Text;AApp:String);
Var App      : TReconfApp;
    I        : Integer;
    PortIdx  : Integer;
    InstName : String;
    CellName : String;
    DynPort  : TDynamicPort;
    ConnType : TConnType;
    Value    : String;
    ConstCells : TConstCellInstances;
    A          : TAssignment;

  Function AddConstCell(AConnType:TConnType;AValue:String) : String;
  Var ConstCellType : TConstCellType;
  Begin
    ConstCellType := FConstCellTypes[AConnType.FName];
    Result := ConstCellType.FName+'_'+AValue;
    if ConstCells.IndexOf(Result) < 0 then
      ConstCells.Add(Result,TConstCellInstance.Create(ConstCellType,Result,AValue));
  End;

Begin
  if FReconfApps.FReconfApps.IndexOf(AApp) < 0 then
    raise Exception.Create('Unknown application '''+AApp+'''');
  if FConstCellTypes.Count = 0 then
    SetupConstCellTypes;
  ConstCells := TConstCellInstances.Create;
  App := FReconfApps.FReconfApps[AApp];
  WriteLn(T,'# Netlist of application ',AApp);
  WriteLn(T,'netlist ',AApp);
  // ports
  For I := 0 to App.FDynamicPorts.Count-1 do
    With App.FDynamicPorts.Data[I] do
      Begin
        A := App.FYosysNetlist.FindAssignment(FName);
        if assigned(A) and (A.FValue is TPort) then
          Begin
            Value := (A.FValue as TPort).FName;
            WriteLn(T,'# assignment between two ports ',A.GetVHDL);
          End
        else
          Value := FName;
        if not (FReconfSignal.FSigConn as TSigConnDyn).FConnTypeOptions.FArray then
          Begin
            CellName := 'Cell' + CPortDirectionPrefix[FReconfSignal.GetDirection] + FReconfSignal.GetPortName;
            InstName := 'Inst' + CPortDirectionPrefix[FReconfSignal.GetDirection] + FReconfSignal.GetPortName;
          End
        else
          Begin
            CellName := 'Cell' + CPortDirectionPrefix[FReconfSignal.GetDirection] + FReconfSignal.GetPortName + '_' + IntToStr(FIndex div (FReconfSignal.FSigConn as TSigConnDyn).FConnType.FWidth);
            InstName := 'Inst' + CPortDirectionPrefix[FReconfSignal.GetDirection] + FReconfSignal.GetPortName + '_' + IntToStr(FIndex div (FReconfSignal.FSigConn as TSigConnDyn).FConnType.FWidth);
          End;
        WriteLn(T,'node ',InstName,' ',CellName,' PORT ',Value);
      End;
  // TODO: how can we check that FDirectPorts stuff is ok?
  For I := 0 to App.FParamPorts.Count-1 do
    With App.FParamPorts.Data[I] do
      Begin
        CellName := 'CellParam' + CPortDirectionPrefix[FDirection] + '_' + FConnType.FName;
        InstName := 'InstParam' + CPortDirectionPrefix[FDirection] + '_' + FName;
        WriteLn(T,'node ',InstName,' ',CellName,' PORT ',FName);
      End;
  For I := 0 to App.FConstantValues.Count-1 do
    With App.FConstantValues.Data[I] do
      Begin
        if not (FReconfSignal.FSigConn is TSigConnDyn) then
          continue;
        if not (FReconfSignal.FSigConn as TSigConnDyn).FConnTypeOptions.FArray then
          Begin
            CellName := 'Cell' + CPortDirectionPrefix[FReconfSignal.GetDirection] + FReconfSignal.GetPortName;
            InstName := 'Inst' + CPortDirectionPrefix[FReconfSignal.GetDirection] + FReconfSignal.GetPortName;
            WriteLn(T,'node ',InstName,' ',CellName,' PORT ', AddConstCell((FReconfSignal.FSigConn as TSigConnDyn).FConnType,IntToBin(FValue,(FReconfSignal.FSigConn as TSigConnDyn).FConnType.FWidth)));
          End
        else
          Begin
            if ((FReconfSignal.FSigConn as TSigConnDyn).FConnTypeOptions.FPadLeftWidth >= 0) or
               ((FReconfSignal.FSigConn as TSigConnDyn).FConnTypeOptions.FPadRightWidth >= 0) then
              raise Exception.Create('TODO: Implement padding for array ports for constant values');
            // note: we can't find out the width of the constant value here
            Value := IntToBin(FValue,FReconfSignal.GetSignal.FType.GetWidthInt);
            For PortIdx := 0 to (FReconfSignal.GetSignal.FType.GetWidthInt div (FReconfSignal.FSigConn as TSigConnDyn).FConnType.FWidth)-1 do
              Begin
                CellName := 'Cell' + CPortDirectionPrefix[FReconfSignal.GetDirection] + FReconfSignal.GetPortName + '_' + IntToStr(PortIdx);
                InstName := 'Inst' + CPortDirectionPrefix[FReconfSignal.GetDirection] + FReconfSignal.GetPortName + '_' + IntToStr(PortIdx);
                WriteLn(T,'node ',InstName,' ',CellName,' PORT ', AddConstCell((FReconfSignal.FSigConn as TSigConnDyn).FConnType,Copy(Value,Length(Value)-(FReconfSignal.FSigConn as TSigConnDyn).FConnType.FWidth+1,Length(Value))));
                SetLength(Value,Length(Value)-(FReconfSignal.FSigConn as TSigConnDyn).FConnType.FWidth);   // strip off end of Value
              End;
          End;
      End;
  For I := 0 to App.FYosysNetlist.FInstances.Count-1 do
    With App.FYosysNetlist.FInstances.Data[I] do
      Begin
        Write(T,'node ',FName,' ',FModule.FName);
        // dynamic ports
        For PortIdx := 0 to FConnections.Count-1 do
          Begin
            if FReconfCells.FReconfCells[FModule.FName].FDynamicPorts.IndexOf(FConnections.Keys[PortIdx]) < 0 then
              Continue;   // not a dynamic port
            DynPort  := FReconfCells.FReconfCells[FModule.FName].FDynamicPorts[FConnections.Keys[PortIdx]];
            ConnType := DynPort.FConnType;
            Write(T,' ',FConnections.Keys[PortIdx],' ');
            if FConnections.Data[PortIdx] is TSignal then
              Write(T,(FConnections.Data[PortIdx] as TSignal).FName)
            else if FConnections.Data[PortIdx] is TValueBit then
              Begin
                Value    := (FConnections.Data[PortIdx] as TValueBit).FValue;
                CellName := AddConstCell(ConnType,Value);
                Write(T,CellName);
              End
            else if FConnections.Data[PortIdx] is TValueVector then
              Begin
                Value    := (FConnections.Data[PortIdx] as TValueVector).FValue;
                CellName := AddConstCell(ConnType,Value);
                Write(T,CellName);
              End
            else
              Write(T,'*Invalid*Connection*',FConnections.Data[PortIdx].ClassName);
          End;
        // config ports
        For PortIdx := 0 to FConnections.Count-1 do
          Begin
            if FReconfCells.FReconfCells[FModule.FName].FConfigPorts.IndexOf(FConnections.Keys[PortIdx]) < 0 then
              Continue;   // not a config port
            Write(T,' ',FConnections.Keys[PortIdx],' ');
            if FConnections.Data[PortIdx] is TValueBit then
              Begin
                Write(T,'''',(FConnections.Data[PortIdx] as TValueBit).FValue);
              End
            else if FConnections.Data[PortIdx] is TValueVector then
              Begin
                Write(T,'''',(FConnections.Data[PortIdx] as TValueVector).FValue);
              End
            else
              Write(T,'*Invalid*Connection*',FConnections.Data[PortIdx].ClassName);
          End;
        WriteLn(T);
      End;
  if ConstCells.Count > 0 then
    WriteLn(T,'# constant cells');
  For I := 0 to ConstCells.Count-1 do
    With ConstCells.Data[I] Do
      Begin
        WriteLn(T,GetNode);
        ConstCells.Data[I].Free;
      End;
  ConstCells.Free;
End;

Procedure TInterSynthHandler.WriteNetlists(Var T:Text);
Var I : Integer;
Begin
  WriteLn(T,'### Netlists');
  For I := 0 to FReconfApps.FReconfApps.Count-1 do
    Begin
      WriteNetlist(T,FReconfApps.FReconfApps.Keys[I]);
      if I < FReconfApps.FReconfApps.Count-1 then
        WriteLn(T);
    End;
End;

Procedure TInterSynthHandler.WriteStdCells(Var T:Text);
Var I : Integer;
Begin
  // Attention: the file format of these standard cells is Verilog, not InterSynth!
  WriteLn(T,'// auto-generated by WriteStdCells');
  For I := 0 to FConstCellTypes.Count-1 do
    WriteLn(T,FConstCellTypes.Data[I].FModule.WriteVerilogDeclaration);
End;

Procedure TInterSynthHandler.ReadCommands(AFilename:String);
Var ReconfSignals : TReconfSignalList;

  Procedure ReadConntype(Tokens:TDynStringArray);
  Var ConnType : TConnType;
  Begin
    // conntype <name> <bit-width> [ <trees> <cost> ]
    if Length(Tokens) < 3 then
      raise Exception.Create('Not enough arguments');
    if FReconfSignals.FConnTypes.IndexOf(Tokens[1]) < 0 then
      raise Exception.Create('Unknown connection type '+Tokens[1]);
    ConnType := FReconfSignals.FConnTypes[Tokens[1]];
    if ConnType.FWidth <> StrToInt(Tokens[2]) then
      raise Exception.Create('Width of connection type '+Tokens[1]+' differs between my knowledge ('+IntToStr(ConnType.FWidth)+') and InterSynth file ('+Tokens[2]+')');
    // store in our list to check if something is missing
    FReadConnTypes.Add(Tokens[1],ConnType);
  End;

  Function PostfixToInt(Var St:String):Integer;
  Var I : Integer;
  Begin
    // e.g. 'Outputs_o_3'
    Result := -1;
    I := Length(St);
    While I > 1 do
      if St[I] in ['0'..'9'] then
        Dec(I)
      else
        break;
    if I = Length(St) then Exit;   // no number
    Result := StrToInt(Copy(St,I+1,Length(St)));
    SetLength(St,I-1);   // cut off number including '_' before it
  End;

  Procedure ReadCellType(Tokens:TDynStringArray);

    Function CheckPortCell(Prefix:String;Dir:TPortDirection) : Boolean;
    Var Index        : Integer;
        ReconfSignal : TReconfSignalBase;
        SigConn      : TSigConnDyn;
    Begin
      if Pos(Prefix,Tokens[1]) <> 1 then Exit(false);
      // celltype !Cell<dir><signal><postfix> <conntype> *PORT
      Delete(Tokens[1],1,Length(Prefix));
      // look at end of name, perhaps there is a number as index
      Index := PostfixToInt(Tokens[1]);              // this is not clean, because it doesn't work with signals which really end with a number
      if ReconfSignals.IndexOf(Tokens[1]) < 0 then
        raise Exception.Create('Unknown reconf.signal '+Tokens[1]);
      ReconfSignal := ReconfSignals[Tokens[1]];
      if ReconfSignal.GetDirection <> Dir then
        raise Exception.Create('Reconf.signal '+Tokens[1]+' has wrong direction ('+CPortDirectionPrefix[ReconfSignal.GetDirection]+', should be '+CPortDirectionPrefix[dirIn]+')');
      if not (ReconfSignal.FSigConn is TSigConnDyn) then
        raise Exception.Create('Reconf.signal '+Tokens[1]+' is not a dynamic signal');
      SigConn := ReconfSignal.FSigConn as TSigConnDyn;
      if SigConn.FConnType.FName <> Tokens[2] then
        raise Exception.Create('Invalid connection type '+Tokens[2]+' of port of cell for reconf.signal '+Tokens[1]+' (should be '+SigConn.FConnType.FName+')');
      if Tokens[3] <> Select('*','',Dir=dirIn)+'PORT' then
        raise Exception.Create('Invalid port name '+Tokens[3]);
      if not SigConn.FConnTypeOptions.FArray then
        Begin
          if Index >= 0 then
            raise Exception.Create('Reconf.signal '+Tokens[1]+' is not an array signal, but a postfix index was found');
          FReadSigCells.Add(Tokens[1],ReconfSignal);
        End
      else
        Begin
          if Index < 0 then
            raise Exception.Create('Reconf.signal '+Tokens[1]+' is an array signal, but no postfix index was found');
          if FReadSigCellArr.IndexOf(Tokens[1]) < 0 then
            Begin
              FReadSigCellArr.Add(Tokens[1],0);
              FReadSigCells.Add(Tokens[1],ReconfSignal);
            End;
          FReadSigCellArr[Tokens[1]] := FReadSigCellArr[Tokens[1]] or (1 shl Index);
        End;
      Result := True;
    End;

  Var Cell     : TReconfCell;
      ConnType : TConnType;
      I        : Integer;
      Dir      : TPortDirection;
      Width    : Integer;
  Begin
    // celltype [!]<name> [ { <conntype> [@|*]<port> | cfg:<bit-width> <cfgport> } .. ]
    if Length(Tokens) < 2 then
      raise Exception.Create('Not enough arguments');
    if Tokens[1][1] = '!' then
      Begin
        Delete(Tokens[1],1,1);
        if      CheckPortCell('CellInOut',dirInOut) then  // nothing to do
        else if CheckPortCell('CellIn',   dirIn)    then  // nothing to do
        else if CheckPortCell('CellOut',  dirOut)   then  // nothing to do
        else if Pos('CellParamIn',Tokens[1]) = 1 then
          Begin
            // celltype !CellParamIn_Bit Bit *PORT
            Delete(Tokens[1],1,12);  // cut off including '_'
            if Tokens[1] <> Tokens[2] then
              raise Exception.Create('Strange name of param in celltype '+Tokens[1]);
            if FReconfSignals.FConnTypes.IndexOf(Tokens[2]) < 0 then
              raise Exception.Create('Unknown connection type for param in cell '+Tokens[2]);
            if Tokens[3] <> '*PORT' then
              raise Exception.Create('Invalid port name '+Tokens[3]);
            FReadParamInCells.Add(Tokens[2],FReconfSignals.FConnTypes[Tokens[2]]);
          End
        else if Pos('CellParamOut',Tokens[1]) = 1 then
          Begin
            // celltype !CellParamOut_Bit Bit PORT
            Delete(Tokens[1],1,13);  // cut off including '_'
            if Tokens[1] <> Tokens[2] then
              raise Exception.Create('Strange name of param out celltype '+Tokens[1]);
            if FReconfSignals.FConnTypes.IndexOf(Tokens[2]) < 0 then
              raise Exception.Create('Unknown connection type '+Tokens[2]+' for param out cell');
            if Tokens[3] <> 'PORT' then
              raise Exception.Create('Invalid port name '+Tokens[3]);
            FReadParamOutCells.Add(Tokens[2],FReconfSignals.FConnTypes[Tokens[2]]);
          End
        else
          raise Exception.Create('Strange non-instantiated cell type '+Tokens[1]);
      End
    else
      Begin
        if Pos('CONST_',Tokens[1]) = 1 then
          Begin
            // celltype CONST_Bit Bit *Value_o cfg:1 CfgValue_i
            Delete(Tokens[1],1,6);  // cut off incluging '_'
            if Tokens[1] <> Tokens[2] then
              raise Exception.Create('Strange name const cell type '+Tokens[1]);
            if FReconfSignals.FConnTypes.IndexOf(Tokens[2]) < 0 then
              raise Exception.Create('Unknown connection type '+Tokens[2]+' for const cell');
            ConnType := FReconfSignals.FConnTypes[Tokens[2]];
            if Tokens[3] <> '*Value_o' then
              raise Exception.Create('Invalid port name '+Tokens[3]);
            if Copy(Tokens[4],1,4) <> 'cfg:' then
              raise Exception.Create('Need a config value');
            Delete(Tokens[4],1,4);
            if StrToInt(Tokens[4]) <> ConnType.FWidth then
              raise Exception.Create('Invalid width for connection type '+Tokens[2]);
            if Tokens[5] <> 'CfgValue_i' then
              raise Exception.Create('Invalid port name '+Tokens[5]);
            FReadConstCells.Add(Tokens[2],ConnType);
          End
        else if FReconfCells.FReconfCells.IndexOf(Tokens[1]) < 0 then
          raise Exception.Create('Unknown cell type '+Tokens[1])
        else
          Begin
            // celltype <name> [ { <conntype> [@|*]<port> | cfg:<bit-width> <cfgport> } .. ]
            Cell := FReconfCells.FReconfCells[Tokens[1]];
            FReadCells.Add(Tokens[1],Cell);
            if Length(Tokens) and $01 <> $00 then  // even number of tokens
              raise Exception.Create('Invalid parameters for celltype');
            I := 2;
            While I < Length(Tokens) do
              Begin
                if Pos('cfg:',Tokens[I]) = 1 then
                  Begin
                    Delete(Tokens[I],1,4);  // remove leading 'cfg:'
                    Width := StrToInt(Tokens[I]);
                    if Cell.FConfigPorts.IndexOf(Tokens[I+1]) < 0 then
                      raise Exception.Create('Unknown config port '+Tokens[I+1]+' of cell '+Tokens[1]+' for '+IntToStr(Width)+' bits');
                    if Cell.FConfigPorts[Tokens[I+1]].FWidth <> Width then
                      raise Exception.Create('Wrong width for config port '+Tokens[I+1]+' of cell '+Tokens[1]+' (read: '+IntToStr(Width)+', should be '+IntToStr(Cell.FConfigPorts[Tokens[I+1]].FWidth)+')');
                    FReadCfgPorts.Add(Tokens[1]+'.'+Tokens[I+1],Width);   // you'll never know for what you can use the width later on :-)
                    Inc(I,2);
                    Continue;
                  End;
                if FReconfSignals.FConnTypes.IndexOf(Tokens[I]) < 0 then
                  raise Exception.Create('Unknown connection type '+Tokens[I]+' for port for cell type '+Tokens[1]);
                ConnType := FReconfSignals.FConnTypes[Tokens[I]];
                if Tokens[I+1][1] = '*' then
                  Begin
                    Dir := dirOut;
                    Delete(Tokens[I+1],1,1);
                  End
                else
                  Dir := dirIn;
                if Cell.FDynamicPorts.IndexOf(Tokens[I+1]) < 0 then
                  raise Exception.Create('Unknown port '+Tokens[I+1]+' of cell '+Tokens[1]+' for dynamic reconf.signal of connection type '+Tokens[I]);
                if Cell.FDynamicPorts[Tokens[I+1]].FDir <> Dir then
                  raise Exception.Create('Wrong direction of port '+Tokens[I+1]+' of cell '+Tokens[1]+' for dynamic reconf.signal of connection type '+Tokens[I]);
                FReadCellPorts.Add(Tokens[1]+'.'+Tokens[I+1],0);
                Inc(I,2);
              End;
          End;
      End;
  End;

  Procedure ReadPort(Tokens:TDynStringArray);
  Var PortName     : String;
      Dir          : TPortDirection;
      Index        : Integer;
      ConnType     : TConnType;
      ReconfSignal : TReconfSignalBase;
      SigConn      : TSigConnDyn;
      Cell         : TReconfCell;
  Begin
    // input|output <name> [*]<bit-width> [ <celltype> { <port>| .<signal> } .. ]
    if      Tokens[0] = 'input'  then Dir := dirIn
    else if Tokens[0] = 'output' then Dir := dirOut
    else
      raise Exception.Create('Internal error: ReadPort doesn''t handle '+Tokens[0]);
    PortName := Tokens[1];
    Index := PostfixToInt(Tokens[1]);              // this is not clean, because it doesn't work with signals which really end with a number
    if Pos('ParamIn',Tokens[1]) = 1 then
      Begin
        // input ParamIn_Bit_i *1 CellParamIn_Bit PORT
        if RightStr(Tokens[1],2) <> '_i' then
          raise Exception.Create('ParamIn port '+Tokens[1]+' doesn''t have ''_i'' suffix');
        Delete(Tokens[1],1,8);  // cut off including '_'
        SetLength(Tokens[1],Length(Tokens[1])-2);  // cut off '_i'
        if FReconfSignals.FConnTypes.IndexOf(Tokens[1]) < 0 then
          raise Exception.Create('Unknown connection type '+Tokens[1]+' for param in port '+PortName);
        ConnType := FReconfSignals.FConnTypes[Tokens[1]];
        if Tokens[2][1] <> '*' then
          raise Exception.Create('Param in ports must be array ports');
        Delete(Tokens[2],1,1);
        if StrToInt(Tokens[2]) <> ConnType.FWidth then
          raise Exception.Create('Invalid port width '+Tokens[2]+' for connection type '+Tokens[1]+' (should be '+IntToStr(ConnType.FWidth)+')');
        if Tokens[3] <> 'CellParamIn_'+Tokens[1] then
          raise Exception.Create('Invalid cell type '+Tokens[3]+' for port '+PortName);
        if Tokens[4] <> 'PORT' then
          raise Exception.Create('Invalid port name '+Tokens[3]);
        FReadParamInPorts.Add(Tokens[1],FReconfSignals.FConnTypes[Tokens[1]]);
      End
    else if Pos('ParamOut',Tokens[1]) = 1 then
      Begin
        // output ParamOut_Bit_o *1 CellParamOut_Bit PORT
        if RightStr(Tokens[1],2) <> '_o' then
          raise Exception.Create('ParamOut port '+Tokens[1]+' doesn''t have ''_o'' suffix');
        Delete(Tokens[1],1,9);  // cut off including '_'
        SetLength(Tokens[1],Length(Tokens[1])-2);  // cut off '_o'
        if FReconfSignals.FConnTypes.IndexOf(Tokens[1]) < 0 then
          raise Exception.Create('Unknown connection type '+Tokens[1]+' for param in port '+PortName);
        ConnType := FReconfSignals.FConnTypes[Tokens[1]];
        if Tokens[2][1] <> '*' then
          raise Exception.Create('Param in ports must be array ports');
        Delete(Tokens[2],1,1);
        if StrToInt(Tokens[2]) <> ConnType.FWidth then
          raise Exception.Create('Invalid port width '+Tokens[2]+' for connection type '+Tokens[1]+' (should be '+IntToStr(ConnType.FWidth)+')');
        if Tokens[3] <> 'CellParamOut_'+Tokens[1] then
          raise Exception.Create('Invalid cell type '+Tokens[3]+' for port '+PortName);
        if Tokens[4] <> 'PORT' then
          raise Exception.Create('Invalid port name '+Tokens[3]);
        FReadParamOutPorts.Add(Tokens[1],FReconfSignals.FConnTypes[Tokens[1]]);
      End
    else if Tokens[1] = 'CfgMode_i' then
      Begin
        if Tokens[2] <> '1' then
          raise Exception.Create('input '+Tokens[1]+' must be 1 bit wide');
        if Length(Tokens) and $01 <> $01 then
          raise Exception.Create('input '+Tokens[1]+' must have an odd number of tokens');
        Index := 3;
        While Index < Length(Tokens) do
          Begin
            if FReconfCells.FReconfCells.IndexOf(Tokens[Index]) < 0 then
              raise Exception.Create('Unknown cell type '+Tokens[Index]+' for input '+Tokens[1]);
            if not assigned(FReconfCells.FReconfCells[Tokens[Index]].FConfigChain) then
              raise Exception.Create('Cell type '+Tokens[Index]+' doesn''t have a config chain');
            if Tokens[Index+1][1] <> '.' then
              raise Exception.Create('Invalid syntax for config mode input of cell type '+Tokens[Index]+' '''+FReconfCells.FReconfCells[Tokens[Index]].FConfigChain.FCfgMode+'''');
            Delete(Tokens[Index+1],1,1);
            if FReconfCells.FReconfCells[Tokens[Index]].FConfigChain.FCfgMode <> Tokens[Index+1] then
              raise Exception.Create('Config mode input of cell type '+Tokens[Index]+' is '''+FReconfCells.FReconfCells[Tokens[Index]].FConfigChain.FCfgMode+''' and not '''+Tokens[Index+1]+'''');
            Inc(Index,2);
          End;
      End
    else if Pos('CfgClk',Tokens[1]) = 1 then
      Begin
        if Tokens[2] <> '*1' then
          raise Exception.Create('input '+Tokens[1]+' must be 1 bit wide array port');
        if Length(Tokens) <> 5 then
          raise Exception.Create('input '+Tokens[1]+' must have five tokens');
        if Tokens[1] <> 'CfgClk_'+Tokens[3]+'_i' then
          raise Exception.Create('input '+Tokens[1]+' name must match with cell type '+Tokens[3]);
        if FReconfCells.FReconfCells.IndexOf(Tokens[3]) < 0 then
          raise Exception.Create('Unknown cell type '+Tokens[3]+' for input '+Tokens[1]);
        if not assigned(FReconfCells.FReconfCells[Tokens[3]].FConfigChain) then
          raise Exception.Create('Cell type '+Tokens[3]+' doesn''t have a config chain');
        if Tokens[4][1] <> '.' then
          raise Exception.Create('Invalid syntax for config clock input of cell type '+Tokens[4]+' '''+FReconfCells.FReconfCells[Tokens[3]].FConfigChain.FCfgClk+'''');
        Delete(Tokens[4],1,1);
        if FReconfCells.FReconfCells[Tokens[3]].FConfigChain.FCfgClk <> Tokens[4] then
          raise Exception.Create('Config clock input of cell type '+Tokens[3]+' is '''+FReconfCells.FReconfCells[Tokens[4]].FConfigChain.FCfgClk+''' and not '''+Tokens[4]+'''');
      End
    else if Pos('CfgShift',Tokens[1]) = 1 then
      Begin
        if Tokens[2] <> '*1' then
          raise Exception.Create('input '+Tokens[1]+' must be 1 bit wide array port');
        if Length(Tokens) <> 5 then
          raise Exception.Create('input '+Tokens[1]+' must have five tokens');
        if Tokens[1] <> 'CfgShift_'+Tokens[3]+'_i' then
          raise Exception.Create('input '+Tokens[1]+' name must match with cell type '+Tokens[3]);
        if FReconfCells.FReconfCells.IndexOf(Tokens[3]) < 0 then
          raise Exception.Create('Unknown cell type '+Tokens[3]+' for input '+Tokens[1]);
        if not assigned(FReconfCells.FReconfCells[Tokens[3]].FConfigChain) then
          raise Exception.Create('Cell type '+Tokens[3]+' doesn''t have a config chain');
        if Tokens[4][1] <> '.' then
          raise Exception.Create('Invalid syntax for config clock input of cell type '+Tokens[4]+' '''+FReconfCells.FReconfCells[Tokens[3]].FConfigChain.FCfgShift+'''');
        Delete(Tokens[4],1,1);
        if FReconfCells.FReconfCells[Tokens[3]].FConfigChain.FCfgShift <> Tokens[4] then
          raise Exception.Create('Config shift input of cell type '+Tokens[3]+' is '''+FReconfCells.FReconfCells[Tokens[4]].FConfigChain.FCfgShift+''' and not '''+Tokens[4]+'''');
      End
    else if Tokens[1] = 'CfgDataIn_i' then
      Begin
        if Tokens[2] <> '1' then
          raise Exception.Create('input '+Tokens[1]+' must be 1 bit wide');
        if Length(Tokens) and $01 <> $01 then
          raise Exception.Create('input '+Tokens[1]+' must have an odd number of tokens');
        Index := 3;
        While Index < Length(Tokens) do
          Begin
            if FReconfCells.FReconfCells.IndexOf(Tokens[Index]) < 0 then
              raise Exception.Create('Unknown cell type '+Tokens[Index]+' for input '+Tokens[1]);
            if not assigned(FReconfCells.FReconfCells[Tokens[Index]].FConfigChain) then
              raise Exception.Create('Cell type '+Tokens[Index]+' doesn''t have a config chain');
            if Tokens[Index+1][1] <> '.' then
              raise Exception.Create('Invalid syntax for config data input of cell type '+Tokens[Index]+' '''+FReconfCells.FReconfCells[Tokens[Index]].FConfigChain.FCfgDataIn+'''');
            Delete(Tokens[Index+1],1,1);
            if FReconfCells.FReconfCells[Tokens[Index]].FConfigChain.FCfgDataIn <> Tokens[Index+1] then
              raise Exception.Create('Config data input of cell type '+Tokens[Index]+' is '''+FReconfCells.FReconfCells[Tokens[Index]].FConfigChain.FCfgDataIn+''' and not '''+Tokens[Index+1]+'''');
            Inc(Index,2);
          End;
      End
    else if Pos('CfgDataOut',Tokens[1]) = 1 then
      Begin
        if Tokens[2] <> '*1' then
          raise Exception.Create('output '+Tokens[1]+' must be 1 bit wide array port');
        if Length(Tokens) <> 5 then
          raise Exception.Create('output '+Tokens[1]+' must have five tokens');
        if Tokens[1] <> 'CfgDataOut_'+Tokens[3]+'_o' then
          raise Exception.Create('output '+Tokens[1]+' name must match with cell type '+Tokens[3]);
        if FReconfCells.FReconfCells.IndexOf(Tokens[3]) < 0 then
          raise Exception.Create('Unknown cell type '+Tokens[3]+' for input '+Tokens[1]);
        if not assigned(FReconfCells.FReconfCells[Tokens[3]].FConfigChain) then
          raise Exception.Create('Cell type '+Tokens[3]+' doesn''t have a config chain');
        if Tokens[4][1] <> '.' then
          raise Exception.Create('Invalid syntax for config clock input of cell type '+Tokens[4]+' '''+FReconfCells.FReconfCells[Tokens[3]].FConfigChain.FCfgDataOut+'''');
        Delete(Tokens[4],1,1);
        if FReconfCells.FReconfCells[Tokens[3]].FConfigChain.FCfgDataOut <> Tokens[4] then
          raise Exception.Create('Config clock input of cell type '+Tokens[3]+' is '''+FReconfCells.FReconfCells[Tokens[4]].FConfigChain.FCfgDataOut+''' and not '''+Tokens[4]+'''');
      End
    else if ReconfSignals.IndexOf(Tokens[1]) >= 0 then
      Begin
        ReconfSignal := ReconfSignals[Tokens[1]];
        if ReconfSignal.FSigConn is TSigConnDyn then
          Begin
            // dynamic signal, going to MUX tree
            // output Outputs_o_5 1 CellOutOutputs_o_5 PORT
            SigConn := ReconfSignal.FSigConn as TSigConnDyn;
            if SigConn.FConnType.FWidth <> StrToInt(Tokens[2]) then
              raise Exception.Create('Invalid connection type '+Tokens[2]+' of port of cell for reconf.signal '+Tokens[1]+' (should be '+SigConn.FConnType.FName+')');
            if Tokens[3] <> 'Cell'+CPortDirectionPrefix[ReconfSignal.GetDirection]+PortName then
              raise Exception.Create('Invalid cell type '+Tokens[3]+' for port '+PortName);
            if Tokens[4] <> 'PORT' then
              raise Exception.Create('Invalid port name '+Tokens[4]);
            if not SigConn.FConnTypeOptions.FArray then
              Begin
                if Index >= 0 then
                  raise Exception.Create('Reconf.signal '+Tokens[1]+' is not an array signal, but a postfix index was found');
                FReadSigPorts.Add(Tokens[1],ReconfSignal);
              End
            else
              Begin
                if Index < 0 then
                  raise Exception.Create('Reconf.signal '+Tokens[1]+' is an array signal, but no postfix index was found');
                if FReadSigPortArr.IndexOf(Tokens[1]) < 0 then
                  Begin
                    FReadSigPortArr.Add(Tokens[1],0);
                    FReadSigPorts.Add(Tokens[1],ReconfSignal);
                  End;
                FReadSigPortArr[Tokens[1]] := FReadSigPortArr[Tokens[1]] or (1 shl Index);
              End;
          End
        else if ReconfSignal.FSigConn is TSigConnDirect then
          Begin
            // TODO: array signals
            if StrToInt(Tokens[2]) <> ReconfSignal.GetSignal.FType.GetWidthInt then
              raise Exception.Create('Invalid bit-width of direct reconf.signal '+Tokens[1]);
            if Length(Tokens) and $01 <> $01 then
              raise Exception.Create('Invalid parameters for input|output <name> [*]<bit-width> [ <celltype> .<signal> .. ] syntax');
            Index := 3;
            While Index < Length(Tokens) do
              Begin
                if FReconfCells.FReconfCells.IndexOf(Tokens[Index]) < 0 then
                  raise Exception.Create('Unknown cell '+Tokens[Index]+' for direct reconf.signal '+Tokens[1]);
                Cell := FReconfCells.FReconfCells[Tokens[Index]];
                if Tokens[Index+1][1] <> '.' then
                  raise Exception.Create('Direct reconf.signal '+Tokens[1]+' must be used with .<signal> syntax');
                Delete(Tokens[Index+1],1,1);  // cut of leading dot
                if Cell.FDirectPorts.IndexOf(Tokens[Index+1]) < 0 then
                  raise Exception.Create('Unknown port '+Tokens[Index+1]+' of cell '+Tokens[Index]+' for direct reconf.signal '+Tokens[1]);
                FReadDirectSigs.Add(Tokens[Index]+'.'+Tokens[Index+1],ReconfSignal);
                Inc(Index,2);
              End;
          End
        else
          raise Exception.Create('Invalid signal connection/usage '+ReconfSignal.FSigConn.ClassName+' of input/output '+Tokens[1]);
      End
    else
      raise Exception.Create('Unknown port '+Tokens[1])
  End;

  Procedure ReadSetCellCount(Tokens:TDynStringArray);
  Var Index        : Integer;
      ReconfSignal : TReconfSignalBase;
  Begin
    // setcellcount <celltype> <num>
    if (Pos('CellIn',Tokens[1]) = 1) or (Pos('CellOut',Tokens[1]) = 1) then
      Begin
        if      Pos('CellIn', Tokens[1]) = 1 then Delete(Tokens[1],1,6)
        else if Pos('CellOut',Tokens[1]) = 1 then Delete(Tokens[1],1,7)
        else
          raise Exception.Create('Strange cell name '+Tokens[1]);
        Index := PostfixToInt(Tokens[1]);              // this is not clean, because it doesn't work with signals which really end with a number
        if ReconfSignals.IndexOf(Tokens[1]) < 0 then
          raise Exception.Create('setcellcount for unknown signal '+Tokens[1]);
        ReconfSignal := ReconfSignals[Tokens[1]];
        if not (ReconfSignal.FSigConn is TSigConnDyn) then
          raise Exception.Create('setcellcount for cell for signal '+Tokens[1]+' which is not dynamic');
        if not (ReconfSignal.FSigConn as TSigConnDyn).FConnTypeOptions.FArray then
          Begin
            if Index >= 0 then
              raise Exception.Create('Signal '+ReconfSignal.GetPortName+' is not an array signal but got an index postfix');
            FReadPortCount.Add(Tokens[1],StrToInt(Tokens[2]));
          End
        else
          Begin
            if Index < 0 then
              raise Exception.Create('Signal '+ReconfSignal.GetPortName+' is an array signal but got no index postfix');
            FReadPortCount.Add(Tokens[1]+'['+IntToStr(Index)+']',StrToInt(Tokens[2]));
          End;
      End
    else if Pos('CellParam',Tokens[1]) = 1 then
      Begin
        // setcellcount CellParamIn_Bit 0
        if      Pos('CellParamIn', Tokens[1]) = 1 then Begin Delete(Tokens[1],1,12); FReadParamInCount. Add(Tokens[1],StrToInt(Tokens[2])); End
        else if Pos('CellParamOut',Tokens[1]) = 1 then Begin Delete(Tokens[1],1,13); FReadParamOutCount.Add(Tokens[1],StrToInt(Tokens[2])); End
        else
          raise Exception.Create('Strange cell name '+Tokens[1]);
      End
    else if Pos('CONST_',Tokens[1]) = 1 then
      Begin
        Delete(Tokens[1],1,6);
        if FReconfSignals.FConnTypes.IndexOf(Tokens[1]) < 0 then
          raise Exception.Create('Unknown connection type '+Tokens[1]+' for constant cell');
        FReadConstCount.Add(Tokens[1],StrToInt(Tokens[2]));
      End
    else if FReconfCells.FReconfCells.IndexOf(Tokens[1]) >= 0 then
      Begin
        FReadCellCount.Add(Tokens[1],StrToInt(Tokens[2]));
      End
    else
      raise Exception.Create('Unknown cell type '+Tokens[1]);
  End;

  Function ReadNetlist(Tokens:TDynStringArray) : TReconfApp;
  Begin
    if FReconfApps.FReconfApps.IndexOf(Tokens[1]) < 0 then
      raise Exception.Create('Unknown application for netlist '+Tokens[1]);
    Result := FReconfApps.FReconfApps[Tokens[1]];
    WriteLn('Reading information on application ',Tokens[1]);
  End;

  Procedure ReadNode(Tokens:TDynStringArray;App:TReconfApp);
  Begin
    if not assigned(App) then
      raise Exception.Create('No application netlist selected');
    // TODO
  End;

  Procedure ReadMapNode(Tokens:TDynStringArray;App:TReconfApp);
  Var Index        : Integer;
      ReconfSignal : TReconfSignalBase;
      Dir          : TPortDirection;
      ParamPort    : TParamPort;
  Begin
    if not assigned(App) then
      raise Exception.Create('No application netlist selected');
    if App.FYosysNetlist.FInstances.IndexOf(Tokens[1]) >= 0 then
      Begin
        FReadNodeMap.Add(App.FName+'.'+Tokens[1], StrToInt(Tokens[2]));
      End
    else if (Pos('InstIn',Tokens[1]) = 1) or (Pos('InstOut',Tokens[1]) = 1) then
      Begin
        if      Pos('InstIn', Tokens[1]) = 1 then Delete(Tokens[1],1,6)
        else if Pos('InstOut',Tokens[1]) = 1 then Delete(Tokens[1],1,7)
        else
          raise Exception.Create('Strange instance name '+Tokens[1]);
        Index := PostfixToInt(Tokens[1]);              // this is not clean, because it doesn't work with signals which really end with a number
        if ReconfSignals.IndexOf(Tokens[1]) < 0 then
          raise Exception.Create('Invalid dynamic reconf.signal '+Tokens[1]);
        ReconfSignal := ReconfSignals[Tokens[1]];
        if not (ReconfSignal.FSigConn is TSigConnDyn) then
          raise Exception.Create('mapnode for cell for signal '+Tokens[1]+' which is not dynamic');
        if not (ReconfSignal.FSigConn as TSigConnDyn).FConnTypeOptions.FArray then
          Begin
            if Index >= 0 then
              raise Exception.Create('Signal '+ReconfSignal.GetPortName+' is not an array signal but got an index postfix');
          End
        else
          Begin
            if Index < 0 then
              raise Exception.Create('Signal '+ReconfSignal.GetPortName+' is an array signal but got no index postfix');
          End;
        if Tokens[2] <> '0' then
          raise Exception.Create('Signal '+ReconfSignal.GetPortName+' cellnum must be 0');
      End
    else if (Pos('InstParamIn',Tokens[1]) = 1) or (Pos('InstParamOut',Tokens[1]) = 1) then
      Begin
        // mapnode InstParamIn_Threshold_i 2
        if      Pos('InstParamIn', Tokens[1]) = 1 then Begin Delete(Tokens[1],1,12); Dir := dirIn;  End
        else if Pos('InstParamOut',Tokens[1]) = 1 then Begin Delete(Tokens[1],1,13); Dir := dirOut; End
        else
          raise Exception.Create('Strange instance name '+Tokens[1]);
        if App.FParamPorts.IndexOf(Tokens[1]) < 0 then
          raise Exception.Create('No parameter '+Tokens[1]+' defined for application '+App.FName);
        ParamPort := App.FParamPorts[Tokens[1]];
        if Dir <> ParamPort.FDirection then
          raise Exception.Create('Wrong direction for parameter '+Tokens[1]+' node instance for application '+App.FName);
        if ParamPort.FMapping >= 0 then
          raise Exception.Create('Parameter '+Tokens[1]+' mapping already set');
        ParamPort.FMapping := StrToInt(Tokens[2]);
      End
    else if Pos('InstCONST_',Tokens[1]) = 1 then
      Begin
        // ignore
      End
    else if Pos('unused-',Tokens[1]) = 1 then
      Begin
        // ignore information on unused nodes
      End
    else
      raise Exception.Create('Unknown cell instance '+Tokens[1]);
  End;

  // this is not a real InterSynth command but we generated by gen-cellmapping.tcl
  Procedure ReadMapInstance(Tokens:TDynStringArray);
  Begin
    // mapinstance Counter 0 cell_128
    FReadNodeInstance.Add(Tokens[1]+'.'+Tokens[2],Tokens[3]);
  End;

Var T      : Text;
    Line   : Integer;
    St     : String;
    Tokens : TDynStringArray;
    App    : TReconfApp;
Begin
  Assign(T,AFilename);
  try
    Reset(T);
  except
    on E : Exception do
      Begin
        E.Message := E.Message + ' (' + AFilename + ')';
        raise;
      End;
  End;
  try
    try
      // first we build a copy of FReconfSignals.FReconfSignals, but with the key from GetPortName instead of FName
      ReconfSignals := TReconfSignalList.Create;
      For Line := 0 to FReconfSignals.FReconfSignals.Count-1 do    // misuse variable 'Line'
        With FReconfSignals.FReconfSignals.Data[Line] do
          ReconfSignals.Add(GetPortName,FReconfSignals.FReconfSignals.Data[Line]);
      // then parse the file
      App := Nil;
      Line := 1;
      While not EOF(T) do
        Begin
          ReadLn(T,St);
          if Pos('#',St) > 0 then St := Copy(St,1,Pos('#',St)-1);
          if St <= '' then Continue;
          Tokens := Split(St);
          Case Tokens[0] of
            'verbose'        : ; // ignore
            'load'           : WriteLn('TODO: load');
            'stats'          : ; // ignore
            'conntype'       : ReadConntype(Tokens);
            'celltype'       : ReadCelltype(Tokens);
            'input'          : ReadPort(Tokens);
            'output'         : ReadPort(Tokens);
            'topology'       : WriteLn('TODO: topology');
            'setcellcount'   : ReadSetCellCount(Tokens);
            'setswitchsizes' : ;  // ignore
            'mapcell'        : ;  // ignore
            'headroom'       : ;  // ignore
            'netlist'        : App := ReadNetlist(Tokens);
            'node'           : ReadNode(Tokens,App);
            'mapnode'        : ReadMapNode(Tokens,App);
            'shuffle'        : WriteLn('TODO: shuffle');
            'presilicon'     : WriteLn('TODO: presilicon');
            'postsilicon'    : WriteLn('TODO: postsilicon');
            'write_data'     : WriteLn('TODO: write_data');
            'write_verilog'  : WriteLn('TODO: write_verilog');
            'write_bitdata'  : WriteLn('TODO: write_bitdata');
            'write_tikz'     : WriteLn('TODO: write_tikz');
            'write_graphviz' : WriteLn('TODO: write_graphviz');
            'write_html'     : WriteLn('TODO: write_html');
            'write_tcl'      : WriteLn('TODO: write_tcl');
            'mapinstance'    : ReadMapInstance(Tokens);    // this is not a real InterSynth command but we generated by gen-cellmapping.tcl
          else
            raise Exception.Create('Unknown command '''+Tokens[0]+'''');
          End;
          Inc(Line);
        End;
    Except
      on E : Exception do
        Begin
          E.Message := AFilename + ':' + IntToStr(Line) + ': ' + E.Message;
          raise;
        End;
    End;
  Finally
    ReconfSignals.Free;
    Close(T);
  End;
  // What we still do not now, even after reading all data files, is the number
  // of config bits, i.e. the width of the InterSynth-Module "bitdata" input.
  // This information is only available in the config files.
End;

Procedure TInterSynthHandler.ReadConfig(AFilename:String);

  Procedure ReadBitdataSize(Tokens:TDynStringArray);
  Begin
    // bitdata size 746
    if FReadBitdataSize < 0 then
      FReadBitdataSize := StrToInt(Tokens[2])
    else
      if FReadBitdataSize <> StrToInt(Tokens[2]) then
        raise Exception.Create('Bitdata size '+Tokens[2]+' differ from previous value '+IntToStr(FReadBitdataSize));
  End;

  Procedure ReadBitdataBits(Tokens:TDynStringArray;App:TReconfApp);
  Var MSB,LSB,I : Integer;
  Begin
    // bitdata bits   489:426  1011110000000000001111000000000000100000000000100011011011011011
    if not assigned(App) then
      raise Exception.Create('No application netlist selected');
    if FReadBitdataSize < 0 then
      raise Exception.Create('Can''t use bitdata bits before there was a bitdata size');
    I := Pos(':',Tokens[2]);
    if I = 0 then
      raise Exception.Create('Invalid syntax for MSB:LSB field '''+Tokens[2]+'''');
    MSB := StrToInt(Copy(Tokens[2],1,I-1));
    LSB := StrToInt(Copy(Tokens[2],I+1,Length(Tokens[2])));
    if Length(Tokens[3]) <> MSB-LSB+1 then
      raise Exception.Create('Length of bit data '+IntToStr(Length(Tokens[3]))+' doesn''t match MSB:LSB width '+IntToStr(MSB-LSB+1));
    For I := MSB downto LSB do
      App.FInterSynthBitdata[I+1] := Tokens[3][MSB-I+1];
  End;

  Procedure ReadBitdata(Tokens:TDynStringArray;App:TReconfApp);
  Begin
    Case Tokens[1] of
      'size' : ReadBitdataSize(Tokens);
      'bits' : ReadBitdataBits(Tokens,App);
    else
      raise Exception.Create('Unknown subcommand '''+Tokens[1]+''' to ''bitdata''');
    End;
  End;

  Function ReadNetlist(Tokens:TDynStringArray) : TReconfApp;
  Begin
    // netlist ADT7310
    if FReconfApps.FReconfApps.IndexOf(Tokens[1]) < 0 then
      raise Exception.Create('Unknown application for netlist '+Tokens[1]);
    if FReadBitdataSize < 0 then
      raise Exception.Create('Can''t use bitdata bits before there was a bitdata size');
    Result := FReconfApps.FReconfApps[Tokens[1]];
    WriteLn('Reading bitdata for application ',Tokens[1]);
    // don't care if we already have old bitdata for the application
    Result.FInterSynthBitdata := StringOfChar('x',FReadBitdataSize);
  End;

  Procedure CheckApp(App:TReconfApp);
  Begin
    if assigned(App) then
      Begin
        // we assume, that if we read "netlist" in a config data, we will get the
        // full config bitstream for this application --> check if it is ok
        if Pos('x',App.FInterSynthBitdata) <> 0 then
          raise Exception.Create('Incomplete bitstream for application '+App.FName+': '+App.FInterSynthBitdata);
      End;
  End;

Var T      : Text;
    Line   : Integer;
    St     : String;
    Tokens : TDynStringArray;
    App    : TReconfApp;
Begin
  Assign(T,AFilename);
  Reset(T);
  try
    try
      App := Nil;
      Line := 1;
      While not EOF(T) do
        Begin
          ReadLn(T,St);
          if Pos('#',St) > 0 then St := Copy(St,1,Pos('#',St)-1);
          if St <= '' then Continue;
          Tokens := Split(St);
          Case Tokens[0] of
            'bitdata'        : ReadBitdata(Tokens,App);
            'netlist'        : Begin CheckApp(App);  App := ReadNetlist(Tokens); End;
            'stamp'          : ;  // ignore
            'cellcfg'        : ;  // ignore
          else
            raise Exception.Create('Unknown command '''+Tokens[0]+'''');
          End;
          Inc(Line);
        End;
    Except
      on E : Exception do
        Begin
          E.Message := AFilename + ':' + IntToStr(Line) + ': ' + E.Message;
          raise;
        End;
    End;
  Finally
    Close(T);
  End;
  CheckApp(App);
End;

Function TInterSynthHandler.CheckMissing : Integer;

  Procedure MyError(St:String);
  Begin
    WriteLn(St);
    Inc(Result);
  End;

Var I,J     : Integer;
    SigConn : TSigConnDyn;
    Cell    : TReconfCell;
    St      : String;
Begin
  Result := 0;
  Prepare;
  For I := 0 to FReconfSignals.FConnTypes.Count-1 do
    Begin
      if FReadConnTypes.IndexOf(FReconfSignals.FConnTypes.Keys[I]) < 0 then
        MyError('Missing connection type '+FReconfSignals.FConnTypes.Keys[I]);
      if (FParamInCount[FReconfSignals.FConnTypes.Keys[I]] > 0) and (FReadParamInCells.IndexOf(FReconfSignals.FConnTypes.Keys[I]) < 0) then
        MyError('Missing param in cell for connection type '+FReconfSignals.FConnTypes.Keys[I]);
      if (FParamInCount[FReconfSignals.FConnTypes.Keys[I]] > 0) and (FReadParamInPorts.IndexOf(FReconfSignals.FConnTypes.Keys[I]) < 0) then
        MyError('Missing input definiton for parameters for connection type '+FReconfSignals.FConnTypes.Keys[I]);
      if (FParamOutCount[FReconfSignals.FConnTypes.Keys[I]] > 0) and (FReadParamOutCells.IndexOf(FReconfSignals.FConnTypes.Keys[I]) < 0) then
        MyError('Missing param out cell for connection type '+FReconfSignals.FConnTypes.Keys[I]);
      if (FParamOutCount[FReconfSignals.FConnTypes.Keys[I]] > 0) and (FReadParamOutPorts.IndexOf(FReconfSignals.FConnTypes.Keys[I]) < 0) then
        MyError('Missing output definiton for parameters for connection type '+FReconfSignals.FConnTypes.Keys[I]);
      if FReadConstCells.IndexOf(FReconfSignals.FConnTypes.Keys[I]) < 0 then
        MyError('Missing constant value cell for connection type '+FReconfSignals.FConnTypes.Keys[I]);
      if FReadConstCells.IndexOf(FReconfSignals.FConnTypes.Keys[I]) < 0 then
        MyError('Missing setcellcount for constant value cell for connection type '+FReconfSignals.FConnTypes.Keys[I]);
      if (FParamInCount[FReconfSignals.FConnTypes.Keys[I]] > 0) and (FReadParamInCount.IndexOf(FReconfSignals.FConnTypes.Keys[I]) < 0) then
        MyError('Missing setcellcount for param in cell for connection type '+FReconfSignals.FConnTypes.Keys[I]);
      if (FParamOutCount[FReconfSignals.FConnTypes.Keys[I]] > 0) and (FReadParamOutCount.IndexOf(FReconfSignals.FConnTypes.Keys[I]) < 0) then
        MyError('Missing setcellcount for param out cell for connection type '+FReconfSignals.FConnTypes.Keys[I]);
    End;
  For I := 0 to FReconfSignals.FReconfSignals.Count-1 do
    With FReconfSignals.FReconfSignals.Data[I] do
      if FSigConn is TSigConnDyn then
        Begin
          if FReadSigCells.IndexOf(GetPortName) < 0 then
            MyError('Missing cell type for reconf.signal '+GetPortName);
          if FReadSigPorts.IndexOf(GetPortName) < 0 then
            MyError('Missing input/output declaration for reconf.signal '+GetPortName);
          SigConn := FReconfSignals.FReconfSignals.Data[I].FSigConn as TSigConnDyn;
          if not SigConn.FConnTypeOptions.FArray then
            Begin
              if FReadPortCount.IndexOf(GetPortName) < 0 then
                MyError('Missing setcellcount for reconf.signal '+GetPortName)
              else
                if FReadPortCount[GetPortName] <> 1 then
                  MyError('Exactly 1 cell for reconf.signal '+GetPortName+' required, but '+IntToStr(FReadPortCount[GetPortName])+' used');
            End
          else
            Begin
              J := ((1 shl (FReconfSignals.FReconfSignals.Data[I].GetSignal.FType.GetWidthInt div SigConn.FConnType.FWidth))-1);
              if (FReadSigCellArr.IndexOf(GetPortName) >= 0) and (FReadSigCellArr[GetPortName] <> J) then
                MyError('Missing cell type for reconf.signal '+GetPortName+' chunks: $'+
                  IntToHex(FReadSigCellArr[GetPortName],8)+' instead of '+IntToHex(J,8));
              if (FReadSigPortArr.IndexOf(GetPortName) >= 0) and (FReadSigPortArr[GetPortName] <> J) then
                MyError('Missing input/output declarations for reconf.signal '+GetPortName+' chunks: $'+
                  IntToHex(FReadSigPortArr[GetPortName],8)+' instead of '+IntToHex(J,8));
              For J := 0 to FReconfSignals.FReconfSignals.Data[I].GetSignal.FType.GetWidthInt-1 do
                Begin
                  St := GetPortName+'['+IntToStr(J)+']';
                  if FReadPortCount.IndexOf(St) < 0 then
                    MyError('Missing setcellcount for array reconf.signal '+St)
                  else
                    if FReadPortCount[St] <> 1 then
                      MyError('Exactly 1 cell for array reconf.signal '+St+' required, but '+IntToStr(FReadPortCount[St])+' instantiated');
                End;
            End;
        End;
  For I := 0 to FReconfCells.FReconfCells.Count-1 do
    Begin
      Cell := FReconfCells.FReconfCells.Data[I];
      if FReadCells.IndexOf(Cell.FName) < 0 then
        MyError('Missing cell type '+Cell.FName);
      if FReadCellCount.IndexOf(Cell.FName) < 0 then
        MyError('Missing cell count for cell type '+Cell.FName);
      For J := 0 to Cell.FDynamicPorts.Count-1 do
        Begin
          if FReadCellPorts.IndexOf(Cell.FName+'.'+Cell.FDynamicPorts.Keys[J]) < 0 then
            MyError('Missing connection to dynamic port '+Cell.FDynamicPorts.Keys[J]+' of cell '+Cell.FName);
        End;
      For J := 0 to Cell.FDirectPorts.Count-1 do
        Begin
          if FReadDirectSigs.IndexOf(Cell.FName+'.'+Cell.FDirectPorts.Keys[J]) < 0 then
            MyError('Missing connection to direct port '+Cell.FDirectPorts.Keys[J]+' of cell '+Cell.FName);
        End;
      For J := 0 to Cell.FConfigPorts.Count-1 do
        Begin
          if FReadCfgPorts.IndexOf(Cell.FName+'.'+Cell.FConfigPorts.Keys[J]) < 0 then
            MyError('Missing config port '+Cell.FConfigPorts.Keys[J]+' of cell '+Cell.FName);
        End;
    End;
  For I := 0 to FReconfApps.FReconfApps.Count-1 do
    Begin
      For J := 0 to FReconfApps.FReconfApps.Data[I].FParamPorts.Count-1 do
        if FReconfApps.FReconfApps.Data[I].FParamPorts.Data[J].FMapping < 0 then
          MyError('Missing mapping for parameter '+FReconfApps.FReconfApps.Data[I].FParamPorts.Data[J].FName+' of application '+FReconfApps.FReconfApps.Data[I].FName);
    End;
  // TODO: check that setcellcount of TR-FSMs is exactly 1
End;

Procedure TInterSynthHandler.ShowInfo(Cols:TStringIntMap;Table:TTable;TextT:TTextTable);
Var I   : Integer;
    Row : Integer;
Begin
  // print beautiful statistics and stuff

  // table with all cells and usage count of each app
  // we already got a table with all cells and usage counts and here we add one
  // more row to show the final InterSynth counts
  Row := Table.Rows;
  Table.SetVal(Row,0,'InterSynth');
  TextT.Update;
  TextT.SetRowSep(Row,'-');
  For I := 0 to Cols.Count-1 do
    Begin
      if Cols.Keys[I][1] = '$' then continue;  // ignore internal cells starting with '$', e.g. '$fsm'
      if FReadCellCount.IndexOf(Cols.Keys[I]) < 0 then
        raise Exception.Create('Didn''t read cell type '+Cols.Keys[I]);
      Table.SetVal(Row, Cols.Data[I], FReadCellCount[Cols.Keys[I]]);
      TextT.SetJustify(Row,Cols.Data[I],jjRight);
    End;
  // warn about cells from InterSynth we don't yet know
  For I := 0 to FReadCellCount.Count-1 do
    Begin
      if Cols.IndexOf(FReadCellCount.Keys[I]) < 0 then
        WriteLn('Warning: InterSynth celltype ',FReadCellCount.Keys[I],' (',FReadCellCount.Data[I],' instances) not yet in table!');
      // better would be to append a new column to the table but we don't have
      // the "NextCol" function here (see TFlowApp.CreateAppUsage) :-(
    End;
  WriteLn(TextT.GetTable);

  // tree info
  // TODO
End;

Function TInterSynthHandler.GetCellInstance(ACell:String;ANum:Integer):String;
Begin
  Result := FReadNodeInstance[ACell+'.'+IntToStr(ANum)];
End;

Function TInterSynthHandler.GetNodeMap(AApp,AInstance:String):Integer;
Begin
  Result := FReadNodeMap[AApp+'.'+AInstance];
End;

Function TInterSynthHandler.GenerateModule(AName:String):TModule;
Var I : Integer;
Begin
  Prepare;
  if FReadBitdataSize <= 0 then
    raise Exception.Create('No information on bitdata width available');

  Result := TModule.Create(AName);
  // reset
  Result.AddPort(TPort.Create('Reset_n_i',Netlist.dirIn,TypeBit),0);
  // clock
  Result.AddPort(TPort.Create('Clk_i',Netlist.dirIn,TypeBit),1);
  // bitdata input
  Result.AddPort(TPort.Create('bitdata',Netlist.dirIn,TType.Create('std_logic_vector',dirDown,FReadBitdataSize-1,0)),2);
  // reconfig signals
  FReconfSignals.Foreach(Nil,Nil,@AddReconfSignal,Result);
  // ParamIn/Out ports
  For I := 0 to FReconfSignals.FConnTypes.Count-1 do
    With FReconfSignals.FConnTypes.Data[I] do
      Begin
        // ParamIn_Bit_i, ParamOut_Bit_o, ...
        if FParamInCount[FName] > 0 then
          Result.AddPort(TPort.Create('ParamIn_'+ FName+'_i',Netlist.dirIn, TType.Create('std_logic_vector',dirDown,FWidth*FReadParamInCount[FName]-1,0)));
        if FParamOutCount[FName] > 0 then
          Result.AddPort(TPort.Create('ParamOut_'+FName+'_o',Netlist.dirOut,TType.Create('std_logic_vector',dirDown,FWidth*FReadParamOutCount[FName]-1,0)));
      End;
  // Config chains
  if FReconfCells.HaveConfigChains then
    Begin
      Result.AddPort(TPort.Create('CfgMode_i',Netlist.dirIn,TypeBit));
      For I := 0 to FReconfCells.FReconfCells.Count-1 do
        With FReconfCells.FReconfCells.Data[I] do
          Begin
            if not assigned(FConfigChain) then continue;
            if FReadCellCount[FName] = 1 then
              Result.AddPort(TPort.Create('CfgClk_'+FName+'_i',Netlist.dirIn,TypeBit))
            else
              Result.AddPort(TPort.Create('CfgClk_'+FName+'_i',Netlist.dirIn,TType.Create('std_logic_vector',dirDown,FReadCellCount[FName]-1,0)))
          End;
      For I := 0 to FReconfCells.FReconfCells.Count-1 do
        With FReconfCells.FReconfCells.Data[I] do
          Begin
            if not assigned(FConfigChain) then continue;
            if FReadCellCount[FName] = 1 then
              Result.AddPort(TPort.Create('CfgShift_'+FName+'_i',Netlist.dirIn,TypeBit))
            else
              Result.AddPort(TPort.Create('CfgShift_'+FName+'_i',Netlist.dirIn,TType.Create('std_logic_vector',dirDown,FReadCellCount[FName]-1,0)))
          End;
      Result.AddPort(TPort.Create('CfgDataIn_i',Netlist.dirIn,TypeBit));
      For I := 0 to FReconfCells.FReconfCells.Count-1 do
        With FReconfCells.FReconfCells.Data[I] do
          Begin
            if not assigned(FConfigChain) then continue;
            if FReadCellCount[FName] = 1 then
              Result.AddPort(TPort.Create('CfgDataOut_'+FName+'_o',Netlist.dirOut,TypeBit))
            else
              Result.AddPort(TPort.Create('CfgDataOut_'+FName+'_o',Netlist.dirOut,TType.Create('std_logic_vector',dirDown,FReadCellCount[FName]-1,0)))
          End;
    End;
End;

Procedure TInterSynthHandler.SetupConfigRegisters;
Var I,J : Integer;
    Name : String;
Begin
  if assigned(FConfigRegisters) then
    Exit;   // TODO: or should we raise an exception?

  FConfigRegisters := TConfigRegisters.Create;
  // for bitdata
  FConfigRegisters.Add('bitdata',TExternalConfigRegister.Create('bitdata',FReadBitdataSize,0{TODO:Default}));
  // for internal config chains
  For I := 0 to FReconfCells.FReconfCells.Count-1 do
    With FReconfCells.FReconfCells.Data[I] do
      if assigned(FConfigChain) then
        For J := 0 to FReadCellCount[FName]-1 do
          Begin
            Name := 'CfgReg_'+FName+'_'+IntToStr(J);
            FConfigRegisters.Add(Name,TInternalConfigRegister.Create(Name,FConfigChain.FChainLen,0{TODO:Default}));
          End;
End;

Function TInterSynthHandler.GenerateConfigRegisterControlGroup(AConfigRegisters:TAddressedConfigRegisters;AISInstance:TInstance) : TConfigRegisterControlGroup;
Var I,J : Integer;
    Name : String;
Begin
  if not assigned(FConfigRegisters) then
    raise Exception.Create('No config register yet, use SetupConfigRegisters');

  Result := TConfigRegisterControlGroup.Create(AISInstance,'CfgMode_i','CfgDataIn_i');  // same port names as in GenerateModule
  For I := 0 to FReconfCells.FReconfCells.Count-1 do
    With FReconfCells.FReconfCells.Data[I] do
      if assigned(FConfigChain) then
        For J := 0 to FReadCellCount[FName]-1 do
          Begin
            Name := 'CfgReg_'+FName+'_'+IntToStr(J);            // same names as in GenerateModule
            Result.Add(TInternalConfigRegisterData.Create(AConfigRegisters.FConfigRegisters[Name]),'CfgClk_'+FName+'_i','CfgShift_'+FName+'_i','CfgDataOut_'+FName+'_o');
          End;
End;

Procedure TInterSynthHandler.SetupParameters;
Var I,J  : Integer;
    Name : String;
Begin
  FParameters := TParameters.Create;
  // parameter name: "Param<In|Out>_<conntype>_<index>"
  // parameter port: "Param<In|Out>_<conntype>_<i|o>"
  For I := 0 to FReconfSignals.FConnTypes.Count-1 do
    With FReconfSignals.FConnTypes.Data[I] do
      Begin
        if FReadParamInCount.IndexOf(FName) >= 0 then
          For J := 0 to FReadParamInCount[FName]-1 do
            Begin
              // reconf.signal input into InterSynth module --> param read
              Name := 'ParamIn_'+FName+'_'+IntToStr(J);
              FParameters.Add(Name,TParameter.Create(Name,pdWrite,FType,false,0{TODO:Default}));
              // parameter name must conform to VHDL/Verilog signal naming rules
              // TODO: default value should be given by the application! see TReconfApp.RegisterParameters
            End;
        if FReadParamOutCount.IndexOf(FName) >= 0 then
          For J := 0 to FReadParamOutCount[FName]-1 do
            Begin
              Name := 'ParamOut_'+FName+'_'+IntToStr(J);
              // reconf.signal output from InterSynth module --> param write
              FParameters.Add(Name,TParameter.Create(Name,pdRead,FType,false,0{TODO:Default}));
            End;
      End;
(* Note: Contrary to the (Ex.)App. parameters and the parameters for the
 * reconf.signals with connection/usage "Param", which are actual instances of
 * parameters, these InterSynth module parameters are rather "variables" or
 * "placeholders".
 * Although, here we just create paremeters with indices and later when the
 * applications and their mappings are considered, this "gap" is filled.
 *)
End;

(**
 * Returns: >0 on errors
 *)
Function TInterSynthHandler.CheckUsage(AApp:TReconfApp;AIgnore:TFPGStringList):Integer;
  Procedure MyError(St:String);
  Begin
    WriteLn(St);
    Inc(Result);
  End;

Var Usage : TCellUsage;
    I     : Integer;
    Cell  : String;
Begin
  Result := 0;
  Usage := AApp.GetUsage;
  For I := 0 to Usage.Count-1 do
    Begin
      if AIgnore.IndexOf(Usage.Keys[I]) >= 0 then
        Continue;
      if FReadCellCount.IndexOf(Usage.Keys[I]) < 0 then
        MyError('Cell '+Usage.Keys[I]+' not in the InterSynth module inventory')
      else if FReadCellCount[Usage.Keys[I]] < Usage.Data[I] then
        MyError('Cell '+Usage.Keys[I]+' has '+IntToStr(FReadCellCount[Usage.Keys[I]])+' instances in the InterSynth module, but the app. '+AApp.FName+' requires '+IntToStr(Usage.Data[I]));
    End;
  Usage.Free;
End;

Procedure TInterSynthHandler.AddReconfSignal(ASignal:TReconfSignalBase;Const AData:Pointer);
Var Module     : TModule;
    PortName   : String;
    TheType    : TType;
    SigConn    : TSigConnDyn;
    I          : Integer;
Begin
  Module := TModule(AData);
  PortName := ASignal.GetPortName;
  if ASignal.FSigConn is TSigConnDyn then
    TheType := (ASignal.FSigConn as TSigConnDyn).FConnType.FType
  else
    TheType  := ASignal.GetSignal.FType;
  // Don't add Reset or Clock port once again (was already added in Create).
  // TODO: The criteria whether we are dealing with one of these two signals
  // should be better (via FReconfInstance.FConnections) than just a comparison
  // of names!
  if (ASignal.FName = 'Reset_n_i') or (ASignal.FName = 'Clk_i') then
    Exit;
  // amend netlist
  if ASignal.FSigConn is TSigConnDyn then
    Begin
      SigConn := ASignal.FSigConn as TSigConnDyn;
      if not SigConn.FConnTypeOptions.FArray then
        Begin
          Module.AddPort(TPort.Create(PortName,ASignal.GetDirection,TheType){,1000 + I});
        End
      else
        Begin
          // array signal: split in single-bit signals for InterSynth module
          For I := 0 to (ASignal.GetSignal.FType.GetWidthInt div SigConn.FConnType.FWidth)-1 do
            Module.AddPort(TPort.Create(PortName+'_'+IntToStr(I),ASignal.GetDirection,TheType){,1000 + I});
        End;
    End
  else if ASignal.FSigConn is TSigConnDirect then
    Begin
      Module.AddPort(TPort.Create(PortName,ASignal.GetDirection,TheType){,1000 + I});
      // TODO: special handling of direct connection array signals
    End;
  // other usages/connections are not connected to the InterSynth module
End;

Procedure TInterSynthHandler.SetupConstCellTypes;
Var I             : Integer;
    ConstCellType : TConstCellType;
Begin
  For I := 0 to FReconfSignals.FConnTypes.Count-1 do
    Begin
      ConstCellType := TConstCellType.Create(FReconfSignals.FConnTypes.Data[I]);
      FConstCellTypes.Add(FReconfSignals.FConnTypes.Keys[I],ConstCellType);
    End;
End;

End.

