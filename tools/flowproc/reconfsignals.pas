Unit ReconfSignals;

{$mode objfpc}{$H+}
{$modeswitch advancedrecords}

Interface

Uses
  Classes, SysUtils, FGL, Netlist;

Type

  TReconfSignalBase = class;

  { TConnType }

  TConnType = class
    FName  : String;
    FWidth : Integer;
    FType  : TType;
    FTrees : Integer;   // used by InterSynth; 0: unset, -1: full, >0: number of trees
    FCost  : Double;    // used by InterSynth
    Constructor Create(AName:String;AWidth:Integer);
  End;
  TConnTypes = specialize TFPGMap<String,TConnType>;

  { TConnTypeOptions }

  TConnTypeOptions = record
    FArray         : Boolean;
    FPadLeft       : Integer;   // should be a real "Vector" type, but lets stay simple for the while
    FPadLeftWidth  : Integer;   // -1: not specified
    FPadRight      : Integer;   // should be a real "Vector" type, but lets stay simple for the while
    FPadRightWidth : Integer;   // -1: not specified
    Procedure Clear;
    Function ToString:String;
  End;

  PConnTypeData = ^TConnTypeData;
  TConnTypeData = record  // used for callback data parameter
    FConnType        : TConnType;
    FConnTypeOptions : TConnTypeOptions;
  End;

  { TSigConnBase }

  TSigConnBase = class
    FSignal : TReconfSignalBase;
    Function Clone : TSigConnBase; virtual;
    Procedure AssignSignal(ASignal:TReconfSignalBase); virtual;          // also can do checks on the signal
  End;

  { TSigConnConst }

  TSigConnConst = class(TSigConnBase)
    FValue : Integer;   // should be a real "Vector" type, but lets stay simple for the while
    FWidth : Integer;   // -1: not specified
    Function Clone : TSigConnBase; override;
    Procedure AssignSignal(ASignal:TReconfSignalBase); override;
    Function GetValue : TValue;
  End;

  { TSigConnConfig }

  TSigConnConfig = class(TSigConnBase)
    // no data, just use as class type
    Procedure AssignSignal(ASignal:TReconfSignalBase); override;
  End;

  { TSigConnParam }

  TSigConnParam = class(TSigConnBase)
  End;

  { TSigConnDirect }

  TSigConnDirect = class(TSigConnBase)
    // no data, just use as class type
  End;

  { TSigConnDyn }

  TSigConnDyn = class(TSigConnBase)
    FConnType        : TConnType;
    FConnTypeOptions : TConnTypeOptions;
    Function Clone : TSigConnBase; override;
    Procedure SetConnType(AConnType:TConnType;AOptions:TConnTypeOptions);
  End;

  TChunk = class   // chunk of a signal
    FLow  : Integer;
    FHigh : Integer;
  End;
  TChunks = specialize TFPGMap<Integer,TChunk>;  // multiple chunks of a signal

  { TConnectedChunk }

  TConnectedChunk = class(TChunk)   // chunks of a signal, which are connected to a port
    FInstance   : TInstance;
    FPort       : TPort;
    FConnChunk  : TChunk;    // chunk of FPort
    Constructor Create;
    Constructor Create(AChunk:TChunk);
  End;
  TConnectedChunks = specialize TFPGMap<Integer,TConnectedChunk>;

  { TReconfSignalBase }

  TReconfSignalBase = class
    FName : String;
    // Connection/Usage
    FSigConn : TSigConnBase;   // initialized Nil --> unset
    Constructor Create(AName:String);

    Procedure SetSigConn(ASigConn:TSigConnBase);

    Function GetSignal    : TSignal;        virtual; abstract;
    Function GetDirection : TPortDirection; virtual; abstract;    // relative to the ReconfigModule
    const Function GetPortName : String;                          // of the ReconfigModule
  End;
  TReconfSignalList = specialize TFPGMap<String,TReconfSignalBase>;

  { TReconfPort }

  TReconfPort = class(TReconfSignalBase)
    FPort   : TPort;
    FChunks : TChunks;
    Function ToString:String; override;
    Function GetSignal    : TSignal;        override;
    Function GetDirection : TPortDirection; override;
  End;

  { TReconfSignal }

  TReconfSignal = class(TReconfSignalBase)
    FSignal      : TSignal;
    FConnections : TConnectedChunks;
    Function ToString:String; override;
    Function GetSignal    : TSignal;        override;
    Function GetDirection : TPortDirection; override;
    Function FindChunk(AHigh,ALow:Integer):Integer;
  End;

  TReconfSignalBoolFunc = Function(const ASignal:TReconfSignalBase;const AData:Pointer):Boolean of object;
  TReconfSignalProc     = Procedure(ASignal:TReconfSignalBase;const AData:Pointer) of object;

  { TReconfSignals }

  TReconfSignals = class
    FParent        : TModule;
    FReconfSignals : TReconfSignalList;
    FConnTypes     : TConnTypes;
    FConfigRegister: TObject;   // TChunkedConfigRegister, damn circular unit reference
    FParameters    : TObject;   // TParameters, damn circular unit reference
    Function GetCount:Integer;
    Function GetReconfSignal(Key:String):TReconfSignalBase;
  public
    Constructor Create;
    Destructor Destroy; override;
    Procedure AddSignal(ASignal,AName:String);
// TODO:   Procedure AddSignal(AName:String;ALow,AHigh:Integer;ADirecton:TPortDirection);
// TODO:   Procedure AddSignal(AName:String;ALow,AHigh:Integer;ADirecton:TPortDirection;ACell,APort:String;APortLow,APortHigh:Integer);
    Procedure UseSignalAlias(AName:String);
    Function DelSignals(const AFunc:TReconfSignalBoolFunc; const AData:Pointer) : Integer;
// TODO:   Procedure DelSignal{...};  // several variants
    Procedure ListReconfSignals(const AFunc:TReconfSignalBoolFunc; const AData:Pointer);
    Function Foreach(Const ASelFunc:TReconfSignalBoolFunc;Const ASelData:Pointer;Const ADoFunc:TReconfSignalProc;Const ADoData:Pointer) : Integer;
    Function ReadUnusedSignals(const Filename:String) : Integer;
// TODO:   Procedure ReadReconfSignals(Filename:String);
// TODO:   Procedure WriteReconfSignals(Filename:String);
    Procedure SetSigConn(ASignal:TReconfSignalBase;const AData:Pointer);
    Procedure SetConnType(ASignal:TReconfSignalBase;const AData:Pointer);
    Function CheckSigConn : Integer;
    Function CheckConnType:Integer;
    Procedure SetupConfigAndParam;
    property Parent : TModule read FParent write FParent;
    property ReconfSignal[Key:String] : TReconfSignalBase read GetReconfSignal; default;
    property Count : Integer read GetCount;
  End;

Implementation
Uses StrUtils, ConfigIntf, ParamIntf, Utils;

{ TSigConnBase }

Function TSigConnBase.Clone:TSigConnBase;
Begin
  Result := TSigConnBase(Self.NewInstance);
  Result.FSignal := FSignal;
End;

Procedure TSigConnBase.AssignSignal(ASignal:TReconfSignalBase);
Begin
  FSignal := ASignal;
  // do nothing else, override if special checks are desired
End;

{ TSigConnConst }

Function TSigConnConst.Clone:TSigConnBase;
Begin
  Result := Inherited Clone;
  (Result as TSigConnConst).FValue := FValue;
  (Result as TSigConnConst).FWidth := FWidth;
End;

Procedure TSigConnConst.AssignSignal(ASignal:TReconfSignalBase);
Var Width : Integer;
Begin
  inherited AssignSignal(ASignal);
  // check direction
  if FSignal.GetDirection <> dirOut then
    raise Exception.Create('Reconf.signal '+FSignal.FName+' with direction '+CPortDirectionVHDL[FSignal.GetDirection]+' cannot be set constant. Only outputs can be.');
  // check width
  if FWidth < 0 then
    Begin
      // width is not specified --> just check if the given value fits in the signal
      if (FSignal.GetSignal.FType = TypeBit) and ((FValue < 0) or (FValue > 1)) then
        raise Exception.Create('Value '+IntToStr(FValue)+' = 0x'+IntToHex(FValue,1)+' is too large for the single-bit reconf.signal '+FSignal.FName)
      else if FSignal.GetSignal.FType.FName = 'std_logic_vector' then
        Begin
          if not FSignal.GetSignal.FType.FRange.GetWidth(Width) then
            raise Exception.Create('Can''t set value to a signal, which vector range is given with non-integer values');
          if FValue > ((1 << Width)-1) then
            raise Exception.CreateFmt('Value %d is too large for reconf.signal %s with width %d',[FValue,FSignal.FName,Width]);
        End;
    End
  else if (FSignal.GetSignal.FType = TypeBit) and (FWidth <> 1) then
    raise Exception.CreateFmt('Can''t set value with a width of %d to a single-bit reconf.signal %s',[FWidth,FSignal.FName])
  else if (FSignal.GetSignal.FType.FName = 'std_logic_vector') then
    Begin
      if not FSignal.GetSignal.FType.FRange.GetWidth(Width) then
        raise Exception.Create('Can''t set value to a signal, which vector range is given with non-integer values');
      if FWidth <> Width then
        raise Exception.CreateFmt('Can''t set value with a width of %d to reconf.signal %s with width %d',[FWidth,FSignal.FName,Width]);
    End
  else
    raise Exception.Create('Unknown type '+FSignal.GetSignal.FType.FName+' of reconf.signal '+FSignal.FName);
End;

Function TSigConnConst.GetValue:TValue;
Begin
  if FSignal.GetSignal.FType = TypeBit then
    Result := TValueBit.Create(Chr(Ord('0')+FValue))
  else
    Result := TValueVector.Create(FSignal.GetSignal.FType.GetWidthInt,FValue);
End;

{ TSigConnConfig }

Procedure TSigConnConfig.AssignSignal(ASignal:TReconfSignalBase);
Begin
  inherited AssignSignal(ASignal);
  // check direction
  if FSignal.GetDirection <> dirOut then
    raise Exception.Create('Reconf.signal '+FSignal.FName+' with direction '+CPortDirectionVHDL[FSignal.GetDirection]+' cannot be connected with a config register. Only outputs can be.');
End;

{ TSigConnDyn }

Function TSigConnDyn.Clone:TSigConnBase;
Begin
  Result := Inherited Clone;
  (Result as TSigConnDyn).FConnType        := FConnType;
  (Result as TSigConnDyn).FConnTypeOptions := FConnTypeOptions;
End;

Procedure TSigConnDyn.SetConnType(AConnType:TConnType;AOptions:TConnTypeOptions);
Var W : Integer;
Begin
  FConnType        := AConnType;
  FConnTypeOptions := AOptions;
  if FSignal.GetSignal.FType.GetWidthInt <> FConnType.FWidth then
    Begin
      if FConnTypeOptions.FArray then
        Begin
          if (FSignal.GetSignal.FType.GetWidthInt mod FConnType.FWidth) <> 0 then
            raise Exception.Create('Reconf.signal '+FSignal.FName+' width '+IntToStr(FSignal.GetSignal.FType.GetWidthInt)+' is not an integer multiple of the connection type '+FConnType.FName+' width '+IntToStr(FConnType.FWidth));
        End
      else if (FConnTypeOptions.FPadLeftWidth > 0) or (FConnTypeOptions.FPadRightWidth > 0) then
        Begin
          // Currently, the padding stuff is a bit dirty. Here we check for the
          // FPad*Width fields, which are 0 or positive only, if the padding
          // value is given with a width (e.g. "000000", 4'b1001, but not 0x09).
          W := FSignal.GetSignal.FType.GetWidthInt;
          if FConnTypeOptions.FPadLeftWidth > 0 then
            W := W + FConnTypeOptions.FPadLeftWidth;
          if FConnTypeOptions.FPadRightWidth > 0 then
            W := W + FConnTypeOptions.FPadRightWidth;
          if W <> FConnType.FWidth then
            raise Exception.Create('Reconf.signal '+FSignal.FName+' width '+IntToStr(FSignal.GetSignal.FType.GetWidthInt)+' plus padding does not match connection type '+FConnType.FName+' width '+IntToStr(FConnType.FWidth));
        End
      else
        raise Exception.Create('Reconf.signal '+FSignal.FName+' width '+IntToStr(FSignal.GetSignal.FType.GetWidthInt)+' does not match connection type '+FConnType.FName+' width '+IntToStr(FConnType.FWidth));
    End;
End;

{ TConnType }

Constructor TConnType.Create(AName:String;AWidth:Integer);
Begin
  inherited Create;
  FName  := AName;
  FWidth := AWidth;
  if FWidth = 1 then
    FType := TypeBit
  else
    FType := TType.Create('std_logic_vector',dirDown,FWidth-1,0);
End;

{ TConnTypeOptions }

Procedure TConnTypeOptions.Clear;
Begin
  FArray         := false;
  FPadLeft       := 0;
  FPadLeftWidth  := -1;
  FPadRight      := 0;
  FPadRightWidth := -1;
End;

Function TConnTypeOptions.ToString:String;
Begin
  Result := 'Array: '+Select('true','false',FArray)+
            ', PadLeft: '+IntToBin(FPadLeft,FPadLeftWidth)+' ('+IntToStr(FPadLeftWidth)+
            '), PadRight: '+IntToBin(FPadRight,FPadRightWidth)+' ('+IntToStr(FPadRightWidth) + ')';
End;

{ TReconfSignalBase }

Constructor TReconfSignalBase.Create(AName:String);
Begin
  inherited Create;
  FName := AName;
End;

Procedure TReconfSignalBase.SetSigConn(ASigConn:TSigConnBase);
Begin
  if assigned(FSigConn) then
    raise Exception.Create('Signal connection of '+FName+' is already set');
  FSigConn := ASigConn;
  FSigConn.AssignSignal(Self);
End;

Function TReconfSignalBase.GetPortName:String;
Begin
  Result := TrimSignalPostfix(FName);
  // add our own postfix
  Case GetDirection of
    dirIn    : Result += '_i';
    dirOut   : Result += '_o';
    dirInOut : Result += '_b';
  End;
End;

{ TConnectedChunk }

Constructor TConnectedChunk.Create;
Begin
  inherited Create;
End;

Constructor TConnectedChunk.Create(AChunk:TChunk);
Begin
  inherited Create;
  FHigh := AChunk.FHigh;
  FLow  := AChunk.FLow;
End;

{ TReconfPort }

Function TReconfPort.ToString:String;
Begin
  Result := Format('%s'^I'%s',[FPort.GetVHDLDeclaration,CPortDirectionVHDL[FPort.FDir]]);
  if assigned(FSigConn) then
    Result := Result + '   (' + FSigConn.ToString + ')';
End;

Function TReconfPort.GetSignal:TSignal;
Begin
  Result := FPort;
End;

Function TReconfPort.GetDirection:TPortDirection;
Begin
  Result := FPort.FDir;
End;

{ TReconfSignal }

Function TReconfSignal.ToString:String;
Var I : Integer;
Begin
  Result := FSignal.GetVHDLDeclaration;
  For I := 0 to FConnections.Count-1 do
    With FConnections.Data[I] do
      Result += ^I + IntToStr(FHigh)+':'+IntToStr(FLow) + ' => ' +
        CPortDirectionVHDL[FPort.FDir] + ' ' + FInstance.FName+'.'+FPort.FName+'('+IntToStr(FConnChunk.FHigh)+':'+IntToStr(FConnChunk.FLow)+')';
  if assigned(FSigConn) then
    Result := Result + ^I'(' + FSigConn.ToString + ')';
End;

Function TReconfSignal.GetSignal:TSignal;
Begin
  Result := FSignal;
End;

Function TReconfSignal.GetDirection:TPortDirection;
Begin
  Case FConnections.Data[0].FPort.FDir of
    dirIn    : Result := dirOut;
    dirOut   : Result := dirIn;
    dirInOut : Result := dirInOut;
  else
    raise Exception.Create('Can''t determine direction of parent signal for '+ToString);
  End;

End;

Function TReconfSignal.FindChunk(AHigh,ALow:Integer):Integer;
Var I : Integer;
Begin
  For I := 0 to FConnections.Count-1 do
    if (FConnections.Data[I].FHigh = AHigh) and (FConnections.Data[I].FLow = ALow) then
      Exit(I);
  Result := -1;
End;

{ TReconfSignals }

Function TReconfSignals.GetReconfSignal(Key:String):TReconfSignalBase;
Begin
  Result := FReconfSignals[Key];
End;

Function TReconfSignals.GetCount:Integer;
Begin
  Result := FReconfSignals.Count;
end;

Constructor TReconfSignals.Create;
Begin
  inherited Create;
  // FParent will be set later
  FReconfSignals := TReconfSignalList.Create;
  FConnTypes     := TConnTypes.Create;
End;

Destructor TReconfSignals.Destroy;
Begin
  FConnTypes.Free;
  FReconfSignals.Free;
  inherited Destroy;
End;

Procedure TReconfSignals.AddSignal(ASignal,AName:String);
Var ParentSig : TSignal;
    PP        : TReconfPort;
    PChunk    : TChunk;
    PS        : TReconfSignal;
    Driver    : TValue;
    CChunk    : TConnectedChunk;
    I,Low     : Integer;
    Driver2   : TValue;
Begin
  if AName = '' then AName := ASignal;
  if not assigned(FParent) then
    raise Exception.Create('Can''t add signals without a parent object');
  if (FReconfSignals.IndexOf(ASignal) >= 0) or (FReconfSignals.IndexOf(AName) >= 0) then
    raise Exception.Create('Reconfigurable signal '''+ASignal+''' already exists');

  ParentSig := FParent.GetSignal(ASignal);
  if not assigned(ParentSig) then
    raise Exception.Create('Reconfigurable signal '''+ASignal+''' is neither a port nor a signal of the parent');

  if (ParentSig is TPort) and ((ParentSig as TPort).FDir = dirIn) then
    Begin
      // ASignal is an input port
      PP           := TReconfPort.Create(AName);
      PP.FPort     := ParentSig as TPort;
      PP.FChunks   := TChunks.Create;
      PP.FChunks.Duplicates := dupError;
      PP.FChunks.Sorted     := true;
      FReconfSignals.Add(AName,PP);
      PChunk       := TChunk.Create;
      PChunk.FHigh := ParentSig.FType.GetLeft;
      PChunk.FLow  := ParentSig.FType.GetRight;
      PP.FChunks.Add(PChunk.FLow,PChunk);
    End
  else if ParentSig is TSignal then
    Begin
      // ASignal is an internal signal
      PS := TReconfSignal.Create(AName);
      PS.FSignal := ParentSig;
      PS.FConnections := TConnectedChunks.Create;
      PS.FConnections.Duplicates := dupError;
      PS.FConnections.Sorted     := true;
      FReconfSignals.Add(AName,PS);

      Driver := FParent.GetDriver(ASignal,true);  // allow to find things different than upper-level or lower-level ports

      if Driver is TConnection then
        Begin
          // driver is a lower-level port, i.e., an output of an instantiated module
          CChunk := TConnectedChunk.Create;
          CChunk.FHigh := ParentSig.FType.GetLeft;
          CChunk.FLow  := ParentSig.FType.GetRight;
          CChunk.FInstance  := (Driver as TConnection).FInstance;
          CChunk.FPort      := (Driver as TConnection).FPort;
          CChunk.FConnChunk :=  TChunk.Create;
          CChunk.FConnChunk.FHigh := CChunk.FPort.FType.GetLeft;
          CChunk.FConnChunk.FLow  := CChunk.FPort.FType.GetRight;
          PS.FConnections.Add(CChunk.FLow,CChunk);
        End
      else if Driver is TValueConcat then
        Begin
          // "driver" is a concatenation of some other things
          Low := 0;
          For I := (Driver as TValueConcat).FValues.Count-1 downto 0 do   // [0] is MSB, [n-1] is LSB
            if (Driver as TValueConcat).FValues.Items[I] is TSignal then
              With (Driver as TValueConcat).FValues.Items[I] as TSignal do
                Begin
                  Driver2 := FParent.GetDriver(FName);   // only find real drivers, i.e. upper-level or lower-level ports
                  if not (Driver2 is TConnection) then
                    raise Exception.Create('Can''t add reconfigurable signal '''+ASignal+''' because using a '+Driver2.ClassName+' as driver of a signal in a concatenation is not yet implemented');
                  // real driver is a lower-level port, i.e. an output of an instantiated module
                  CChunk := TConnectedChunk.Create;
                  CChunk.FHigh := Low+(Driver2 as TConnection).FPort.FType.GetWidthInt-1;
                  CChunk.FLow  := Low;
                  CChunk.FInstance  := (Driver2 as TConnection).FInstance;
                  CChunk.FPort      := (Driver2 as TConnection).FPort;
                  CChunk.FConnChunk :=  TChunk.Create;
                  CChunk.FConnChunk.FHigh := CChunk.FPort.FType.GetLeft;
                  CChunk.FConnChunk.FLow  := CChunk.FPort.FType.GetRight;
                  PS.FConnections.Add(Low,CChunk);
                  Low := Low + FType.GetWidthInt;
                End
            else
              raise Exception.Create('Can''t add reconfigurable signal '''+ASignal+''' because using a '+(Driver as TValueConcat).FValues.Items[I].ClassName+' in a concatenation is not yet implemented');
        End
      else if Driver is TValueIndex then
        Begin
          if (Driver as TValueIndex).FValue is TSignal then
            Begin;
              Driver2 := FParent.GetDriver(Driver);
              if not (Driver2 is TConnection) then
                raise Exception.Create('Can''t add reconfigurable signal '''+ASignal+''' because using a '+Driver2.ClassName+' as driver of a signal in an index is not yet implemented');
              CChunk := TConnectedChunk.Create;
              CChunk.FHigh := ParentSig.FType.GetLeft;
              CChunk.FLow  := ParentSig.FType.GetRight;
              CChunk.FInstance  := (Driver2 as TConnection).FInstance;
              CChunk.FPort      := (Driver2 as TConnection).FPort;
              CChunk.FConnChunk :=  TChunk.Create;
              if not assigned((Driver2 as TConnection).FIndex) then
                Begin
                  CChunk.FConnChunk.FHigh := CChunk.FPort.FType.GetLeft;
                  CChunk.FConnChunk.FLow  := CChunk.FPort.FType.GetRight;
                End
              else if (Driver2 as TConnection).FIndex is TValueInteger then
                Begin
                  CChunk.FConnChunk.FHigh := ((Driver2 as TConnection).FIndex as TValueInteger).FValue;
                  CChunk.FConnChunk.FLow  := CChunk.FConnChunk.FHigh;
                End
              else
                Begin
                  raise Exception.Create('Can''t add reconfigurable signal '''+ASignal+''' because using a '+(Driver2 as TConnection).FIndex.ClassName+' as index is not yet implemented');
                End;
              PS.FConnections.Add(CChunk.FLow,CChunk);
            End
          else
            raise Exception.Create('Can''t add reconfigurable signal '''+ASignal+''' because using a '+(Driver as TValueIndex).FValue.ClassName+' in an index is not yet implemented');
        End
      else
        raise Exception.Create('Can''t add reconfigurable signal '''+ASignal+''' because using a '+Driver.ClassName+' as driver is not yet implemented');
    End
  else
    Begin
      raise Exception.Create('Can''t add reconfigurable signal '''+ASignal+''' because finding its driver is not yet implemented');
      // find driver of this net
      // TODO (implement as Netlist-feature, not just here!)
      // create TReconfSignal which refers to the driver
      // TODO (see below at ReadUnusedSignals)
    End;
End;

Procedure TReconfSignals.UseSignalAlias(AName:String);
Var Assignment : TAssignment;
    Index      : Integer;
    Concat     : TValueConcat;
    I,J        : Integer;
    V          : TValue;
    S          : TSignal;
    RS         : TReconfSignal;
    Complete   : Cardinal;     // used to fill in bits
Begin
  // first, check if this signal really exists
  if FParent.FSignals.IndexOf(AName) < 0 then
    raise Exception.Create('Signal '''+AName+''' doesn''t exist and therefore can''t be used');
  Index := FReconfSignals.IndexOf(AName);
  if Index >= 0 then
    Begin
      WriteLn('Warning: Not using alias of ',AName,' because it is already a reconf.signal.');
      Exit;
    End;
  // then check if it really an alias to an unused signal
  Assignment := FParent.FindAssignment(AName);
  if not assigned(Assignment) then
    raise Exception.Create('There is no assignment to the signal '''+AName+'''');
  // search in list of unused signals
  if Assignment.FValue is TSignal then
    Begin
      Index := FReconfSignals.IndexOf((Assignment.FValue as TSignal).FName);
      if Index < 0 then
        raise Exception.Create('Assignment value '+Assignment.FValue.GetVHDLValue+' is not an unused signal');
      // Now we have to use this aliased signal instead of the multi-chunk
      // unused signal.
      RS := FReconfSignals.Data[Index] as TReconfSignal;
      S := Assignment.FDest as TSignal;
      // use alias as reconfigurable signal instead of old signal
      WriteLn('Replacing unused ',RS.ToString);
      Index := FReconfSignals.IndexOf(RS.FName);
      RS.FName   := S.FName;
      RS.FSignal := S;
      FReconfSignals.Delete(Index);
      FReconfSignals.Add(S.FName,RS);
      WriteLn('    by its alias ',RS.ToString);
      // switch destination and value of assignment
      Assignment.Swap;
    End
  else if Assignment.FValue is TValueConcat then
    Begin
      // this is very dirty!
      Concat := Assignment.FValue as TValueConcat;
      RS := Nil;
      Complete := 0;
      For I := 0 to Concat.FValues.Count-1 do
        Begin
          V := Concat.FValues[I];
          if V is TSignal then
            Begin
              raise Exception.Create('Concatenation with pure signals not yet implemented');
              // TODO
            End
          else if V is TValueIndex then
            Begin
              if not ((V as TValueIndex).FValue is TSignal) then
                raise Exception.Create('Can''t use an alias with a concatenation of an indexed value of '+(V as TValueIndex).FValue.ClassName);
              S := (V as TValueIndex).FValue as TSignal;
              Index := FReconfSignals.IndexOf(S.FName);
              if Index < 0 then
                raise Exception.Create('Assignment value '+S.GetVHDLValue+' is not an unused signal');
              if assigned(RS) and (RS <> (FReconfSignals.Data[Index] as TReconfSignal)) then
                raise Exception.Create('Concatenation from different signals not yet supported') // TODO
              else
                RS := FReconfSignals.Data[Index] as TReconfSignal;
              Index := RS.FindChunk((V as TValueIndex).GetLeft,(V as TValueIndex).GetRight);  // reuse "Index"
              if Index < 0 then
                raise Exception.Create('Concatenation element '+V.GetVHDLValue+' is not an unused signal chunk');
              if Index > 31 then
                raise Exception.Create('Concatenation of unused signals with more than 32 chunks is not yet supported');  // TODO
              Complete := Complete or (1 shl Index);
            End
          else
            raise Exception.Create('Can''t use an alias with a concatenations of '+V.ClassName);
        End;
      if not assigned(RS) then
        raise Exception.Create('This case is not yet supported'); // TODO
      // concatenation used some indexed values
      if Complete <> (1 shl RS.FConnections.Count)-1 then
        raise Exception.Create('Using an alias of a concatenation of indexed signals with incomplete usage of an unused signal is not yet supported');  // TODO
      // Ok, here we know that the alias uses a concatenation of the chunks of
      // a single signal, which all are unused.
      // Now we have to use this aliased signal instead of the multi-chunk
      // unused signal.
      S := Assignment.FDest as TSignal;
      // use alias as reconfigurable signal instead of old signal
      WriteLn('Replacing unused ',RS.ToString);
      Index := FReconfSignals.IndexOf(RS.FName);
      RS.FName   := S.FName;
      RS.FSignal := S;
      // correct chunk indices
      J := 0;
      For I := 0 to RS.FConnections.Count-1 do        // this relies on the fact, that FConnections is sorted by FLow!
        Begin
          RS.FConnections.Data[I].FLow := J;
          J := J + RS.FConnections.Data[I].FConnChunk.FHigh-RS.FConnections.Data[I].FConnChunk.FLow;
          RS.FConnections.Data[I].FHigh := J;
          Inc(J);
        End;
      FReconfSignals.Delete(Index);
      FReconfSignals.Add(S.FName,RS);
      WriteLn('    by its alias ',RS.ToString);
      // switch destination and value of assignment
      Assignment.Swap;
    End
  else if Assignment.FValue is TValueIndex then
    Begin
      raise Exception.Create('Using an indexed signal '+Assignment.FValue.GetVHDLValue+' as alias for '+AName+' is not yet implemented');
      // TODO
    End
  else
    raise Exception.Create('Can''t use alias of type '+Assignment.FValue.ClassName);
End;

(*
Procedure TReconfSignals.AddSignal(AName:String;ALow,AHigh:Integer;ADirecton:TPortDirection);
Var Signal : TReconfSignal;
Begin
  Signal := TReconfSignal.Create;
  Signal.FName      := AName;
  Signal.FHigh      := AHigh;
  Signal.FLow       := ALow;
  Signal.FDirection := ADirecton;
  AddSignal(Signal);
End;

Procedure TReconfSignals.AddSignal(AName:String;ALow,AHigh:Integer;ADirecton:TPortDirection;ACell,APort:String;APortLow,APortHigh:Integer);
Var Signal : TReconfSignal;
Begin
  Signal := TReconfSignal.Create;
  Signal.FName      := AName;
  Signal.FHigh      := AHigh;
  Signal.FLow       := ALow;
  Signal.FDirection := ADirecton;
  Signal.FCell      := ACell;
  Signal.FPort      := APort;
  Signal.FPortHigh  := APortHigh;
  Signal.FPortLow   := APortLow;
  AddSignal(Signal);
End;
*)
Function TReconfSignals.DelSignals(Const AFunc:TReconfSignalBoolFunc;Const AData:Pointer):Integer;
Var Index  : Integer;
Begin
  Index  := 0;
  Result := 0;
  While Index < FReconfSignals.Count do
    Begin
      if AFunc(FReconfSignals.Data[Index], AData) then
        Begin
          WriteLn('Deleting ',FReconfSignals.Data[Index].ToString);
          FReconfSignals.Delete(Index);
          Inc(Result);
        End
      else
        Inc(Index);
    End;
  WriteLn('Deleted ',Result,' signals.');
End;

Procedure TReconfSignals.ListReconfSignals(Const AFunc:TReconfSignalBoolFunc;Const AData:Pointer);
Var Index : Integer;
    HCount : Integer;
Begin
  HCount := 0;
  For Index := 0 to FReconfSignals.Count-1 do
    if (not assigned(AFunc)) or AFunc(FReconfSignals.Data[Index], AData) then
      Begin
        WriteLn(FReconfSignals.Data[Index].ToString);
        Inc(HCount);
      End;
  if assigned(AFunc) then
    WriteLn(HCount,' of ',FReconfSignals.Count,' signals.')
  else
    WriteLn(FReconfSignals.Count,' signals.');
End;

Function TReconfSignals.Foreach(Const ASelFunc:TReconfSignalBoolFunc;Const ASelData:Pointer;Const ADoFunc:TReconfSignalProc;Const ADoData:Pointer):Integer;
Var Index : Integer;
Begin
  Result := 0;
  For Index := 0 to FReconfSignals.Count-1 do
    if (not assigned(ASelFunc)) or ASelFunc(FReconfSignals.Data[Index], ASelData) then
      Begin
        ADoFunc(FReconfSignals.Data[Index],ADoData);
        Inc(Result);
      End;
End;

Function TReconfSignals.ReadUnusedSignals(Const Filename:String):Integer;

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
            raise Exception.Create('Invalid syntax')
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
            raise Exception.Create('Invalid syntax');
          // else: I is already Length+1
        End;
      Result := Copy(St,P,I-P);
      P := I+1;
    End;

    Function GetUntilReverse(Ch:Char;Required:Boolean=true):String;
    Var I : Integer;
    Begin
      I := Length(St);
      While (I >= P) and not (St[I] = Ch) do
        Dec(I);
      if I < P then
        Begin
          if Required then
            raise Exception.Create('Invalid syntax');
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

  Var
      VName      : String;
      PChunk     : TChunk;
      VDirection : TPortDirection;
      VCell      : String;
      VPort      : String;
      SChunk     : TConnectedChunk;

      PP : TReconfPort;
      PS : TReconfSignal;
  Begin
    // Format:
    //   wire[h:l] dir                     # top-module port
    //   wire[h:l] dir cell.port[h:l]      # cell port
    // where "dir" is "in", "out", "inout" or "unknown"
    P := 1;
    VName        := GetUntil('[');
    PChunk       := TChunk.Create;
    PChunk.FHigh := StrToInt(GetUntil(':'));
    PChunk.FLow  := StrToInt(GetUntil(']'));
    SkipWhitespace;
    VDirection   := StrToPortDir(GetUntil([' ',^I],false));  // variable is unused, but we have to parse the string
    if P > Length(St) then
      Begin
        if FReconfSignals.IndexOf(VName) < 0 then
          Begin
            PP := TReconfPort.Create(VName);
            PP.FPort := FParent.GetSignal(VName) as TPort;   // will raise an exception if this is not a TPort
            if PP.FPort = Nil then
              raise Exception.Create('Reconfigurable signal '''+VName+''' is neither a port nor a signal of the parent');
            PP.FChunks := TChunks.Create;
            PP.FChunks.Duplicates := dupError;
            PP.FChunks.Sorted     := true;
            FReconfSignals.Add(VName,PP);
          End
        else
          PP := FReconfSignals[VName] as TReconfPort;
        PP.FChunks.Add(PChunk.FLow,PChunk);
        Exit;
      End;
    // cell port --> parse remaining parameters
    if FReconfSignals.IndexOf(VName) < 0 then
      Begin
        PS := TReconfSignal.Create(VName);
        PS.FSignal := FParent.GetSignal(VName);
        if PS.FSignal = Nil then
          raise Exception.Create('Reconfigurable signal '''+VName+''' is neither a port nor a signal of the parent');
        PS.FConnections := TConnectedChunks.Create;
        PS.FConnections.Duplicates := dupError;
        PS.FConnections.Sorted     := true;
        FReconfSignals.Add(VName,PS);
      End
    else
      PS := FReconfSignals[VName] as TReconfSignal;
    SChunk            := TConnectedChunk.Create(PChunk);  // create new descendent class and copy contents of PChunk
    VCell             := GetUntilReverse('.');
    VPort             := GetUntil('[');
    PChunk.FHigh      := StrToInt(GetUntil(':'));  // reuse PChunk
    PChunk.FLow       := StrToInt(GetUntil(']'));
    SChunk.FInstance  := FParent.FInstances[VCell];
    if SChunk.FInstance.FModule.FEntityAttributes.FAttrValList.IndexOf('pas_ilang_autogen') >= 0 then
      WriteLn('WARNING: Unknown direction of port ',VPort,' of auto-generated module ',SChunk.FInstance.FModule.FName,' instance ',SChunk.FInstance.FName,
        ' connected to signal ',VName);
    SChunk.FPort      := SChunk.FInstance.FModule.FPorts[VPort];
    SChunk.FConnChunk := PChunk;
    PS.FConnections.Add(SChunk.FLow,SChunk);
    // TODO
  End;

Var T    : Text;
    Line : Integer;
    St   : String;
Begin
  if not assigned(FParent) then
    raise Exception.Create('Can''t read unused signals without a parent object');
  Assign(T,Filename);
  Reset(T);
  Line := 1;
  try
    try
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
  Result := Line-1;
End;

Procedure TReconfSignals.SetSigConn(ASignal:TReconfSignalBase;Const AData:Pointer);
Begin
  ASignal.SetSigConn(TSigConnBase(AData).Clone);    // clone, because this might be used for multiple signals
End;

Procedure TReconfSignals.SetConnType(ASignal:TReconfSignalBase;Const AData:Pointer);
Begin
  if not assigned(ASignal.FSigConn) then
    Exit;//raise Exception.Create('Cannot set connection type of reconf.signal '+ASignal.FName+' because its connection is not yet set');
  if not (ASignal.FSigConn is TSigConnDyn) then
    Exit;//raise Exception.Create('Cannot set connection type of reconf.signal '+ASignal.FName+' because its connection is not set as dynamic');
  (ASignal.FSigConn as TSigConnDyn).SetConnType(PConnTypeData(AData)^.FConnType,PConnTypeData(AData)^.FConnTypeOptions);
End;

Function TReconfSignals.CheckSigConn:Integer;
Var Index : Integer;
Begin
  Result := 0;
  For Index := 0 to FReconfSignals.Count-1 do
    With FReconfSignals.Data[Index] do
      if not assigned(FSigConn) then
        Begin
          WriteLn('No signal connection set for signal '+ToString);
          Inc(Result);
        End;
End;

Function TReconfSignals.CheckConnType:Integer;
Var Index : Integer;
Begin
  Result := 0;
  For Index := 0 to FReconfSignals.Count-1 do
    With FReconfSignals.Data[Index] do
      if assigned(FSigConn) and (FSigConn is TSigConnDyn) and (not assigned ((FSigConn as TSigConnDyn).FConnType)) then
        Begin
          WriteLn('No conntype set for signal '+GetSignal.GetVHDLDeclaration);
          Inc(Result);
        End;
End;

Procedure TReconfSignals.SetupConfigAndParam;
Var ConfigRegister : TChunkedConfigRegister;   // damn circular unit reference, use local variables with correct type
    Parameters     : TParameters;
    I              : Integer;
Begin
  if assigned(FConfigRegister) then
    Exit;
  WriteLn('Creating parameter list of parameterized reconf.signals.');
  ConfigRegister  := TChunkedConfigRegister.Create('ReconfSignals');
  FConfigRegister := ConfigRegister;
  Parameters      := TParameters.Create;
  FParameters     := Parameters;

  // iterate over all reconf signals
  For I := 0 to FReconfSignals.Count-1 do
    With FReconfSignals.Data[I] do
      Begin
        if FSigConn is TSigConnConfig then
          Begin
            // connection/usage: config
            ConfigRegister.Add(TConfigChunk.Create(GetPortName,GetSignal.FType.GetWidthInt,0{TODO:Default}));
          End
        else if FSigConn is TSigConnParam then
          Begin
            // connection/usage: param
            if GetDirection = dirIn then
              Begin
                // reconf.signal input into reconf.module --> param read
                Parameters.Add(FName,TParameter.Create(FName,pdRead,GetSignal.FType,false,0{TODO:Default}));
                // TODO: default value should be given by the application! see TReconfApp.RegisterParameters
              End
            else
              Begin
                // reconf.signal output from reconf.module --> param write
                Parameters.Add(FName,TParameter.Create(FName,pdWrite,GetSignal.FType,false,0{TODO:Default}));
              End;
          End;
        // others are ignored
      End;
  ConfigRegister.AssignAddresses;     // assign addresses of config bitstream chunks
End;

End.

