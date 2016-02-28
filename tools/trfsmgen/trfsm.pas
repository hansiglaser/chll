Unit TRFSM;

{$mode objfpc}{$H+}
{$INTERFACES CORBA}

Interface

Uses
  Classes, SysUtils, Bitstream, FSMDef, TRFSMBitstream, Utils, TRFSMSpec;

Type

  TStateMapper = (
    smSimple,          // simply increasing binary value and TFSMDefinition's index
    smRandomState,     // randomize TFSMDefinition's index
    smRandomBinary);   // randomize binary value

  TTRIndex = record
    Width : Integer;
    Index : Integer;
  End;

  { TTRFSM }

  TTRFSM = class
  private
    FSpecification : TTRFSMSpecification;
    FDefinition    : TFSMDefinition;
    FBitstream     : TTRFSMBitstream;
    FInputMap      : Array of String;   // index = our physical input  number, value = logical FDefinition.FInput [I] name
    FOutputMap     : Array of String;   // index = our physical output number, value = logical FDefinition.FOutput[I] name
    FStateMap      : Array of String;   // index = our physical state  number, value = logical FDefinition.FState [I] name
    FTransitionMap : TDynInteger2DArray;  // FTransitionMap[Width][Num] = index in FDefinition.Transition[I]
    FISM           : Array of Array of TISMBitstream; // FISM[Width][Num] = ISM of that TR
    FStateMapper   : TStateMapper;
    FMapperSeed    : Cardinal;

    Function FindFreeTR(MinWidth:Integer;ConstRef ATransitionMap:TDynInteger2DArray):TTRIndex;
    Function TryMapTransitions(ADefinition:TFSMDefinition;Var ATransitionMap:TDynInteger2DArray):Integer;

    { mapping queries: logical -> physical }
    Function GetMappedInput     (AInput      : String)  : Integer;
    Function GetMappedOutput    (AOutput     : String)  : Integer;
    Function GetMappedState     (AState      : String)  : Integer;
    Function GetMappedTransition(ATransition : Integer) : TTRIndex;
    { mapping queries: physical -> logical }
    Function GetMappedInput     (AInput      : Integer) : String;
    Function GetMappedOutput    (AOutput     : Integer) : String;
    Function GetMappedState     (AState      : Integer) : String;
    Function GetMappedTransition(Width,Index : Integer) : Integer;
    { mapping queries: special }
    Function GetMappedInputPattern(AInput:String):String;
    { bitstream queries }
    Function  GetBitstreamLength:Cardinal;
    { bitstream generation }
    Procedure GenerateBitstreamTR (Width,Index:Integer;Transition:TTransition);
    Procedure GenerateBitstreamSSG(Width,Index:Integer;Transition:TTransition);  // lookup a mapped state in our FStateMap and return its index
    Procedure GenerateBitstreamISM(Width,Index:Integer;Transition:TTransition);
    Procedure GenerateBitstreamIPG(Width,Index:Integer;Transition:TTransition);
    Procedure GenerateBitstreamNSR(Width,Index:Integer;Transition:TTransition);
    Procedure GenerateBitstreamOPR(Width,Index:Integer;Transition:TTransition);
  public
    Constructor Create(ASpecification:TTRFSMSpecification);
    Destructor  Destroy; override;
    Function  CheckDefinition(ADefinition:TFSMDefinition):Boolean;
    Function  HasDefinition : Boolean;
    Procedure SetDefinition(ADefinition:TFSMDefinition);
    Procedure MapInput (ALogical:String;APhysical:Integer);
    Procedure MapOutput(ALogical:String;APhysical:Integer);
    Procedure PrintMapping;
    property  Specification : TTRFSMSpecification read FSpecification;
    property  StateMap      : TDynStringArray read FStateMap;
    property  StateMapper   : TStateMapper read FStateMapper write FStateMapper;
    property  MapperSeed    : Cardinal read FMapperSeed write FMapperSeed;
    property  BitstreamLength : Cardinal read GetBitstreamLength;
    Procedure Map;
    Procedure GenerateBitstream;
    Function  GetBitstream(ID:Cardinal) : TBitstream;
  End;

Implementation

{ TTRFSM }

Constructor TTRFSM.Create(ASpecification:TTRFSMSpecification);
Var Width,Index : Integer;
Begin
  inherited Create;
  FSpecification := ASpecification;
  Case FSpecification.Version of
    'SNOPS1' : FBitstream := TTRFSMBitstream.   Create(FSpecification);
    'REG'    : FBitstream := TTRFSMBitstreamReg.Create(FSpecification);
  else
    raise Exception.Create('Invalid TR-FSM version '''+FSpecification.Version+'''');
  End;
  SetLength(FInputMap,      FSpecification.Inputs);
  SetLength(FOutputMap,     FSpecification.Outputs);
  SetLength(FStateMap,      FSpecification.States);
  SetLength(FTransitionMap, FSpecification.MaxTRWidth+1);
  SetLength(FISM,           FSpecification.MaxTRWidth+1);
  For Width := 0 to FSpecification.MaxTRWidth do   // including MaxTRWidth!
    Begin
      SetLength(FTransitionMap[Width],FSpecification.NumTRs[Width]);
      SetLength(FISM          [Width],FSpecification.NumTRs[Width]);
      For Index := 0 to FSpecification.NumTRs[Width]-1 do
        Begin
          FTransitionMap[Width][Index] := -1;   // -1 means unused, >=0 is index into FDefinition.FTransition
          FISM          [Width][Index] := TISMBitstream.Create(FSpecification.Inputs,Width);
        End;
    End;
  FStateMapper := smSimple;
  FMapperSeed  := 0;
End;

Destructor TTRFSM.Destroy;
Var Width,Index : Integer;
Begin
  FSpecification.Free;
  FDefinition.Free;
  For Width := 0 to FSpecification.MaxTRWidth do   // including MaxTRWidth!
    For Index := 0 to FSpecification.NumTRs[Width]-1 do
      FISM[Width][Index].Free;
  Inherited Destroy;
End;

Function TTRFSM.CheckDefinition(ADefinition:TFSMDefinition) : Boolean;
Var TransitionMap : TDynInteger2DArray;
Begin
  Result := False;
  if ADefinition.InputCount > FSpecification.Inputs then
    Exit;
  if ADefinition.OutputCount > FSpecification.Outputs then
    Exit;
  if ADefinition.StateCount > FSpecification.States then
    Exit;
  if TryMapTransitions(ADefinition,TransitionMap) >= 0 then
    Exit;
  Result := True;
End;

Function TTRFSM.HasDefinition:Boolean;
Begin
  Result := assigned(FDefinition);
End;

Procedure TTRFSM.SetDefinition(ADefinition:TFSMDefinition);
Begin
  FDefinition.Free;
  if ADefinition.InputCount > FSpecification.Inputs then
    raise Exception.CreateFmt('Specification requires more inputs (%d) than available (%d)',[ADefinition.InputCount,FSpecification.Inputs]);
  if ADefinition.OutputCount > FSpecification.Outputs then
    raise Exception.CreateFmt('Specification requires more outputs (%d) than available (%d)',[ADefinition.OutputCount,FSpecification.Outputs]);
  if ADefinition.StateCount > FSpecification.States then
    raise Exception.CreateFmt('Specification requires more states (%d) than available (%d)',[ADefinition.StateCount,FSpecification.States]);
  if ADefinition.OutputRegistered and (FSpecification.Version <> 'REG') then
    raise Exception.Create('Specification requires a registered FSM which can''t be implemented in version '''+FSpecification.Version+'''');
  FDefinition := ADefinition;
End;

Procedure TTRFSM.MapInput(ALogical:String;APhysical:Integer);
Begin
  if (APhysical < 0) or (APhysical >= FSpecification.Inputs) then
    raise Exception.CreateFmt('Physical input %d out of range [0..%d]',[APhysical,FSpecification.Inputs-1]);
  if FInputMap[APhysical] > ''  then
    raise Exception.CreateFmt('Physical input %d already mapped to logical input "%s"',[APhysical,FInputMap[APhysical]]);
  if FDefinition.TryGetInput(ALogical) < 0 then
    raise Exception.Create('Logical input "'+ALogical+'" unknown');
  FInputMap[APhysical] := ALogical;
End;

Procedure TTRFSM.MapOutput(ALogical:String;APhysical:Integer);
Begin
  if (APhysical < 0) or (APhysical >= FSpecification.Outputs) then
    raise Exception.CreateFmt('Physical output %d out of range [0..%d]',[APhysical,FSpecification.Outputs-1]);
  if FOutputMap[APhysical] > ''  then
    raise Exception.CreateFmt('Physical output %d already mapped to logical input "%s"',[APhysical,FOutputMap[APhysical]]);
  if FDefinition.TryGetOutput(ALogical) < 0 then
    raise Exception.Create('Logical output "'+ALogical+'" unknown');
  FOutputMap[APhysical] := ALogical;
End;

Function TTRFSM.FindFreeTR(MinWidth:Integer;ConstRef ATransitionMap:TDynInteger2DArray):TTRIndex;
Begin
  Result.Width := -1;
  Result.Index := -1;
  if MinWidth >= Length(ATransitionMap) then   // '>=' used because we have 0-width too
    Exit;
  // start at Width
  Result.Width := MinWidth;
  repeat
    // search in TRs of Width
    Result.Index := 0;
    while Result.Index < FSpecification.NumTRs[Result.Width] do
      if (Length(ATransitionMap[Result.Width]) > Result.Index) and (ATransitionMap[Result.Width][Result.Index] < 0) then
        Exit   // found one
      else
        Inc(Result.Index);
    Inc(Result.Width);
  until Result.Width >= Length(ATransitionMap);
  Result.Width := -1;
  Result.Index := -1;
End;

(**
 * Result:  -1: success
 *         >=0: number of transition in ADefinition which couldn't be mapped
 *)
Function TTRFSM.TryMapTransitions(ADefinition:TFSMDefinition;Var ATransitionMap:TDynInteger2DArray) : Integer;
Var I        : Integer;
    Observed : Integer;
    TR       : TTRIndex;
Begin
  SetLength(ATransitionMap,FSpecification.MaxTRWidth+1);
  For I := 0 to FSpecification.MaxTRWidth do   // including MaxTRWidth!
    Begin
      SetLength(ATransitionMap[I],FSpecification.NumTRs[I]);
      For Observed := 0 to FSpecification.NumTRs[I]-1 do    // reusing variable Observed
        ATransitionMap[I][Observed] := -1;   // -1 means unused, >=0 is index into FDefinition.FTransition
    End;

  { map transitions to TRs }
  For I := 0 to ADefinition.TransitionCount-1 do
    Begin
      Observed := ADefinition.Transition[I].NumObservedInputs;
      // find free TR
      TR := FindFreeTR(Observed,ATransitionMap);  // returns <0 if no free TR was found
      if TR.Width < 0 then
        Exit(I);    // no matching TR available --> return transition index
      // map TR
      ATransitionMap[TR.Width][TR.Index] := I;
    End;
  // Note: Here we simply go through all transitions, but probably we should
  // sort these before mapping.
  Result := -1;
End;

{-- mapping queries ----------------------------------------------------------}

Function TTRFSM.GetMappedInput(AInput:String):Integer;
Begin
  For Result := 0 to Length(FInputMap)-1 do
    if FInputMap[Result] = AInput then
      Exit;
  raise Exception.Create('Input "'+AInput+'" is not mapped');
End;

Function TTRFSM.GetMappedOutput(AOutput:String):Integer;
Begin
  For Result := 0 to Length(FOutputMap)-1 do
    if FOutputMap[Result] = AOutput then
      Exit;
  raise Exception.Create('Output "'+AOutput+'" is not mapped');
End;

Function TTRFSM.GetMappedState(AState:String):Integer;
Begin
  For Result := 0 to Length(FStateMap)-1 do
    if FStateMap[Result] = AState then
      Exit;
  raise Exception.Create('State "'+AState+'" is not mapped');
End;

Function TTRFSM.GetMappedTransition(ATransition:Integer):TTRIndex;
Begin
(*  For Result.Width := 0 to Length(FTransitionMap)-1 do
    For Result.Index := 0 to Length(FTransitionMap[Result.Width])-1 do
      if FTransitionMap[Result.Width][Result.Index] = ATransition then
        Exit;*)
  raise Exception.Create('Transition "'+IntToStr(ATransition)+'" is not mapped');
End;

Function TTRFSM.GetMappedInput(AInput:Integer):String;
Begin
  Result := FInputMap[AInput];
End;

Function TTRFSM.GetMappedOutput(AOutput:Integer):String;
Begin
  Result := FOutputMap[AOutput];
End;

Function TTRFSM.GetMappedState(AState:Integer):String;
Begin
  Result := FStateMap[AState];
End;

Function TTRFSM.GetMappedTransition(Width,Index:Integer):Integer;
Begin
  Result := FTransitionMap[Width][Index];
End;

Function TTRFSM.GetMappedInputPattern(AInput:String):String;
Var Logical,Physical : Integer;
Begin
  if Length(AInput) <> FDefinition.InputCount then
    raise Exception.CreateFmt('Input pattern "%s" to map has wrong length %d compared to definition %d',[AInput,Length(AInput),FDefinition.InputCount]);
  // prepare all don't cares for physical input pattern
  Result := StringOfChar('-',FSpecification.Inputs);
  For Logical := 0 to FDefinition.InputCount-1 do
    Begin
      Physical := GetMappedInput(FDefinition.Input[Logical]);
      if      AInput[Logical+1] = '1' then Result[Physical+1] := '1'
      else if AInput[Logical+1] = '0' then Result[Physical+1] := '0';  // '-' is default
    End;
End;

Function TTRFSM.GetBitstreamLength:Cardinal;
Begin
  Result := FBitstream.Count;
End;

{-- Generate Bitstream -------------------------------------------------------}

Procedure TTRFSM.GenerateBitstreamSSG(Width,Index:Integer;Transition:TTransition);
Var State : Integer;
Begin
  State := GetMappedState(FDefinition.State[Transition.State]);
  FBitstream.SetSSG(Width,Index,State);  // LSB stays LSB
End;

Procedure TTRFSM.GenerateBitstreamISM(Width,Index:Integer;Transition:TTransition);
Var Observed    : String;
    Logical     : Integer;
    Physical    : Integer;
Begin
  Observed := Transition.ObservedInputs;
  For Logical := 0 to Length(Observed)-1 do
    if Observed[Logical+1] = '1' then
      Begin
        // logical input index => physical input index
        Physical := GetMappedInput(FDefinition.Input[Logical]);
        //WriteLn('using physical input ',Physical,' for logical input ',FDefinition.Input[Logical]);
        FISM[Width][Index].Connect(Physical);
      End;
  FBitstream.SetISM(Width,Index,FISM[Width][Index]);
End;

Procedure TTRFSM.GenerateBitstreamIPG(Width,Index:Integer;Transition:TTransition);
Var PatternIndex : Integer;
    Physical     : String;
    IPGInput     : String;
    IPG          : TIPGBitstream;
Begin
  IPG := TIPGBitstream.Create(Width);
  For PatternIndex := 0 to Length(Transition.Input)-1 do
    Begin
      { map logical input pattern to physical input pattern }
      Physical := GetMappedInputPattern(Transition.Input[PatternIndex]);
      { map physical input pattern through ISM }
      IPGInput := FISM[Width][Index].MapPattern(Physical);
      { mark combinations for IPG }
      IPG.SetPattern(IPGInput);
      WriteLn(PatternIndex,': ',Transition.Input[PatternIndex],' -> ',Physical,' -> ',IPGInput,' -> ',IPG.GetStringReverse);  // GetStringReverse->LSB is right-most character
    End;
  FBitstream.SetIPG(Width,Index,IPG);
  IPG.Free;
End;

Procedure TTRFSM.GenerateBitstreamNSR(Width,Index:Integer;Transition:TTransition);
Var State : Integer;
Begin
  State := GetMappedState(FDefinition.State[Transition.NextState]);
  FBitstream.SetNSR(Width,Index,State);  // LSB stays LSB
End;

Procedure TTRFSM.GenerateBitstreamOPR(Width,Index:Integer;Transition:TTransition);
Var LogIndex : Integer;
    PhysBits : Integer;
    Physical : Integer;
Begin
  PhysBits := 0;   // default;
  For LogIndex := 0 to FDefinition.OutputCount-1 do
    if Transition.Output[LogIndex+1] = '1' then
      Begin
        Physical := GetMappedOutput(FDefinition.Output[LogIndex]);
        PhysBits := PhysBits or (1 shl Physical);
        WriteLn('Using physical output ',Physical,' for ',FDefinition.Output[LogIndex],': ',IntToBinLM(PhysBits,FSpecification.Outputs));
      End;
  FBitstream.SetOPR(Width,Index,PhysBits);
End;

Procedure TTRFSM.GenerateBitstreamTR(Width,Index:Integer;Transition:TTransition);
Begin
  GenerateBitstreamSSG(Width,Index,Transition);
  if Width > 0 then
    Begin
      GenerateBitstreamISM(Width,Index,Transition);
      GenerateBitstreamIPG(Width,Index,Transition);
    End;
  GenerateBitstreamNSR(Width,Index,Transition);
  GenerateBitstreamOPR(Width,Index,Transition);
End;

Procedure TTRFSM.PrintMapping;
Var I,J: Integer;
Begin
  if not assigned(FDefinition) then
    Exit;
  For I := 0 to FSpecification.Inputs-1 do
    if FInputMap[I] > '' then
      WriteLn('Physical input  ',I:2,' mapped to logical input  ',FInputMap[I])
    else
      WriteLn('Physical input  ',I:2,' is unused');
  For I := 0 to FSpecification.Outputs-1 do
    if FOutputMap[I] > '' then
      WriteLn('Physical output ',I:2,' mapped to logical output ',FOutputMap[I])
    else
      WriteLn('Physical output ',I:2,' is unused');
  For I := 0 to FSpecification.States-1 do
    if FStateMap[I] > '' then
      WriteLn('Physical state  ',I:2,' mapped to logical state ',FStateMap[I])
    else
      WriteLn('Physical state  ',I:2,' is unused');
  For I := 0 to FSpecification.MaxTRWidth do
    For J := 0 to FSpecification.NumTRs[I]-1 do
      if FTransitionMap[I][J] >= 0 then
        With FDefinition.Transition[FTransitionMap[I][J]] do
          WriteLn('TR-',I,'[',J,'] mapped to transition ',FTransitionMap[I][J],':  ',
            FDefinition.State[State],' ',InputString,' (',ObservedInputs,') ',' => ',Output,' ',FDefinition.State[NextState])
      else
        WriteLn('TR-',I,'[',J,'] is unused');
End;

(**
 *
 * We assume that FDefinition.Check was already run and didn't result in any
 * errors.
 *)
Procedure TTRFSM.Map;

  Procedure MapStateSimple;
  Var I : Integer;
  Begin
    // no reset state -> simplest mapping
    For I := 0 to FDefinition.StateCount-1 do
      Begin
        FStateMap[I] := FDefinition.State[I];
      End;
  End;

  Procedure MapStateSimpleWithReset;
  Var I,J : Integer;
  Begin
    // reset state given -> must be state '0000'
    FStateMap[0] := FDefinition.ResetState;
    J := 1;
    For I := 0 to FDefinition.StateCount-1 do
      Begin
        if FDefinition.State[I] = FDefinition.ResetState then
          continue; // skip reset state
        FStateMap[J] := FDefinition.State[I];
        Inc(J);
      End;
  End;

  Procedure MapStateGeneric;
  Var States : Array of String;

    Function FindState(AState:String) : Integer;
    Begin
      For Result := 0 to Length(States)-1 do
        if States[Result] = AState then
          Exit;
      raise Exception.Create('Invalid state "'+AState+'"');
    End;

    Function NumStatesAvail : Integer;
    Var I : Integer;
    Begin
      Result := 0;
      For I := 0 to FDefinition.StateCount-1 do
        if States[I] > '' then
          Inc(Result);
    End;

    Function NumMappedStatesAvail : Integer;
    Var I : Integer;
    Begin
      Result := 0;
      For I := 0 to Length(FStateMap)-1 do
        if FStateMap[I] = '' then
          Inc(Result);
    End;

    // Find Index'th free (=unmapped) state
    Function FreeState(Index:Integer) : Integer;
    Begin
      For Result := 0 to Length(States)-1 do
        if States[Result] > '' then
          Begin
            if Index = 0 then
              Exit;
            Dec(Index);
          End;
      // not found
      Result := -1;
    End;

    // Find Index'th free (=unmapped) state
    Function FreeMappedState(Index:Integer) : Integer;
    Begin
      For Result := 0 to Length(FStateMap)-1 do
        if FStateMap[Result] = '' then
          Begin
            if Index = 0 then
              Exit;
            Dec(Index);
          End;
      // not found
      Result := -1;
    End;

  Var I      : Integer;
      X,Y    : Integer;
  Begin
    // copy FDefinition.State[*] --> States
    SetLength(States,FDefinition.StateCount);
    For I := 0 to FDefinition.StateCount-1 do
      States[I] := FDefinition.State[I];
    // map reset state
    if FDefinition.ResetState > '' then
      Begin
        FStateMap[0] := FDefinition.ResetState;           // map reset state
        States[FindState(FDefinition.ResetState)] := '';  // remove state from list
      End;
    // map residual states
    RandSeed := FMapperSeed;
    While NumStatesAvail > 0 do
      Begin
        Case FStateMapper of
          smRandomState:
            Begin
              //Write('MapStateGeneric: NumStatesAvail = ',NumStatesAvail);
              X := FreeMappedState(0); // first free entry in FStateMap
              Y := Random(NumStatesAvail);
            End;
          smRandomBinary :
            Begin
              //Write('Avail: ',NumMappedStatesAvail);
              X := Random(NumMappedStatesAvail);
              //Write(', Rand = ',X);
              X := FreeMappedState(X);
              //Write(', X = ',X);
              Y := FreeState(0);  // first next unmapped state
              //WriteLn('  --> Mapping state ',Y,' ''',States[Y],''' to ',X);
            End;
        else
          raise Exception.Create('Internal Error: Invalid value of FStateMapper in MapStateGeneric');
        End;
        // check
        if FStateMap[X] <> '' then
          raise Exception.CreateFmt('Can''t map state %s to %d, because %d is already used',[States[Y],X]);
        if States[Y] = '' then
          raise Exception.CreateFmt('Can''t map state %d to %d, because %d was already mapped',[Y,X,Y]);
        // map
        FStateMap[X] := States[Y];  // map state
        States[Y] := '';            // remove state from list
      End;
  End;

Var I           : Integer;
Begin
  // nothing to do if we didn't get a definition
  if not assigned(FDefinition) then Exit;
  { check input map }
  // TODO
  { check output map }
  // TODO
  { map states }
  Case FStateMapper of
    smSimple :
      if FDefinition.ResetState > '' then
        // reset state given -> must be state '0000'
        MapStateSimpleWithReset
      else
        // no reset state -> simplest mapping
        MapStateSimple;
    smRandomState,
    smRandomBinary :
      MapStateGeneric;
  else
    raise Exception.Create('Internal Error: Invalid value of FStateMapper');
  End;
  { map transitions to TRs }
  I := TryMapTransitions(FDefinition,FTransitionMap);
  if I >= 0 then
    raise Exception.CreateFmt('Couldn''t map transition %d: No TR of width %d or higher available',[I,FDefinition.Transition[I].NumObservedInputs]);
End;

Procedure TTRFSM.GenerateBitstream;
Var Width,Index : Integer;
Begin
  // set everything
  FBitstream.Clear;
  // nothing to do if we didn't get a definition
  if not assigned(FDefinition) then Exit;
  // check whether state '11111' is used and warn
  if FStateMap[FSpecification.States-1] > '' then
    WriteLn('WARNING: All states are used so deactivated TRs are all active!');
  // set bits for every TR
  For Width := 0 to FSpecification.MaxTRWidth do
    For Index := 0 to FSpecification.NumTRs[Width]-1 do
      if FTransitionMap[Width][Index] >= 0 then
        Begin
          // used TR
          GenerateBitstreamTR(Width,Index,FDefinition.Transition[FTransitionMap[Width][Index]]);
        End
      else
        Begin
          // unused TR
          FBitstream.SetSSG(Width,Index,$FF);    // TODO: improve to allow more than 8 bits for the state vector
        End;
  // outputs registered
  if FSpecification.Version = 'REG' then
    (FBitstream as TTRFSMBitstreamReg).SetOutputsRegistered(FDefinition.OutputRegistered);
End;

Function TTRFSM.GetBitstream(ID:Cardinal):TBitstream;
Begin
  if ID <> 0 then
    raise Exception.CreateFmt('Invalid bitstream ID %d',[ID]);
  if not assigned(FDefinition) then   // TR-FSM without definition --> assume unused TR-FSM
    FBitstream.Clear;   // set everything to default values
  Result := FBitstream;
End;

End.

