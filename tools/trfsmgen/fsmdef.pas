Unit FSMDef;

{$mode objfpc}{$H+}

Interface

Uses
  Classes, SysUtils, Math, Utils;

Type
  TInputIndex    = Integer;
  TOutputIndex   = Integer;
  TStateIndex    = Integer;
  TInputPattern  = String;            // one pattern e.g. '--111-001-'
  TInputPatterns = TDynStringArray;   // list of patterns e.g. '--111-001-'
  TOutputPattern = String;
  TSignalValue = record
    Signal : String;
    Value  : Integer;   // 0, 1, anything else is "don't care"
  End;
  TSignalDefinition = Array of TSignalValue;

  TSEDirection = (sedUndefined,sedInput,sedOutput);

  { TTransition }

  TTransition = object
    State     : TStateIndex;
    Input     : TInputPatterns;
    NextState : TStateIndex;
    Output    : TOutputPattern;
  private
    Function GetInputString    : String;
    Function GetObservedInputs : String;
    Function GetNumObservedInputs:Integer;
  public
    Function CountObservedInputs : Integer;
    property InputString    : String read GetInputString;    // concatenated input patterns, e.g. '111--,01--1,000-0'
    property ObservedInputs : String read GetObservedInputs; // merges all observed inputs and returns a string, e.g. '111-1'
    property NumObservedInputs : Integer read GetNumObservedInputs;
  End;

  { TFSMDefinition }

  TFSMDefinition = class
  private
    FName        : String;
    FInputs      : Array of String;
    FOutputs     : Array of String;
    FStates      : Array of String;
    FTransitions : Array of TTransition;
    FResetState  : String;            // empty means: no reset state specified
    FOutReg      : Boolean;           // true: outputs are registered, default: false
    Function GetInputCount:Integer;
    Function GetOutputCount:Integer;
    Function GetStateCount:Integer;
    Function GetTransitionCount:Integer;
    Function GetInput(Index:Integer):String;
    Function GetOutput(Index:Integer):String;
    Function GetState(Index:Integer):String;
    Function GetTransition(Index:Integer):TTransition;
  public
    Constructor Create(AName:String);
    Constructor Create(AFSMDefinition:TFSMDefinition);
    Procedure AddInput (AInput :String);
    Procedure AddOutput(AOutput:String);
    Procedure AddState (AState :String);
    Procedure Add(AState:TStateIndex;AInput:TInputPatterns;ANextState:TStateIndex;AOutput:TOutputPattern);
    Procedure Add(AState:String;AInput:TInputPatterns;ANextState:String;AOutput:TOutputPattern);
    Procedure SetResetState(AResetState:String);
    Function  TryGetInput (AInput :String):TInputIndex;
    Function  TryGetOutput(AOutput:String):TOutputIndex;
    Function  TryGetState (AState :String):TStateIndex;
    Function  GetInput (AInput :String):TInputIndex;
    Function  GetOutput(AOutput:String):TOutputIndex;
    Function  GetState (AState :String):TStateIndex;
    Function  GetInputPattern (AInputs: TSignalDefinition) : TInputPattern;
    Function  GetInputPattern (Const AInputs:Array Of Const):TInputPattern;
    Function  GetInputPatterns(Const AInputPatterns:Array Of Const):TInputPatterns;
    Function  GetOutputPattern(AOutputs:TSignalDefinition) : TOutputPattern;
    Function  GetPortDirection(APort:String):TSEDirection;
    Function GetObserved:TDynIntegerArray;
    Function  Check : TDynStringArray;
    Procedure Print;
    property Name            : String  read FName;
    property InputCount      : Integer read GetInputCount;
    property OutputCount     : Integer read GetOutputCount;
    property StateCount      : Integer read GetStateCount;
    property TransitionCount : Integer read GetTransitionCount;
    property Input     [Index:Integer] : String read GetInput;
    property Output    [Index:Integer] : String read GetOutput;
    property State     [Index:Integer] : String read GetState;
    property Transition[Index:Integer] : TTransition read GetTransition;
    property ResetState                : string read FResetState write SetResetState;
    property OutputRegistered          : Boolean read FOutReg write FOutReg;
  End;

Implementation

{ TTransition }

Function TTransition.GetInputString:String;
Begin
  Result := Join(',',Input);
End;

(**
 * Merge all observed inputs and returns as a string, e.g. '111-1'
 *
 * If no input patterns are defined, an empty string '' is returned.
 *)
Function TTransition.GetObservedInputs:String;
Var I,J : Integer;
Begin
  // no inputs patterns -> return empty string
  if Length(Input) = 0 then
    Exit('');
  // at least 1 input pattern: return string
  Result := StringOfChar('-',Length(Input[0]));
  // iterate over all input patterns and over all its chars and mark places
  For I := 0 to Length(Input)-1 do
    For J := 1 to Length(Input[I]) do
      if Input[I][J] <> '-' then
        Result[J] := '1';
End;

Function TTransition.GetNumObservedInputs:Integer;
Begin
  Result := CountChars('1',GetObservedInputs);
End;

(**
 * Merge all observed inputs and return the total number
 *
 * If no input patterns are defined, 0 is returned.
 *)
Function TTransition.CountObservedInputs:Integer;
Var I  : Integer;
    St : String;
Begin
  St := GetObservedInputs;
  Result := 0;
  For I := 1 to Length(St) do
    if St[I] = '1' then
      Inc(Result);
End;

{ TFSMDefinition }

Constructor TFSMDefinition.Create(AName:String);
Begin
  inherited Create;
  FName := AName;
End;

Constructor TFSMDefinition.Create(AFSMDefinition:TFSMDefinition);
Begin
  // copy constructor
  inherited Create;
  FName        := AFSMDefinition.FName;
  FInputs      := AFSMDefinition.FInputs;
  FOutputs     := AFSMDefinition.FOutputs;
  FStates      := AFSMDefinition.FStates;
  FTransitions := AFSMDefinition.FTransitions;
  FResetState  := AFSMDefinition.FResetState;
  FOutReg      := AFSMDefinition.FOutReg;
End;

Procedure TFSMDefinition.AddInput(AInput:String);
Begin
  SetLength(FInputs,Length(FInputs)+1);
  FInputs[Length(FInputs)-1] := AInput;
End;

Procedure TFSMDefinition.AddOutput(AOutput:String);
Begin
  SetLength(FOutputs,Length(FOutputs)+1);
  FOutputs[Length(FOutputs)-1] := AOutput;
End;

Procedure TFSMDefinition.AddState(AState:String);
Begin
  SetLength(FStates,Length(FStates)+1);
  FStates[Length(FStates)-1] := AState;
End;

Procedure TFSMDefinition.Add(AState:TStateIndex;AInput:TInputPatterns;ANextState:TStateIndex;AOutput:TOutputPattern);
Begin
  SetLength(FTransitions,Length(FTransitions)+1);
  With FTransitions[Length(FTransitions)-1] do
    Begin
      State     := AState;
      Input     := AInput;
      NextState := ANextState;
      Output    := AOutput;
    End;
End;

Procedure TFSMDefinition.Add(AState:String;AInput:TInputPatterns;ANextState:String;AOutput:TOutputPattern);
Var I : Integer;
Begin
  // check input patterns
  For I := 0 to Length(AInput)-1 do
    if Length(AInput[I]) <> Length(FInputs) then
      raise Exception.CreateFmt('Input pattern %d "%s" specifies wrong number of Inputs (%d, need %d)',[I,AInput[I],Length(AInput),Length(FInputs)]);
  // check output pattern
  if Length(AOutput) <> Length(FOutputs) then
    raise Exception.CreateFmt('Output pattern "%s" specifies wrong number of outputs (%d, need %d)',[AOutput,Length(AOutput),Length(FOutputs)]);
  // add transition
  Add(GetState(AState),AInput,GetState(ANextState),AOutput);
End;

Procedure TFSMDefinition.SetResetState(AResetState:String);
Begin
  GetState(AResetState);  // will raise an exception if the state doesn't exist
  FResetState := AResetState;
End;

Function TFSMDefinition.TryGetInput(AInput:String):TInputIndex;
Begin
  For Result := 0 to Length(FInputs)-1 do
    if AInput = FInputs[Result] then Exit;
  Result := -1;
End;

Function TFSMDefinition.TryGetOutput(AOutput:String):TOutputIndex;
Begin
  For Result := 0 to Length(FOutputs)-1 do
    if AOutput = FOutputs[Result] then Exit;
  Result := -1;
End;

Function TFSMDefinition.TryGetState(AState:String):TStateIndex;
Begin
  For Result := 0 to Length(FStates)-1 do
    if AState = FStates[Result] then Exit;
  Result := -1;
End;

Function TFSMDefinition.GetInput(AInput:String):TInputIndex;
Begin
  For Result := 0 to Length(FInputs)-1 do
    if AInput = FInputs[Result] then Exit;
  raise Exception.Create('Invalid input "'+AInput+'"');
End;

Function TFSMDefinition.GetOutput(AOutput:String):TOutputIndex;
Begin
  For Result := 0 to Length(FOutputs)-1 do
    if AOutput = FOutputs[Result] then Exit;
  raise Exception.Create('Invalid output "'+AOutput+'"');
End;

Function TFSMDefinition.GetState(AState:String):TStateIndex;
Begin
  For Result := 0 to Length(FStates)-1 do
    if AState = FStates[Result] then Exit;
  raise Exception.Create('Invalid state "'+AState+'"');
End;

Function TFSMDefinition.GetInputPattern(AInputs:TSignalDefinition):TInputPattern;
Var I : Integer;
    C : Char;
Begin
  Result := StringOfChar('-',Length(FInputs));   // default: don't care
  For I := 0 to Length(AInputs)-1 do
    Begin
      Case AInputs[I].Value of
        0 : C := '0';
        1 : C := '1';
      else
        C := '-';
      End;
      Result[GetInput(AInputs[I].Signal)+1] := C;
    End;
End;

(**
 *
 * @param AInputs   const array with alternating string and integer
 *)
Function TFSMDefinition.GetInputPattern(Const AInputs : Array of Const):TInputPattern;
Var I : Integer;
    C : Char;
Begin
  if Length(AInputs) and $01 <> 0 then
    raise Exception.Create('Need an even number of input pattern definitions: [''In0'',1]');
  Result := StringOfChar('-',Length(FInputs));   // default: don't care
  I := 0;
  While I < Length(AInputs) do
    Begin
      if AInputs[I+1].Vtype <> vtInteger then
        raise Exception.CreateFmt('Need a signal value as integer as parameter %d',[I+1]);
      Case AInputs[I+1].VInteger of
        0 : C := '0';
        1 : C := '1';
      else
        C := '-';
      End;
      Case AInputs[I].Vtype of
        vtString     : Result[GetInput(AInputs[I].VString^)               +1] := C;
        vtAnsiString : Result[GetInput(AnsiString(AInputs[I].VAnsiString))+1] := C;
      else
        raise Exception.CreateFmt('Need a signal name as string as parameter %d',[I]);
      End;
      Inc(I,2);
    End;
End;

Function TFSMDefinition.GetInputPatterns(Const AInputPatterns:Array Of Const):TInputPatterns;
Var I : Integer;
Begin
  SetLength(Result,Length(AInputPatterns));
  For I := 0 to Length(AInputPatterns)-1 do
    Begin
      Case AInputPatterns[I].Vtype of
        vtString     : Result[I] := AInputPatterns[I].VString^;
        vtAnsiString : Result[I] := AnsiString(AInputPatterns[I].VAnsiString);
      else
        raise Exception.CreateFmt('Need a signal name as string as parameter %d',[I]);
      End;
    End;
End;

Function TFSMDefinition.GetOutputPattern(AOutputs:TSignalDefinition):TOutputPattern;
Var I : Integer;
    C : Char;
Begin
  Result := StringOfChar('X',Length(FOutputs));   // default: invalid
  For I := 0 to Length(AOutputs)-1 do
    Begin
      Case AOutputs[I].Value of
        0 : C := '0';
        1 : C := '1';
      else
        C := '-';
      End;
      Result[GetOutput(AOutputs[I].Signal)+1] := C;
    End;
  { check that all outputs were set }
  if Pos('X',Result) > 0 then
    raise Exception.Create('Missing output definitions for "'+Result+'"');
End;

Function TFSMDefinition.GetPortDirection(APort:String):TSEDirection;
Begin
  if TryGetInput(APort) >= 0 then
    Exit(sedInput);
  if TryGetOutput(APort) >= 0 then
    Exit(sedOutput);
  raise Exception.CreateFmt('Port "%s" is neither an input nor an output, i.e. it is not a port of this FSM',[APort]);
End;

Function TFSMDefinition.GetObserved:TDynIntegerArray;
Var I : Integer;
Begin
  SetLength(Result, Length(FInputs)+1);   // 0 .. Length(FInputs)
  FillChar(Result[0],Length(Result)*SizeOf(Result[0]),0);
  For I := 0 to Length(FTransitions)-1 do
    Inc(Result[FTransitions[I].CountObservedInputs]);
End;

(**
 *
 * Result: array of strings with errors / warnings
 *)
Function TFSMDefinition.Check : TDynStringArray;
Var IntArr : TDynIntegerArray;
    I,J,K  : Integer;
    CurrentState : String;

  Procedure ClearIntArr;
  Var I : Integer;
  Begin
    For I := 0 to Length(IntArr)-1 do
      IntArr[I] := 0;
  End;

  Procedure PrepareIntArr(Length:Integer);
  Begin
    SetLength(IntArr,Length);
    ClearIntArr;
  End;

  Procedure AddError(Msg:String);
  Begin
    SetLength(Result,Length(Result)+1);
    Result[Length(Result)-1] := Msg;
  End;

  Procedure MarkInputPattern(Input:String);

    (**
     * Create value from input pattern filling don't cares
     *)
    Function PatternToValue(Pattern:String;DontCareIndex:Integer):Integer;
    Var I : Integer;
    Begin
      Result := 0;
      For I := Length(Pattern) downto 1 do
        Begin
          Result := Result shl 1;
          if Pattern[I] = '1' then
            Result := Result or 1
          else if Pattern[I] = '-' then
            Begin
              Result := Result or (DontCareIndex and $01);
              DontCareIndex := DontCareIndex shr 1;
            End;
        End;
    End;

  Var NumDontCare,I,Value : Integer;
  Begin
    NumDontCare := CountChars('-',Input);
    // iterate over all don't care combinations
    For I := 0 to (1 shl NumDontCare)-1 do
      Begin
        Value := PatternToValue(Input,I);
        //WriteLn(Input,' (',I:3,'): ',IntToBinLM(Value,Length(Input)));
        if IntArr[Value shr (2+3)] and (1 shl (Value and ((1 shl (2+3))-1))) <> 0 then
          AddError('ERROR: State "'+CurrentState+'": Input value '+IntToBinLM(Value,Length(Input))+' is covered multiple times')
        else
          IntArr[Value shr (2+3)] := IntArr[Value shr (2+3)] or (1 shl (Value and ((1 shl (2+3))-1)));
      End;
  End;

Begin
  SetLength(Result,0);
  (*
   * Check that all transition's State, NextState, input and output patterns
   * are valid
   *)
  For I := 0 to Length(FTransitions)-1 do
    Begin
      // TODO
    End;
  (*
   * Check that there is only one item in FTransitions for every conbination
   * State->NextState
   *)
(*  For I := 0 to Length(FStates)-1 do
    Begin
      // iterate over all states and look for all next-states in the transitions
      PrepareIntArr(Length(FStates));
      For J := 0 to Length(FTransitions)-1 do
        if FTransitions[J].State = I then
          Begin
            Inc(IntArr[FTransitions[J].NextState]);
            if IntArr[FTransitions[J].NextState] > 1 then
              AddError('ERROR: '+IntToStr(IntArr[FTransitions[J].NextState])+'. transition from '+FStates[I]+' to '+FStates[FTransitions[J].NextState]+' (input: "'+FTransitions[J].InputString+'", output: "'+FTransitions[J].Output+'")');
          End;
    End;
Really? what is for transitions with different outputs but the same start and
destination state?
*)

  (*
   * Check that every state except the first state (= reset state) has at least
   * one transition to enter
   *)
  PrepareIntArr(Length(FStates));
  // count number of entering transitions
  For I := 0 to Length(FTransitions)-1 do
    Inc(IntArr[FTransitions[I].NextState]);
  For I := 1 to Length(IntArr)-1 do
    if IntArr[I] = 0 then
      AddError('ERROR: State '+FStates[I]+' has no entering transitions');

  (*
   * Check that every state has at least one transition to leave (optional)
   *)
  PrepareIntArr(Length(FStates));
  For I := 0 to Length(FTransitions)-1 do
    Inc(IntArr[FTransitions[I].State]);
  For I := 1 to Length(IntArr)-1 do
    if IntArr[I] = 0 then
      AddError('ERROR: State '+FStates[I]+' has no leaving transitions');

  (*
   * Check that every state's leaving transitions cover all input combinations
   *)
  // Simplest algorithm:
  //  1) make an set with 2^Length(FInputs) bits
  //  2) go through all transitions
  //    A) go through all input pattern combinations (don't cares create more!)
  //      a) check whether the bit is already set -> error
  //      b) mark the according bits
  //    B) search unset bits -> error
  //
  // Improvement:
  //  A) combine all input patterns for a given transition to find out which
  //     pins are observed and which are not
  //  B) use only 2^Used(FInputs) bits
  //
  // but this is not very smart :-(

  // use IntArr as bit-vector
  if Length(FInputs) > 16 then
    raise Exception.CreateFmt('Can''t check FSMs with %d inputs with current algorithm. Maximum is %d.',[Length(FInputs),16]);
  I := 1 shl Length(FInputs);    // number of bits we need
  I := I shr (2 + 3);            // divide by number of bits in an IntArr element (4 bytes with 8 bits each -> 32 bits)
  // e.g. N = 8 -> I = 1 shl 8 = $0100 = 256 -> I = I shr 5 -> $0008 = 8
  // e.g. N = 5 -> I = 1 shl 5 = $0020 =  32 -> I = I shr 5 -> $0001 = 1
  // e.g. N = 3 -> I = 1 shl 3 = $0008 =>  8 -> I = I shr 5 -> $0000 = 0
  PrepareIntArr(max(1,I));
  // go through every starting state
  For I := 0 to Length(FStates)-1 do
    Begin
      ClearIntArr;
      CurrentState := FStates[I];
      // iterate over all transitions and look for all states
      For J := 0 to Length(FTransitions)-1 do
        With FTransitions[J] do
          Begin
            if State <> I then
              Continue;
            // ok, we have a transition starting from State I, now look at all
            // its input patterns and mark them
            For K := 0 to Length(Input)-1 do
              MarkInputPattern(Input[K]);
            // TODO: improvement: within one all patterns of one transition we
            // should allow overlaps, only between different transitions
            // starting from the same we should complain
          End;
      // now all transitions starting from state I have been considered
      // Check if all combinations are met
      For J := 0 to (1 shl Length(FInputs))-1 do
        if IntArr[J shr (2+3)] and (1 shl (J and ((1 shl (2+3))-1))) = 0 then
          AddError('ERROR: State "'+CurrentState+'": Input value '+IntToBinLM(J,Length(FInputs))+' is not covered by any transition')
    End;

  // Waaaay better algorithm:
  // When the above one is analogous to a pixel image, this one is analogous
  // to a vector image.
  // Let N = Length(FInputs), assume an N-dimensional space with coordinates 0
  // and 1 at every dimension. Then every input pattern represents a unique
  // position in this space. Input patterns with M don't-cares represent an
  // M-dimensional rectangular solid.
  //
  // Therefore for every input pattern we allocate a rectangular solid, which
  // is defined by two points in the N-dimensional space. These can then be
  // merged/united/joined to larger rectangular solids. No overlaps must
  // occur and the final result must be a solid which spans from (0,0,0...) to
  // (1,1,1...).

  // Implementation idea of this algorithm:
  // Merging two rectangles can be done on a dimension by dimension base, i.e.
  // merge on one dimension at a time. E.g.
  //    10---, 11---  ->  1----
  // This can even happen without translating the patterns to dimensions but
  // simply using the strings.
  // 1) Sort all strings alphabetically (-> next point is only o(n) instead of
  //    o(n^2))
  // 2) if two neighboring only differ in one place which is '0' and '1'
  //    -> replace both by one string with a '-' at this place
  //    -> insert at sorted position
  // 3) repeat until no further merges are possible
  // 4) if the result is '-----' -> done
  //    else: complain
  // How to complain? It is difficult to say whether a combination is twice or
  // it is missing. Simply reporting the remaining patterns would delegate this
  // evaluation to the user.
  // One more idea to complain: With every combined vector also collect the list
  // of original vectors (e.g. "1---" was combined from "10--,110-,111-").

  (*
   * Check that no leaving transitions overlap input patterns
   *)
  // TODO
End;

Procedure TFSMDefinition.Print;
Var I,J      : Integer;
    Observed : Array of Integer;
Begin
  WriteLn(Length(FInputs) :2,' Inputs:  (',Join(',',FInputs),')');
  WriteLn(Length(FOutputs):2,' Outputs: (',Join(',',FOutputs),')');
  WriteLn(Length(FStates) :2,' States:  (',Join(',',FStates),')');
  if FResetState > '' then
    WriteLn('Reset: ',FResetState);
  WriteLn('Outputs ',Select('','not ',FOutReg),'registered');
  { histogram of observed inputs }
  Observed := GetObserved;
  // find maximum
  For I := Length(Observed)-1 downto 0 do
    if Observed[I] <> 0 then
      Begin
        J := I;
        Break;
      End;
  WriteLn('Inputs observed by Transitions:');
  For I := 0 to J do
    WriteLn('  ',I,' inputs: ',Observed[I],' transitions');

  WriteLn(Length(FTransitions),' Transitions:');
  { sort transitions (better: create an index-list instead of sorting) }
  // TODO
  For I := 0 to Length(FTransitions)-1 do
    With FTransitions[I] do
      WriteLn('  ',FStates[State],' ',InputString,' (',ObservedInputs,') ',' => ',Output,' ',FStates[NextState]);
End;

Function TFSMDefinition.GetInputCount:Integer;
Begin
  Result := Length(FInputs);
End;

Function TFSMDefinition.GetOutputCount:Integer;
Begin
  Result := Length(FOutputs);
End;

Function TFSMDefinition.GetStateCount:Integer;
Begin
  Result := Length(FStates);
End;

Function TFSMDefinition.GetTransitionCount:Integer;
Begin
  Result := Length(FTransitions);
End;

Function TFSMDefinition.GetInput(Index:Integer):String;
Begin
  if Index >= Length(FInputs) then
    raise Exception.CreateFmt('Input %d out of range [0..%d]',[Index,Length(FInputs)-1]);
  Result := FInputs[Index];
End;

Function TFSMDefinition.GetOutput(Index:Integer):String;
Begin
  if Index >= Length(FOutputs) then
    raise Exception.CreateFmt('Output %d out of range [0..%d]',[Index,Length(FOutputs)-1]);
  Result := FOutputs[Index];
End;

Function TFSMDefinition.GetState(Index:Integer):String;
Begin
  if Index >= Length(FStates) then
    raise Exception.CreateFmt('State %d out of range [0..%d]',[Index,Length(FStates)-1]);
  Result := FStates[Index];
End;

Function TFSMDefinition.GetTransition(Index:Integer):TTransition;
Begin
  if Index >= Length(FTransitions) then
    raise Exception.CreateFmt('Transition %d out of range [0..%d]',[Index,Length(FTransitions)-1]);
  Result := FTransitions[Index];
end;

End.

