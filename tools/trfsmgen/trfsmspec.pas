Unit TRFSMSpec;

{$mode objfpc}{$H+}

Interface

Uses
  Classes, SysUtils, FGL, Utils;

Type

  { TTRFSMSpecification }

  TTRFSMSpecification = class
  private
    FVersion   : String;
    FInputs    : Integer;
    FOutputs   : Integer;
    FStateBits : Integer; // number of bits of the state vector
    FNumTRs    : TDynIntegerArray;
    Function GetMaxTRWidth:Integer; inline;
    Function GetNumTRs(Index:Integer):Integer; inline;
    Function GetStates:Integer; inline;
    Function GetTotalTRs:Integer; inline;
  public
    Constructor Create(AVersion:String;AInputs,AOutputs,AStateBits:Integer;ANumTRs:TDynIntegerArray);
    Procedure Print;
    property Version   : String  read FVersion;
    property Inputs    : Integer read FInputs;
    property Outputs   : Integer read FOutputs;
    property StateBits : Integer read FStateBits;
    property States    : Integer read GetStates;
    property NumTRs[Index: Integer]  : Integer read GetNumTRs;
    property MaxTRWidth : Integer read GetMaxTRWidth;
    property TotalTRs  : Integer read GetTotalTRs;
  End;

Implementation

{ TTRFSMSpecification }

Constructor TTRFSMSpecification.Create(AVersion:String;AInputs,AOutputs,AStateBits:Integer;ANumTRs:TDynIntegerArray);
Begin
  if (AVersion <> 'SNOPS1') and (AVersion <> 'REG') then
    raise Exception.Create('Invalid TR-FSM version '''+AVersion+'''');
  FVersion   := AVersion;
  FInputs    := AInputs;
  FOutputs   := AOutputs;
  FStateBits := AStateBits;
  FNumTRs    := ANumTRs;
  // TODO: Check what happens if the last entries in FNumTRs have the value 0,
  // i.e. if 0 TRs with the maximum given width are used. Is anybody assuming
  // that the last number is >0? Should we clean up the array here?
End;

Procedure TTRFSMSpecification.Print;
Var I : Integer;
Begin
  WriteLn(FInputs,' inputs, ',FOutputs,' outputs, ',FStateBits,' state bits');
  For I := 0 to Length(FNumTRs)-1 do
    WriteLn('  TR-',I,': ',FNumTRs[I]);
End;

Function TTRFSMSpecification.GetStates:Integer;
Begin
  Result := 1 shl FStateBits;
End;

Function TTRFSMSpecification.GetNumTRs(Index:Integer):Integer;
Begin
  Result := FNumTRs[Index];
End;

Function TTRFSMSpecification.GetMaxTRWidth:Integer;
Begin
  Result := Length(FNumTRs)-1;
End;

Function TTRFSMSpecification.GetTotalTRs:Integer;
Var I : Integer;
Begin
  Result := 0;
  For I := 0 to Length(FNumTRs)-1 do
    Result := Result + FNumTRs[I];
End;

End.

