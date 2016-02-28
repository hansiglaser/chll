Unit Bitstream;

{$mode objfpc}{$H+}

Interface

Uses
  Classes, SysUtils, Utils;

Type
  TDynByteArray = Array of Byte;

  { TBitstream }

  (**
   * Bitstream handling
   *
   * A (configuration) bit stream is stored and accessed with this class. The
   * LSB (i.e. GetBit(0), i.e. FBitstream[0] and $01) corresponds to the first
   * bit shifted into the chain and therefore corresponds to the last
   * flip-flop!
   *
   * This also means that the output of GetString is reversed compared to the
   * order of the flip-flops, i.e. the first character corresponds to the last
   * flip-flop. The function GetStringReverse returns the contents of the
   * first flipflop first.
   *
   * TODO: Design Improvements:
   *  1) generalize TBitstream
   *    a) use a PByteArray and GetMem/FreeMem for the real data
   *    b) add a "FOffset" field and the possibility to use a foreign
   *       FBitstream, this allows sub-bitstreams
   *  2) Instead of inheriting from TBitstream (e.g. to TTRFSMBitsteam) use
   *     the TBitsteam as an instance variable in these classes which are
   *     supplied from outside (or something like a factory pattern).
   *)
  TBitstream = class
  private
    FBitstream : TDynByteArray;
    FLength    : Integer;
  protected
    Procedure SetBit(Pos:Integer;Value:Boolean=true);
    Procedure SetBits(Start:Integer;Const Bits:Array of Byte;Length:Integer);   // intentionally left as "Array of Byte" because the compiler does not like a typecast of e.g. an Array[0..4] to a TByteArray :-(
    Procedure SetBits(Start:Integer;Bits:String);
    Procedure SetBits(Start:Integer;Bits:TBitstream);
    Procedure SetBits(Start:Integer;Bits:Integer;Length:Integer);
    Function  GetBit(Pos:Integer):Boolean;
    Function  GetBitNoCheck(Pos:Integer):Boolean; inline;
    Procedure GetBits(Start:Integer;Out Bits:Array of Byte;Length:Integer);
    Function  GetBits(Start:Integer;Length:Integer) : TDynByteArray;
    Function  GetBitCount : Integer;
  public
    Constructor Create(ALength:Integer);
    Constructor Create(ABits : String);
    Destructor  Destroy; override;
    Procedure Clear;
    Procedure ClearWithOnes;
    class Function  GetString   (Const ABitstream:TByteArray;ALength:Integer) : String;
    class Function  GetString   (Const ABitstream:TByteArray;ALength:Integer;AGroup:Integer) : String;
    class Function  GetStringReverse(Const ABitstream:TByteArray;ALength:Integer) : String;
    class Function  GetStringReverse(Const ABitstream:TByteArray;ALength:Integer;AGroup:Integer) : String;
    class Function  GetHexString(Const ABitstream:TByteArray;ALength:Integer) : String;
    class Function  GetHexStringReverse(Const ABitstream:TByteArray;ALength:Integer):String;
    class Function  GetCArray   (Const ABitstream:TByteArray;ALength:Integer) : String;
    class Function  GetString   (ABitstream:TDynByteArray;ALength:Integer) : String;
    class Function  GetString   (ABitstream:TDynByteArray;ALength:Integer;AGroup:Integer) : String;
    class Function  GetStringReverse(ABitstream:TDynByteArray;ALength:Integer) : String;
    class Function  GetStringReverse(ABitstream:TDynByteArray;ALength:Integer;AGroup:Integer) : String;
    class Function  GetHexString(ABitstream:TDynByteArray;ALength:Integer) : String;
    class Function  GetHexStringReverse(ABitstream:TDynByteArray;ALength:Integer) : String;
    class Function  GetCArray   (ABitstream:TDynByteArray;ALength:Integer) : String;
    class Function  CountBits(Bits:String) : Integer;
    class Function  String2Bits(Bits:String;Out Bitstream:TDynByteArray):Integer;
    class Function  IsEqual(Const A,B:Array of Byte;Length:Integer) : Boolean;
    Function  GetString : String;
    Function  GetStringReverse : String;
    Function  GetString(Group:Integer) : String;
    Function  GetStringReverse(Group:Integer) : String;
    Function  GetHexString : String;
    Function  GetHexStringReverse : String;
    Function  GetCArray : String;
    Function  GetModelSimTCLString(Instance:String)        :String; virtual;
    Function  GetLECString        (Instance,Options:String):String; virtual;
    Function  GetFormalityString  (Instance:String)        :String; virtual;
    property Count : Integer read FLength;
    property BitAt[Index:Integer] : Boolean read GetBit; default;
  End;


  { TCustomBitstream }

  TCustomBitstream = class(TBitstream)   // this is not good design style, we should make individual classes for every single type of bitsteam
  public
    { change visibility to public for a few methods }
    Procedure SetBit(Pos:Integer;Value:Boolean=true);
    Procedure SetBits(Start:Integer;Bits:String);
    Procedure SetBits(Start:Integer;Bits:TBitstream);
    Procedure SetBits(Start:Integer;Bits:Integer;Length:Integer);
    { simpler functions }
    Procedure SetBit (Value:Boolean=true);
    Procedure SetBits(Bits:String);
    Procedure SetBits(Bits:TBitstream);
    Procedure SetBits(Bits:Integer);
  End;

  TBitstreamClass = class of TBitstream;
  TBitstreamArray = Array of TBitstream;

  { TMultiBitstream }

  (**
   * Combination of multiple bitstreams into one
   *)
  TMultiBitstream = class(TBitstream)
  private
    FSubBitstreams : TBitstreamArray;
  public
    Constructor Create(Const ASubBitstreams : Array of Const);
    Constructor Create(Const ASubBitstreams:TBitstreamArray);
    Procedure Copy;
    Function  GetGroupedString : String;
    Class Procedure AddPart(Var AParts:TBitstreamArray;ABitstream:TBitstream);
  End;

Implementation

{ TBitstream }

Constructor TBitstream.Create(ALength:Integer);
Begin
  if ALength < 1 then
    raise Exception.Create('Minimum length is 1 bit');
  FLength := ALength;
  // allocate memory
  SetLength(FBitstream,(FLength+7) shr 3);
  Clear;
End;

(**
 * Character to Bit Mapping:
 *   ABits[1] = Bit(0)
 *   ABits[2] = Bit(1)
 *   ...
 * i.e. the string prints reversed to a VHDL vector
 *)
Constructor TBitstream.Create(ABits:String);
Var Bitstream : TDynByteArray;
    BitLen    : Integer;
Begin
  BitLen := String2Bits(ABits,Bitstream);
  Create(BitLen);
  SetBits(0,PByteArray(@(Bitstream[0]))^,BitLen);
End;

Destructor TBitstream.Destroy;
Begin
  SetLength(FBitstream,0);
  Inherited Destroy;
End;

Procedure TBitstream.Clear;
Begin
  // clear memory
  Fillchar(FBitstream[0],(FLength+7) shr 3,0);
End;

Procedure TBitstream.ClearWithOnes;
Begin
  // clear memory
  Fillchar(FBitstream[0],(FLength+7) shr 3,$FF);
End;

Procedure TBitstream.SetBit(Pos:Integer;Value:Boolean);
Begin
  if Pos > FLength then
    raise Exception.Create('Destination out of bounds');
  if Value then
    FBitstream[Pos shr 3] := FBitstream[Pos shr 3] or (1 shl (Pos and $07))
  else
    FBitstream[Pos shr 3] := FBitstream[Pos shr 3] and (not (1 shl (Pos and $07)));
End;

Procedure TBitstream.SetBits(Start:Integer;Const Bits:Array of Byte;Length:Integer);
Var Pos   : Integer;
    SPos  : Integer;
    Mask  : Byte;
    Shift : Integer;
Begin
  if Length = 0 then Exit;
  if (Start < 0) or (Length < 0) or (Start + Length > FLength) then
    raise Exception.Create('Destination out of bounds');
  Shift := Start and $07;
  if Shift = 0 then
    Begin
      { Byte border aligned }
      { 1st: copy bytes }
      if Length shr 3 <> 0 then
        Move(Bits[0],FBitstream[Start shr 3],Length shr 3);
      { 2nd: do bit operations on the last, not fully filled byte }
      if Length and $07 <> 0 then
        Begin
          Pos  := (Start + Length) shr 3;
          Mask := Integer(1 shl (Length and $07)) - 1;   // e.g. Length = 3 -> 00001000 -> 00000111
          FBitstream[Pos] := (FBitstream[Pos] and (not Mask)) or (Bits[Length shr 3] and Mask);
        End;
    End
  else
    Begin
      { not aligned at Byte borders, we have to shift around }
      { 1st: set first byte }
      Mask := Integer(1 shl Shift) - 1;   // e.g. Shift = 3 -> 00001000 -> 00000111
      if Length < 8-Shift then  // special case: Length+Shift < 8 bits
        Mask := Mask or (not (Integer(1 shl (Length+Shift))-1));
      Pos  := Start shr 3;
      SPos := 0;
      FBitstream[Pos] := (FBitstream[Pos] and Mask) or ((Bits[SPos] shl Shift) and (not Mask));
      Inc(Pos);
      Length := Length - (8-Shift);
      { 2nd: set full bytes }
      While Length >= 8 do
        Begin
          FBitstream[Pos] := (Integer(Integer(Bits[SPos]) or (Integer(Bits[SPos+1]) shl 8)) shr (8 - Shift)) and $FF;
          Inc(Pos);
          Inc(SPos);
          Dec(Length,8);
        End;
      { 3rd: set last byte }
      if Length > 0 then
        Begin
          Mask := Integer(1 shl (Length and $07)) - 1; // e.g. Length = 6 -> 01000000 -> 00111111
          FBitstream[Pos] := (FBitstream[Pos] and (not Mask)) or ((Integer(Integer(Bits[SPos]) or (Integer(Bits[SPos+1]) shl 8)) shr (8 - Shift)) and Mask);
        End;
    End;
End;

Procedure TBitstream.SetBits(Start:Integer;Bits:String);
Var Bitstream : TDynByteArray;
    BitLen    : Integer;
Begin
  BitLen := String2Bits(Bits,Bitstream);
  SetBits(Start,PByteArray(@(Bitstream[0]))^,BitLen);
End;

Procedure TBitstream.SetBits(Start:Integer;Bits:TBitstream);
Begin
  SetBits(Start,PByteArray(@(Bits.FBitstream[0]))^,Bits.FLength);
End;

Procedure TBitstream.SetBits(Start:Integer;Bits:Integer;Length:Integer);
Begin
  SetBits(Start,PByteArray(@(Bits))^,Length);
End;

Function TBitstream.GetBit(Pos:Integer):Boolean;
Begin
  if (Pos < 0) or (Pos >= FLength) then
    raise Exception.CreateFmt('Position %d out of bounds 0..%d',[Pos,FLength-1]);
  Result := (FBitstream[Pos shr 3] and (1 shl (Pos and $07))) <> 0;
End;

Function TBitstream.GetBitNoCheck(Pos:Integer):Boolean;Inline;
Begin
  Result := (FBitstream[Pos shr 3] and (1 shl (Pos and $07))) <> 0;
End;

Procedure TBitstream.GetBits(Start:Integer;Out Bits:Array of Byte;Length:Integer);
Var DPos  : Integer;
    SPos  : Integer;
    Mask  : Byte;
    Shift : Integer;
Begin
  if Start + Length > FLength then
    raise Exception.Create('Destination out of bounds');
  Shift := Start and $07;
  if Shift = 0 then
    Begin
      { Byte border aligned }
      { 1st: copy bytes }
      if Length shr 3 <> 0 then
        Move(FBitstream[Start shr 3],Bits[0],Length shr 3);
      { 2nd: do bit operations on the last, not fully filled byte }
      if Length and $07 <> 0 then
        Begin
          SPos := (Start + Length) shr 3;
          Mask := Integer(1 shl (Length and $07)) - 1;   // e.g. Length = 3 -> 00001000 -> 00000111
          Bits[Length shr 3] := FBitstream[SPos] and Mask;
        End;
    End
  else
    Begin
      { not aligned at Byte borders, we have to shift around }
      { 1st: get full bytes }
      DPos := 0;
      SPos := Start shr 3;
      While Length shr 3 <> 0 do
        Begin
          Bits[DPos] := (Integer(Integer(FBitstream[SPos]) or (Integer(FBitstream[SPos+1]) shl 8)) shr Shift) and $FF;
          Inc(DPos);
          Inc(SPos);
          Dec(Length,8);
        End;
      { 2nd: set last byte }
      Mask := Integer(1 shl Length) - 1;   // e.g. Remaining Bits = 3 -> 00001000 -> 00000111
      Bits[DPos] := (Integer(Integer(FBitstream[SPos]) or (Integer(FBitstream[SPos+1]) shl 8)) shr Shift) and Mask;
    End;
End;

Function TBitstream.GetBits(Start:Integer;Length:Integer):TDynByteArray;
Begin
  SetLength(Result,(Length+7) shr 3);
  GetBits(Start,PByteArray(@(Result[0]))^,Length);
End;

Function TBitstream.GetBitCount:Integer;
Var I : Integer;
Begin
  Result := 0;
  For I := 0 to FLength-1 do
    if FBitstream[I shr 3] and (1 shl (I and $07)) <> 0 then
      Inc(Result);
End;

Class Function TBitstream.GetString(Const ABitstream:TByteArray;ALength:Integer):String;
Var I : Integer;
Begin
  SetLength(Result,ALength);
  Fillchar(Result[1],ALength ,'0');  // fill with '0', set to '1' or '0'
  For I := 0 to ALength-1 do
    Begin
      if ABitstream[I shr 3] and (1 shl (I and $07)) <> 0 then
        Result[I+1] := '1';  // '0' is default
    End;
End;

Class Function TBitstream.GetString(Const ABitstream:TByteArray;ALength:Integer;AGroup:Integer):String;
Var I : Integer;
    P : Integer;
Begin
  SetLength(Result,ALength + ((ALength-1) div AGroup));
  Fillchar(Result[1],ALength + ((ALength-1) div AGroup),'0');  // fill with '0', set to '1' or '0'
  P := 1;
  For I := 0 to ALength-1 do
    Begin
      if ABitstream[I shr 3] and (1 shl (I and $07)) <> 0 then
        Result[P] := '1';  // '0' is default
      Inc(P);
      if (I+1) mod AGroup = 0 then
        Begin
          Result[P] := ' ';
          Inc(P);
        End;
    End;
End;

Class Function TBitstream.GetStringReverse(Const ABitstream:TByteArray;ALength:Integer):String;
Var I : Integer;
Begin
  SetLength(Result,ALength);
  Fillchar(Result[1],ALength ,'0');  // fill with '0', set to '1' or '0'
  For I := 0 to ALength-1 do
    Begin
      if ABitstream[I shr 3] and (1 shl (I and $07)) <> 0 then
        Result[ALength-I] := '1';  // '0' is default
    End;
End;

Class Function TBitstream.GetStringReverse(Const ABitstream:TByteArray;ALength:Integer;AGroup:Integer):String;
Var I : Integer;
    P : Integer;
Begin
  SetLength(Result,ALength + ((ALength-1) div AGroup));
  Fillchar(Result[1],ALength + ((ALength-1) div AGroup),'0');  // fill with '0', set to '1' or '0'
  P := Length(Result);
  For I := 0 to ALength-1 do
    Begin
      if ABitstream[I shr 3] and (1 shl (I and $07)) <> 0 then
        Result[P] := '1';  // '0' is default
      Dec(P);
      if (I+1) mod AGroup = 0 then
        Begin
          Result[P] := ' ';
          Dec(P);
        End;
    End;
End;

Const HexDigits  = '0123456789ABCDEF';

Class Function TBitstream.GetHexString(Const ABitstream:TByteArray;ALength:Integer):String;
Var I : Integer;
Begin
  SetLength(Result,(ALength+3) shr 2);
  For I := 0 to ((ALength+3) shr 2)-1 do
    Begin
      if I and $01 = 0 then
        Result[I+1] := HexDigits[(ABitstream[I shr 1] and $0F)+1]
      else
        Result[I+1] := HexDigits[(ABitstream[I shr 1] shr 4)+1];
    End;
End;

(**
 * Get Hex string as it was a number, i.e. right-most digit is LSB
 *)
Class Function TBitstream.GetHexStringReverse(Const ABitstream:TByteArray;ALength:Integer):String;
Var I : Integer;
    L : Integer;
Begin
  L := (ALength+3) shr 2;
  SetLength(Result,L);
  For I := 0 to L-1 do
    Begin
      if I and $01 = 0 then
        Result[L-I] := HexDigits[(ABitstream[I shr 1] and $0F)+1]
      else
        Result[L-I] := HexDigits[(ABitstream[I shr 1] shr 4)+1];
    End;
End;

Class Function TBitstream.GetCArray(Const ABitstream:TByteArray;ALength:Integer):String;
Var I,P : Integer;
    B   : Byte;
Begin
  SetLength(Result, 5 * (ALength shr 3) + 4 + (ALength shr (3+4)));
  P := 0;  // pointer in result string
  { 1st to one-before-last byte }
  For I := 0 to (ALength shr 3)-1 do
    Begin
      B := ABitstream[I];
      Inc(P); Result[P] := '0';
      Inc(P); Result[P] := 'x';
      Inc(P); Result[P] := HexDigits[(B shr 4)+1];
      Inc(P); Result[P] := HexDigits[(B and $0F)+1];
      Inc(P); Result[P] := ',';
      { append newline every 16 bytes }
      if I and $0F = $0F then
        Begin
          Inc(P); Result[P] := ^J;
        End;
    End;
  { last byte }
  B := ABitstream[ALength shr 3] and ((1 shl (ALength and $07))-1);
  Inc(P); Result[P] := '0';
  Inc(P); Result[P] := 'x';
  Inc(P); Result[P] := HexDigits[(B shr 4)+1];
  Inc(P); Result[P] := HexDigits[(B and $0F)+1];
  if P <> Length(Result) then
    raise Exception.CreateFmt('Internal Error in string length calculation: Length = %d, P = %d',[Length(Result),P]);
End;

Class Function TBitstream.GetString(ABitstream:TDynByteArray;ALength:Integer):String;
Begin
  Result := GetString(PByteArray(@(ABitstream[0]))^,ALength);
End;

Class Function TBitstream.GetString(ABitstream:TDynByteArray;ALength:Integer;AGroup:Integer):String;
Begin
  Result := GetString(PByteArray(@(ABitstream[0]))^,ALength,AGroup);
End;

Class Function TBitstream.GetStringReverse(ABitstream:TDynByteArray;ALength:Integer):String;
Begin
  Result := GetStringReverse(PByteArray(@(ABitstream[0]))^,ALength);
End;

Class Function TBitstream.GetStringReverse(ABitstream:TDynByteArray;ALength:Integer;AGroup:Integer):String;
Begin
  Result := GetStringReverse(PByteArray(@(ABitstream[0]))^,ALength,AGroup);
End;

Class Function TBitstream.GetHexString(ABitstream:TDynByteArray;ALength:Integer):String;
Begin
  Result := GetHexString(PByteArray(@(ABitstream[0]))^,ALength);
End;

Class Function TBitstream.GetHexStringReverse(ABitstream:TDynByteArray;ALength:Integer):String;
Begin
  Result := GetHexStringReverse(PByteArray(@(ABitstream[0]))^,ALength);
End;

Class Function TBitstream.GetCArray(ABitstream:TDynByteArray;ALength:Integer):String;
Begin
  Result := GetCArray(PByteArray(@(ABitstream[0]))^,ALength);
End;

Class Function TBitstream.CountBits(Bits:String):Integer;
Var I : Integer;
Begin
  Result := 0;
  For I := 1 to Length(Bits) do
    if Bits[I] in ['0','1'] then
      Inc(Result);
End;

Class Function TBitstream.String2Bits(Bits:String;Out Bitstream:TDynByteArray):Integer;
Var BitLen    : Integer;
    StrPos    : Integer;
    BitPos    : Integer;
Begin
  BitLen := CountBits(Bits);
  SetLength(Bitstream,(BitLen+7) shr 3);
  Fillchar(Bitstream[0],(BitLen+7) shr 3,0);
  BitPos := 0;
  For StrPos := 1 to Length(Bits) do
    if Bits[StrPos] = '1' then
      Begin
        Bitstream[BitPos shr 3] := Bitstream[BitPos shr 3] or (1 shl (BitPos and $07));
        Inc(BitPos);
      End
    else if Bits[StrPos] = '0' then
      Inc(BitPos);
    // else: ignore the character
  Result := BitLen;
End;

Class Function TBitstream.IsEqual(Const A,B:Array Of Byte;Length:Integer):Boolean;
Var I,Mask : Integer;
Begin
  Result := False;
  { check full bytes }
  if Length >= 8 then
    For I := 0 to (Length shr 3)-1 do
      if A[I] <> B[I] then Exit;
  { check remaining bits }
  I := ((Length+7) shr 3) - 1;
  Mask := Integer(1 shl (Length and $07)) - 1;   // e.g. Length = 3 -> 00001000 -> 00000111
  if Length and $07 <> 0 then
    if (A[I] and Mask) <> (B[I] and Mask) then Exit;
  { everything ok, they are equal }
  Result := True;
End;

Function TBitstream.GetString:String;
Begin
  Result := GetString(FBitstream,FLength);
End;

Function TBitstream.GetStringReverse:String;
Begin
  Result := GetStringReverse(FBitstream,FLength);
End;

Function TBitstream.GetString(Group:Integer):String;
Begin
  Result := GetString(FBitstream,FLength,Group);
End;

Function TBitstream.GetStringReverse(Group:Integer):String;
Begin
  Result := GetStringReverse(FBitstream,FLength,Group);
End;

Function TBitstream.GetHexString:String;
Begin
  Result := GetHexString(FBitstream,FLength);
End;

Function TBitstream.GetHexStringReverse:String;
Begin
  Result := GetHexStringReverse(FBitstream,FLength);
End;

Function TBitstream.GetCArray:String;
Begin
  Result := GetCArray(FBitstream,FLength);
End;

Function TBitstream.GetModelSimTCLString(Instance:String):String;
Begin
  Result := 'force -freeze ' + Instance + '/ValueShift "'+GetStringReverse+'"' + ^J;
End;

Function TBitstream.GetLECString(Instance,Options:String):String;
Var I : Integer;
Begin
  Result := '';
  For I := 0 to FLength-1 do
    Result += 'add instance constraint ' + Chr(Ord('0')+Ord(GetBit(I))) + ' ' + Instance + '/ValueShift_reg[' + IntToStr(I) + ']' + Options + ^J;
End;

Function TBitstream.GetFormalityString(Instance:String):String;
Var I : Integer;
Begin
  Result := '';
  For I := 0 to FLength-1 do
    Result += 'set_constant -type cell {' + Instance + '/ValueShift_reg[' + IntToStr(I) + ']' + '} ' + Chr(Ord('0')+Ord(GetBit(I))) + ^J;
End;

{ TMultiBitstream }

(**
 * Create the object with a set of sub-bitsteams
 *
 * The order of the sub-bitsteams is from LSB to MSB according to the array
 * index in ASubBitstreams, i.e. from left to right.
 *)
Constructor TMultiBitstream.Create(Const ASubBitstreams:Array Of Const);
Var TotalLength : Cardinal;
    I           : Cardinal;
Begin
  TotalLength := 0;
  For I := 0 to Length(ASubBitstreams)-1 do
    Begin
      // check that the type is ok
      // TODO: support arrays, but how to handle their direction?
      if (ASubBitstreams[I].VType <> vtObject) or
         not (ASubBitstreams[I].VObject is TBitstream) then
        raise Exception.CreateFmt('Parameter %d is not a TBitstream',[I]);
      //WriteLn('Adding at ',TotalLength,' new bitstream with ',(ASubBitstreams[I].VObject as TBitstream).Count,' bits.');
      // safe reference to the sub-bitstream
      SetLength(FSubBitstreams,Length(FSubBitstreams)+1);
      FSubBitstreams[Length(FSubBitstreams)-1] := (ASubBitstreams[I].VObject as TBitstream);
      // calculate total length
      TotalLength := TotalLength + (ASubBitstreams[I].VObject as TBitstream).Count;
    End;
  // finally create our class with the calculated total length
  inherited Create(TotalLength);
End;

Constructor TMultiBitstream.Create(Const ASubBitstreams:TBitstreamArray);
Var TotalLength : Cardinal;
    I           : Cardinal;
Begin
  TotalLength := 0;
  For I := 0 to Length(ASubBitstreams)-1 do
    Begin
      // check that the type is ok
      // safe reference to the sub-bitstream
      SetLength(FSubBitstreams,Length(FSubBitstreams)+1);
      FSubBitstreams[Length(FSubBitstreams)-1] := ASubBitstreams[I];
      // calculate total length
      TotalLength := TotalLength + ASubBitstreams[I].Count;
    End;
  // finally create our class with the calculated total length
  inherited Create(TotalLength);
End;

(**
 * Copy the contents of all sub-bitstreams to our own FBitstream
 *)
Procedure TMultiBitstream.Copy;
Var I   : Cardinal;
    Pos : Cardinal;
Begin
  Pos := 0;
  For I := 0 to Length(FSubBitstreams)-1 do
    Begin
      SetBits(Pos,FSubBitstreams[I]);
      Pos := Pos + FSubBitstreams[I].Count;
    End;
End;

Function TMultiBitstream.GetGroupedString:String;
Var I   : Cardinal;
Begin
  Result := '';
  For I := 0 to Length(FSubBitstreams)-1 do
    Result := Result + FSubBitstreams[I].GetString + ' ';
End;

class Procedure TMultiBitstream.AddPart(Var AParts:TBitstreamArray;ABitstream:TBitstream);
Begin
  SetLength(AParts,Length(AParts)+1);
  AParts[Length(AParts)-1] := ABitstream;
End;

{ TCustomBitstream }

Procedure TCustomBitstream.SetBit(Pos:Integer;Value:Boolean);
Begin
  inherited SetBit(Pos,Value);
End;

Procedure TCustomBitstream.SetBits(Start:Integer;Bits:String);
Begin
  inherited SetBits(Start,Bits);
End;

Procedure TCustomBitstream.SetBits(Start:Integer;Bits:TBitstream);
Begin
  inherited SetBits(Start,Bits);
End;

Procedure TCustomBitstream.SetBits(Start:Integer;Bits:Integer;Length:Integer);
Begin
  inherited SetBits(Start,Bits,Length);
End;

Procedure TCustomBitstream.SetBit(Value:Boolean);
Begin
  if Count <> 1 then
    raise Exception.Create('SetBit can only be used for single-bit TCustomBitstreams');
  SetBit(0,Value);
End;

Procedure TCustomBitstream.SetBits(Bits:String);
Begin
  if Length(Bits) <> Count then
    raise Exception.CreateFmt('SetBits(String) must be called with the exact number of bits (%d instead of %d)',[Count,Length(Bits)]);
  SetBits(0,Bits);
End;

Procedure TCustomBitstream.SetBits(Bits:TBitstream);
Begin
  if Bits.Count <> Count then
    raise Exception.CreateFmt('SetBits(TBitstream) must be called with the exact number of bits (%d instead of %d)',[Count,Bits.Count]);
  SetBits(0,Bits);
End;

Procedure TCustomBitstream.SetBits(Bits:Integer);
Begin
  SetBits(0,Bits,Count);
End;

End.

