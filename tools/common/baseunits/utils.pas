Unit Utils;

{$mode objfpc}{$H+}

Interface

Uses
  Classes, SysUtils, StrUtils, FGL, BaseUnix;

Type
  TDynStringArray  = Array of String;
  TDynIntegerArray = Array of Integer;
  TDynInteger2DArray = Array of Array of Integer;
  TCharSet         = set of Char;
  TFPGStringList   = specialize TFPGList<String>;   // TStringList is already defined in the unit Classes, so choose a different name
  TStringIntMap    = specialize TFPGMap<String,Integer>;
  TStringStringMap = specialize TFPGMap<String,String>;

Function Join(Delim:String;Const Strings:TDynStringArray) : String;
Function Split(Const St : String):TDynStringArray;
Procedure PrintStrings(Const Strings:TDynStringArray);
Function CountChars(Ch:Char;St:String):Integer;
Function StrConsistsOf(St:String;Chs:TCharSet):Boolean;
Function IntToBin(Value:Integer;Len:Integer) : String;
Function IntToBinLM(Value:Integer;Len:Integer) : String;
Function IntToOct(Value:Integer;Len:Integer) : String;
Function HexToInt(St:String):Cardinal;
Function HexToInt64(St:String):Int64;
Function OctToInt(St:String):Cardinal;
Function BinToInt(St:String):Cardinal;
Function CreateDynIntegerArray(Const Integers : Array of Const) : TDynIntegerArray;
Function BitReverse(I:Integer;Length:Integer):Integer;
Function RoundUpLd(I:Cardinal):Cardinal;
Function Prefix(St:String;PrefixSt:String;FirstLine:Boolean=true):String;
Function PrefixIf(St:String;PrefixSt:String;Tail:String;FirstLine:Boolean=true):String;
Function Indent(St:String;Prefix:Cardinal;FirstLine:Boolean=true):String;
Procedure Swap(Var A,B:Integer);
Function CountOnes(Value:Cardinal):Integer;
Function Log2(Value:Cardinal):Integer;
Procedure FilePutContents(Filename:String;St:String);
Function  FileGetContents(Filename:String):String;
Function Select(T,F:String;B:Boolean) : String;
Function Select(T,F:Char;B:Boolean) : Char;
Function FileSearchGlob(Glob:AnsiString):AnsiString;
Function FileSearchGlobList(Glob:AnsiString):TStringList;
Function ParseSignalValue(St:String;Out Width:Integer):Integer;
Procedure ParseIndex(St:String;Out IndexL,IndexR:Integer);
Function TrimSignalPostfix(ASignal:String) : String;
Function StrTr(St,Old,New:String) : String;
Procedure CopyList(ASrc,ADst:TFPSList);
Procedure CopyMap(ASrc,ADst:TFPSMap);

Implementation

Function Join(Delim:String;Const Strings:TDynStringArray):String;
Var I : Integer;
Begin
  if Length(Strings) = 0 then Exit('');
  if Length(Strings) = 1 then Exit(Strings[0]);
  Result := Strings[0];
  For I := 1 to Length(Strings)-1 do
    Result := Result + Delim + Strings[I];
End;

Function Split(Const St:String):TDynStringArray;
Var P1,P2 : Integer;
    White : Boolean;
Begin
  SetLength(Result,0);
  P2 := 0;
  White := true;
  For P1 := 1 to Length(St) do
    Begin
      if White and not (St[P1] in [' ',^J,^M,^I]) then
        Begin
          // whitespace to character transition
          P2 := P1;
          White := false;
        End
      else if not White and (St[P1] in [' ',^J,^M,^I]) then
        Begin
          // character to whitespace transition
          SetLength(Result,Length(Result)+1);
          Result[Length(Result)-1] := Copy(St,P2,P1-P2);
          White := true;
        End;
    End;
  // end of string, check if it ended with a word
  if not White then
    Begin
      SetLength(Result,Length(Result)+1);
      Result[Length(Result)-1] := Copy(St,P2,Length(St));
    End;

End;

Procedure PrintStrings(Const Strings:TDynStringArray);
Var I : Integer;
Begin
  For I := 0 to Length(Strings)-1 do
    WriteLn(Strings[I]);
End;

Function CountChars(Ch:Char;St:String):Integer;
Var I : Integer;
Begin
  Result := 0;
  For I := 1 to Length(St) do
    if St[I] = Ch then
      Inc(Result);
End;

Function StrConsistsOf(St:String;Chs:TCharSet):Boolean;
Var I : Integer;
Begin
  Result := false;
  For I := 1 to Length(St) do
    if not (St[I] in Chs) then
      Exit;
  Result := true;
End;

(**
 * Convert an integer to a string, MSB is at left!
 * MSB -> LSB
 *)
Function IntToBin(Value:Integer;Len:Integer):String;
Var I : Integer;
Begin
  Result := StringOfChar('0',Len);    // fill with '0', set to '1' or '0'
  For I := 0 to Len-1 do
    Begin
      if Value and (1 shl I) <> 0 then
        Result[Len-I] := '1';  // '0' is default
    End;
End;

(**
 * Convert an integer to a string, LSB is at left!
 * LSB -> MSB
 *)
Function IntToBinLM(Value:Integer;Len:Integer):String;
Var I : Integer;
Begin
  Result := StringOfChar('0',Len);    // fill with '0', set to '1' or '0'
  For I := 0 to Len-1 do
    Begin
      if Value and (1 shl I) <> 0 then
        Result[I+1] := '1';  // '0' is default
    End;
End;

Function IntToOct(Value:Integer;Len:Integer):String;
Var I : Integer;
Begin
  Result := StringOfChar('0',Len);    // fill with '0', set to '1' or '0'
  I := Len;
  While (I >= 1) and (Value > 0) do
    Begin
      Result[I] := Chr(Ord('0') + (Value and $07));
      Value := Value shr 3;
      Dec(I);
    End;
End;

Function HexToInt(St:String):Cardinal;
Var I : Integer;
Begin
  if Length(St) > 8 then
    raise Exception('Hex value '''+St+''' is larger than 32 bits');
  Result := 0;
  For I := 1 to Length(St) do
    Begin
      Result := Result shl 4;
           if (St[I] >= '0') and (St[I] <= '9') then Result := Result or (Ord(St[I])-Ord('0'))
      else if (St[I] >= 'A') and (St[I] <= 'F') then Result := Result or (Ord(St[I])-Ord('A')+10)
      else if (St[I] >= 'a') and (St[I] <= 'f') then Result := Result or (Ord(St[I])-Ord('a')+10)
      else
        raise Exception.Create('Invalid hex digit '''+St[I]+''' at position '+IntToStr(I)+' in hex value '''+St+'''');
    End;
End;

Function HexToInt64(St:String):Int64;
Var I : Integer;
Begin
  if Length(St) > 16 then
    raise Exception('Hex value '''+St+''' is larger than 64 bits');
  Result := 0;
  For I := 1 to Length(St) do
    Begin
      Result := Result shl 4;
           if (St[I] >= '0') and (St[I] <= '9') then Result := Result or (Ord(St[I])-Ord('0'))
      else if (St[I] >= 'A') and (St[I] <= 'F') then Result := Result or (Ord(St[I])-Ord('A')+10)
      else if (St[I] >= 'a') and (St[I] <= 'f') then Result := Result or (Ord(St[I])-Ord('a')+10)
      else
        raise Exception.Create('Invalid hex digit '''+St[I]+''' at position '+IntToStr(I)+' in hex value '''+St+'''');
    End;
End;

Function OctToInt(St:String):Cardinal;
Var I : Integer;
Begin
  if (Length(St) > 11) or ((Length(St) = 11) and (St[1] > '3')) then
    raise Exception('Octal value '''+St+''' is larger than 32 bits');
  Result := 0;
  For I := 1 to Length(St) do
    Begin
      Result := Result shl 4;
      if (St[I] >= '0') and (St[I] <= '7') then Result := Result or (Ord(St[I])-Ord('0'))
      else
        raise Exception.Create('Invalid octal digit '''+St[I]+''' at position '+IntToStr(I)+' in octal value '''+St+'''');
    End;
End;

Function BinToInt(St:String):Cardinal;
Var I : Integer;
Begin
  if Length(St) > 32 then
    raise Exception('Binary value '''+St+''' is larger than 32 bits');
  Result := 0;
  For I := 1 to Length(St) do
    Begin
      Result := Result shl 1;
      if St[I] = '0' then
        // do nothing
      else if St[I] = '1' then
        Result := Result or 1
      else
        raise Exception.Create('Invalid binary digit '''+St[I]+''' at position '+IntToStr(I)+' in binary value '''+St+'''');
    End;
End;

Function CreateDynIntegerArray(Const Integers:Array Of Const):TDynIntegerArray;
Var I : Integer;
Begin
  SetLength(Result,Length(Integers));
  For I := 0 to Length(Integers)-1 do
    Begin
      if Integers[I].Vtype <> vtInteger then
        raise Exception.CreateFmt('Not an integer as parameter %d',[I]);
      Result[I] := Integers[I].VInteger;
    End;
End;

Function BitReverse(I:Integer;Length:Integer):Integer;
Begin
  Result := 0;
  While Length > 0 do
    Begin
      Result := (Result shl 1) or (I and $01);
      I := I shr 1;
      Dec(Length);
    End;
End;

(**
 * Calculate the number of bits used to represent the given number
 *
 * E.g. 6 =  110b -> 3
 *      7 =  111b -> 3
 *      8 = 1000b -> 4
 *      0 =    0b -> 0
 *      1 =    1b -> 1
 *)
Function RoundUpLd(I:Cardinal):Cardinal;
Begin
  Result := 0;
  While I > 0 do
    Begin
      Inc(Result);
      I := I shr 1;
    End;
End;

Function Prefix(St:String;PrefixSt:String;FirstLine:Boolean=true):String;
Var P,N : Cardinal;
    PS  : String;
Begin
  if PrefixSt = '' then Exit(St);
  Result := '';
  P      := 1;
  if FirstLine then PS := PrefixSt
  else PS := '';
  repeat
    N := PosEx(^J,St,P);
    if N = P then
      Result := Result + ^J        // empty line
    else if N > 0 then
      Result := Result + PS + Copy(St,P,N-P+1)
    else if P > Length(St) then    // string was processed completely
      Exit
    else
      Begin
        // N = 0 --> last line
        Result := Result + PS + Copy(St,P,Length(St));
        Exit;
      End;
    P := N+1;
    PS := PrefixSt;
  Until P > Length(St);
End;

Function PrefixIf(St:String;PrefixSt:String;Tail:String;FirstLine:Boolean=true):String;
Begin
  Result := '';
  if St > '' then
    Result := Prefix(St,PrefixSt,FirstLine) + Tail
End;

Function Indent(St:String;Prefix:Cardinal;FirstLine:Boolean=true):String;
Begin
  if Prefix = 0 then Exit(St);
  Result := Utils.Prefix(St,StringOfChar(' ',Prefix),FirstLine);
End;

Procedure Swap(Var A,B:Integer);
Var D : Integer;
Begin
  D := A;
  A := B;
  B := D;
End;

Function CountOnes(Value:Cardinal):Integer;
Begin
  Result := 0;
  While Value > 0 do
    Begin
      if (Value and $01) <> 0 then Inc(Result);
      Value := Value shr 1;
    End;
End;

Function Log2(Value:Cardinal):Integer;
Begin
  Result := -1;
  While Value > 0 do
    Begin
      Inc(Result);
      Value := Value shr 1;
    End;
End;

Procedure FilePutContents(Filename:String;St:String);
Var T : Text;
Begin
  Assign(T,Filename);
  Rewrite(T);
  Write(T,St);
  Close(T);
End;

Function FileGetContents(Filename:String):String;
Const ChunkSize = 4096;
Var F : Integer;
    P : TSize;
    L : Integer;
Begin
  // using Unix commands because Reset() tries to open RDWR, which is not allowed e.g. for /proc/self/maps
  F := FpOpen(Filename,O_RDONLY);
  P := 0;
  repeat
    SetLength(Result,P+ChunkSize);
    L := FpRead(F,Result[P+1],ChunkSize);
    P := P + L;
  Until L = 0;
  SetLength(Result,P);
  FpClose(F);
End;

Function Select(T,F:String;B:Boolean):String;
Begin
  if B then Result := T
  else      Result := F;
End;

Function Select(T,F:Char;B:Boolean):Char;
Begin
  if B then Result := T
  else      Result := F;
end;

Function FileSearchGlob(Glob:AnsiString):AnsiString;
Var SR : TSearchRec;
Begin
  Result := '';
  Glob := ExpandFileName(Glob);
  if FindFirst(Glob,faAnyFile,SR) = 0 then
    Result := ExtractFilePath(Glob) + SR.Name;
  FindClose(SR);
End;

Function FileSearchGlobList(Glob:AnsiString):TStringList;
Var SR : TSearchRec;
Begin
  Result := TStringList.Create;
  Glob := ExpandFileName(Glob);
  if FindFirst(Glob,faAnyFile,SR) = 0 then
    repeat
      Result.Add(ExtractFilePath(Glob) + SR.Name);
    until FindNext(SR) <> 0;
  FindClose(SR);
End;

Function ParseSignalValue(St:String;Out Width:Integer):Integer;
Var P : Integer;
Begin
  if Length(St) < 1 then
    raise Exception.Create('Invalid value');
  Width := -1;   // "not specified"
  if (Length(St) = 3) and (St[1] = '''') and (St[2] in ['0','1']) and (St[3] = '''') then
    Begin
      // VHDL std_logic constant, e.g. '1'
      Width := 1;
      Result := Ord(St[2]) - Ord('0');
    End
  else if (Length(St) >= 3) and (St[1] = '"') and (St[Length(St)] = '"') then
    Begin
      // VHDL std_logic_vector constant, e.g. "00110101"
      Width := Length(St)-2;
      Result := BinToInt(Copy(St,2,Length(St)-2));
    End
  else if (Length(St) >= 3) and (St[1] in ['0'..'9']) and (Pos('''',St) > 1) then
    Begin
      // Verilog vector constant, e.g. 8'b00110101
      P := Pos('''',St);
      Width := StrToInt(Copy(St,1,P-1));
      Case St[P+1] of
        'B','b' : Result := BinToInt(Copy(St,P+2,Length(St)));
        'O','o' : Result := OctToInt(Copy(St,P+2,Length(St)));
        'D','d' : Result := StrToInt(Copy(St,P+2,Length(St)));
        'H','h' : Result := HexToInt(Copy(St,P+2,Length(St)));
      else
        raise Exception.Create('Invalid base specifier '''+St[P+1]+''' in value '''+St+'''');
      End;
    End
  else if (St[1] = '0') and StrConsistsOf(St,['0'..'7']) then
    Begin
      // C octal value, e.g. 065
      Result := OctToInt(Copy(St,2,Length(St)));
    End
  else if StrConsistsOf(St,['0'..'9']) then
    Begin
      // decimal value, e.g. 53
      Result := StrToInt(St);
    End
  else if (Length(St) >= 3) and (St[1] = '0') and (St[2] = 'x') then
    Begin
      // C hex value, e.g. 0x35
      Result := HexToInt(Copy(St,3,Length(St)-2));
    End
  else if (Length(St) >= 2) and (St[1] = '$') then
    Begin
      // Pascal hex value, e.g. $35
      Result := HexToInt(Copy(St,2,Length(St)-1));
    End
  else
    raise Exception.Create('Invalid format of value '''+St+'''');
End;

Procedure ParseIndex(St:String;Out IndexL,IndexR:Integer);
Var P : Integer;
Begin
  P := Pos (':',St);
  if P = 0 then
    // single value
    IndexL := StrToInt(St)
  else
    Begin
      // range, e.g. "7:3"
      IndexL := StrToInt(Copy(St,1,P-1));
      IndexR := StrToInt(Copy(St,P+1,Length(St)));
      if (IndexL < 0) or (IndexR < 0) then
        raise Exception.Create('Only positive index values are allowed');
    End;
End;

Function TrimSignalPostfix(ASignal:String) : String;
Begin
  Result := ASignal;
  // cut off postfix
  if      Copy(Result,Length(Result)-2+1,2) = '_i' then SetLength(Result,Length(Result)-2)
  else if Copy(Result,Length(Result)-2+1,2) = '_o' then SetLength(Result,Length(Result)-2)
  else if Copy(Result,Length(Result)-2+1,2) = '_b' then SetLength(Result,Length(Result)-2)
  else if Copy(Result,Length(Result)-2+1,2) = '_s' then SetLength(Result,Length(Result)-2);
End;

Function StrTr(St,Old,New:String):String;
Var StPos,TrPos : Integer;
Begin
  if (Length(New) <> 1) and (Length(Old) <> Length(New)) then
    raise Exception.Create('Old and New must be the same length');
  For StPos := 1 to Length(St) do
    For TrPos := 1 to Length(Old) do
      if St[StPos] = Old[TrPos] then
        Begin
          if Length(New) = 1 then
            St[StPos] := New[1]
          else
            St[StPos] := New[TrPos];
        End;
  Result := St;
End;

Procedure CopyList(ASrc,ADst:TFPSList);
Var I : Integer;
Begin
  For I := 0 to ASrc.Count-1 do
    ADst.Insert(I,ASrc[I]);
End;

Procedure CopyMap(ASrc,ADst:TFPSMap);
Var I : Integer;
Begin
  ADst.Duplicates := ASrc.Duplicates;
  ADst.Sorted     := ASrc.Sorted;
  For I := 0 to ASrc.Count-1 do
    ADst.Add(ASrc.Keys[I],ASrc.Data[I]);
End;

End.

