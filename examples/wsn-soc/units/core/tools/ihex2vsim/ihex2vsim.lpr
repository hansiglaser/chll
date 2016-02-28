Program IHex2VSim;

{$mode objfpc}{$H+}

Uses
  Classes, SysUtils, IntelHex;


Procedure ConvertIHex2VSim(InFile:String;Offset:Integer;Variable:String;Width:Integer;Var OutFile:Text);
Var IHexData : PIntelHexRecord;
    R        : PIntelHexRecord;
    I        : Integer;
Begin
  if (Width <> 8) and (Width <> 16) then
    raise Exception.CreateFmt('Width = %d is not supported. Only 8 and 16.',[Width]);
  IHexData := ReadHexFile(InFile);

  R := IHexData;
  While R <> Nil do
    Begin
      if (R^.TheType = $00) and (R^.Length > 0) then
        Begin
          // change sim:/core_tb/DUT/PMem_0/mem\[0\] 0000000000000000
          // change sim:/core_tb/DUT/PMem_0/mem\[0\] 16#FFFF
          // change sim:/core_tb/DUT/PMem_0/mem\[0\]\[11:4\] 16#A5#
          if Width = 8 then
            Begin
              For I := 0 to R^.Length-1 do
                WriteLn(OutFile,'change ',Variable,'\[16#',IntToHex((R^.Address-Offset+I) shr 1,4),'#\] 16#',IntToHex(R^.Data[I],2),'#');
            End
          else if Width = 16 then
            Begin
              I := 0;
              if (R^.Address-Offset + I) and $0001 <> 0 then
                Begin
                  // first odd-aligned byte
                  WriteLn(OutFile,'change ',Variable,'\[16#',IntToHex((R^.Address-Offset+I) shr 1,4),'#\]\[15:8\] 16#',IntToHex(R^.Data[I],2),'#');
                  Inc(I);
                End;
              While I < R^.Length-1 do  // don't use last single-byte in this loop
                Begin
                  WriteLn(OutFile,'change ',Variable,'\[16#',IntToHex((R^.Address-Offset+I) shr 1,4),'#\] 16#',IntToHex(R^.Data[I+1],2),IntToHex(R^.Data[I],2),'#');
                  Inc(I,2);
                End;
              if I < R^.Length then
                Begin
                  // last odd-aligned byte
                  WriteLn(OutFile,'change ',Variable,'\[16#',IntToHex((R^.Address-Offset+I) shr 1,4),'#\]\[7:0\] 16#',IntToHex(R^.Data[I],2),'#');
                  Inc(I);
                End;
            End
          else
            raise Exception.CreateFmt('Internal Error: Width should not be %d',[Width]);
        End;
      R := R^.Next;
    End;
  FreeIntelHex(IHexData);
End;

Procedure Usage;
Begin
  WriteLn('Usage: ',ParamStr(0),' infile offset variable wordwidth [outfile]');
  Halt(1);
End;

Var InFile   : String;
    Offset   : Integer;
    Variable : String;
    Width    : Integer;
    OutFile  : Text;
    I        : Integer;

Begin
  if (ParamCount < 4) or (ParamCount > 5) then
    Usage;

  InFile   := ParamStr(1);
  Offset   := StrToInt(ParamStr(2));
  Variable := ParamStr(3);
  Width    := StrToInt(ParamStr(4));
  OutFile  := StdOut;  // default value
  if ParamCount = 5 then
    Begin
      Assign(OutFile,ParamStr(5));
      Rewrite(OutFile);
    End;

  ConvertIHex2VSim(InFile,Offset,Variable,Width,OutFile);

  Close(OutFile);   // this also works if OutFile = StdOut
End.

