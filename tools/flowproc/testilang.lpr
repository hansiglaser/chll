Program TestILang;

{$mode objfpc}{$H+}

Uses
  Classes, ILang, Netlist;

Var Modules : TModuleList;

Begin
  if ParamCount <> 1 then
    Begin
      WriteLn('Usage: ',ParamStr(0),' filename');
      Halt(1);
    End;
  Modules := Nil;
  Parse(ParamStr(1),Modules);
  WriteLn(Modules.Data[0].GetVHDL);
End.

