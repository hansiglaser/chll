Program TestLiberty;

Uses Liberty;

Var Libraries : TLibraryList;

Begin
  if ParamCount <> 1 then
    Begin
      WriteLn('Usage: ',ParamStr(0),' filename');
      Halt(1);
    End;
  Libraries := Nil;
  Parse(ParamStr(1),Libraries);
  WriteLn('Read ',Libraries.Count,' libraries:');
  Libraries.Data[0].PrintAll;
End.

