(**
 *
 * ILang Specification:
 *  - list of modules
 *     - list of attributes
 *     - "module" + name
 *     - list of wires
 *     - list of cells
 *     - list of processes
 *        - list of assign, switch and sync statements
 *     - list of connections
 *
 *)
Unit ILang;

{$mode objfpc}{$H+}

Interface

Uses
  FGL, Netlist;

Procedure CreateYosysCells(Var AModules : TModuleList);

Procedure Parse(Filename:String;Var AModules : TModuleList);

Function Synthesize(AModule : TModule) : TModule;

Implementation
Uses SysUtils, yacclib, lexlib;

Var ModuleList : TModuleList;

// {$DEFINE yydebug}
{$INCLUDE ilang-parser.pas}
{$INCLUDE ilang-lex.pas}

Function MyYyWrap() : Boolean;
Begin
  Close(yyinput);
  Result := true;
End;

(**
 * Parse ILang file
 *
 * If AModules is Nil, a new instance of this list will be generated. Otherwise
 * the existing modules are used when instances of them are built.
 *)
Procedure Parse(Filename:String;Var AModules : TModuleList);
Var I  : Integer;
    St : String;
Begin
  Assign(yyinput,Filename);
  {$I-} Reset(yyinput); {$I+}
  if IOResult <> 0 then
    Begin
      WriteLn('Error: File ',Filename,' not found.');
      Exit;
    End;

  if not assigned(AModules) then
    AModules := TModuleList.Create;
  ModuleList := AModules;

  yylineno := 0;
  yyclear;
  yywrap:= @MyYyWrap;
  try
    yyparse();
  except
    on E : Exception do
      Begin
        yyerror('Exception: ' + E.Message);
        raise;
      End;
  End;
//  yylex_destroy();

  // check for auto-generated modules
  St := '';
  For I := 0 to ModuleList.Count-1 do
    if ModuleList.Data[I].FEntityAttributes.FAttrValList.IndexOf('pas_ilang_autogen') >= 0 then
      St += ModuleList.Data[I].FName + ', ';
  if St > '' then
    Begin
      SetLength(St,Length(St)-2);  // cut off trailing ', '
      WriteLn('WARNING: Module list still contains auto-generated modules:');
      WriteLn('           ',St);
      WriteLn('         These most probably have wrong port directions!');
    End;

  // yyparse() also calls yywrap(), which is a procedure variable, which
  // originally pointed to yylex_yywrap(), which closes yyinput (so we don't
  // have to close it here) and yyoutput (which initially didn't harm, but when
  // using this function twice, WriteLn doesn't have an open output handle any
  // more).
  // Therefore we make our own MyYyWrap() function which only closes yyinput
  // but not yyoutput.
End;

Function CreateYosysCellNot : TModule;
Begin
  Result := TModule.Create('$not');
  Result.AddGeneric(TGeneric.Create('A_SIGNED',TypeInt,TValueInteger.Create(0)));
  Result.AddGeneric(TGeneric.Create('A_WIDTH', TypeInt,TValueInteger.Create(1)));
  Result.AddGeneric(TGeneric.Create('Y_WIDTH', TypeInt,TValueInteger.Create(1)));
  Result.AddPort(TPort.Create('A',dirIn, TypeBit));   // TODO: this is not clean, because A_WIDTH and Y_WIDTH can be >1
  Result.AddPort(TPort.Create('Y',dirOut,TypeBit));
End;

Function CreateYosysCellAnd : TModule;
Begin
  Result := TModule.Create('$and');
  Result.AddGeneric(TGeneric.Create('A_SIGNED',TypeInt,TValueInteger.Create(0)));
  Result.AddGeneric(TGeneric.Create('B_SIGNED',TypeInt,TValueInteger.Create(0)));
  Result.AddGeneric(TGeneric.Create('A_WIDTH', TypeInt,TValueInteger.Create(1)));
  Result.AddGeneric(TGeneric.Create('B_WIDTH', TypeInt,TValueInteger.Create(1)));
  Result.AddGeneric(TGeneric.Create('Y_WIDTH', TypeInt,TValueInteger.Create(1)));
  Result.AddPort(TPort.Create('A',dirIn, TypeBit));   // TODO: this is not clean, because A_WIDTH and Y_WIDTH can be >1
  Result.AddPort(TPort.Create('B',dirIn, TypeBit));   // TODO: this is not clean, because A_WIDTH and Y_WIDTH can be >1
  Result.AddPort(TPort.Create('Y',dirOut,TypeBit));
End;

Function CreateYosysCellOr : TModule;
Begin
  Result := TModule.Create('$or');
  Result.AddGeneric(TGeneric.Create('A_SIGNED',TypeInt,TValueInteger.Create(0)));
  Result.AddGeneric(TGeneric.Create('B_SIGNED',TypeInt,TValueInteger.Create(0)));
  Result.AddGeneric(TGeneric.Create('A_WIDTH', TypeInt,TValueInteger.Create(1)));
  Result.AddGeneric(TGeneric.Create('B_WIDTH', TypeInt,TValueInteger.Create(1)));
  Result.AddGeneric(TGeneric.Create('Y_WIDTH', TypeInt,TValueInteger.Create(1)));
  Result.AddPort(TPort.Create('A',dirIn, TypeBit));   // TODO: this is not clean, because A_WIDTH and Y_WIDTH can be >1
  Result.AddPort(TPort.Create('B',dirIn, TypeBit));   // TODO: this is not clean, because A_WIDTH and Y_WIDTH can be >1
  Result.AddPort(TPort.Create('Y',dirOut,TypeBit));
End;

Function CreateYosysCellXor : TModule;
Begin
  Result := TModule.Create('$xor');
  Result.AddGeneric(TGeneric.Create('A_SIGNED',TypeInt,TValueInteger.Create(0)));
  Result.AddGeneric(TGeneric.Create('B_SIGNED',TypeInt,TValueInteger.Create(0)));
  Result.AddGeneric(TGeneric.Create('A_WIDTH', TypeInt,TValueInteger.Create(1)));
  Result.AddGeneric(TGeneric.Create('B_WIDTH', TypeInt,TValueInteger.Create(1)));
  Result.AddGeneric(TGeneric.Create('Y_WIDTH', TypeInt,TValueInteger.Create(1)));
  Result.AddPort(TPort.Create('A',dirIn, TypeBit));   // TODO: this is not clean, because A_WIDTH and Y_WIDTH can be >1
  Result.AddPort(TPort.Create('B',dirIn, TypeBit));   // TODO: this is not clean, because A_WIDTH and Y_WIDTH can be >1
  Result.AddPort(TPort.Create('Y',dirOut,TypeBit));
End;

Function CreateYosysCellReduceAnd : TModule;
Begin
  Result := TModule.Create('$reduce_and');
  Result.AddGeneric(TGeneric.Create('A_SIGNED',TypeInt,TValueInteger.Create(0)));
  Result.AddGeneric(TGeneric.Create('A_WIDTH', TypeInt,TValueInteger.Create(0)));
  Result.AddGeneric(TGeneric.Create('Y_WIDTH', TypeInt,TValueInteger.Create(0)));
  Result.AddPort(TPort.Create('A',dirIn, TypeBit));   // TODO: this is not clean, because A_WIDTH and Y_WIDTH can be >1
  Result.AddPort(TPort.Create('Y',dirOut,TypeBit));
End;

Function CreateYosysCellReduceOr : TModule;
Begin
  Result := TModule.Create('$reduce_or');
  Result.AddGeneric(TGeneric.Create('A_SIGNED',TypeInt,TValueInteger.Create(0)));
  Result.AddGeneric(TGeneric.Create('A_WIDTH', TypeInt,TValueInteger.Create(0)));
  Result.AddGeneric(TGeneric.Create('Y_WIDTH', TypeInt,TValueInteger.Create(0)));
  Result.AddPort(TPort.Create('A',dirIn, TypeBit));   // TODO: this is not clean, because A_WIDTH and Y_WIDTH can be >1
  Result.AddPort(TPort.Create('Y',dirOut,TypeBit));
End;

Function CreateYosysCellEq : TModule;
Begin
  Result := TModule.Create('$eq');
  Result.AddGeneric(TGeneric.Create('A_SIGNED',TypeInt,TValueInteger.Create(0)));
  Result.AddGeneric(TGeneric.Create('B_SIGNED',TypeInt,TValueInteger.Create(0)));
  Result.AddGeneric(TGeneric.Create('A_WIDTH', TypeInt,TValueInteger.Create(1)));
  Result.AddGeneric(TGeneric.Create('B_WIDTH', TypeInt,TValueInteger.Create(1)));
  Result.AddGeneric(TGeneric.Create('Y_WIDTH', TypeInt,TValueInteger.Create(1)));
  Result.AddPort(TPort.Create('A',dirIn, TypeBit));   // TODO: this is not clean, because A_WIDTH and Y_WIDTH can be >1
  Result.AddPort(TPort.Create('B',dirIn, TypeBit));   // TODO: this is not clean, because A_WIDTH and Y_WIDTH can be >1
  Result.AddPort(TPort.Create('Y',dirOut,TypeBit));
End;

Function CreateYosysCellLogicAnd : TModule;
Begin
  Result := TModule.Create('$logic_and');
  Result.AddGeneric(TGeneric.Create('A_SIGNED',TypeInt,TValueInteger.Create(0)));
  Result.AddGeneric(TGeneric.Create('B_SIGNED',TypeInt,TValueInteger.Create(0)));
  Result.AddGeneric(TGeneric.Create('A_WIDTH', TypeInt,TValueInteger.Create(1)));
  Result.AddGeneric(TGeneric.Create('B_WIDTH', TypeInt,TValueInteger.Create(1)));
  Result.AddGeneric(TGeneric.Create('Y_WIDTH', TypeInt,TValueInteger.Create(1)));
  Result.AddPort(TPort.Create('A',dirIn, TypeBit));   // TODO: this is not clean, because A_WIDTH and Y_WIDTH can be >1
  Result.AddPort(TPort.Create('B',dirIn, TypeBit));   // TODO: this is not clean, because A_WIDTH and Y_WIDTH can be >1
  Result.AddPort(TPort.Create('Y',dirOut,TypeBit));
End;

Function CreateYosysCellLogicOr : TModule;
Begin
  Result := TModule.Create('$logic_or');
  Result.AddGeneric(TGeneric.Create('A_SIGNED',TypeInt,TValueInteger.Create(0)));
  Result.AddGeneric(TGeneric.Create('B_SIGNED',TypeInt,TValueInteger.Create(0)));
  Result.AddGeneric(TGeneric.Create('A_WIDTH', TypeInt,TValueInteger.Create(1)));
  Result.AddGeneric(TGeneric.Create('B_WIDTH', TypeInt,TValueInteger.Create(1)));
  Result.AddGeneric(TGeneric.Create('Y_WIDTH', TypeInt,TValueInteger.Create(1)));
  Result.AddPort(TPort.Create('A',dirIn, TypeBit));   // TODO: this is not clean, because A_WIDTH and Y_WIDTH can be >1
  Result.AddPort(TPort.Create('B',dirIn, TypeBit));   // TODO: this is not clean, because A_WIDTH and Y_WIDTH can be >1
  Result.AddPort(TPort.Create('Y',dirOut,TypeBit));
End;

Function CreateYosysCellMux : TModule;
Begin
  Result := TModule.Create('$mux');
  Result.AddGeneric(TGeneric.Create('WIDTH', TypeInt,TValueInteger.Create(0)));
  Result.AddPort(TPort.Create('A',dirIn, TypeBit));   // TODO: this is not clean, because A_WIDTH and Y_WIDTH can be >1
  Result.AddPort(TPort.Create('B',dirIn, TypeBit));   // TODO: this is not clean, because A_WIDTH and Y_WIDTH can be >1
  Result.AddPort(TPort.Create('S',dirIn, TypeBit));   // TODO: this is not clean, because A_WIDTH and Y_WIDTH can be >1
  Result.AddPort(TPort.Create('Y',dirOut,TypeBit));
End;

Function CreateYosysCellADFF : TModule;
Begin
  Result := TModule.Create('$adff');
  Result.AddGeneric(TGeneric.Create('WIDTH',        TypeInt,TValueInteger.Create(0)));
  Result.AddGeneric(TGeneric.Create('CLK_POLARITY', TypeBit,TValueBit.Create('1')));
  Result.AddGeneric(TGeneric.Create('ARST_POLARITY',TypeBit,TValueBit.Create('1')));
  Result.AddGeneric(TGeneric.Create('ARST_VALUE',   TypeInt,TValueInteger.Create(0)));
  Result.AddPort(TPort.Create('CLK', dirIn, TypeBit));
  Result.AddPort(TPort.Create('ARST',dirIn, TypeBit));
  Result.AddPort(TPort.Create('D',   dirIn, TypeBit));   // TODO: this is not clean, because A_WIDTH and Y_WIDTH can be >1
  Result.AddPort(TPort.Create('Q',   dirout,TypeBit));   // TODO: this is not clean, because A_WIDTH and Y_WIDTH can be >1
End;

Procedure CreateYosysCells(Var AModules : TModuleList);
Begin
  if not assigned(AModules) then
    AModules := TModuleList.Create;
  AModules.Add('$not',CreateYosysCellNot);
  AModules.Add('$and',CreateYosysCellAnd);
  AModules.Add('$or', CreateYosysCellOr);
  AModules.Add('$xor',CreateYosysCellXor);
  AModules.Add('$reduce_and',CreateYosysCellReduceAnd);
  AModules.Add('$reduce_or', CreateYosysCellReduceOr);
  AModules.Add('$eq', CreateYosysCellEq);
  AModules.Add('$logic_and',CreateYosysCellLogicAnd);
  AModules.Add('$logic_or', CreateYosysCellLogicOr);
  AModules.Add('$mux',CreateYosysCellMux);
  AModules.Add('$adff',CreateYosysCellADFF);
  // TODO: add more cells
End;

(**
 * Simple synthesis on a given module
 *
 * This function performs some simple synthesis tasks on the given module and
 * returns a new module. The old module and all its referenced signals, types,
 * instances, ... are not changed
 *
 * Currently the following tasks are performed
 *  - assignments from operators are replaced by dedicated cells
 *    only operator "not" is supported at the moment
 *)
Function Synthesize(AModule:TModule):TModule;
Var I        : Integer;
    Modules  : TModuleList;
    Module   : TModule;
    Instance : TInstance;
    Remove   : Array of Integer;
Begin
  Result := AModule.Clone;
  Modules := TModuleList.Create;

  // check assignments
  SetLength(Remove,0);
  For I := 0 to Result.FAssignments.Count-1 do
    With Result.FAssignments[I] do
      Begin
        if (FValue is TSignal) or (FValue is TValueConcat) or (FValue is TValueIndex) or (FValue is TValueBit) or (FValue is TValueVector) then
          Continue;
        if FValue is TValueOperatorNot then
          Begin
            if not (FDest is TSignal) then
              raise Exception.Create('Can only handle assignments to signals');
            if not ((FValue as TValueOperatorNot).FValue is TSignal) then
            raise Exception.Create('Can only handle assignments from signals');
            if (FDest as TSignal).FType <> TypeBit then
              Continue;   // we currently only handle single-bit $not --> TODO
            if Modules.IndexOf('$not') >= 0 then
              Module := Modules['$not']
            else
              Begin
                Module := TModule.Create('$not');
                Module.AddGeneric(TGeneric.Create('A_SIGNED',TypeInt,TValueInteger.Create(0)));
                Module.AddGeneric(TGeneric.Create('A_WIDTH', TypeInt,TValueInteger.Create(1)));
                Module.AddGeneric(TGeneric.Create('Y_WIDTH', TypeInt,TValueInteger.Create(1)));
                Module.AddPort(TPort.Create('A',dirIn, TypeBit));   // TODO: this is not clean, because A_WIDTH and Y_WIDTH can be >1
                Module.AddPort(TPort.Create('Y',dirOut,TypeBit));
                Modules.Add(Module.FName,Module);
              End;
            // instantiate $not gate
            Instance := Result.AddInstance(TInstance.Create('$not$assign$'+(FDest as TSignal).FName,Module));
            Instance.SetGeneric('A_SIGNED',TValueInteger.Create(0));
            Instance.SetGeneric('A_WIDTH', TValueInteger.Create(1));
            Instance.SetGeneric('Y_WIDTH', TValueInteger.Create(1));
            Instance.ConnectPort('A',(FValue as TValueOperatorNot).FValue);
            Instance.ConnectPort('Y',FDest);
            // store for removal of assignment
            SetLength(Remove,Length(Remove)+1);
            Remove[Length(Remove)-1] := I;
          End;
      End;
  // remove assignment
  For I := 0 to Length(Remove)-1 do
    Result.FAssignments.Delete(Remove[I]);

  Modules.Free;
End;

End.

