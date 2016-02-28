Unit Liberty;

{$mode objfpc}{$H+}

Interface

Uses
  Classes, SysUtils, FGL, Netlist;

Type
  TLibertyAttr = class
    // empty base class
  End;

  TLibertyValType = (LIBERTY__VAL_BOOLEAN, LIBERTY__VAL_INT, LIBERTY__VAL_DOUBLE, LIBERTY__VAL_STRING, LIBERTY__VAL_EXPR);

Const
  CLibertyValType : Array[TLibertyValType] of String = ('Bool','Int','Double','String','Expr');
Type

  { TLibertyAttrVal }

  TLibertyAttrVal = class(TLibertyAttr)
    ValType   : TLibertyValType;
    BoolVal   : Boolean;
    IntVal    : Integer;
    DoubleVal : Double;
    StringVal : String;
    Next      : TLibertyAttrVal;   // linked list
    Function GetBool   : Boolean;
    Function GetInt    : Integer;
    Function GetDouble : Double;
    Function GetString : String;
    Function ToString : String; override;
  End;

  TLibertyExprType = (SI2DR_EXPR_OP_ADD, SI2DR_EXPR_OP_SUB, SI2DR_EXPR_OP_MUL, SI2DR_EXPR_OP_DIV, SI2DR_EXPR_OP_PAREN);
  TLibertyExpr = class(TLibertyAttr)
    ExprType : TLibertyExprType;
    Left     : TLibertyAttr;
    Right    : TLibertyAttr;
    Constructor Create(AExprType:TLibertyExprType;ALeft,ARight:TLibertyAttr);
  End;

  TLibertyHead = class
    name     : String;
    list     : TLibertyAttrVal;
    lineno   : Integer;
    filename : String;
  End;

  { TLibertyGroup }

  TLibertyGroup = class
    Parent : TLibertyGroup;
    Constructor Create(AParent:TLibertyGroup;AHead:TLibertyHead); virtual;
    Procedure AddSimpleAttr(AIdent:String;AAttr:TLibertyAttr); virtual;
    Procedure AddComplexAttr(AHead:TLibertyHead); virtual;
    Procedure AddGroup(AHead:TLibertyHead;AGroup:TLibertyGroup); virtual;
    Procedure PrintAll; virtual; abstract;
  End;

  TLibertyGroupLibrary = class;

  TLibraryList = specialize TFPGMap<String,TLibertyGroupLibrary>;

  TLibertyGroupCell = class;

  TCellList = specialize TFPGMap<String,TLibertyGroupCell>;

  { TLibertyGroupLibrary }

  TLibertyGroupLibrary = class(TLibertyGroup)
    FName : String;
    FCurrentUnit : Double;
    FCells : TCellList;
    Constructor Create(AParent:TLibertyGroup;AHead:TLibertyHead); override;
    Destructor Destroy; override;
    Procedure AddSimpleAttr(AIdent:String;AAttr:TLibertyAttr); override;
    Procedure AddComplexAttr(AHead:TLibertyHead); override;
    Procedure AddGroup(AHead:TLibertyHead;AGroup:TLibertyGroup); override;
    Procedure PrintAll; override;
  End;

  TLibertyGroupPin = class;

  TPinList = specialize TFPGMap<String,TLibertyGroupPin>;

  { TLibertyGroupCell }

  TLibertyGroupCell = class(TLibertyGroup)
    FName    : String;
    FPadCell : Boolean;
    FDontUse : Boolean;
    FPins    : TPinList;
    Constructor Create(AParent:TLibertyGroup;AHead:TLibertyHead); override;
    Destructor Destroy; override;
    Procedure AddSimpleAttr(AIdent:String;AAttr:TLibertyAttr); override;
    Procedure AddComplexAttr(AHead:TLibertyHead); override;
    Procedure AddGroup(AHead:TLibertyHead;AGroup:TLibertyGroup); override;
    Procedure PrintAll; override;
    Function GetPad      : TLibertyGroupPin;   // can also return Nil
    Function GetInternal : TLibertyGroupPin;   // can also return Nil
    Function CreateModule : TModule;
  End;

  { TLibertyGroupPin }

  TLibertyGroupPin = class(TLibertyGroup)
    FName         : String;
    FDirection    : TPortDirection;
    FFunction     : String;
    FIsPad        : Boolean;
    FThreeState   : String;
    FDriveCurrent : Double;   // relative to current_unit
    Constructor Create(AParent:TLibertyGroup;AHead:TLibertyHead); override;
    Destructor Destroy; override;
    Procedure AddSimpleAttr(AIdent:String;AAttr:TLibertyAttr); override;
    Procedure AddComplexAttr(AHead:TLibertyHead); override;
    Procedure AddGroup(AHead:TLibertyHead;AGroup:TLibertyGroup); override;
    Procedure PrintAll; override;
  End;

Procedure Parse(Filename:String;Var ALibraries : TLibraryList);

Implementation
Uses yacclib, lexlib;

Function LibertyStrToPortDir(St:String):TPortDirection;
Begin
  For Result := Low(TPortDirection) to High(TPortDirection) do
    if St = CPortDirectionVerilog[Result] then Exit;
  // not found
  raise Exception.Create('Invalid direction '''+St+'''');
End;

Procedure AppendAttrVal(List,New : TLibertyAttrVal);
Begin
  repeat
    if not assigned(List.Next) then
      Begin
        List.Next := New;
        Exit;
      End;
    List := List.Next;
  until false;
End;

{ TLibertyAttrVal }

Function TLibertyAttrVal.GetBool:Boolean;
Begin
  if ValType <> LIBERTY__VAL_BOOLEAN then
    raise Exception.Create('Bool value requested but attribute is ' + CLibertyValType[ValType]);
  Result := BoolVal;
End;

Function TLibertyAttrVal.GetInt:Integer;
Begin
  if ValType <> LIBERTY__VAL_INT then
    raise Exception.Create('Int value requested but attribute is ' + CLibertyValType[ValType]);
  Result := IntVal;
End;

Function TLibertyAttrVal.GetDouble:Double;
Begin
  if (ValType <> LIBERTY__VAL_INT) and (ValType <> LIBERTY__VAL_DOUBLE) then
    raise Exception.Create('Double value requested but attribute is ' + CLibertyValType[ValType]);
  if ValType = LIBERTY__VAL_INT then
    Result := IntVal
  else
    Result := DoubleVal;
End;

Function TLibertyAttrVal.GetString:String;
Begin
  if ValType <> LIBERTY__VAL_STRING then
    raise Exception.Create('String value requested but attribute is ' + CLibertyValType[ValType]);
  Result := StringVal;
End;

Function TLibertyAttrVal.ToString:String;
Begin
  Case ValType of
    LIBERTY__VAL_BOOLEAN : if BoolVal then Result := 'true' else Result := 'false';
    LIBERTY__VAL_INT     : Result := IntToStr(IntVal);
    LIBERTY__VAL_DOUBLE  : Result := FloatToStr(DoubleVal);
    LIBERTY__VAL_STRING  : Result := StringVal;
    LIBERTY__VAL_EXPR    : Result := 'EXPRESSION';
  End;
End;

{ TLibertyGroup }

Constructor TLibertyGroup.Create(AParent:TLibertyGroup;AHead:TLibertyHead);
Begin
  inherited Create;
  Parent := AParent;
End;

Procedure TLibertyGroup.AddSimpleAttr(AIdent:String;AAttr:TLibertyAttr);
Begin
  // ignore
End;

Procedure TLibertyGroup.AddComplexAttr(AHead:TLibertyHead);
Begin
  // ignore
End;

Procedure TLibertyGroup.AddGroup(AHead:TLibertyHead;AGroup:TLibertyGroup);
Begin
  // ignore
End;

{ TLibertyGroupLibrary }

Constructor TLibertyGroupLibrary.Create(AParent:TLibertyGroup;AHead:TLibertyHead);
Begin
  Inherited Create(AParent,AHead);
  FCells := TCellList.Create;
  if not assigned(AHead.list) then
    raise Exception.Create('Group ''library'' needs a parameter');
  if AHead.list.ValType <> LIBERTY__VAL_STRING then
    raise Exception.Create('Parameter of group ''liberty'' must be a string');
  FName := AHead.list.StringVal;
End;

Destructor TLibertyGroupLibrary.Destroy;
Var I : Integer;
Begin
  For I := 0 to FCells.Count-1 do
    FCells.Data[I].Free;
  FCells.Free;
  Inherited Destroy;
End;

Procedure TLibertyGroupLibrary.AddSimpleAttr(AIdent:String;AAttr:TLibertyAttr);
Begin
  if AIdent = 'current_unit' then
    Begin
      case (AAttr as TLibertyAttrVal).GetString of
        '1uA' : FCurrentUnit := 1E-6;
      else
        Exception.Create('Unsupported current unit '''+(AAttr as TLibertyAttrVal).GetString+'''');
      End;
    End
  else
//    WriteLn('Library: Ignoring simple attr ',AIdent,' = ',AAttr.ClassName);
    // ignore
    ;
End;

Procedure TLibertyGroupLibrary.AddComplexAttr(AHead:TLibertyHead);
//Var Val : TLibertyAttrVal;
Begin
(*  WriteLn('Library: Ignoring complex attr ',AHead.name);
  Val := AHead.list;
  repeat
    WriteLn('  ',Val.ValType);
    Val := Val.Next;
  Until Val = Nil;*)
End;

Procedure TLibertyGroupLibrary.AddGroup(AHead:TLibertyHead;AGroup:TLibertyGroup);
Var Cell : TLibertyGroupCell;
Begin
//  WriteLn('Library: Adding Group ',AGroup.ClassName,' ',AHead.name);
  if AHead.name = 'cell' then
    Begin
      Cell := AGroup as TLibertyGroupCell;
      FCells.Add(Cell.FName,Cell);
      // TODO
    End
  else
//    WriteLn('Library: Ignoring Group ',AGroup.ClassName,' ',AHead.name);
    // ignore
    ;
End;

Procedure TLibertyGroupLibrary.PrintAll;
Var I : Integer;
Begin
  WriteLn('Library ''',FName,'''');
  For I := 0 to FCells.Count-1 do
    FCells.Data[I].PrintAll;
End;

{ TLibertyGroupCell }

Constructor TLibertyGroupCell.Create(AParent:TLibertyGroup;AHead:TLibertyHead);
Begin
  Inherited Create(AParent,AHead);
  FPins := TPinList.Create;
  if not assigned(AHead.list) then
    raise Exception.Create('Group ''cell'' needs a parameter');
  if AHead.list.ValType <> LIBERTY__VAL_STRING then
    raise Exception.Create('Parameter of group ''cell'' must be a string');
  FName := AHead.list.StringVal;
End;

Destructor TLibertyGroupCell.Destroy;
Var I : Integer;
Begin
  For I := 0 to FPins.Count-1 do
    FPins.Data[I].Free;
  FPins.Free;
  Inherited Destroy;
End;

Procedure TLibertyGroupCell.AddSimpleAttr(AIdent:String;AAttr:TLibertyAttr);
Begin
  if AIdent = 'pad_cell' then
    FPadCell := true
  else if AIdent = 'dont_use' then
    FDontUse := true
  else
//    WriteLn('Cell: Ignoring simple attr ',AIdent,' = ',AAttr.ClassName);
    // ignore
    ;
End;

Procedure TLibertyGroupCell.AddComplexAttr(AHead:TLibertyHead);
//Var Val : TLibertyAttrVal;
Begin
(*  WriteLn('Cell: Ignoring complex attr ',AHead.name);
  Val := AHead.list;
  repeat
    WriteLn('  ',Val.ValType);
    Val := Val.Next;
  Until Val = Nil;*)
End;

Procedure TLibertyGroupCell.AddGroup(AHead:TLibertyHead;AGroup:TLibertyGroup);
Var Pin : TLibertyGroupPin;
Begin
//  WriteLn('Cell: Adding Group ',AGroup.ClassName,' ',AHead.name);
  if AHead.name = 'pin' then
    Begin
      Pin := AGroup as TLibertyGroupPin;
      FPins.Add(Pin.FName,Pin);
      // TODO
    End
  else
    WriteLn('Cell: Ignoring Group ',AGroup.ClassName,' ',AHead.name);
    // ignore
    ;
End;

Procedure TLibertyGroupCell.PrintAll;
Var I : Integer;
Begin
  WriteLn('  Cell ''',FName,'''');
  For I := 0 to FPins.Count-1 do
    FPins.Data[I].PrintAll;
End;

Function TLibertyGroupCell.GetPad:TLibertyGroupPin;
Var I : Integer;
Begin
  Result := Nil;
  For I := 0 to FPins.Count-1 do
    if FPins.Data[I].FIsPad then
      Exit(FPins.Data[I]);
End;

Function TLibertyGroupCell.GetInternal:TLibertyGroupPin;
Var I : Integer;
Begin
  Result := Nil;
  if FPins.Count <> 2 then
    Exit;
  For I := 0 to FPins.Count-1 do
    if not FPins.Data[I].FIsPad then
      Exit(FPins.Data[I]);
End;

Function TLibertyGroupCell.CreateModule:TModule;
Var I : Integer;
Begin
  Result := TModule.Create(FName);
  For I := 0 to FPins.Count-1 do
    Result.AddPort(TPort.Create(FPins.Data[I].FName,FPins.Data[I].FDirection,TypeBit));
End;

{ TLibertyGroupPin }

Constructor TLibertyGroupPin.Create(AParent:TLibertyGroup;AHead:TLibertyHead);
Begin
  Inherited Create(AParent,AHead);
  if not assigned(AHead.list) then
    raise Exception.Create('Group ''pin'' needs a parameter');
  if AHead.list.ValType <> LIBERTY__VAL_STRING then
    raise Exception.Create('Parameter of group ''pin'' must be a string');
  FName := AHead.list.StringVal;
End;

Destructor TLibertyGroupPin.Destroy;
Begin
  Inherited Destroy;
End;

Procedure TLibertyGroupPin.AddSimpleAttr(AIdent:String;AAttr:TLibertyAttr);
Begin
  if AIdent = 'direction' then
    FDirection := LibertyStrToPortDir((AAttr as TLibertyAttrVal).GetString)
  else if AIdent = 'function' then
    FFunction := (AAttr as TLibertyAttrVal).GetString
  else if AIdent = 'is_pad' then
    FIsPad := (AAttr as TLibertyAttrVal).GetBool
  else if AIdent = 'three_state' then
    FThreeState := (AAttr as TLibertyAttrVal).GetString
  else if AIdent = 'drive_current' then
    Begin
      if Parent.Parent is TLibertyGroupLibrary then
        if (Parent.Parent as TLibertyGroupLibrary).FCurrentUnit > 0.0 then
          FDriveCurrent := (AAttr as TLibertyAttrVal).GetDouble * (Parent.Parent as TLibertyGroupLibrary).FCurrentUnit;
      //WriteLn('Pin: Cell ',(Parent as TLibertyGroupCell).FName,' pad ',FName,' drive current = ',FDriveCurrent*1000:1:1,'mA');
    End
  else
//    WriteLn('Pin: Ignoring simple attr ',AIdent,' = ',AAttr.ClassName);
    // ignore
    ;
End;

Procedure TLibertyGroupPin.AddComplexAttr(AHead:TLibertyHead);
//Var Val : TLibertyAttrVal;
Begin
(*  WriteLn('Pin: Ignoring complex attr ',AHead.name);
  Val := AHead.list;
  repeat
    WriteLn('  ',Val.ValType);
    Val := Val.Next;
  Until Val = Nil;*)
End;

Procedure TLibertyGroupPin.AddGroup(AHead:TLibertyHead;AGroup:TLibertyGroup);
Begin
//  WriteLn('Pin: Ignoring Group ',AGroup.ClassName,' ',AHead.name);
End;

Procedure TLibertyGroupPin.PrintAll;
Begin
  WriteLn('    Pin ''',FName,'''');
End;

{ TLibertyExpr }

Constructor TLibertyExpr.Create(AExprType:TLibertyExprType;ALeft,ARight:TLibertyAttr);
Begin
  WriteLn('EXPRESSION');
  inherited Create;
  ExprType := AExprType;
  Left     := ALeft;
  Right    := ARight;
End;

Var
  CurrentGroup : TLibertyGroup;
  LibraryList  : TLibraryList;

Procedure push_group(H:TLibertyHead);
Var Group : TLibertyGroup;
Begin
  Case H.name of
    'library' : Group := TLibertyGroupLibrary.Create(CurrentGroup,H);
    'cell'    : Group := TLibertyGroupCell.   Create(CurrentGroup,H);
    'pin'     : Group := TLibertyGroupPin.    Create(CurrentGroup,H);
  else
    Group := TLibertyGroup.Create(CurrentGroup,H);
  End;
  if assigned(CurrentGroup) then
    CurrentGroup.AddGroup(H,Group);
  CurrentGroup := Group;

  if H.name = 'library' then
    LibraryList.Add((Group as TLibertyGroupLibrary).FName,Group as TLibertyGroupLibrary);
End;

Procedure pop_group(H:TLibertyHead);
Var Group : TLibertyGroup;
Begin
//  WriteLn('End of group ',H.name);
  Group := CurrentGroup;
  CurrentGroup := Group.Parent;
  if Group.ClassType = TLibertyGroup then
    Group.Free;   // free unused groups
End;


{$INCLUDE liberty-parser.pas}
{$INCLUDE liberty-token.pas}

Function MyYyWrap() : Boolean;
Begin
  Close(yyinput);
  Result := true;
End;

(**
 * Parse Liberty file
 *
 * If AModules is Nil, a new instance of this list will be generated. Otherwise
 * the existing modules are used when instances of them are built.
 *)
Procedure Parse(Filename:String;Var ALibraries : TLibraryList);
Begin
  Assign(yyinput,Filename);
  {$I-} Reset(yyinput); {$I+}
  if IOResult <> 0 then
    Begin
      WriteLn('Error: File ',Filename,' not found.');
      Exit;
    End;

  if not assigned(ALibraries) then
    ALibraries := TLibraryList.Create;
  LibraryList := ALibraries;

  CurrentGroup := Nil;

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


  // yyparse() also calls yywrap(), which is a procedure variable, which
  // originally pointed to yylex_yywrap(), which closes yyinput (so we don't
  // have to close it here) and yyoutput (which initially didn't harm, but when
  // using this function twice, WriteLn doesn't have an open output handle any
  // more).
  // Therefore we make our own MyYyWrap() function which only closes yyinput
  // but not yyoutput.
End;


End.

