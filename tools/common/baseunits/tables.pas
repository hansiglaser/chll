Unit Tables;

{$mode objfpc}{$H+}

Interface

Uses
  Classes, SysUtils, Variants;

Type

  { TTable }

  TTable = class
  private
    Type TRow = Array of Variant;
    Type TAggrFunc = Function(A,B:Integer):Integer;
  private var
    FTable : Array of TRow;
    FRows  : Integer;
    FCols  : Integer;
    Procedure Enlarge(ARows,ACols:Integer);
  public
    Constructor Create;
    Destructor  Destroy; override;
    Procedure SetVal(ARow,ACol:Integer;AValue:Variant);
    Function  GetVal(ARow,ACol:Integer):Variant;
    Procedure AddAggregateRow(ARows,ACols:TIntegerSet;StartVal:Integer;Func:TAggrFunc);
    Procedure AddAggregateCol(ARows,ACols:TIntegerSet;StartVal:Integer;Func:TAggrFunc);
    property Rows : Integer read FRows;
    property Cols : Integer read FCols;
  End;

  { TTableVisualizer }

  TTableVisualizer = class
  protected type
    TJustification = (jjLeft,jjCenter,jjRight);
    TJustificationMatrix = Array of Array of TJustification;
    TLineType = (ltNone,ltSingle,ltDouble);
    TLineTypes = Array of TLineType;
  protected var
    FTable           : TTable;
    FOldRows         : Integer;
    FOldCols         : Integer;
    FJustification   : TJustificationMatrix;
    FRowSep          : Array of String;
    FColSep          : Array of String;
    FWidth           : Array of Integer;
    Function GetString(ARow,ACol:Integer) : String;  virtual; abstract;
    Function GetWidth (ARow,ACol:Integer) : Integer; virtual; abstract;
    Function Justify(ASt:String;AJustification:TJustification;AWidth:Integer) : String; virtual; abstract;
  public
    Constructor Create(ATable:TTable);
    Procedure Update;
    Procedure SetJustify(ARow,ACol:Integer;AJustification:TJustification);
    Procedure SetJustifyRow(ARow:Integer;AJustification:TJustification);
    Procedure SetJustifyCol(ACol:Integer;AJustification:TJustification);
    Procedure SetRowSep(ARow:Integer;ASep:String);
    Procedure SetColSep(ACol:Integer;ASep:String);
    Function  GetTable : String;
  End;

  { TTextTable }

  TTextTable = class(TTableVisualizer)
  protected
    Function GetString(ARow,ACol:Integer) : String;  override;
    Function GetWidth (ARow,ACol:Integer) : Integer; override;
    Function Justify(ASt:String;AJustification:TJustification;AWidth:Integer) : String; override;
  End;

Implementation

{ TTable }

Constructor TTable.Create;
Begin
  inherited Create;
End;

Destructor TTable.Destroy;
Begin
  Inherited Destroy;
End;

Procedure TTable.SetVal(ARow,ACol:Integer;AValue:Variant);
Begin
  Enlarge(ARow+1,ACol+1);
  FTable[ARow][ACol] := AValue;
End;

Function TTable.GetVal(ARow,ACol:Integer):Variant;
Begin
  if (ARow >= FRows) or (ACol >= FCols) then Exit(Nil);
  Result := FTable[ARow][ACol];
End;

Procedure TTable.AddAggregateRow(ARows,ACols:TIntegerSet;StartVal:Integer;Func:TAggrFunc);
Var NewRow : Integer;
    R,C    : Integer;
    V      : Integer;
Begin
  NewRow := FRows;   // row index
  For C := 0 to FCols-1 do
    Begin
      if not (C in ACols) then Continue;
      V := StartVal;
      For R := 0 to NewRow-1 do
        Begin
          if not (R in ARows) then Continue;
          if not (VarType(FTable[R][C]) in [varInteger,varShortInt,varByte,varLongWord]) then Continue;
          V := Func(V,FTable[R][C]);
        End;
      if V <> StartVal then
        SetVal(NewRow,C,V);
    End;
End;

Procedure TTable.AddAggregateCol(ARows,ACols:TIntegerSet;StartVal:Integer;Func:TAggrFunc);
Var NewCol : Integer;
    R,C    : Integer;
    V      : Integer;
Begin
  NewCol := FCols;   // col index
  For R := 0 to NewCol-1 do
    Begin
      if not (R in ARows) then Continue;
      V := StartVal;
      For C := 0 to FCols-1 do
        Begin
          if not (C in ACols) then Continue;
          if not (VarType(FTable[R][C]) in [varInteger,varShortInt,varByte,varLongWord]) then Continue;
          V := Func(V,FTable[R][C]);
        End;
      if V <> StartVal then
        SetVal(R,NewCol,V);
    End;
End;

Procedure TTable.Enlarge(ARows,ACols:Integer);
Var R : Integer;
    C : Integer;
Begin
  if (FRows >= ARows) and (FCols >= ACols) then Exit;

  if FRows < ARows then
    Begin
      SetLength(FTable,ARows);
      For R := FRows to ARows-1 do
        Begin
          SetLength(FTable[R],FCols);
          For C := 0 to FCols-1 do
            Begin
              VarClear(FTable[R][C]);
            End;
        End;
      FRows := ARows;
    End;

  if FCols < ACols then
    Begin
      For R := 0 to FRows-1 do
        Begin
          SetLength(FTable[R],ACols);
          For C := FCols to ACols-1 do
            Begin
              VarClear(FTable[R][C]);
            End;
        End;
      FCols := ACols;
    End;
End;

{ TTableVisualizer }

Constructor TTableVisualizer.Create(ATable:TTable);
Begin
  inherited Create;
  FTable := ATable;
  FOldRows := 0;
  FOldCols := 0;
  Update;
End;

Procedure TTableVisualizer.Update;
Var R,C,W : Integer;
Begin
  SetLength(FJustification,FTable.FRows);
  SetLength(FRowSep,FTable.FRows+1);
  SetLength(FColSep,FTable.FCols+1);
  SetLength(FWidth,FTable.FCols);
  FillChar(FWidth[0],Length(FWidth)*SizeOf(FWidth[0]),0);
  For R := 0 to FOldRows-1 do
    Begin
      SetLength(FJustification[R],FTable.FCols);
      For C := FOldCols to FTable.FCols-1 do
        FJustification[R][C] := jjLeft;
      For C := 0 to FTable.FCols-1 do
        Begin
          W := GetWidth(R,C);
          if W > FWidth[C] then
            FWidth[C] := W;
        End;
    End;
  For R := FOldRows to FTable.FRows-1 do
    Begin
      FRowSep[R] := '';
      SetLength(FJustification[R],FTable.FCols);
      For C := 0 to FTable.FCols-1 do
        Begin
          FJustification[R][C] := jjLeft;
          W := GetWidth(R,C);
          if W > FWidth[C] then
            FWidth[C] := W;
        End;
    End;
  FRowSep[FTable.FRows] := '';
  if FOldCols = 0 then
    FColSep[0] := '';
  For C := FOldCols+1 to FTable.FCols-1 do
    FColSep[C] := ' ';
  if FTable.FCols > FOldCols then
    FColSep[FTable.FCols] := '';
  FOldRows := FTable.FRows;
  FOldCols := FTable.FCols;
End;

Procedure TTableVisualizer.SetJustify(ARow,ACol:Integer;AJustification:TJustification);
Begin
  if (ARow > FTable.FRows) or (ACol > FTable.FCols) then
    raise Exception.CreateFmt('Row index %d or column index %d exceeds maximum (%d, %d)',[ARow,ACol,FTable.FRows,FTable.FCols]);
  FJustification[ARow][ACol] := AJustification;
End;

Procedure TTableVisualizer.SetJustifyRow(ARow:Integer;AJustification:TJustification);
Var C : Integer;
Begin
  if ARow > FTable.FRows then
    raise Exception.CreateFmt('Row index %d exceeds maximum (%d)',[ARow,FTable.FRows]);
  For C := 0 to FTable.FCols-1 do
    FJustification[ARow][C] := AJustification;
End;

Procedure TTableVisualizer.SetJustifyCol(ACol:Integer;AJustification:TJustification);
Var R : Integer;
Begin
  if ACol > FTable.FCols then
    raise Exception.CreateFmt('Column index %d exceeds maximum (%d)',[ACol,FTable.FCols]);
  For R := 0 to FTable.FRows-1 do
    FJustification[R][ACol] := AJustification;
End;

Procedure TTableVisualizer.SetRowSep(ARow:Integer;ASep:String); // put before ARow
Begin
  if ARow > FTable.FRows+1 then
    raise Exception.CreateFmt('Row index %d exceeds maximum (%d)',[ARow,FTable.FRows+1]);
  FRowSep[ARow] := ASep;
End;

Procedure TTableVisualizer.SetColSep(ACol:Integer;ASep:String); // put before ACol
Begin
  if ACol > FTable.FCols+1 then
    raise Exception.CreateFmt('Column index %d exceeds maximum (%d)',[ACol,FTable.FCols+1]);
  FColSep[ACol] := ASep;
End;

Function TTableVisualizer.GetTable:String;
Var R,C : Integer;
    W   : Integer;
    St  : String;
Begin
  Result := '';
  W := Length(FColSep[FTable.FCols]);
  For C := 0 to FTable.FCols-1 do
    W := W + Length(FColSep[C]) + FWidth[C];
  For R := 0 to FTable.FRows-1 do
    Begin
      if R > 0 then
        Result += LineEnding;
      if Length(FRowSep[R]) > 0 then
        Begin
          St := '';
          For C := 0 to (W div Length(FRowSep[R])) do    // upper bound intentionally without '-1'
            St := St + FRowSep[R];
          Result += Copy(St,1,W) + LineEnding;
        End;
      For C := 0 to FTable.FCols-1 do
        Begin
          St := Justify(GetString(R,C), FJustification[R][C], FWidth[C]);
          Result += FColSep[C] + St;
        End;
      Result += FColSep[FTable.FCols];
    End;
End;

{ TTextTable }

Function TTextTable.GetString(ARow,ACol:Integer):String;
Var V : Variant;
Begin
  V := FTable.GetVal(ARow,ACol);
  Result := VarToStr(V);
End;

Function TTextTable.GetWidth(ARow,ACol:Integer):Integer;
Begin
  Result := Length(GetString(ARow,ACol));
End;

Function TTextTable.Justify(ASt:String;AJustification:TJustification;AWidth:Integer):String;
Begin
  Case AJustification of
    jjLeft   : Result := ASt + StringOfChar(' ',AWidth-Length(ASt));
    jjCenter : Result := StringOfChar(' ',(AWidth-Length(ASt)) shr 1) + ASt + StringOfChar(' ',(AWidth-Length(ASt)+1) shr 1);
    jjRight  : Result := StringOfChar(' ',AWidth-Length(ASt)) + ASt;
  End;
End;

End.

