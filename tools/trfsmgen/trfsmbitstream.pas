Unit TRFSMBitstream;

{$mode objfpc}{$H+}

Interface

Uses
  Classes, SysUtils, Bitstream, Utils, TRFSMSpec;

Type

(*
TR-FSM Bitsteam:
----------------
The configuration chain starts at the smallest TR and exits at the widest TR:
  CfgDataIn_i => TR0[0] => TR0[1] => ... => TR0[N0-1] => TR1[0] => TR1[1] => ... => TRN[NN-1] => CfgDataOut_o
i.e. the first bit shifted in finally ends up in the last widest TR. Since
TBitstream's LSB corresponds to the first bit shifted in, the order of the
bit stream sections is as follows:
  TRN[NN-1] . TRN[NN-2] . ... . TRN[0] . ... . TR0[N0-1] . ... TR0[0]
  LSB ........................................................... MSB

TR Bitsteam:
------------
In every TR the configuration chain starts at the SSG and ends at the OPR:
  CfgDataIn_i => SSG => ISM => IPG => NSR => OPR => CfgDataOut_o
Therefore the bit stream LSB corresponds to OPR's LSB, i.e. the bit stream
is partitioned in
  OPR . NSR . IPG . ISM . SSG
  LSB ................... MSB

ConfigRegister:
---------------
All configurable entities use the ConfigRegister entity. It internally works
as follows:
  ValueShift(Width-1)          <= CfgDataIn_i;
  ValueShift(Width-2 downto 0) <= ValueShift(Width-1 downto 1);
  CfgDataOut_o                 <= ValueShift(0);
i.e. we fill in at the MSB and get out the data from the LSB. Therefore the
first bit shifted in must be the LSB of the value.

SSG Bitstream:
--------------
The output function of the SSG is defined by
  Match_o <= '1' when State_i = CfgValue else '0';
i.e. if the state vector matches the CfgValue from the ConfigRegister.

ISM:
----
CfgValue(InputWidth-1  downto 0)
Input_i (InputWidth-1  downto 0)
Output_o(OutputWidth-1 downto 0)

       InputWidth = 10
           Input_i
/--------------------------\

9  8  7  6  5  4  3  2  1  0
|  |  |  |  |  |  |  |  |  |
v  v  v  v  v  v  v  v  v  v
|  |  |  |  |  |  |  |  |  |
X--X--X--X--X--X--X--+--+--+---> 3  \
|  |  |  |  |  |  |  |  |  |        |
+--X--X--X--X--X--X--X--+--+---> 2  |
|  |  |  |  |  |  |  |  |  |        | Output_o
+--+--X--X--X--X--X--X--X--+---> 1  | OutputWidth = 4
|  |  |  |  |  |  |  |  |  |        |
+--+--+--X--X--X--X--X--X--X---> 0  /

Therefore the first bit shifted in corresponds to Input_i(0), ...

IPG:
----
signal CfgValue : std_logic_vector(2**InputWidth-1 downto 0);
Match_o <= Enable_i and CfgValue(conv_integer(Input_i));

Therefore the first bit shifted in corresponds to the input pattern "0000".

NSR:
----
The NSR is a pure ConfigRegister. Therefore the state coding is identical to
the coding in the SSG.

OPR:
----
The OPR is a pure ConfigRegister. Therefore the first bit shifted in
corresponds to Output_o(0).

*)

  { TTRFSMBitstream }

  TTRFSMBitstream = class(TBitstream)
  private
    // TR-FSM specification
    FTRFSM     : TTRFSMSpecification;
    // internal constants
    FTRLengths : TDynIntegerArray;
    FOffsetTR  : TDynIntegerArray;
    FOffsetSSG : TDynIntegerArray;
    FOffsetISM : TDynIntegerArray;
    FOffsetIPG : Integer;
    FOffsetNSR : Integer;
    FOffsetOPR : Integer;
    Procedure CheckVersion; virtual;
    { fill all internal constants and returns the total number of bits }
    Function CalcLength : Integer; virtual;
    { get offset of one TR }
    Function GetStart(TRWidth,Num:Integer) : Integer; virtual;
    Procedure FillTRsString(Var Bit,Pos:Integer;Var Result:String);
  public
    Constructor Create(ATRFSM:TTRFSMSpecification);
    Function  GetTRFSMString : String; virtual;
    Function  GetModelSimTCLString(Instance:String)        :String; override;
    Function  GetLECString        (Instance,Options:String):String; override;
    Function  GetFormalityString  (Instance:String)        :String; override;
    { static byte array functions }
    Procedure SetSSG(TRWidth,Num:Integer;Const Bits:Array of Byte);
    Procedure SetISM(TRWidth,Num:Integer;Const Bits:Array of Byte);
    Procedure SetIPG(TRWidth,Num:Integer;Const Bits:Array of Byte);
    Procedure SetNSR(TRWidth,Num:Integer;Const Bits:Array of Byte);
    Procedure SetOPR(TRWidth,Num:Integer;Const Bits:Array of Byte);
    { simple integer parameter functions }
    Procedure SetSSG(TRWidth,Num:Integer;Bits:Integer);
    Procedure SetISM(TRWidth,Num:Integer;Bits:Integer);
    Procedure SetIPG(TRWidth,Num:Integer;Bits:Integer);
    Procedure SetNSR(TRWidth,Num:Integer;Bits:Integer);
    Procedure SetOPR(TRWidth,Num:Integer;Bits:Integer);
    { TBitstream functions }
    Procedure SetSSG(TRWidth,Num:Integer;Bits:TBitstream);
    Procedure SetISM(TRWidth,Num:Integer;Bits:TBitstream);
    Procedure SetIPG(TRWidth,Num:Integer;Bits:TBitstream);
    Procedure SetNSR(TRWidth,Num:Integer;Bits:TBitstream);
    Procedure SetOPR(TRWidth,Num:Integer;Bits:TBitstream);
    { String functions }
    Procedure SetSSG(TRWidth,Num:Integer;Bits:String);
    Procedure SetISM(TRWidth,Num:Integer;Bits:String);
    Procedure SetIPG(TRWidth,Num:Integer;Bits:String);
    Procedure SetNSR(TRWidth,Num:Integer;Bits:String);
    Procedure SetOPR(TRWidth,Num:Integer;Bits:String);
  End;

  { TTRFSMBitstreamReg }

  TTRFSMBitstreamReg = class(TTRFSMBitstream)
  private
    FOffsetRegOut : Integer;
    Procedure CheckVersion; override;
    { fill all internal constants and returns the total number of bits }
    Function CalcLength : Integer; override;
  public
    Constructor Create(ATRFSM:TTRFSMSpecification);
    Function  GetTRFSMString : String; override;
    Function  GetModelSimTCLString(Instance:String)        :String; override;
    Function  GetLECString        (Instance,Options:String):String; override;
    Function  GetFormalityString  (Instance:String)        :String; override;
    { setters }
    Procedure SetOutputsRegistered(OutReg:Boolean);
  End;

  { TISMBitstream }

  TISMBitstream = class(TBitstream)
  private
    FInputs  : Integer;
    FOutputs : Integer;
    Function CalcLength : Integer;
    Function GetOutput(AInput:Integer):Integer;
  public
    Constructor Create(AInputs,AOutputs:Integer);
    Procedure Connect(AInput:Integer);
    Function  MapPattern(AInput:String):String;
    property Output[Index:Integer] : Integer read GetOutput; default;
  End;

  { TOSMBitstream }

  TOSMBitstream = class(TBitstream)
  private
    FInputs  : Integer;
    FOutputs : Integer;
    Function CalcLength : Integer;
    Function GetInput(AOutput:Cardinal):Cardinal;
  public
    Constructor Create(AInputs,AOutputs:Integer);
    Procedure Connect(AOutput:Integer);
    Function  MapPattern(AInput:String):String;
    property Input[Output:Cardinal] : Cardinal read GetInput; default;
  End;

  { TIPGBitstream }

  TIPGBitstream = class(TBitstream)
  private
    FInputs : Integer;
    Function CalcLength : Integer;
  public
    Constructor Create(AInputs:Integer);
    Procedure SetPattern(Input:String);
  End;

Implementation

Uses TypInfo;

{ TTRFSMBitstream }

Constructor TTRFSMBitstream.Create(ATRFSM:TTRFSMSpecification);
Begin
  FTRFSM := ATRFSM;
  CheckVersion;
  Inherited Create(CalcLength);
End;

Function TTRFSMBitstream.GetTRFSMString:String;
Var Bit : Integer;
    Pos : Integer;
Begin
  SetLength(Result,Count
                   + FTRFSM.TotalTRs*1                      { newlines after every TR }
                   + FTRFSM.NumTRs[0] * 2                   { spaces between SSG-NSR-OPR for 0-TRs}
                   + (FTRFSM.TotalTRs-FTRFSM.NumTRs[0]) * 4 { spaces between SSG-ISM-IPG-NSR-OPR });
  Fillchar(Result[1],Length(Result),'0');  // fill with '0', set to '1' or ' ' or ^J
  Bit := Count-1;
  Pos := 1;
  FillTRsString(Bit,Pos,Result);
End;

Procedure TTRFSMBitstream.FillTRsString(Var Bit,Pos : Integer;Var Result : String);

  Procedure SetBits(Num:Integer);
  Var I : Integer;
  Begin
    if Bit < 0 then
      raise Exception.CreateFmt('Bit index %d exceeds bitstream length %d',[Bit,Count]);
    For I := 0 to Num-1 do
      Begin
        if GetBitNoCheck(Bit){FBitstream[Bit shr 3] and (1 shl (Bit and $07)) <> 0} then
          Result[Pos] := '1';  // '0' is default
        Dec(Bit);
        Inc(Pos);
      End;
  End;

Var Width,Index : Integer;
Begin
  For Index := 0 to FTRFSM.NumTRs[0]-1 do
    Begin
      // SSG
      SetBits(FTRFSM.StateBits);
      Result[Pos] := ' '; Inc(Pos);
      // NSR
      SetBits(FTRFSM.StateBits);
      Result[Pos] := ' '; Inc(Pos);
      // OPR
      SetBits(FTRFSM.Outputs);
      Result[Pos] := ^J; Inc(Pos);
    End;
  For Width := 1 to FTRFSM.MaxTRWidth do
    For Index := 0 to FTRFSM.NumTRs[Width]-1 do
      Begin
        // SSG
        SetBits(FTRFSM.StateBits);
        Result[Pos] := ' '; Inc(Pos);
        // ISM
        SetBits(FTRFSM.Inputs);
        Result[Pos] := ' '; Inc(Pos);
        // IPG
        SetBits(1 shl Width);
        Result[Pos] := ' '; Inc(Pos);
        // NSR
        SetBits(FTRFSM.StateBits);
        Result[Pos] := ' '; Inc(Pos);
        // OPR
        SetBits(FTRFSM.Outputs);
        Result[Pos] := ^J; Inc(Pos);
      End;
End;

Function TTRFSMBitstream.GetModelSimTCLString(Instance:String):String;
Var Bit         : Integer;
    Width,Index : Integer;
    Prefix      : String;

  Function GetBits(Num:Integer) : String;
  Var I : Integer;
  Begin
    Result := StringOfChar('0',Num);
    if Bit < 0 then
      raise Exception.CreateFmt('Bit index %d exceeds bitstream length %d',[Bit,Count]);
    For I := 0 to Num-1 do
      Begin
        if GetBitNoCheck(Bit){FBitstream[Bit shr 3] and (1 shl (Bit and $07)) <> 0} then
          Result[I+1] := '1';  // '0' is default
        Dec(Bit);
      End;
  End;

Begin
  Result := '';
  Prefix := 'force -freeze ' + Instance + '/';
  Bit := Count-1;
  For Index := 0 to FTRFSM.NumTRs[0]-1 do
    Begin
      // TR Width = 0
      // SSG
      Result := Result + Prefix + 'GenerateTransitionRowsOfWidth(0)/GenerateTransitionRows('+IntToStr(Index)+')/TransitionRow_inst/StateSelectionGate_inst/Cfg/ValueShift "'+GetBits(FTRFSM.StateBits)+'"' + ^J;
      // NSR
      Result := Result + Prefix + 'GenerateTransitionRowsOfWidth(0)/GenerateTransitionRows('+IntToStr(Index)+')/TransitionRow_inst/NextStateRegister_inst/ValueShift "'+GetBits(FTRFSM.StateBits)+'"' + ^J;
      // OPR
      Result := Result + Prefix + 'GenerateTransitionRowsOfWidth(0)/GenerateTransitionRows('+IntToStr(Index)+')/TransitionRow_inst/OutputPatternRegister_inst/ValueShift "'+GetBits(FTRFSM.Outputs)+'"' + ^J;
    End;
  For Width := 1 to FTRFSM.MaxTRWidth do
    For Index := 0 to FTRFSM.NumTRs[Width]-1 do
      Begin
        // SSG
        Result := Result + Prefix + 'GenerateTransitionRowsOfWidth('+IntToStr(Width)+')/GenerateTransitionRows('+IntToStr(Index)+')/TransitionRow_inst/StateSelectionGate_inst/Cfg/ValueShift "'+GetBits(FTRFSM.StateBits)+'"' + ^J;
        // ISM
        Result := Result + Prefix + 'GenerateTransitionRowsOfWidth('+IntToStr(Width)+')/GenerateTransitionRows('+IntToStr(Index)+')/TransitionRow_inst/GenISMIPG/InputSwitchingMatrix_inst/Cfg/ValueShift "'+GetBits(FTRFSM.Inputs)+'"' + ^J;
        // IPG
        Result := Result + Prefix + 'GenerateTransitionRowsOfWidth('+IntToStr(Width)+')/GenerateTransitionRows('+IntToStr(Index)+')/TransitionRow_inst/GenISMIPG/InputPatternGate_inst/Cfg/ValueShift "'+GetBits(1 shl Width)+'"' + ^J;
        // NSR
        Result := Result + Prefix + 'GenerateTransitionRowsOfWidth('+IntToStr(Width)+')/GenerateTransitionRows('+IntToStr(Index)+')/TransitionRow_inst/NextStateRegister_inst/ValueShift "'+GetBits(FTRFSM.StateBits)+'"' + ^J;
        // OPR
        Result := Result + Prefix + 'GenerateTransitionRowsOfWidth('+IntToStr(Width)+')/GenerateTransitionRows('+IntToStr(Index)+')/TransitionRow_inst/OutputPatternRegister_inst/ValueShift "'+GetBits(FTRFSM.Outputs)+'"' + ^J;
      End;
End;

Function TTRFSMBitstream.GetLECString(Instance,Options:String):String;
Var Bit         : Integer;
    Width,Index : Integer;

  Procedure AddInstanceConstraint(ThisInstance:String;Num:Integer);
  Var I : Integer;
  Begin
    if Bit < 0 then
      raise Exception.CreateFmt('Bit index %d exceeds bitstream length %d',[Bit,Count]);
    Dec(Bit,Num);
    For I := 0 to Num-1 do
      Result := Result + 'add instance constraint ' + Chr(Ord('0')+Ord(GetBit(Bit+I+1))) + ' ' + ThisInstance + '[' + IntToStr(I) + ']' + Options + ^J;
  End;

Begin
  Result := '';
  Bit := Count-1;
  For Index := 0 to FTRFSM.NumTRs[0]-1 do
    Begin
      // TR Width = 0
      // SSG
      AddInstanceConstraint(Instance + '/TransitionRow_inst_0_'+IntToStr(Index)+'/StateSelectionGate_inst/Cfg/ValueShift_reg',FTRFSM.StateBits);
      // NSR
      AddInstanceConstraint(Instance + '/TransitionRow_inst_0_'+IntToStr(Index)+'/NextStateRegister_inst/ValueShift_reg',     FTRFSM.StateBits);
      // OPR
      AddInstanceConstraint(Instance + '/TransitionRow_inst_0_'+IntToStr(Index)+'/OutputPatternRegister_inst/ValueShift_reg', FTRFSM.Outputs);

    End;
  For Width := 1 to FTRFSM.MaxTRWidth do
    For Index := 0 to FTRFSM.NumTRs[Width]-1 do
      Begin
        // SSG
        AddInstanceConstraint(Instance + '/TransitionRow_inst_'+IntToStr(Width)+'_'+IntToStr(Index)+'/StateSelectionGate_inst/Cfg/ValueShift_reg',  FTRFSM.StateBits);
        // ISM
        AddInstanceConstraint(Instance + '/TransitionRow_inst_'+IntToStr(Width)+'_'+IntToStr(Index)+'/InputSwitchingMatrix_inst/Cfg/ValueShift_reg',FTRFSM.Inputs);
        // IPG
        AddInstanceConstraint(Instance + '/TransitionRow_inst_'+IntToStr(Width)+'_'+IntToStr(Index)+'/InputPatternGate_inst/Cfg/ValueShift_reg',    1 shl Width);
        // NSR
        AddInstanceConstraint(Instance + '/TransitionRow_inst_'+IntToStr(Width)+'_'+IntToStr(Index)+'/NextStateRegister_inst/ValueShift_reg',       FTRFSM.StateBits);
        // OPR
        AddInstanceConstraint(Instance + '/TransitionRow_inst_'+IntToStr(Width)+'_'+IntToStr(Index)+'/OutputPatternRegister_inst/ValueShift_reg',   FTRFSM.Outputs);
      End;
End;

Function TTRFSMBitstream.GetFormalityString(Instance:String):String;
Var Bit         : Integer;
    Width,Index : Integer;

  Procedure SetConstant(ThisInstance:String;Num:Integer);
  Var I : Integer;
  Begin
    if Bit < 0 then
      raise Exception.CreateFmt('Bit index %d exceeds bitstream length %d',[Bit,Count]);
    Dec(Bit,Num);
    For I := 0 to Num-1 do
      Result := Result + 'set_constant -type cell {' + ThisInstance + '[' + IntToStr(I) + ']' + '} ' + Chr(Ord('0')+Ord(GetBit(Bit+I+1))) + ^J;
  End;

Begin
  Result := '';
  Bit := Count-1;
  For Index := 0 to FTRFSM.NumTRs[0]-1 do
    Begin
      // TR Width = 0
      // SSG
      SetConstant(Instance + '/TransitionRow_inst_0_'+IntToStr(Index)+'/StateSelectionGate_inst/Cfg/ValueShift_reg',FTRFSM.StateBits);
      // NSR
      SetConstant(Instance + '/TransitionRow_inst_0_'+IntToStr(Index)+'/NextStateRegister_inst/ValueShift_reg',     FTRFSM.StateBits);
      // OPR
      SetConstant(Instance + '/TransitionRow_inst_0_'+IntToStr(Index)+'/OutputPatternRegister_inst/ValueShift_reg', FTRFSM.Outputs);

    End;
  For Width := 1 to FTRFSM.MaxTRWidth do
    For Index := 0 to FTRFSM.NumTRs[Width]-1 do
      Begin
        // SSG
        SetConstant(Instance + '/TransitionRow_inst_'+IntToStr(Width)+'_'+IntToStr(Index)+'/StateSelectionGate_inst/Cfg/ValueShift_reg',  FTRFSM.StateBits);
        // ISM
        SetConstant(Instance + '/TransitionRow_inst_'+IntToStr(Width)+'_'+IntToStr(Index)+'/InputSwitchingMatrix_inst/Cfg/ValueShift_reg',FTRFSM.Inputs);
        // IPG
        SetConstant(Instance + '/TransitionRow_inst_'+IntToStr(Width)+'_'+IntToStr(Index)+'/InputPatternGate_inst/Cfg/ValueShift_reg',    1 shl Width);
        // NSR
        SetConstant(Instance + '/TransitionRow_inst_'+IntToStr(Width)+'_'+IntToStr(Index)+'/NextStateRegister_inst/ValueShift_reg',       FTRFSM.StateBits);
        // OPR
        SetConstant(Instance + '/TransitionRow_inst_'+IntToStr(Width)+'_'+IntToStr(Index)+'/OutputPatternRegister_inst/ValueShift_reg',   FTRFSM.Outputs);
      End;
End;

Procedure TTRFSMBitstream.SetSSG(TRWidth,Num:Integer;Const Bits:Array Of Byte);
Begin
  SetBits(GetStart(TRWidth,Num)+FOffsetSSG[TRWidth],Bits,FTRFSM.StateBits);
End;

Procedure TTRFSMBitstream.SetISM(TRWidth,Num:Integer;Const Bits:Array Of Byte);
Begin
  SetBits(GetStart(TRWidth,Num)+FOffsetISM[TRWidth],Bits,FTRFSM.Inputs);
End;

Procedure TTRFSMBitstream.SetIPG(TRWidth,Num:Integer;Const Bits:Array Of Byte);
Begin
  SetBits(GetStart(TRWidth,Num)+FOffsetIPG,Bits,1 shl TRWidth);
End;

Procedure TTRFSMBitstream.SetNSR(TRWidth,Num:Integer;Const Bits:Array Of Byte);
Begin
  SetBits(GetStart(TRWidth,Num)+FOffsetNSR,Bits,FTRFSM.StateBits);
End;

Procedure TTRFSMBitstream.SetOPR(TRWidth,Num:Integer;Const Bits:Array Of Byte);
Begin
  SetBits(GetStart(TRWidth,Num)+FOffsetOPR,Bits,FTRFSM.Outputs);
End;

Procedure TTRFSMBitstream.SetSSG(TRWidth,Num:Integer;Bits:Integer);
Begin
  SetBits(GetStart(TRWidth,Num)+FOffsetSSG[TRWidth],PByteArray(@Bits)^,FTRFSM.StateBits);
End;

Procedure TTRFSMBitstream.SetISM(TRWidth,Num:Integer;Bits:Integer);
Begin
  SetBits(GetStart(TRWidth,Num)+FOffsetISM[TRWidth],PByteArray(@Bits)^,FTRFSM.Inputs);
End;

Procedure TTRFSMBitstream.SetIPG(TRWidth,Num:Integer;Bits:Integer);
Begin
  SetBits(GetStart(TRWidth,Num)+FOffsetIPG,PByteArray(@Bits)^,1 shl TRWidth);
End;

Procedure TTRFSMBitstream.SetNSR(TRWidth,Num:Integer;Bits:Integer);
Begin
  SetBits(GetStart(TRWidth,Num)+FOffsetNSR,PByteArray(@Bits)^,FTRFSM.StateBits);
End;

Procedure TTRFSMBitstream.SetOPR(TRWidth,Num:Integer;Bits:Integer);
Begin
  SetBits(GetStart(TRWidth,Num)+FOffsetOPR,PByteArray(@Bits)^,FTRFSM.Outputs);
End;

Procedure TTRFSMBitstream.SetSSG(TRWidth,Num:Integer;Bits:TBitstream);
Begin
  if Bits.Count <> FTRFSM.StateBits then
    raise Exception.CreateFmt('Invalid SSG config bitstream "%s": requires %d bits',[Bits.GetString,FTRFSM.StateBits]);
  SetBits(GetStart(TRWidth,Num)+FOffsetSSG[TRWidth],Bits);
End;

Procedure TTRFSMBitstream.SetISM(TRWidth,Num:Integer;Bits:TBitstream);
Begin
  if Bits.Count <> FTRFSM.Inputs then
    raise Exception.CreateFmt('Invalid ISM config bitstream "%s": requires %d bits',[Bits.GetString,FTRFSM.Inputs]);
  SetBits(GetStart(TRWidth,Num)+FOffsetISM[TRWidth],Bits);
End;

Procedure TTRFSMBitstream.SetIPG(TRWidth,Num:Integer;Bits:TBitstream);
Begin
  if Bits.Count <> 1 shl TRWidth then
    raise Exception.CreateFmt('Invalid IPG config bitstream "%s": requires %d bits',[Bits.GetString,1 shl TRWidth]);
  SetBits(GetStart(TRWidth,Num)+FOffsetIPG,Bits);
End;

Procedure TTRFSMBitstream.SetNSR(TRWidth,Num:Integer;Bits:TBitstream);
Begin
  if Bits.Count <> FTRFSM.StateBits then
    raise Exception.CreateFmt('Invalid NSR config bitstream "%s": requires %d bits',[Bits.GetString,FTRFSM.StateBits]);
  SetBits(GetStart(TRWidth,Num)+FOffsetNSR,Bits);
End;

Procedure TTRFSMBitstream.SetOPR(TRWidth,Num:Integer;Bits:TBitstream);
Begin
  if Bits.Count <> FTRFSM.Outputs then
    raise Exception.CreateFmt('Invalid OPR config bitstream "%s": requires %d bits',[Bits.GetString,FTRFSM.Outputs]);
  SetBits(GetStart(TRWidth,Num)+FOffsetOPR,Bits);
End;

Procedure TTRFSMBitstream.SetSSG(TRWidth,Num:Integer;Bits:String);
Begin
  if CountBits(Bits) <> FTRFSM.StateBits then
    raise Exception.CreateFmt('Invalid SSG config bits "%s": requires %d bits',[Bits,FTRFSM.StateBits]);
  SetBits(GetStart(TRWidth,Num)+FOffsetSSG[TRWidth],Bits);
End;

Procedure TTRFSMBitstream.SetISM(TRWidth,Num:Integer;Bits:String);
Begin
  if CountBits(Bits) <> FTRFSM.Inputs then
    raise Exception.CreateFmt('Invalid ISM config bits "%s": requires %d bits',[Bits,FTRFSM.Inputs]);
  SetBits(GetStart(TRWidth,Num)+FOffsetISM[TRWidth],Bits);
End;

Procedure TTRFSMBitstream.SetIPG(TRWidth,Num:Integer;Bits:String);
Begin
  if CountBits(Bits) <> 1 shl TRWidth then
    raise Exception.CreateFmt('Invalid IPG config bits "%s": requires %d bits',[Bits,1 shl TRWidth]);
  SetBits(GetStart(TRWidth,Num)+FOffsetIPG,Bits);
End;

Procedure TTRFSMBitstream.SetNSR(TRWidth,Num:Integer;Bits:String);
Begin
  if CountBits(Bits) <> 1 shl FTRFSM.StateBits then
    raise Exception.CreateFmt('Invalid NSR config bits "%s": requires %d bits',[Bits,FTRFSM.StateBits]);
  SetBits(GetStart(TRWidth,Num)+FOffsetNSR,Bits);
End;

Procedure TTRFSMBitstream.SetOPR(TRWidth,Num:Integer;Bits:String);
Begin
  if CountBits(Bits) <> 1 shl FTRFSM.Outputs then
    raise Exception.CreateFmt('Invalid OPR config bits "%s": requires %d bits',[Bits,FTRFSM.Outputs]);
  SetBits(GetStart(TRWidth,Num)+FOffsetOPR,Bits);
End;

Procedure TTRFSMBitstream.CheckVersion;
Begin
  if FTRFSM.Version <> 'SNOPS1' then
    raise Exception.Create('TTRFSMBitstream only works with TR-FSM version SNOPS1, not '''+FTRFSM.Version+'''');
End;

Function TTRFSMBitstream.CalcLength:Integer;
Var I : Integer;
Begin
  With FTRFSM do
    Begin
      if Length(FTRLengths) = 0 then
        Begin
          { first time, calculate everything }
          Result := 0;
          SetLength(FTRLengths,MaxTRWidth+1);
          // prepare offsets within TR
          SetLength(FOffsetSSG,MaxTRWidth+1);
          SetLength(FOffsetISM,MaxTRWidth+1);
          SetLength(FOffsetTR, MaxTRWidth+1);
          { SSG[MSB]..SSG[LSB] . ISM[MSB]..ISM[LSB] . IPG[MSB]..IPG[LSB] . NSR[MSB]..NSR[LSB] . OPR[MSB]..OPR[LSB] }
          { N .................................................................................................. 0 }
          FOffsetOPR    := 0;
          FOffsetNSR    := FOffsetOPR + Outputs;
          FOffsetIPG    := FOffsetNSR + StateBits;
          { TRN..1 }
          For I := MaxTRWidth downto 1 do
            Begin
              FOffsetTR [I] := Result;
              FOffsetISM[I] := FOffsetIPG + (1 shl I);
              FOffsetSSG[I] := FOffsetISM[I] + Inputs;
              FTRLengths[I] := FOffsetSSG[I] + StateBits;
              Result := Result + NumTRs[I] * FTRLengths[I];
            End;
          { TR0 }
          FOffsetTR [0] := Result;
          FOffsetSSG[0] := FOffsetNSR + StateBits;
          FTRLengths[0] := FOffsetSSG[0] + StateBits;
          Result := Result + NumTRs[0] * FTRLengths[0];
        End
      else
        { we were already called before, returned the precalculated value }
        Result := Count;
    End;
End;

Function TTRFSMBitstream.GetStart(TRWidth,Num:Integer):Integer;
Begin
  if (TRWidth < 0) or (TRWidth > FTRFSM.MaxTRWidth) then
    raise Exception.CreateFmt('Invalid TR width %d',[TRWidth]);
  if (Num < 0) or (Num >= FTRFSM.NumTRs[TRWidth]) then
    raise Exception.CreateFmt('TR index %d for width %d out of range',[Num,TRWidth]);
  Result := FOffsetTR[TRWidth] + (FTRFSM.NumTRs[TRWidth]-1-Num) * FTRLengths[TRWidth];
End;

{ TTRFSMBitstreamReg }

Constructor TTRFSMBitstreamReg.Create(ATRFSM:TTRFSMSpecification);
Begin
  inherited Create(ATRFSM);
End;

Function TTRFSMBitstreamReg.GetTRFSMString:String;
Var Bit : Integer;
    Pos : Integer;
Begin
  SetLength(Result,Count
                   + 1                                      { ^J after register MUX}
                   + FTRFSM.TotalTRs*1                      { newlines after every TR }
                   + FTRFSM.NumTRs[0] * 2                   { spaces between SSG-NSR-OPR for 0-TRs}
                   + (FTRFSM.TotalTRs-FTRFSM.NumTRs[0]) * 4 { spaces between SSG-ISM-IPG-NSR-OPR });
  Fillchar(Result[1],Length(Result),'0');  // fill with '0', set to '1' or ' ' or ^J

  Bit := Count-1;
  Pos := 1;
  Result[1] := Select('1','0',GetBitNoCheck(FOffsetRegOut));
  Result[2] := ^J;
  Dec(Bit);
  Inc(Pos,2);
  FillTRsString(Bit,Pos,Result);
End;

Function TTRFSMBitstreamReg.GetModelSimTCLString(Instance:String):String;
Begin
  raise Exception.Create('TODO: not yet implemented');
End;

Function TTRFSMBitstreamReg.GetLECString(Instance,Options:String):String;
Begin
  raise Exception.Create('TODO: not yet implemented');
End;

Function TTRFSMBitstreamReg.GetFormalityString(Instance:String):String;
Begin
  raise Exception.Create('TODO: not yet implemented');
End;

Procedure TTRFSMBitstreamReg.SetOutputsRegistered(OutReg:Boolean);
Begin
  SetBit(FOffsetRegOut,OutReg);
End;

Procedure TTRFSMBitstreamReg.CheckVersion;
Begin
  if FTRFSM.Version <> 'REG' then
    raise Exception.Create('TTRFSMBitstreamReg only works with TR-FSM version REG, not '''+FTRFSM.Version+'''');
End;

Function TTRFSMBitstreamReg.CalcLength:Integer;
Var I : Integer;
Begin
  With FTRFSM do
    Begin
      if Length(FTRLengths) = 0 then
        Begin
          { first time, calculate everything }
          Result := 0;
          // TRs
          SetLength(FTRLengths,MaxTRWidth+1);
          // prepare offsets within TR
          SetLength(FOffsetSSG,MaxTRWidth+1);
          SetLength(FOffsetISM,MaxTRWidth+1);
          SetLength(FOffsetTR, MaxTRWidth+1);
          { SSG[MSB]..SSG[LSB] . ISM[MSB]..ISM[LSB] . IPG[MSB]..IPG[LSB] . NSR[MSB]..NSR[LSB] . OPR[MSB]..OPR[LSB] }
          { N .................................................................................................. 0 }
          FOffsetOPR    := 0;
          FOffsetNSR    := FOffsetOPR + Outputs;
          FOffsetIPG    := FOffsetNSR + StateBits;
          { TRN..1 }
          For I := MaxTRWidth downto 1 do
            Begin
              FOffsetTR [I] := Result;
              FOffsetISM[I] := FOffsetIPG + (1 shl I);
              FOffsetSSG[I] := FOffsetISM[I] + Inputs;
              FTRLengths[I] := FOffsetSSG[I] + StateBits;
              Result := Result + NumTRs[I] * FTRLengths[I];
            End;
          { TR0 }
          FOffsetTR [0] := Result;
          FOffsetSSG[0] := FOffsetNSR + StateBits;
          FTRLengths[0] := FOffsetSSG[0] + StateBits;
          Result := Result + NumTRs[0] * FTRLengths[0];
          // MUX around OutputRegister
          FOffsetRegOut := Result;
          Inc(Result);
        End
      else
        { we were already called before, returned the precalculated value }
        Result := Count;
    End;
End;

{ TISMBitstream }

Constructor TISMBitstream.Create(AInputs,AOutputs:Integer);
Begin
  FInputs  := AInputs;
  FOutputs := AOutputs;
  Inherited Create(CalcLength);
End;

Procedure TISMBitstream.Connect(AInput:Integer);
Begin
  if GetBit(AInput) then
    raise Exception.CreateFmt('Input %d is already connected',[AInput]);
  if GetBitCount >= FOutputs then
    raise Exception.CreateFmt('All %d inputs are already used',[FInputs]);
  SetBit(AInput);
End;

Function TISMBitstream.CalcLength:Integer;
Begin
  Result := FInputs;
End;

Function TISMBitstream.GetOutput(AInput:Integer):Integer;
Var I : Integer;
Begin
  if not GetBit(AInput) then
    raise Exception.CreateFmt('Input %d is not connected to any output',[AInput]);
  Result := FOutputs;
  For I := FInputs-1 downto AInput do
    Begin
      //Write('I = ',I,', bit = ',GetBit(I),', Result = ',Result);
      if GetBit(I) or (Result > I) then
        Dec(Result);
      //WriteLn(' -> ',Result);
    End;
End;

Function TISMBitstream.MapPattern(AInput:String):String;
Var PosIn : Integer;
Begin
  if Length(AInput) <> FInputs then
    raise Exception.CreateFmt('Input pattern "%s" has wrong number of chars (%d), need %d',[AInput,Length(AInput),FInputs]);
  // prepare all don't cares for output pattern
  Result := StringOfChar('-',FOutputs);
  For PosIn := 0 to FInputs-1 do
    Begin
      if AInput[PosIn+1] <> '-' then
        Result[GetOutput(PosIn)+1] := AInput[PosIn+1];
    End;
End;

{ TOSMBitstream }

Constructor TOSMBitstream.Create(AInputs,AOutputs:Integer);
Begin
  FInputs  := AInputs;
  FOutputs := AOutputs;
  Inherited Create(CalcLength);
End;

Function TOSMBitstream.CalcLength:Integer;
Begin
  Result := FOutputs;
End;

Procedure TOSMBitstream.Connect(AOutput:Integer);
Begin
  if GetBit(AOutput) then
    raise Exception.CreateFmt('Output %d is already connected',[AOutput]);
  if GetBitCount >= FInputs then
    raise Exception.CreateFmt('All %d inputs are already used',[FInputs]);
  SetBit(AOutput);
End;

Function TOSMBitstream.MapPattern(AInput:String):String;
Begin
  raise Exception.Create('TODO');
End;

Function TOSMBitstream.GetInput(AOutput:Cardinal):Cardinal;
Var I : Integer;
Begin
  if not GetBit(AOutput) then
    raise Exception.CreateFmt('Output %d is not connected from any input',[AOutput]);
  Result := FInputs;
  For I := FOutputs-1 downto AOutput do
    Begin
      if GetBit(I) or (Result > I) then
        Dec(Result);
    End;
End;

{ TIPGBitstream }

Constructor TIPGBitstream.Create(AInputs:Integer);
Begin
  FInputs := AInputs;
  Inherited Create(CalcLength);
End;

Procedure TIPGBitstream.SetPattern(Input:String);

  (**
   * Create value from input pattern filling don't cares
   *
   * Pattern[1] = MSB, Pattern[Length] = LSB
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
  if Length(Input) <> FInputs then
    raise Exception.CreateFmt('Invalid input pattern "%s": Length %d does not match number of inputs %d',[Input,Length(Input),FInputs]);
  NumDontCare := CountChars('-',Input);
  // iterate over all don't care combinations
  For I := 0 to (1 shl NumDontCare)-1 do
    Begin
      Value := PatternToValue(Input,I);
      SetBit(Value);
    End;
End;

Function TIPGBitstream.CalcLength:Integer;
Begin
  Result := 1 shl FInputs;
End;

End.

