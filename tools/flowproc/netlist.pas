Unit Netlist;

{$mode objfpc}{$H+}
{$INTERFACES CORBA}

Interface

Uses
  Classes, SysUtils, FGL, RegExpr,
  Utils, StackTrace;

Type

  { TValue }

  TValue = class abstract
    Function GetVerilogValue : String; virtual;
    Function GetVHDLValue    : String; virtual;
    Function GetILangValue   : String; virtual;
  End;

  { TValueInteger }

  TValueInteger = class(TValue)
    FValue : Integer;
    Constructor Create(AValue:Integer);
    Function GetVerilogValue : String; override;
    Function GetVHDLValue : String; override;
    Function GetILangValue : String; override;
  End;

  { TValueIntegerHex }

  TValueIntegerHex = class(TValueInteger)
    Function GetVerilogValue : String; override;
    Function GetVHDLValue : String; override;
    Function GetILangValue : String; override;
  End;

  { TValueFloat }

  TValueFloat = class(TValue)
    FValue : Double;
    Constructor Create(AValue:Double);
    Function GetVerilogValue : String; override;
    Function GetVHDLValue : String; override;
  End;

  TTimeUnit = (tuFS,tuPS,tuNS,tuUS,tuMS,tuSec,tuMin,tuHr);

  { TValueTime }

  TValueTime = class(TValueFloat)
    FUnit : TTimeUnit;
    Constructor Create(AValue:Double;AUnit:TTimeUnit);
    Function GetVerilogValue : String; override;
    Function GetVHDLValue : String; override;
  End;

  { TValueString }

  TValueString = class(TValue)   // never used
    FValue : String;
    Constructor Create(AValue:String);
    Function GetVerilogValue : String; override;
    Function GetVHDLValue : String; override;
    Function GetILangValue : String; override;
  End;

  { TValueBit }

  TValueBit = class(TValue)
    FValue : Char;
    Constructor Create(AValue:Char);
    Function GetVerilogValue : String; override;
    Function GetVHDLValue : String; override;
    Function GetILangValue : String; override;
  End;

  { TValueVector }

  TValueVector = class(TValue)
    FWidth : Integer;
    FValue : String;
    Constructor Create(AWidth:Integer;AValue:String);
    Constructor Create(AWidth:Integer;AValue:Integer);
    Function GetVerilogValue : String; override;
    Function GetVHDLValue : String; override;
    Function GetILangValue : String; override;
  End;

  { TValueOperator }

  TValueOperator = class abstract (TValue)
  End;

  { TValueOperatorUnary }

  TValueOperatorUnary = class(TValueOperator)  // takes 1 operand
    FValue : TValue;
    Constructor Create(AValue : TValue);
    Function GetVerilogValue : String; override;
    Function GetVHDLValue : String; override;
    class Function GetVerilogOperator : String; virtual;
    class Function GetVHDLOperator    : String; virtual;
  End;

  { TValueOperatorNot }

  TValueOperatorNot = class(TValueOperatorUnary)
    class Function GetVerilogOperator : String; override;
    class Function GetVHDLOperator    : String; override;
  End;

  { TValueOperatorFunction }

  TValueOperatorFunction = class(TValueOperatorUnary)
    FName : String;
    Constructor Create(AName : String; AValue : TValue);
    Function GetVerilogValue : String; override;
    Function GetVHDLValue : String; override;
  End;

  { TValueOperatorBinary }

  TValueOperatorBinary = class(TValueOperator)   // takes 2 operands
    FValueA, FValueB : TValue;
    Constructor Create(AValueA,AValueB : TValue);
    Function GetVerilogValue : String; override;
    Function GetVHDLValue : String; override;
    class Function GetVerilogOperator : String; virtual; abstract;
    class Function GetVHDLOperator    : String; virtual; abstract;
  End;

  { TValueOperatorPlus }

  TValueOperatorPlus = class(TValueOperatorBinary)
    class Function GetVerilogOperator : String; override;
    class Function GetVHDLOperator    : String; override;
  End;

  { TValueOperatorMinus }

  TValueOperatorMinus = class(TValueOperatorBinary)
    class Function GetVerilogOperator : String; override;
    class Function GetVHDLOperator    : String; override;
  End;

  { TValueOperatorTimes }

  TValueOperatorTimes = class(TValueOperatorBinary)
    class Function GetVerilogOperator : String; override;
    class Function GetVHDLOperator    : String; override;
  End;

  { TValueOperatorDivide }

  TValueOperatorDivide = class(TValueOperatorBinary)
    class Function GetVerilogOperator : String; override;
    class Function GetVHDLOperator    : String; override;
  End;

  { TValueOperatorAnd }

  TValueOperatorAnd = class(TValueOperatorBinary)
    class Function GetVerilogOperator : String; override;
    class Function GetVHDLOperator    : String; override;
  End;

  { TValueOperatorOr }

  TValueOperatorOr = class(TValueOperatorBinary)
    class Function GetVerilogOperator : String; override;
    class Function GetVHDLOperator    : String; override;
  End;

  { TValueOperatorXor }

  TValueOperatorXor = class(TValueOperatorBinary)
    class Function GetVerilogOperator : String; override;
    class Function GetVHDLOperator    : String; override;
  End;

  { TValueOperatorEqual }

  TValueOperatorEqual = class(TValueOperatorBinary)
    class Function GetVerilogOperator : String; override;
    class Function GetVHDLOperator    : String; override;
  End;

  { TValueOperatorUnequal }

  TValueOperatorUnequal = class(TValueOperatorBinary)
    class Function GetVerilogOperator : String; override;
    class Function GetVHDLOperator    : String; override;
  End;

  { TValueOperatorLess }

  TValueOperatorLess = class(TValueOperatorBinary)
    class Function GetVerilogOperator : String; override;
    class Function GetVHDLOperator    : String; override;
  End;

  { TValueOperatorGreater }

  TValueOperatorGreater = class(TValueOperatorBinary)
    class Function GetVerilogOperator : String; override;
    class Function GetVHDLOperator    : String; override;
  End;

  { TValueOperatorLessOrEqual }

  TValueOperatorLessOrEqual = class(TValueOperatorBinary)
    class Function GetVerilogOperator : String; override;
    class Function GetVHDLOperator    : String; override;
  End;

  { TValueOperatorGreaterOrEqual }

  TValueOperatorGreaterOrEqual = class(TValueOperatorBinary)
    class Function GetVerilogOperator : String; override;
    class Function GetVHDLOperator    : String; override;
  End;

  TType = class;

  { TValueConcat }

  TValueConcat = class(TValue)
  type
    TValueList = specialize TFPGList<TValue>;     // [0] is MSB, [n-1] is LSB
  var
    FValues : TValueList;
    Constructor Create;
    Destructor  Destroy; override;
    Procedure Add(AValue:TValue);
    Procedure Copy(AValue:TValueConcat);
    Function  GetType : TType;
    Function  GetVerilogValue : String; override;
    Function  GetVHDLValue : String; override;
    Function  GetILangValue : String; override;
  End;

  { TValueIndex }

  TValueIndex = class(TValue)
    FValue : TValue;   // especially TSignal
    FIndex : TValue;   // especially TValueInteger, TValueRange
    Constructor Create(AValue,AIndex:TValue);
    Function  GetType : TType;
    Function  GetWidthInt : Integer;
    Function  GetLeft     : Integer;
    Function  GetRight    : Integer;
    Function  GetVerilogValue : String; override;
    Function  GetVHDLValue : String; override;
    Function  GetILangValue : String; override;
  End;

  { TValueWhen }

  TValueWhen = class(TValue)
  private type
    TValueCondition = record
      FValue     : TValue;
      FCondition : TValue;
    End;
    TValueConditions = Array of TValueCondition; // specialize TFPGList<TValueCondition>; doesn't work because oprator '=' doesn't exist
  public var
    FValues : TValueConditions;
    FElse   : TValue;
    Procedure AddValue(AValue:TValue;ACondition:TValue);
    Procedure AddElse(AValue:TValue);
    Function GetVerilogValue : String; override;
    Function GetVHDLValue    : String; override;
  End;

  TSignal = class;

  { TExtName }

  TExtName = class(TValue)
    FValue : TSignal;            // use e.g. TSignal and set its name to e.g. ".tb.DUT.Clk_i"
    Constructor Create(AValue:TSignal);
    Function GetVerilogValue : String; override;
    Function GetVHDLValue    : String; override;
  End;

  IHdlWriter = interface
    Function WriteDeclaration : String;
    Function WriteInstance    : String;
  End;
  IVerilogWriter = interface(IHdlWriter)
  End;
  IVhdlWriter = interface(IHdlWriter)
  End;

  TRangeDirection = (dirUp,dirDown);

Const
  CRangeDirectionVHDL : Array[TRangeDirection] of String = ('to','downto');

Type

  { TRange }

  TRange = class(TValue)
  private
    Function GetWidth:TValue;
  public
    FDirection : TRangeDirection;
    FLeft      : TValue;
    FRight     : TValue;
    property Width : TValue read GetWidth;
    Function GetWidth(Out AWidth : Integer) : Boolean;
    Function GetWidthInt : Integer;
    Function GetLeft     : Integer;
    Function GetRight    : Integer;
    Constructor Create(ADir:TRangeDirection;ALeft,ARight:TValue);
    Constructor Create(ADir:TRangeDirection;ALeft,ARight:Integer);
    Constructor Create(ARange:TRange); // copy constructor, only works when FLeft and FRight are TValueInteger
    Function GetVerilogValue : String; override;
    Function GetVHDLValue : String; override;
    Function GetILangValue : String; override;
  End;

  { TRangeAttrib }

  TRangeAttrib = class(TRange)
    FSignal : TSignal;
    Constructor Create(ASignal:TSignal);
    Function GetVerilogValue : String; override;
    Function GetVHDLValue : String; override;
    Function GetILangValue : String; override;
  End;

  { TTypeDecl }

  TTypeDecl = class
    FName    : String;
    FType    : TType;
    FComment : String;
    Constructor Create(AName:String);
    Function GetVerilog : String; virtual; abstract;
    Function GetVHDL    : String; virtual; abstract;
  End;

  { TTypeDeclArray }

  TTypeDeclArray = class(TTypeDecl)
    FRange    : TRange;
    FItemType : TType;
    Constructor Create(AName:String;ARange:TRange;AItemType:TType);
    Constructor Create(AName:String;ADir:TRangeDirection;ALeft,ARight:TValue;AItemType:TType);
    Constructor Create(AName:String;ADir:TRangeDirection;ALeft,ARight:Integer;AItemType:TType);
    Function GetVerilog : String; override;
    Function GetVHDL    : String; override;
  End;

  { TType }

  TType = class
    FDecl  : TTypeDecl;   // may also be Nil
    FName  : String;
    FRange : TRange;  // may also be Nil
    Constructor Create(AName:String);
    Constructor Create(AName:String;ARange:TRange);
    Constructor Create(AName:String;ADir:TRangeDirection;ALeft,ARight:TValue);
    Constructor Create(AName:String;ADir:TRangeDirection;ALeft,ARight:Integer);
    Constructor Create(AType:TType);  // copy constructor
    Function GetWidth(Out AWidth : Integer) : Boolean;
    Function GetWidthInt : Integer;
    Function GetLeft     : Integer;
    Function GetRight    : Integer;
    Function GetVerilog : String;
    Function GetVerilogRange : String;
    Function GetVHDL : String;
    Function GetILang : String;
  End;

  { TAttribute }

  TAttribute = class
    FName : String;
    FType : TType;
    Constructor Create(AName:String;AType:TType);
    Constructor Create(AName:String;AType:String);
    // being lazy, currently not yet creating range types
    Function GetVHDLDeclaration : String;
  End;
  TAttributeList = specialize TFPGMap<String,TAttribute>;

  { TAttributeValue }

  TAttributeValue = class
    FAttribute : TAttribute;
    FValue     : TValue;
    FComment   : String;
    Constructor Create(AAttribute:TAttribute;AValue:TValue);
    Function GetVerilog:String;
    Function GetVHDLSpecification(ItemName,ItemType:String):String;
    Function GetILang:String;
  End;

  { TAttributeValues }

  TAttributeValues = class
  private
    Type TAttrValList = specialize TFPGMap<String,TAttributeValue>;  // Attribute Name --> Attrib+Value, this is a workaround because FPC doesn't yet support class types as map keys, e.g. <TAttribute,TValue> :-(
  public
    Var FAttrValList : TAttrValList;
    Constructor Create;
    Procedure Clone(AAttributeValues:TAttributeValues);
    Procedure Add(AAttribute:TAttribute;AValue:TValue);
    Function GetVerilog : String;
    Function GetVHDLSpecifications(ItemName,ItemType:String):String;
    Function GetILang : String;
    Function Count : Integer;
    Function CountRegEx(RegExpr:String):Integer;
  End;

  { TSignal }

  TSignal = class(TValue,IVerilogWriter)
    FName : String;
    FType : TType;
    FAttributes : TAttributeValues;
    FComment    : String;
    FDefault    : TValue;
    Constructor Create(AName:String;AType:TType);
    Constructor Create(AName:String;AType:String);
    Constructor Create(AName:String;AType:String;ARange:TRange);
    Constructor Create(AName:String;AType:String;ARangeDir:TRangeDirection;ALeft,ARight:TValue);
    Constructor Create(AName:String;AType:String;ARangeDir:TRangeDirection;ALeft,ARight:Integer);
    Function WriteVerilogDeclaration : String; virtual;
    Function WriteVerilogInstance    : String; virtual;
    Function IVerilogWriter.WriteDeclaration = WriteVerilogDeclaration;
    Function IVerilogWriter.WriteInstance    = WriteVerilogInstance;
    Function GetVerilogValue : String; override;
    Function GetVHDLValue : String; override;
    Function GetVHDLDeclaration : String; virtual;
    Function GetILangDeclaration : String; virtual;
    Function GetILangValue : String; override;
  End;
  TSignalList     = specialize TFPGMap<String,TSignal>;    // Name -> TSignal
  TSignalSortList = specialize TFPGMap<Integer,TSignal>;   // SortID --> TSignal

  { TVariable }

  TVariable = class(TSignal)
    Function GetVHDLDeclaration : String; override;
  End;

  { TConstant }

  TConstant = class(TSignal)
  // use FDefault for value
    Constructor Create(AName:String;AType:TType;AValue:TValue);
    Constructor Create(AName:String;AType:String;AValue:TValue);
    Constructor Create(AName:String;AType:String;ARange:TRange;AValue:TValue);
    Constructor Create(AName:String;AType:String;ARangeDir:TRangeDirection;ALeft,ARight:TValue;AValue:TValue);
    Function WriteVerilogDeclaration : String; override;
    Function GetVHDLDeclaration : String; override;
  End;

  { TAlias }

  TAlias = class(TSignal)
    // use FDefault for aliased signal
    Constructor Create(AName:String;AType:TType;AValue:TValue);
    Constructor Create(AName:String;AType:String;AValue:TValue);
    Constructor Create(AName:String;AType:String;ARange:TRange;AValue:TValue);
    Constructor Create(AName:String;AType:String;ARangeDir:TRangeDirection;ALeft,ARight:TValue;AValue:TValue);
    Function WriteVerilogDeclaration : String; override;
    Function GetVHDLDeclaration : String; override;
  End;

  TPortDirection = (dirUnknown,dirIn,dirOut,dirInOut);

Const
  CPortDirectionVerilog : Array[TPortDirection] of String = ('unknown','input','output','inout');
  CPortDirectionVHDL    : Array[TPortDirection] of String = ('unknown','in','out','inout');

Function StrToPortDir(St:String):TPortDirection;

Type
  { TPort }

  TPort = class(TSignal)
    FDir  : TPortDirection;
    Constructor Create(AName:String;APortDir:TPortDirection;AType:TType);
    Constructor Create(AName:String;APortDir:TPortDirection;AType:String);
    Constructor Create(AName:String;APortDir:TPortDirection;AType:String;ARange:TRange);
    Constructor Create(AName:String;APortDir:TPortDirection;AType:String;ARangeDir:TRangeDirection;ALeft,ARight:TValue);
    Function WriteVerilogDeclaration : String; override;
    Function WriteVerilogInstance    : String; override;
    Function GetVerilogValue         : String; override;
    Function GetVHDLValue            : String; override;
    Function GetVHDLDeclaration      : String; override;
    Function GetILangDeclaration     : String; override;  // don't use this!
    Function GetILangDeclaration(APortID:Integer) : String;
  End;
  TPortList     = specialize TFPGMap<String,TPort>;    // Name -> TPort
  TPortSortList = specialize TFPGMap<Integer,TPort>;   // SortID --> TPort

  { TGeneric }

  TGeneric = class(TValue,IVerilogWriter)
    FName  : String;
    FType  : TType;
    FValue : TValue;   // default value
    FAttributes : TAttributeValues;
    FComment    : String;
    Constructor Create(AName:String;AType:TType;AValue:TValue);
    Constructor Create(AName:String;AType:String;AValue:TValue);
    // no helper constructors to initialize a range, because we hopefully don't need that
    // helper constructors for some types
    // TODO
    Function GetVerilogValue : String; override;
    Function WriteVerilogDeclaration : String; virtual;
    Function WriteVerilogInstance    : String; virtual;
    Function IVerilogWriter.WriteDeclaration = WriteVerilogDeclaration;
    Function IVerilogWriter.WriteInstance    = WriteVerilogInstance;
    Function GetVHDLValue : String; override;
    Function GetVHDLDeclaration : String;
  End;
  TGenericList     = specialize TFPGMap<String,TGeneric>;    // Name -> TGeneric
  TGenericSortList = specialize TFPGMap<Integer,TGeneric>;   // SortID --> TGeneric

  TModule = class;
  TInstance = class;
  TConnections   = specialize TFPGMap<String,TValue>;  // use String instead of TPort, since there is no "<" operator for TPort
  TGenericValues = specialize TFPGMap<String,TValue>;

  { TConnection }

  TConnection = class(TValue)  // used to return an instance port when searching for a signal driver/sink
    FInstance : TInstance;
    FPort     : TPort;
    FIndex    : TValue;
    Constructor Create(AInstance:TInstance;APort:TPort;AIndex:TValue=Nil);
  End;

  { TInstance }

  TInstance = class(IVerilogWriter)
    FName          : String;
    FModule        : TModule;
    FConnections   : TConnections;
    FGenericValues : TGenericValues;
    FAttributes    : TAttributeValues;   // TODO: include in VHDL and Verilog code generation
    FComment       : String;

    Constructor Create(AName:String;AModule:TModule);
    Procedure SetGeneric(AGeneric:String;AValue:TValue);
    Procedure ConnectPort(APort:String;ASignal:TValue);  // no APort:TPort version, because we have to ensure that we really reference the module's ports
    Function CheckConnections : Integer;   // returns the number of unconnected module ports
    Function CheckGenerics    : Integer;   // returns the number of unset module generics
    Function WriteVerilogDeclaration : String; virtual;
    Function WriteVerilogInstance    : String; virtual;
    Function IVerilogWriter.WriteDeclaration = WriteVerilogDeclaration;
    Function IVerilogWriter.WriteInstance    = WriteVerilogInstance;
    Function GetVHDL : String;
    Function GetILang : String;
  End;
  TInstanceList     = specialize TFPGMap<String,TInstance>;    // Name -> TInstance

  { TStatement }

  TStatement = class
    FComment : String;
    Function GetVerilog : String; virtual;
    Function GetVHDL    : String; virtual;
    Function GetILang   : String; virtual;
  End;
  TStatementList = specialize TFPGList<TStatement>;

  { TEmptyLine }

  TEmptyLine = class(TStatement)
    Function GetVerilog : String; override;
    Function GetVHDL    : String; override;
    Function GetILang   : String; override;
  End;

  { TAssignment }

  TAssignment = class(TStatement)
    FDest  : TValue;
    FValue : TValue;
    FDelay : TValue;
    Constructor Create(ADest:TValue;AValue:TValue);
    Procedure Swap; // swap destination and value
    Function GetVerilog : String; override;
    Function GetVHDL    : String; override;
    Function GetILang   : String; override;
  End;
  TAssignmentList = specialize TFPGList<TAssignment>;

  { TVarAssignment }

  TVarAssignment = class(TAssignment)
    Function GetVHDL : String; override;
  End;

  { TCondition }

  TCondition = class(TStatement)
    FCondition : TValue;
    FThen      : TStatementList;
    FElse      : TStatementList;
    Constructor Create(ACondition:TValue);
    Function AddStatementThen(AStatement:TStatement):TStatement;
    Function AddStatementElse(AStatement:TStatement):TStatement;
    Function GetVHDL    : String; override;
  End;

  { TForLoop }

  TForLoop = class(TStatement)
    FLoopVar : TVariable;
    FRange   : TRange;
    FBody    : TStatementList;
    Constructor Create(ALoopVar:String;ARange:TRange);
    Function AddStatement(AStatement:TStatement):TStatement;
    Function GetVHDL    : String; override;
  End;

  TSeverity = (svNote, svWarning, svError, svFailure);

  { TAssertion }

  TAssertion = class(TStatement)
    FCondition : TValue;
    FMessage   : String;
    FSeverity  : TSeverity;
    Constructor Create(ACondition:TValue;AMessage:String;ASeverity:TSeverity);
    Function GetVHDL    : String; override;
  End;

  TWaitType = (wtInfinite, wtFor, wtUntil, wtOn{not implemented!});

  { TWait }

  TWait = class(TStatement)
    FType     : TWaitType;
    FDuration : TValue;
    FUntil    : TValue;
    Constructor Create;
    Constructor Create(AType:TWaitType;AValue:TValue);
    Function GetVHDL    : String; override;
  End;

  { TProcess }

  TProcess = class
    FComment         : String;
    FName            : String;
    FSensitivityList : TSignalSortList;
    FVariables       : TSignalSortList;
    FStatements      : TStatementList;
    Constructor Create(AName:String);

    Function AddSensitive(ASignal:TSignal;ASortID:Integer) : TSignal;
    Function AddVariabe(AVariable:TSignal;ASortID:Integer) : TSignal;
    Function AddStatement(AStatement:TStatement) : TStatement;

    Function GetVHDL : String;
  End;
  TProcessList     = specialize TFPGMap<String,TProcess>;    // Name -> TProcess

  TSortList = specialize TFPGMap<Integer,TObject>;   // SortID --> TObject

  TModuleComparison = (mcName,mcMoreAttrs,mcLessAttrs,mcMoreGenerics,mcLessGenerics,mcMorePorts,mcLessPorts,mcMorePortAttrs,mcLessPortAttrs);
  TModuleComparisonSet = set of TModuleComparison;

  TValueList = specialize TFPGList<TValue>;

  TSignalPort    = (spSignal,spPort);
  TSignalPortSet = set of TSignalPort;

  { TModule }

  TModule = class(IVerilogWriter)
    FName              : String;
    FArchitectureName  : String;
    // Entity
    FGenerics          : TGenericList;
    FSortedGenerics    : TGenericSortList;
    FPorts             : TPortList;
    FSortedPorts       : TPortSortList;
    // Architecture Declaration
    FArchDecl          : TSortList;
    FSignals           : TSignalList;
      // TODO: components
      // TODO: constants
      // TODO: aliases
      // TODO: types
      // TODO: comments
    // Architecture Body
    FArchBody          : TSortList;
    FInstances         : TInstanceList;
    FAssignments       : TAssignmentList;
    FProcesses         : TProcessList;
      // TODO: comments
    // Attributes
    FEntityAttrDecls        : TAttributeList;
    FArchitectureAttrDecls  : TAttributeList;
    FEntityAttributes       : TAttributeValues;
    FArchitectureAttributes : TAttributeValues;
    Constructor Create(AName:String);
    Function Clone : TModule;
    Function Compare(ARevised:TModule;AComparison:TModuleComparisonSet) : TDynStringArray;
    Function AddGeneric(AGeneric:TGeneric;ASortID:Integer=-1) : TGeneric;
    Function AddPort(APort:TPort;ASortID:Integer=-1) : TPort;
    Function AddType(AType:TTypeDecl;ASortID:Integer=-1) : TTypeDecl;
    Function AddSignal(ASignal:TSignal;ASortID:Integer=-1) : TSignal;
    Function AddInstance(AInstance:TInstance;ASortID:Integer=-1) : TInstance;
    Function AddAssignment(ADest:TValue;AValue:TValue;ASortID:Integer=-1):TAssignment;
    Function AddProcess(AProcess:TProcess;ASortID:Integer=-1) : TProcess;
    Function FindAssignment(ADest:String):TAssignment;
    Function FindInstance(AModule:String):TInstance;
    Function FindModule(AModule:String):TModule;
    Function WriteVerilogDeclaration : String; virtual;
    Function WriteVerilogInstance    : String; virtual;
    Function IVerilogWriter.WriteDeclaration = WriteVerilogDeclaration;
    Function IVerilogWriter.WriteInstance    = WriteVerilogInstance;
    Function GetVHDLEntityInner(WithAttributes:Boolean):String;
    Function GetVHDLEntity : String;
    Function GetVHDLComponent : String;
    Function GetVHDLArchitecture : String;
    Function GetVHDLHeader:String;
    Function GetVHDL : String;
    Function GetILang : String;
    Function GetSignal(AName:String;ASignalPort:TSignalPortSet = [spSignal,spPort]) : TSignal;
    Function FindSignal(RegEx:String;ASignalPort:TSignalPortSet = [spSignal,spPort]):TSignal;
    Function FindSignals(RegEx:String;Var List:TSignalList;ASignalPort:TSignalPortSet = [spSignal,spPort]):Integer;
    Function GetDriver(AValue:TValue;AllowNonPort:Boolean=false;TTL:Integer=10):TValue;
    Function GetDriver(ASignal:String;AllowNonPort:Boolean=false;TTL:Integer=10):TValue;
    Function GetSinks(ASignal:String;All:Boolean=false;List:TValueList=Nil;TTL:Integer=10):TValueList;
  End;
  TModuleList = specialize TFPGMap<String,TModule>;

Function GetVerilogIdentifier(St:String):String;

Var
  TypeInt    : TType;
  TypeReal   : TType;
  TypeTime   : TType;
  TypeBit    : TType;
  TypeString : TType;

Implementation

Function StrToPortDir(St:String):TPortDirection;
Begin
  For Result := Low(TPortDirection) to High(TPortDirection) do
    if St = CPortDirectionVHDL[Result] then Exit;
  // not found
  raise Exception.Create('Invalid direction '''+St+'''');
End;

Function GetVerilogIdentifier(St:String):String;
Var I : Integer;
Begin
  For I := 1 to Length(St) do
    if not (St[I] in ['0'..'9','A'..'Z','a'..'z','_']) then
      Exit('\' + St + ' ');
  Result := St;
End;

Function GetILangIdentifier(St:String):String;
Begin
  if St[1] = '$' then Exit(St);
  Result := '\' + St;
End;

{ TValue }

Function TValue.GetVerilogValue:String;
Begin
  raise Exception.Create(ClassName+'.GetVerilogValue is not implemented');
  Result := '';
End;

Function TValue.GetVHDLValue:String;
Begin
  raise Exception.Create(ClassName+'.GetVHDLValue (Value = '''+GetVerilogValue+''') is not implemented');
  Result := '';
End;

Function TValue.GetILangValue:String;
Begin
  raise Exception.Create(ClassName+'.GetILangValue (Value = '''+GetVerilogValue+''') is not implemented');
  Result := '';
End;

{ TValueInteger }

Constructor TValueInteger.Create(AValue:Integer);
Begin
  inherited Create;
  FValue := AValue;
End;

Function TValueInteger.GetVerilogValue : String;
Begin
  Result := IntToStr(FValue);
End;

Function TValueInteger.GetVHDLValue:String;
Begin
  Result := IntToStr(FValue);
End;

Function TValueInteger.GetILangValue:String;
Begin
  Result := IntToStr(FValue);
End;

{ TValueIntegerHex }

Function TValueIntegerHex.GetVerilogValue:String;
Begin
  Result := '''h' + IntToHex(FValue,4);
End;

Function TValueIntegerHex.GetVHDLValue:String;
Begin
  Result := '16#' + IntToHex(FValue,4) + '#';
End;

Function TValueIntegerHex.GetILangValue:String;
Begin
  Result := IntToStr(FValue);   // no hex numbers in ILang --> decimal
End;

{ TValueFloat }

Constructor TValueFloat.Create(AValue:Double);
Begin
  inherited Create;
  FValue := AValue;
End;

Function TValueFloat.GetVerilogValue:String;
Begin
  Result := FloatToStr(FValue);
End;

Function TValueFloat.GetVHDLValue:String;
Begin
  Result := GetVerilogValue;
End;

{ TValueTime }

Constructor TValueTime.Create(AValue:Double;AUnit:TTimeUnit);
Begin
  inherited Create(AValue);
  FUnit := AUnit;
End;

Const CTimeUnitMult : Array[TTimeUnit] of Double = (1E-15,1E-12,1E-9,1E-6,1E-3,1.0,60.0,3600.0);
      CTimeUnitVHDL : Array[TTimeUnit] of String = ('fs','ps','ns','us','ms','sec','min','hr');

Function TValueTime.GetVerilogValue:String;
Begin
  Result := FloatToStr(FValue * CTimeUnitMult[FUnit]);
End;

Function TValueTime.GetVHDLValue:String;
Begin
  Result := FloatToStr(FValue) + ' ' + CTimeUnitVHDL[FUnit];
End;

{ TValueString }

Constructor TValueString.Create(AValue:String);
Begin
  inherited Create;
  FValue := AValue;
End;

Function TValueString.GetVerilogValue:String;
Begin
  Result := '"' + FValue + '"';
End;

Function TValueString.GetVHDLValue:String;
Begin
  Result := '"' + FValue + '"';
End;

Function TValueString.GetILangValue:String;
Var I : Integer;
Begin
  Result := '';
  For I := 1 to Length(FValue) do
    Begin
      Case FValue[I] of
        ^J : Result := Result + '\n';
        ^I : Result := Result + '\t';
        #0..^H,
        ^K..#31 : Result := Result + '\' + IntToOct(Ord(FValue[I]),3);
        '"' : Result := Result + '\"';
        '\' : Result := Result + '\\';
      else
        Result := Result + FValue[I];
      End;
    End;

  Result := '"' + Result + '"';
End;

{ TValueBit }

Constructor TValueBit.Create(AValue:Char);
Begin
  inherited Create;
  FValue := AValue;
End;

Function TValueBit.GetVerilogValue:String;
Begin
  Result := '1''b' + FValue;
End;

Function TValueBit.GetVHDLValue:String;
Begin
  Result := '''' + FValue + '''';
End;

Function TValueBit.GetILangValue:String;
Begin
  Result := '1''' + FValue;
End;

{ TValueVector }

Constructor TValueVector.Create(AWidth:Integer;AValue:String);
Begin
  if Length(AValue) <> AWidth then
    raise Exception.CreateFmt('The value ''%s'' has %d bits, but the vector is defined with %d bits.',[AValue,Length(AValue),AWidth]);
  inherited Create;
  FWidth := AWidth;
  FValue := AValue;
End;

Function IntToBin(AValue:Integer;AWidth:Integer) : String;
Var I : Integer;
Begin
  SetLength(Result,AWidth);
  FillChar(Result[1],AWidth,'0');
  For I := AWidth downto 1 do
    Begin
      if AValue and $0001 <> 0 then
        Result[I] := '1';
      AValue := AValue shr 1;
    End;
End;

Constructor TValueVector.Create(AWidth:Integer;AValue:Integer);
Begin
  if AValue >= (1 shl AWidth) then
    raise Exception.CreateFmt('The value %x is too large for a vector with %d bits',[AValue,AWidth]);
  Create(AWidth,IntToBin(AValue,AWidth));
End;

Function TValueVector.GetVerilogValue:String;
Begin
  Result := IntToStr(FWidth) + '''b' + FValue;
End;

Function TValueVector.GetVHDLValue:String;
Begin
  Result := '"' + FValue + '"';
End;

Function TValueVector.GetILangValue:String;
Begin
  Result := IntToStr(FWidth) + '''' + FValue;
End;

{ TValueOperatorUnary }

Constructor TValueOperatorUnary.Create(AValue:TValue);
Begin
  inherited Create;
  FValue := AValue;
End;

Function TValueOperatorUnary.GetVerilogValue:String;
Begin
  Result := GetVerilogOperator;
  if FValue is TValueOperator then
    Result := Result + '(' + FValue.GetVerilogValue + ')'
  else
    Result := Result + FValue.GetVerilogValue
End;

Function TValueOperatorUnary.GetVHDLValue:String;
Begin
  Result := GetVHDLOperator;
  if FValue is TValueOperator then
    Result := Result + '(' + FValue.GetVHDLValue + ')'
  else
    Result := Result + FValue.GetVHDLValue;
End;

Class Function TValueOperatorUnary.GetVerilogOperator:String;
Begin
  raise Exception.Create('Not implemented');
  Result := '';
End;

Class Function TValueOperatorUnary.GetVHDLOperator:String;
Begin
  raise Exception.Create('Not implemented');
  Result := '';
End;

{ TValueOperatorNot }

Class Function TValueOperatorNot.GetVerilogOperator:String;
Begin
  Result := '~';
End;

Class Function TValueOperatorNot.GetVHDLOperator:String;
Begin
  Result := 'not ';
End;

{ TValueOperatorFunction }

Constructor TValueOperatorFunction.Create(AName:String;AValue:TValue);
Begin
  inherited Create(AValue);
  FName := AName;
End;

Function TValueOperatorFunction.GetVerilogValue:String;
Begin
  Result := FName + '(' + FValue.GetVerilogValue + ')';
End;

Function TValueOperatorFunction.GetVHDLValue:String;
Begin
  Result := FName + '(' + FValue.GetVHDLValue + ')';
End;

{ TValueOperatorBinary }

Constructor TValueOperatorBinary.Create(AValueA,AValueB:TValue);
Begin
  inherited Create;
  FValueA := AValueA;
  FValueB := AValueB;
End;

Function TValueOperatorBinary.GetVerilogValue:String;
Begin
  Result := FValueA.GetVerilogValue;
  if FValueA.InheritsFrom(TValueOperator) then Result := '(' + Result + ')';
  Result := Result + GetVerilogOperator;
  if FValueB.InheritsFrom(TValueOperator) then Result := Result + '(' + FValueB.GetVerilogValue + ')'
  else Result := Result + FValueB.GetVerilogValue;
End;

Function TValueOperatorBinary.GetVHDLValue:String;
Begin
  Result := FValueA.GetVHDLValue;
  if FValueA.InheritsFrom(TValueOperator) then Result := '(' + Result + ')';
  Result := Result + GetVHDLOperator;
  if FValueB.InheritsFrom(TValueOperator) then Result := Result + '(' + FValueB.GetVHDLValue + ')'
  else Result := Result + FValueB.GetVHDLValue;
End;

{ TValueOperatorPlus }

Class Function TValueOperatorPlus.GetVerilogOperator:String;
Begin
  Result := '+';
End;

Class Function TValueOperatorPlus.GetVHDLOperator:String;
Begin
  Result := GetVerilogOperator;
End;

{ TValueOperatorMinus }

Class Function TValueOperatorMinus.GetVerilogOperator:String;
Begin
  Result := '-';
End;

Class Function TValueOperatorMinus.GetVHDLOperator:String;
Begin
  Result := GetVerilogOperator;
End;

{ TValueOperatorTimes }

Class Function TValueOperatorTimes.GetVerilogOperator:String;
Begin
  Result := '*';
End;

Class Function TValueOperatorTimes.GetVHDLOperator:String;
Begin
  Result := GetVerilogOperator;
End;

{ TValueOperatorDivide }

Class Function TValueOperatorDivide.GetVerilogOperator:String;
Begin
  Result := '/';
End;

Class Function TValueOperatorDivide.GetVHDLOperator:String;
Begin
  Result := GetVerilogOperator;
End;

{ TValueOperatorAnd }

Class Function TValueOperatorAnd.GetVerilogOperator:String;
Begin
  Result := ' & ';
End;

Class Function TValueOperatorAnd.GetVHDLOperator:String;
Begin
  Result := ' and ';
End;

{ TValueOperatorOr }

Class Function TValueOperatorOr.GetVerilogOperator:String;
Begin
  Result := ' | ';
End;

Class Function TValueOperatorOr.GetVHDLOperator:String;
Begin
  Result := ' or ';
End;

{ TValueOperatorXor }

Class Function TValueOperatorXor.GetVerilogOperator:String;
Begin
  Result := ' ^ ';
End;

Class Function TValueOperatorXor.GetVHDLOperator:String;
Begin
  Result := ' xor ';
End;

{ TValueOperatorEqual }

Class Function TValueOperatorEqual.GetVerilogOperator:String;
Begin
  Result := ' = ';
End;

Class Function TValueOperatorEqual.GetVHDLOperator:String;
Begin
  Result := GetVerilogOperator;
End;

{ TValueOperatorUnequal }

Class Function TValueOperatorUnequal.GetVerilogOperator:String;
Begin
  Result := ' != ';
End;

Class Function TValueOperatorUnequal.GetVHDLOperator:String;
Begin
  Result := ' /= ';
End;

{ TValueOperatorLess }

Class Function TValueOperatorLess.GetVerilogOperator:String;
Begin
  Result := ' < ';
End;

Class Function TValueOperatorLess.GetVHDLOperator:String;
Begin
  Result := GetVerilogOperator;
End;

{ TValueOperatorGreater }

Class Function TValueOperatorGreater.GetVerilogOperator:String;
Begin
  Result := ' > ';
End;

Class Function TValueOperatorGreater.GetVHDLOperator:String;
Begin
  Result := GetVerilogOperator;
End;

{ TValueOperatorLessOrEqual }

Class Function TValueOperatorLessOrEqual.GetVerilogOperator:String;
Begin
  Result := ' <= ';
End;

Class Function TValueOperatorLessOrEqual.GetVHDLOperator:String;
Begin
  Result := GetVerilogOperator;
End;

{ TValueOperatorGreaterOrEqual }

Class Function TValueOperatorGreaterOrEqual.GetVerilogOperator:String;
Begin
  Result := ' >= ';
End;

Class Function TValueOperatorGreaterOrEqual.GetVHDLOperator:String;
Begin
  Result := GetVerilogOperator;
End;

{ TValueConcat }

Constructor TValueConcat.Create;
Begin
  inherited Create;
  FValues := TValueList.Create;
End;

Destructor TValueConcat.Destroy;
Begin
  Inherited Destroy;
End;

(**
 * Add as MSB
 *)
Procedure TValueConcat.Add(AValue:TValue);
Begin
  FValues.Insert(0,AValue);
End;

Procedure TValueConcat.Copy(AValue:TValueConcat);
Var I : Integer;
Begin
  if AValue.FValues.Count = 0 then Exit;
  For I := AValue.FValues.Count-1 downto 0 do
    FValues.Insert(0,AValue.FValues[I]);
End;

Function TValueConcat.GetType:TType;
Var I : Integer;
    V : TValue;
Begin
  if FValues.Count = 0 then
    raise Exception.Create('Can''t GetType when TValueConcat is empty');
  if      FValues[0] is TValueBit    then Result := TType.Create('std_logic_vector',dirDown,0,0)
  else if FValues[0] is TValueVector then Result := TType.Create('std_logic_vector',dirDown,(FValues[0] as TValueVector).FWidth,0)
  else if FValues[0] is TValueIndex  then Result := TType.Create('std_logic_vector',dirDown,(FValues[0] as TValueIndex).GetWidthInt-1,0)
  else if FValues[0] is TSignal      then Result := TType.Create((FValues[0] as TSignal).FType) // need a copy of that because we modify it, see below
  else if FValues[0] is TPort        then Result := TType.Create((FValues[0] as TSignal).FType) // need a copy of that because we modify it, see below
  else if FValues[0] is TValueString then Result := TypeString
  else
    raise Exception.Create('Can''t TValueConcat.GetType of '+FValues[0].ClassName);
  // TSignal or TPort might be a TypeBit, so replace by std_logic_vector
  if Result.FName = 'std_logic' then
    Begin
      Result.Free;
      Result := TType.Create('std_logic_vector',dirDown,0,0);
    End;
  // TypeString doesn't consider the length, and we are lazy don't check if
  // all concatenated entries are of TypeString too
  if Result = TypeString then Exit;
  For I := 1 to FValues.Count-1 do
    Begin
      V := FValues.Items[I];
      if      V is TValueBit    then
        Inc((Result.FRange.FLeft as TValueInteger).FValue)
      else if V is TValueVector then
        Inc((Result.FRange.FLeft as TValueInteger).FValue,(V as TValueVector).FWidth)
      else if (V is TSignal) or (V is TPort) then
        Begin
          // in here it is quite dirty with way too less type checking
          if      (V as TSignal).FType = TypeBit    then
            Inc((Result.FRange.FLeft as TValueInteger).FValue)
          else if (V as TSignal).FType.FName = 'std_logic_vector' then
            Inc((Result.FRange.FLeft as TValueInteger).FValue,((V as TSignal).FType.FRange.Width as TValueInteger).FValue)
          else
            raise Exception.Create('Can''t concatenate 2 '+(V as TSignal).FType.GetVHDL+' to std_logic_vector');
        End
      else if V is TValueIndex then
        Begin
          Inc((Result.FRange.FLeft as TValueInteger).FValue,(V as TValueIndex).GetWidthInt);
        End
      else
        raise Exception.Create('Can''t concatenate 3 '+V.ClassName+' to std_logic_vector');
    End;
End;

Function TValueConcat.GetVerilogValue:String;
Var I : Integer;
Begin
  if FValues.Count = 0 then Exit('{}');
  Result := '{ ' + FValues[0].GetVerilogValue;
  For I := 1 to FValues.Count-1 do
    Result += ', ' + FValues[I].GetVerilogValue;
  Result += ' }';
End;

Function TValueConcat.GetVHDLValue:String;
Var I : Integer;
Begin
  if FValues.Count = 0 then Exit('');
  Result := FValues[0].GetVHDLValue;
  For I := 1 to FValues.Count-1 do
    Result += ' & ' + FValues[I].GetVHDLValue;
End;

Function TValueConcat.GetILangValue:String;
Var I : Integer;
Begin
  if FValues.Count = 0 then Exit('{}');
  Result := '{';
  For I := 0 to FValues.Count-1 do
    Result += ' ' + FValues[I].GetILangValue;
  Result += ' }';
End;

{ TValueIndex }

Constructor TValueIndex.Create(AValue,AIndex:TValue);
Begin
  inherited Create;
  FValue := AValue;
  FIndex := AIndex;
End;

Function TValueIndex.GetType:TType;
Begin
  if FIndex is TRange then
    Begin
      if (FValue is TValueVector) or (FValue is TSignal) or (FValue is TPort) then
        Result := TType.Create('std_logic_vector',FIndex as TRange)
      else if FValue is TValueString then
        Result := TypeString
      else
        raise Exception.Create('Can''t index '+FValue.ClassName);
    End
  else
    Begin
      // assuming it is a TValueInteger or something which resolves to a single number
      if (FValue is TValueVector) or (FValue is TSignal) or (FValue is TPort) then
        Result := TypeBit
      {else if FValue is TValueString then
        Result := TypeString}   // we don't yet have TypeChar
      else
        raise Exception.Create('Can''t index '+FValue.ClassName);
    End;
End;

Function TValueIndex.GetWidthInt:Integer;
Begin
  if FIndex is TRange then Result := (FIndex as TRange).GetWidthInt
  else Result := 1;
End;

Function TValueIndex.GetLeft:Integer;
Begin
  if FIndex is TRange then Result := (FIndex as TRange).GetLeft
  else Result := 0;
End;

Function TValueIndex.GetRight:Integer;
Begin
  if FIndex is TRange then Result := (FIndex as TRange).GetRight
  else Result := 0;
End;

Function TValueIndex.GetVerilogValue:String;
Begin
  Result := FValue.GetVerilogValue + '[' + FIndex.GetVerilogValue + ']';
End;

Function TValueIndex.GetVHDLValue:String;
Begin
  Result := FValue.GetVHDLValue + '(' + FIndex.GetVHDLValue + ')';
End;

Function TValueIndex.GetILangValue:String;
Begin
  Result := FValue.GetILangValue + ' [' + FIndex.GetILangValue + ']';
End;

{ TValueWhen }

Procedure TValueWhen.AddValue(AValue:TValue;ACondition:TValue);
Begin
  SetLength(FValues,Length(FValues)+1);
  FValues[Length(FValues)-1].FValue     := AValue;
  FValues[Length(FValues)-1].FCondition := ACondition;
End;

Procedure TValueWhen.AddElse(AValue:TValue);
Begin
  FElse := AValue;
End;

Function TValueWhen.GetVerilogValue:String;
Begin
  Result := 'TODO: implement';
  //raise Exception.Create('TODO: implement');
  // use ?: syntax
End;

Function TValueWhen.GetVHDLValue:String;
Var I : Integer;
Begin
  if Length(FValues) = 0 then
    raise Exception.Create('TValueWhen needs at least one value');
  if not assigned(FElse) then
    raise Exception.Create('TValueWhen needs an ''else'' value');
  Result := '';
  For I := 0 to Length(FValues)-1 do
    With FValues[I] do
      Result += FValue.GetVHDLValue + ' when ' + FCondition.GetVHDLValue + ' else ' + LineEnding;
  Result += FElse.GetVHDLValue;
End;

{ TExtName }

Constructor TExtName.Create(AValue:TSignal);
Begin
  inherited Create;
  FValue := AValue;
End;

Function TExtName.GetVerilogValue:String;
Begin
  Result := FValue.GetVerilogValue;   // no special handling, Verilog can do hierarchial names on its own
End;

Function TExtName.GetVHDLValue:String;
Begin
  Result := '<< ' + FValue.GetVHDLDeclaration + ' >>';
End;

{ TRange }

Constructor TRange.Create(ADir:TRangeDirection;ALeft,ARight:TValue);
Begin
  inherited Create;
  FDirection := ADir;
  FLeft      := ALeft;
  FRight     := ARight;
End;

Constructor TRange.Create(ADir:TRangeDirection;ALeft,ARight:Integer);
Begin
  Create(ADir,TValueInteger.Create(ALeft),TValueInteger.Create(ARight));
End;

Constructor TRange.Create(ARange:TRange);
Begin
  Create(ARange.FDirection,
         TValueInteger.Create((ARange.FLeft  as TValueInteger).FValue),
         TValueInteger.Create((ARange.FRight as TValueInteger).FValue));
End;

Function TRange.GetWidth:TValue;
Begin
  if (FLeft is TValueInteger) and (FRight is TValueInteger) then
    Result := TValueInteger.Create(abs((FLeft as TValueInteger).FValue - (FRight as TValueInteger).FValue) + 1)
  else
    Begin
      if FDirection = dirUp then
        Result := TValueOperatorMinus.Create(FRight,FLeft)
      else
        Result := TValueOperatorMinus.Create(FLeft,FRight);
      Result := TValueOperatorPlus.Create(Result,TValueInteger.Create(1));
    End;
End;

Function TRange.GetWidth(Out AWidth:Integer):Boolean;
Begin
  Result := true;
  if (FLeft is TValueInteger) and (FRight is TValueInteger) then
    AWidth := abs((FLeft as TValueInteger).FValue - (FRight as TValueInteger).FValue) + 1
  else
    Result := false;
End;

Function TRange.GetWidthInt:Integer;
Begin
  if (FLeft is TValueInteger) and (FRight is TValueInteger) then
    Result := abs((FLeft as TValueInteger).FValue - (FRight as TValueInteger).FValue) + 1
  else
    raise Exception.Create('Can''t get width of range where boundaries are specified with non-integer values');
End;

Function TRange.GetLeft:Integer;
Begin
  if FLeft is TValueInteger then
    Result := (FLeft as TValueInteger).FValue
  else
    raise Exception.Create('Can''t get left boundary of range as integer because it is specified as non-integer value');
End;

Function TRange.GetRight:Integer;
Begin
  if FRight is TValueInteger then
    Result := (FRight as TValueInteger).FValue
  else
    raise Exception.Create('Can''t get right boundary of range as integer because it is specified as non-integer value');
End;

Function TRange.GetVerilogValue:String;
Begin
  Result := FLeft.GetVerilogValue + ':' + FRight.GetVerilogValue;
End;

Function TRange.GetVHDLValue:String;
Begin
  Result := FLeft.GetVHDLValue;
  Result += ' ' + CRangeDirectionVHDL[FDirection] + ' ';
  Result += FRight.GetVHDLValue;
End;

Function TRange.GetILangValue:String;
Begin
  Result := FLeft.GetILangValue + ':' + FRight.GetILangValue;
End;

{ TRangeAttrib }

Constructor TRangeAttrib.Create(ASignal:TSignal);
Begin
  inherited Create(ASignal.FType.FRange);
  FSignal := ASignal;
End;

Function TRangeAttrib.GetVerilogValue:String;
Begin
  Result := '// TODO: implement';
End;

Function TRangeAttrib.GetVHDLValue:String;
Begin
  if assigned(FSignal) then
    Result := FSignal.GetVHDLValue+'''range'
  else
    Result := inherited GetVHDLValue;
End;

Function TRangeAttrib.GetILangValue:String;
Begin
  raise Exception.Create(ClassName+' not implmented for ILang');
End;

{ TTypeDecl }

Constructor TTypeDecl.Create(AName:String);
Begin
  inherited Create;
  FName := AName;
  FType := TType.Create(AName);
  FType.FDecl := Self;
End;

{ TTypeDeclArray }

Constructor TTypeDeclArray.Create(AName:String;ARange:TRange;AItemType:TType);
Begin
  inherited Create(AName);
  FRange    := ARange;
  FItemType := AItemType;
End;

Constructor TTypeDeclArray.Create(AName:String;ADir:TRangeDirection;ALeft,ARight:TValue;AItemType:TType);
Begin
  inherited Create(AName);
  FRange    := TRange.Create(ADir,ALeft,ARight);
  FItemType := AItemType;
End;

Constructor TTypeDeclArray.Create(AName:String;ADir:TRangeDirection;ALeft,ARight:Integer;AItemType:TType);
Begin
  inherited Create(AName);
  FRange    := TRange.Create(ADir,ALeft,ARight);
  FItemType := AItemType;
End;

Function TTypeDeclArray.GetVerilog:String;
Begin
  Result := 'TODO: implement';
End;

Function TTypeDeclArray.GetVHDL:String;
Begin
  Result := 'type ' + FName + ' is array('+FRange.GetVHDLValue+') of '+FItemType.GetVHDL;;
End;

{ TType }

Constructor TType.Create(AName:String);
Begin
  inherited Create;
  FName := AName;
  // FRange defaults to Nil
End;

Constructor TType.Create(AName:String;ARange:TRange);
Begin
  Create(AName);
  FRange := ARange;
End;

Constructor TType.Create(AName:String;ADir:TRangeDirection;ALeft,ARight:TValue);
Begin
  Create(AName,TRange.Create(ADir,ALeft,ARight));
End;

Constructor TType.Create(AName:String;ADir:TRangeDirection;ALeft,ARight:Integer);
Begin
  Create(AName,ADir,TValueInteger.Create(ALeft),TValueInteger.Create(ARight));
End;

Constructor TType.Create(AType:TType);
Begin
  if not assigned(AType.FRange) then
    Create(AType.FName)
  else
    Create(AType.FName,TRange.Create(AType.FRange));
End;

Function TType.GetWidth(Out AWidth:Integer):Boolean;
Begin
  Result := true;
  if FRange = Nil then AWidth := 1
  else Result := FRange.GetWidth(AWidth);
End;

Function TType.GetWidthInt:Integer;
Begin
  if FRange = Nil then Result := 1
  else Result := FRange.GetWidthInt;
End;

Function TType.GetLeft:Integer;
Begin
  if FRange = Nil then Result := 0
  else Result := FRange.GetLeft;
End;

Function TType.GetRight:Integer;
Begin
  if FRange = Nil then Result := 0
  else Result := FRange.GetRight;
End;

Function TType.GetVerilog:String;
Begin
  Result := GetVerilogRange;
End;

Function TType.GetVerilogRange:String;
Begin
  Result := '';
  if assigned(FRange) then
    Result := '[' + FRange.GetVerilogValue + ']';
End;

Function TType.GetVHDL:String;
Begin
  Result := FName;
  if assigned(FRange) then
    Result += '(' + FRange.GetVHDLValue + ')';
End;

Function TType.GetILang:String;
Begin
  Result := '';
  if assigned(FRange) then
    Result := '[' + FRange.GetVerilogValue + ']';
End;

{ TAttribute }

Constructor TAttribute.Create(AName:String;AType:TType);
Begin
  inherited Create;
  FName := AName;
  FType := AType;
End;

Constructor TAttribute.Create(AName:String;AType:String);
Begin
  Create(AName,TType.Create(AType));
End;

Function TAttribute.GetVHDLDeclaration:String;
Begin
  Result := 'attribute ' + FName + ' : ' + FType.GetVHDL + ';';
End;

{ TAttributeValue }

Constructor TAttributeValue.Create(AAttribute:TAttribute;AValue:TValue);
Begin
  inherited Create;
  FAttribute := AAttribute;
  FValue     := AValue;
End;

Function TAttributeValue.GetVerilog : String;
Begin
  Result := '';
  if FComment > '' then
    Result := '/* ' + FComment + ' */ ';  // lets see if this works, at least indentation is gone for multi-line comments :-(
  Result += FAttribute.FName + ' = ' + FValue.GetVerilogValue;
End;

Function TAttributeValue.GetVHDLSpecification(ItemName,ItemType:String):String;
Begin
  Result := '';
  if FComment > '' then
    Result := Prefix(FComment,'-- ') + LineEnding;
  Result += 'attribute ' + FAttribute.FName + ' of ' + ItemName + ' : ' + ItemType + ' is ' + FValue.GetVHDLValue + ';';
End;

Function TAttributeValue.GetILang:String;
Begin
  Result := '';
  if FComment > '' then
    Result := '# ' + FComment + LineEnding;
  Result += 'attribute ' + GetILangIdentifier(FAttribute.FName) + ' ' + FValue.GetILangValue;
End;

{ TAttributeValues }

Constructor TAttributeValues.Create;
Begin
  inherited Create;
  FAttrValList := TAttrValList.Create;
End;

Procedure TAttributeValues.Clone(AAttributeValues:TAttributeValues);
Begin
  if FAttrValList.Count <> 0 then
    raise Exception.Create('Can''t copy to non-empty TAttributeValues');
  CopyMap(AAttributeValues.FAttrValList,FAttrValList);
End;

Procedure TAttributeValues.Add(AAttribute:TAttribute;AValue:TValue);
Begin
  FAttrValList.Add(AAttribute.FName,TAttributeValue.Create(AAttribute,AValue));
End;

Function TAttributeValues.GetVerilog : String;
Var I : Integer;
Begin
  if FAttrValList.Count = 0 then Exit('');
  Result := '(* ';
  For I := 0 to FAttrValList.Count-1 do
    Begin
      Result += FAttrValList.Data[I].GetVerilog;
      if I < FAttrValList.Count-1 then
        Result += ', ';
    End;
  Result += ' *)' + LineEnding;  // trailing space so it can easily be prepended to other Verilog constructs
End;

Function TAttributeValues.GetVHDLSpecifications(ItemName,ItemType:String):String;
Var I : Integer;
Begin
  Result := '';
  For I := 0 to FAttrValList.Count-1 do
    Result += FAttrValList.Data[I].GetVHDLSpecification(ItemName,ItemType) + LineEnding;
End;

Function TAttributeValues.GetILang:String;
Var I : Integer;
Begin
  Result := '';
  For I := 0 to FAttrValList.Count-1 do
    Result += FAttrValList.Data[I].GetILang + LineEnding;
End;

Function TAttributeValues.Count:Integer;
Begin
  Result := FAttrValList.Count;
End;

Function TAttributeValues.CountRegEx(RegExpr:String):Integer;
Var I : Integer;
Begin
  Result := 0;
  For I := 0 to FAttrValList.Count-1 do
    if ExecRegExpr(RegExpr,FAttrValList.Keys[I]) then
      Inc(Result);
End;

{ TSignal }

Constructor TSignal.Create(AName:String;AType:TType);
Begin
  inherited Create;
  FName := AName;
  FType := AType;
  FAttributes := TAttributeValues.Create;
End;

Constructor TSignal.Create(AName:String;AType:String);
Begin
  Create(AName,TType.Create(AType));
End;

Constructor TSignal.Create(AName:String;AType:String;ARange:TRange);
Begin
  inherited Create;
  FName := AName;
  FType := TType.Create(AType,ARange);
  FAttributes := TAttributeValues.Create;
End;

Constructor TSignal.Create(AName:String;AType:String;ARangeDir:TRangeDirection;ALeft,ARight:TValue);
Begin
  inherited Create;
  FName := AName;
  FType := TType.Create(AType,ARangeDir,ALeft,ARight);
  FAttributes := TAttributeValues.Create;
End;

Constructor TSignal.Create(AName:String;AType:String;ARangeDir:TRangeDirection;ALeft,ARight:Integer);
Begin
  inherited Create;
  FName := AName;
  FType := TType.Create(AType,ARangeDir,ALeft,ARight);
  FAttributes := TAttributeValues.Create;
End;

Function TSignal.WriteVerilogDeclaration:String;
Begin
  Result := FAttributes.GetVerilog + 'wire ';
  if Assigned(FType.FRange) then
    Result += FType.GetVerilogRange + ' ';
  Result += GetVerilogIdentifier(FName);
  if assigned(FDefault) then
    Result += ' = ' + FDefault.GetVerilogValue;
  Result := Indent(Result,2);
End;

Function TSignal.WriteVerilogInstance:String;
Begin
  Result := GetVerilogIdentifier(FName);   // ".SignalName(WireName),"
End;

Function TSignal.GetVerilogValue:String;
Begin
  Result := GetVerilogIdentifier(FName);
End;

Function TSignal.GetVHDLValue:String;
Begin
  Result := FName;
End;

Function TSignal.GetVHDLDeclaration:String;
Begin
  Result := 'signal ' + FName + ' : ' + FType.GetVHDL;
  if assigned(FDefault) then
    Result += ' := ' + FDefault.GetVHDLValue;
End;

Function TSignal.GetILangDeclaration:String;
Begin
  Result := FAttributes.GetILang + 'wire ';
  if Assigned(FType.FRange) then
    Result += 'width ' + IntToStr(FType.FRange.GetWidthInt) + ' ';
  if FType.GetRight <> 0 then
    Result += 'offset ' + IntToStr(FType.FRange.GetRight) + ' ';
  Result += GetILangIdentifier(FName);
  Result := Indent(Result,2);
End;

Function TSignal.GetILangValue:String;
Begin
  Result := GetILangIdentifier(FName);
End;

{ TVariable }

Function TVariable.GetVHDLDeclaration:String;
Begin
  Result := 'variable ' + FName + ' : ' + FType.GetVHDL;
End;

{ TConstant }

Constructor TConstant.Create(AName:String;AType:TType;AValue:TValue);
Begin
  inherited Create(AName,AType);
  FDefault := AValue;
End;

Constructor TConstant.Create(AName:String;AType:String;AValue:TValue);
Begin
  inherited Create(AName,AType);
  FDefault := AValue;
End;

Constructor TConstant.Create(AName:String;AType:String;ARange:TRange;AValue:TValue);
Begin
  inherited Create(AName,AType,ARange);
  FDefault := AValue;
End;

Constructor TConstant.Create(AName:String;AType:String;ARangeDir:TRangeDirection;ALeft,ARight:TValue;AValue:TValue);
Begin
  inherited Create(AName,AType,ARangeDir,ALeft,ARight);
  FDefault := AValue;
End;

Function TConstant.WriteVerilogDeclaration:String;
Begin
  Result := '  localparam ' + FName + ' = ' + FDefault.GetVerilogValue;
End;

Function TConstant.GetVHDLDeclaration:String;
Begin
  Result := 'constant ' + FName + ' : ' + FType.GetVHDL + ' := ' + FDefault.GetVHDLValue;
End;

{ TAlias }

Constructor TAlias.Create(AName:String;AType:TType;AValue:TValue);
Begin
  inherited Create(AName,AType);
  FDefault := AValue;
End;

Constructor TAlias.Create(AName:String;AType:String;AValue:TValue);
Begin
  inherited Create(AName,AType);
  FDefault := AValue;
End;

Constructor TAlias.Create(AName:String;AType:String;ARange:TRange;AValue:TValue);
Begin
  inherited Create(AName,AType,ARange);
  FDefault := AValue;
End;

Constructor TAlias.Create(AName:String;AType:String;ARangeDir:TRangeDirection;ALeft,ARight:TValue;AValue:TValue);
Begin
  inherited Create(AName,AType,ARangeDir,ALeft,ARight);
  FDefault := AValue;
End;

Function TAlias.WriteVerilogDeclaration:String;
Begin
  raise Exception.Create('TODO: implement TAlias for Verilog');
End;

Function TAlias.GetVHDLDeclaration:String;
Begin
  if assigned(FType) then
    Result := 'alias ' + FName + ' : ' + FType.GetVHDL + ' is ' + FDefault.GetVHDLValue
  else
    Result := 'alias ' + FName + ' is ' + FDefault.GetVHDLValue;
End;

{ TPort }

Constructor TPort.Create(AName:String;APortDir:TPortDirection;AType:TType);
Begin
  inherited Create(AName,AType);
  FDir  := APortDir;
End;

Constructor TPort.Create(AName:String;APortDir:TPortDirection;AType:String);
Begin
  inherited Create(AName,AType);
  FDir  := APortDir;
End;

Constructor TPort.Create(AName:String;APortDir:TPortDirection;AType:String;ARange:TRange);
Begin
  inherited Create(AName,AType,ARange);
  FDir  := APortDir;
End;

Constructor TPort.Create(AName:String;APortDir:TPortDirection;AType:String;ARangeDir:TRangeDirection;ALeft,ARight:TValue);
Begin
  inherited Create(AName,AType,ARangeDir,ALeft,ARight);
  FDir  := APortDir;
End;

Function TPort.WriteVerilogDeclaration:String;
Begin
  Result := Indent(FAttributes.GetVerilog + CPortDirectionVerilog[FDir] + FType.GetVerilogRange + ' ' + GetVerilogIdentifier(FName),2);
End;

Function TPort.WriteVerilogInstance:String;
Begin
  Result := GetVerilogIdentifier(FName);   // ".PortName(WireName),"
End;

Function TPort.GetVerilogValue:String;
Begin
  Result := GetVerilogIdentifier(FName);
End;

Function TPort.GetVHDLValue:String;
Begin
  Result := FName;
End;

Function TPort.GetVHDLDeclaration:String;
Begin
  Result := FName + ' : ' + CPortDirectionVHDL[FDir] + ' ' + FType.GetVHDL;
End;

Function TPort.GetILangDeclaration:String;
Begin
  raise Exception.Create('Use special version of GetILangDeclaration with PortID for ILang');
  Result := '';
End;

Function TPort.GetILangDeclaration(APortID:Integer):String;
Begin
  Result := FAttributes.GetILang + 'wire ';
  if Assigned(FType.FRange) then
    Result += 'width ' + IntToStr(FType.FRange.GetWidthInt) + ' ';
  if FType.GetRight <> 0 then
    Result += 'offset ' + IntToStr(FType.FRange.GetRight) + ' ';
  Case FDir of
    dirIn      : Result += 'input '  + IntToStr(APortID) + ' ';
    dirOut     : Result += 'output ' + IntToStr(APortID) + ' ';
    dirInOut   : Result += 'inout '  + IntToStr(APortID) + ' ';
  else
    // including dirUnknown
    raise Exception.Create('Strange FDir, not supported for ILang writer');
  End;
  Result += GetILangIdentifier(FName);
  Result := Indent(Result,2);
End;

{ TGeneric }

Constructor TGeneric.Create(AName:String;AType:TType;AValue:TValue);
Begin
  inherited Create;
  FName  := AName;
  FType  := AType;
  FValue := AValue;
  FAttributes := TAttributeValues.Create;
End;

Constructor TGeneric.Create(AName:String;AType:String;AValue:TValue);
Begin
  Create(AName,TType.Create(AType),AValue);
End;

Function TGeneric.GetVerilogValue : String;
Begin
  Result := FName;
End;

Function TGeneric.WriteVerilogDeclaration:String;
Begin
  Result := '  ' + FAttributes.GetVerilog + 'parameter ' + FName + ' = ' + FValue.GetVerilogValue;
End;

Function TGeneric.WriteVerilogInstance:String;
Begin
  Result := FName;
End;

Function TGeneric.GetVHDLValue : String;
Begin
  Result := FName;
End;

Function TGeneric.GetVHDLDeclaration:String;
Begin
  Result := FName + ' : ' + FType.GetVHDL;
  if assigned(FValue) then
    Result += ' := ' + FValue.GetVHDLValue;
End;

{ TConnection }

Constructor TConnection.Create(AInstance:TInstance;APort:TPort;AIndex:TValue);
Begin
  inherited Create;
  FInstance := AInstance;
  FPort     := APort;
  FIndex    := AIndex;
End;

{ TInstance }

Constructor TInstance.Create(AName:String;AModule:TModule);
Begin
  inherited Create;
  FName   := AName;
  FModule := AModule;
  FGenericValues := TGenericValues.Create;
  FConnections   := TConnections.Create;
  FAttributes    := TAttributeValues.Create;
End;

Procedure TInstance.SetGeneric(AGeneric:String;AValue:TValue);
Begin
  // accept new generics for blackboxed modules
  if (FModule.FGenerics.IndexOf(AGeneric) < 0) and (FModule.FEntityAttributes.FAttrValList.IndexOf('blackbox') < 0) then
    raise Exception.Create('The generic '''+AGeneric+''' doesn''t exist in the instantiated module '''+FModule.FName+'''');
  if FGenericValues.IndexOf(AGeneric) >= 0 then
    raise Exception.Create('The generic '''+AGeneric+''' was already set to the value '+FGenericValues[AGeneric].GetVerilogValue+', can''t set new value '+AValue.GetVerilogValue);
  FGenericValues[AGeneric] := AValue;
End;

Procedure TInstance.ConnectPort(APort:String;ASignal:TValue);
Begin
  if FModule.FPorts.IndexOf(APort) < 0 then
    raise Exception.Create('The port '''+APort+''' doesn''t exist in the instantiated module '''+FModule.FName+'''');
  if FConnections.IndexOf(APort) >= 0 then
    raise Exception.Create('The port '''+APort+''' was already connected with signal '+FConnections[APort].GetVHDLValue+', can''t connect to new signal '+ASignal.GetVHDLValue);
  // don't check for Nil, this means that the port stays "open"
  FConnections[APort] := ASignal;
End;

Function TInstance.CheckConnections:Integer;
Var I : Integer;
Begin
  Result := 0;
  For I := 0 to FModule.FPorts.Count-1 do
    Begin
      if FConnections.IndexOf(FModule.FPorts.Data[I].FName) < 0 then
        Begin
          if Result = 0 then
            WriteLn('Checking port connections of module ',FModule.FName,' as instance ',FName);
          WriteLn('  unconnected port ',FName,'.',FModule.FPorts.Data[I].FName);
          Inc(Result);
        End;
    End;
End;

Function TInstance.CheckGenerics:Integer;
Var I : Integer;
Begin
  Result := 0;
  For I := 0 to FModule.FGenerics.Count-1 do
    Begin
      if FGenericValues.IndexOf(FModule.FGenerics.Data[I].FName) < 0 then
        Begin
          if Result = 0 then
            WriteLn('Checking generic values of module ',FModule.FName,' as instance ',FName);
          WriteLn('  unset generic ',FName,'.',FModule.FGenerics.Data[I].FName);
          Inc(Result);
        End;
    End;
End;

Function TInstance.WriteVerilogDeclaration:String;
Var I : Integer;
Begin
  Result := Indent(FAttributes.GetVerilog + GetVerilogIdentifier(FModule.FName),2);
  if FGenericValues.Count > 0 then Begin
    Result += ' #(' + LineEnding;
    For I := 0 to FGenericValues.Count-1 do
      Begin
        Result += '    .' + FGenericValues.Keys[I] + '(' + FGenericValues.Data[I].GetVerilogValue + ')';
        if I < FGenericValues.Count-1 then
          Result += ',';
        Result += LineEnding;
      End;
    Result += '  )';
  End;
  Result += ' ' + GetVerilogIdentifier(FName) + ' (' + LineEnding;
  For I := 0 to FConnections.Count-1 do
    Begin
      if assigned(FConnections.Data[I]) then
        Result += '    .' + FConnections.Keys[I] + '(' + FConnections.Data[I].GetVerilogValue + ')'
      else
        Result += '    .' + FConnections.Keys[I] + '()';   // explicitely unconnected port
      if I < FConnections.Count-1 then
        Result += ',';
      Result += LineEnding;
    End;
  Result += '  );' + LineEnding;
End;

Function TInstance.WriteVerilogInstance:String;
Begin
  Result := 'TODO: TInstance.WriteVerilogInstance not yet implemented';
End;

Function TInstance.GetVHDL:String;
Var I : Integer;
Begin
  Result := '  ' + FName + ': ' + FModule.FName + LineEnding;
  if FGenericValues.Count > 0 then Begin
    Result += '    generic map (' + LineEnding;
    For I := 0 to FGenericValues.Count-1 do
      Begin
        Result += '      ' + FGenericValues.Keys[I] + ' => ' + FGenericValues.Data[I].GetVHDLValue;
        if I < FGenericValues.Count-1 then
          Result += ',';
        Result += LineEnding;
      End;
    Result += '    )' + LineEnding;
  End;
  Result += '    port map (' + LineEnding;
  For I := 0 to FConnections.Count-1 do
    Begin
      if assigned(FConnections.Data[I]) then
        Result += '      ' + FConnections.Keys[I] + ' => ' + FConnections.Data[I].GetVHDLValue
      else
        Result += '      ' + FConnections.Keys[I] + ' => open';
      if I < FConnections.Count-1 then
        Result += ',';
      Result += LineEnding;
    End;

  Result += '    );' + LineEnding;
End;

Function TInstance.GetILang:String;
Var I : Integer;
Begin
  Result := Indent(FAttributes.GetILang,2);
  Result += '  cell ' +GetILangIdentifier(FModule.FName) + ' ' + GetILangIdentifier(FName) + LineEnding;
  For I := 0 to FGenericValues.Count-1 do
    Result += '    parameter ' + GetILangIdentifier(FGenericValues.Keys[I]) + ' ' + FGenericValues.Data[I].GetILangValue + LineEnding;
  For I := 0 to FConnections.Count-1 do
    if assigned(FConnections.Data[I]) then
      Result += '    connect ' + GetILangIdentifier(FConnections.Keys[I]) + ' ' + FConnections.Data[I].GetILangValue + LineEnding
    else
      raise Exception.Create('Explicitely unconnected port ' + FConnections.Keys[I] + ' not supported for ILang');
  Result += '  end' + LineEnding;
End;

Operator <(A,B:TSignal) : Boolean;
Begin
  Result := A.FName < B.FName;
End;

{ TStatement }

Function TStatement.GetVerilog:String;
Begin
  raise Exception.Create('Not implemented');
  Result := '';
End;

Function TStatement.GetVHDL:String;
Begin
  raise Exception.Create('Not implemented');
  Result := '';
End;

Function TStatement.GetILang:String;
Begin
  raise Exception.Create('Not implemented');
  Result := '';
End;

{ TEmptyLine }

Function TEmptyLine.GetVerilog:String;
Begin
  Result := '';
  if FComment > '' then
    Result := '/* ' + FComment + ' */ ' + LineEnding;  // lets see if this works, at least indentation is gone for multi-line comments :-(
  Result += LineEnding;
End;

Function TEmptyLine.GetVHDL:String;
Begin
  Result := '';
  if FComment > '' then
    Result := Prefix(FComment,'-- ') + LineEnding;
  Result += LineEnding;
End;

Function TEmptyLine.GetILang:String;
Begin
  Result := '';
  if FComment > '' then
    Result := Prefix(FComment,'# ') + LineEnding;
  Result += LineEnding;
End;

{ TAssignment }

Constructor TAssignment.Create(ADest:TValue;AValue:TValue);
Begin
  inherited Create;
  FDest  := ADest;
  FValue := AValue;
End;

Procedure TAssignment.Swap;
Var V : TValue;
Begin
  V      := FDest;
  FDest  := FValue;
  FValue := V;
End;

Function TAssignment.GetVerilog:String;
Begin
  Result := '';
  if FComment > '' then
    Result := '/* ' + FComment + ' */ ' + LineEnding;  // lets see if this works, at least indentation is gone for multi-line comments :-(
  Result += 'assign ';
  if assigned(FDelay) then
    Begin
      Result += '#';
      if FDelay.InheritsFrom(TValueOperator)then
        Result += '(' + FDelay.GetVHDLValue + ') '
      else
        Result += FDelay.GetVHDLValue + ' ';
    End;
  Result += FDest.GetVerilogValue + ' = ' + FValue.GetVerilogValue;
End;

Function TAssignment.GetVHDL:String;
Var LeftWidth : Integer;
Begin
  Result := FDest.GetVHDLValue+ ' <= ';
  LeftWidth := Length(Result);
  if FComment > '' then
    Result := Prefix(FComment,'-- ') + LineEnding + Result;
  Result += Indent(FValue.GetVHDLValue,LeftWidth,false);
  if assigned(FDelay) then
    Result += ' after ' + FDelay.GetVHDLValue;
End;

Function TAssignment.GetILang:String;
Begin
  if assigned(FDelay) then
    raise Exception.Create('Can''t use assignments with delays in ILang: '+GetVHDL);
  if not ((FValue is TSignal) or (FValue is TValueConcat) or (FValue is TValueIndex) or (FValue is TValueBit) or (FValue is TValueVector)) then
    raise Exception.Create('ILang only allows assignments from constants, ports, signals, concatenations and indexes but not from '+FValue.ClassName);
  Result := '  connect ' + FDest.GetILangValue + ' ' + FValue.GetILangValue;
End;

{ TVarAssignment }

Function TVarAssignment.GetVHDL:String;
Begin
  Result := '';
  if FComment > '' then
    Result := Prefix(FComment,'-- ') + LineEnding;
  Result += FDest.GetVHDLValue + ' := ' + FValue.GetVHDLValue;
End;

{ TCondition }

Constructor TCondition.Create(ACondition:TValue);
Begin
  inherited Create;
  FCondition := ACondition;
  FThen := TStatementList.Create;
  FElse := TStatementList.Create;
End;

Function TCondition.AddStatementThen(AStatement:TStatement):TStatement;
Begin
  FThen.Add(AStatement);
  Result := AStatement;
End;

Function TCondition.AddStatementElse(AStatement:TStatement):TStatement;
Begin
  FElse.Add(AStatement);
  Result := AStatement;
End;

Function TCondition.GetVHDL:String;
Var I : Integer;
Begin
  Result := '';
  if FComment > '' then
    Result := Prefix(FComment,'-- ') + LineEnding;
  Result += 'if ' + FCondition.GetVHDLValue + ' then' + LineEnding;
  For I := 0 to FThen.Count-1 do
    Result += Indent(FThen[I].GetVHDL,2) + ';' + LineEnding;
  if FElse.Count > 0 then
    Begin
      Result += 'else' + LineEnding;
      For I := 0 to FElse.Count-1 do
        Result += Indent(FElse[I].GetVHDL,2) + ';' + LineEnding;
    End;
  Result += 'end if';   // without line ending
End;

{ TForLoop }

Constructor TForLoop.Create(ALoopVar:String;ARange:TRange);
Begin
  inherited Create;
  FLoopVar := TVariable.Create(ALoopVar,TypeInt);
  FRange   := ARange;
  FBody    := TStatementList.Create;
End;

Function TForLoop.AddStatement(AStatement:TStatement):TStatement;
Begin
  FBody.Add(AStatement);
End;

Function TForLoop.GetVHDL:String;
Var I : Integer;
Begin
  Result := '';
  if FComment > '' then
    Result := Prefix(FComment,'-- ') + LineEnding;
  Result += 'for ' + FLoopVar.FName + ' in ' + FRange.GetVHDLValue + ' loop' + LineEnding;
  For I := 0 to FBody.Count-1 do
    Result += Indent(FBody[I].GetVHDL,2) + ';' + LineEnding;
  Result += 'end loop';   // without line ending
End;

Const CSeverityVHDLStr : Array[TSeverity] of String = ('note','warning','error','failure');

{ TAssertion }

Constructor TAssertion.Create(ACondition:TValue;AMessage:String;ASeverity:TSeverity);
Begin
  inherited Create;
  FCondition := ACondition;
  FMessage   := AMessage;
  FSeverity  := ASeverity;
End;

Function TAssertion.GetVHDL:String;
Begin
  Result := '';
  if FComment > '' then
    Result := Prefix(FComment,'-- ') + LineEnding;
  if assigned(FCondition) then
    Result := 'assert ' + FCondition.GetVHDLValue + LineEnding + '  ';
  Result += 'report "' + FMessage + '"' + ' severity ' + CSeverityVHDLStr[FSeverity];
End;

{ TWait }

Constructor TWait.Create;
Begin
  inherited Create;
  FType := wtInfinite;
End;

Constructor TWait.Create(AType:TWaitType;AValue:TValue);
Begin
  inherited Create;
  FType := AType;
  Case AType of
    wtFor   : FDuration := AValue;
    wtUntil : FUntil := AValue;
  End;
End;

Function TWait.GetVHDL:String;
Begin
  Result := '';
  if FComment > '' then
    Result := Prefix(FComment,'-- ') + LineEnding;
  Result += 'wait';
  Case FType of
    wtFor   : Result += ' for ' + FDuration.GetVHDLValue;
    wtUntil : Result += ' until ' + FUntil.GetVHDLValue;
  End;
  // final ';' is added by TProcess.GetVHDL
End;

{ TProcess }

Constructor TProcess.Create(AName:String);
Begin
  inherited Create;
  FName := AName;
  FSensitivityList := TSignalSortList.Create;
  FVariables       := TSignalSortList.Create;
  FStatements      := TStatementList.Create;
End;

Function TProcess.AddSensitive(ASignal:TSignal;ASortID:Integer):TSignal;
Begin
  FSensitivityList.Add(ASortID,ASignal);
  Result := ASignal;
End;

Function TProcess.AddVariabe(AVariable:TSignal;ASortID:Integer):TSignal;
Begin
  FVariables.Add(ASortID,AVariable);
  Result := AVariable;
End;

Function TProcess.AddStatement(AStatement:TStatement):TStatement;
Begin
  FStatements.Add(AStatement);
  Result := AStatement;
End;

Function TProcess.GetVHDL:String;
Var I  : Integer;
Begin
  Result := FName + ': ' + 'process';
  if FSensitivityList.Count > 0 then
    Begin
      Result += ' (';
      For I := 0 to FSensitivityList.Count-1 do
        Result += FSensitivityList.Data[I].FName + ',';
      Result[Length(Result)] := ')';  // replace trailing ',' by ')'
    End;
  Result += LineEnding;
  For I := 0 to FVariables.Count-1 do
    Result += Indent(FVariables.Data[I].GetVHDLDeclaration,2) + ';' + LineEnding;

  Result += 'begin' + LineEnding;
  For I := 0 to FStatements.Count-1 do
    Begin
      Result += Indent(FStatements[I].GetVHDL,2);
      if not (FStatements[I] is TEmptyLine) then
        Result += ';' + LineEnding;
    End;
  Result += 'end process ' + FName + ';' + LineEnding;
End;

{ TModule }

Constructor TModule.Create(AName:String);
Begin
  inherited Create;
  FName              := AName;
  FArchitectureName  := 'struct';   // default value
  FGenerics          := TGenericList.Create;
  FSortedGenerics    := TGenericSortList.Create;
  FPorts             := TPortList.Create;
  FSortedPorts       := TPortSortList.Create;
  FArchDecl          := TSortList.Create;
  FArchBody          := TSortList.Create;
  FSignals           := TSignalList.Create;
  FInstances         := TInstanceList.Create;
  FAssignments       := TAssignmentList.Create;
  FProcesses         := TProcessList.Create;
  FEntityAttrDecls        := TAttributeList.Create;
  FArchitectureAttrDecls  := TAttributeList.Create;
  FEntityAttributes       := TAttributeValues.Create;
  FArchitectureAttributes := TAttributeValues.Create;

  FGenerics.         Duplicates := dupError;
  FSortedGenerics.   Duplicates := dupError;
  FPorts.            Duplicates := dupError;
  FSortedPorts.      Duplicates := dupError;
  FSignals.          Duplicates := dupError;
  FInstances.        Duplicates := dupError;
  FArchDecl.         Duplicates := dupError;
  FArchBody.         Duplicates := dupError;
  FGenerics.         Sorted := true;
  FSortedGenerics.   Sorted := true;
  FPorts.            Sorted := true;
  FSortedPorts.      Sorted := true;
  FSignals.          Sorted := true;
  FInstances.        Sorted := true;
  FArchDecl.         Sorted := true;
  FArchBody.         Sorted := true;
End;

Function TModule.Clone:TModule;
Begin
  Result := TModule.Create(FName);
  Result.FArchitectureName := FArchitectureName;
  CopyMap (FGenerics,               Result.FGenerics);
  CopyMap (FSortedGenerics,         Result.FSortedGenerics);
  CopyMap (FPorts,                  Result.FPorts);
  CopyMap (FSortedPorts,            Result.FSortedPorts);
  CopyMap (FArchDecl,               Result.FArchDecl);
  CopyMap (FArchBody,               Result.FArchBody);
  CopyMap (FSignals,                Result.FSignals);
  CopyMap (FInstances,              Result.FInstances);
  CopyList(FAssignments,            Result.FAssignments);
  CopyMap (FProcesses,              Result.FProcesses);
  CopyMap (FEntityAttrDecls,        Result.FEntityAttrDecls);
  CopyMap (FArchitectureAttrDecls,  Result.FArchitectureAttrDecls);
  Result.FEntityAttributes.      Clone(FEntityAttributes);
  Result.FArchitectureAttributes.Clone(FArchitectureAttributes);
End;

Function TModule.Compare(ARevised:TModule;AComparison:TModuleComparisonSet):TDynStringArray;
Var I,J,K : Integer;
    MP,NP : TPort;  // FModule.FPorts, FYosysNetlist.FPorts
    MA,NA : TAttributeValue;

  Function AddError(Msg:String) : Boolean;  // return value is always true so that we can write "if MyError('blah') then continue;"
  Begin
    SetLength(Compare,Length(Compare)+1);
    Compare[Length(Compare)-1] := Msg;
    Result := true;
  End;

Begin
  if (AComparison - [mcName,mcLessPorts,mcLessPortAttrs,mcMorePorts]) <> [] then
    raise Exception.Create('Comparison for these factors not yet implemented.');   // TODO: list not supported factors
  SetLength(Result,0);
  // check name
  if mcName in AComparison then
    if FName <> ARevised.FName then
      AddError('Revised name '+ARevised.FName+' doesn''t match reference name '+FName);
  // check ports
  For I := 0 to FPorts.Count-1 do
    Begin
      MP := FPorts.Data[I];
      // find port
      J := ARevised.FPorts.IndexOf(MP.FName);
      if J < 0 then
        Begin
          if mcLessPorts in AComparison then
            AddError('Port '''+MP.FName+''' is missing in netlist');
          continue;
        End;
      NP := ARevised.FPorts.Data[J];
      // compare port type
      if (MP.FType.FName <> NP.FType.FName) or (MP.FType.GetLeft <> NP.FType.GetLeft) or (MP.FType.GetRight <> NP.FType.GetRight) then
        AddError('Types of port '''+MP.FName+''' differ (reference: '''+MP.FType.GetVHDL+''', netlist: '''+NP.FType.GetVHDL+''')');
      // compare port direction
      if MP.FDir <> NP.FDir then
        AddError('Directions of port '''+MP.FName+''' differ (reference: '''+CPortDirectionVerilog[MP.FDir]+''', netlist: '''+CPortDirectionVerilog[NP.FDir]+''')');
      // compare port attributes
      For J := 0 to MP.FAttributes.Count-1 do
        Begin
          MA := MP.FAttributes.FAttrValList.Data[J];
          // find attribute
          K := NP.FAttributes.FAttrValList.IndexOf(MA.FAttribute.FName);
          if K < 0 then
            Begin
              if mcLessPortAttrs in AComparison then
                AddError('Attribute '''+MA.FAttribute.FName+''' of port '''+MP.FName+''' missing in netlist');
              continue;
            End;
          NA := NP.FAttributes.FAttrValList.Data[K];
          // compare attribute type
          if (MA.FAttribute.FType.FName <> NA.FAttribute.FType.FName) or (MA.FAttribute.FType.GetLeft <> NA.FAttribute.FType.GetLeft) or (MA.FAttribute.FType.GetRight <> NA.FAttribute.FType.GetRight) then
            AddError('Types of the attribute '''+MA.FAttribute.FName+''' of port '''+MP.FName+''' differ (reference: '''+MA.FAttribute.FType.GetVHDL+''', netlist: '''+NA.FAttribute.FType.GetVHDL+''')');
          // compare attribute value
          if MA.FValue.GetVerilogValue <> NA.FValue.GetVerilogValue then
            AddError('Values of the attribute '''+MA.FAttribute.FName+''' of port '''+MP.FName+''' differ (reference: '''+MA.FValue.GetVerilogValue+''', netlist: '''+NA.FValue.GetVerilogValue+''')');
        End;
      // don't check if there are more attributes in the netlist than what we
      // generated, because this is perfectly ok
      // TODO: mcMorePortAttrs
      // ignore comment and default value
    End;
  // now check for superfluous ports
  if mcMorePorts in AComparison then
    For I := 0 to ARevised.FPorts.Count-1 do
      Begin
        NP := ARevised.FPorts.Data[I];
        // find port
        if FPorts.IndexOf(NP.FName) < 0 then
          AddError('Superfluous port '+CPortDirectionVerilog[NP.FDir]+' '+NP.FType.GetVerilog+' '+NP.FName+' in netlist');
      End;
End;

Procedure CheckIntegerKey(Var AKey:Integer;AMap:TFPSMap);
Begin
  if AKey >= 0 then Exit;
  if AMap.Count = 0 then
    AKey := 0
  else
    AKey := Integer(AMap.Keys[AMap.Count-1]^)+1;
End;

Function TModule.AddGeneric(AGeneric:TGeneric;ASortID:Integer) : TGeneric;
Begin
  CheckIntegerKey(ASortID,FSortedGenerics);
  FGenerics.Add(AGeneric.FName,AGeneric);
  FSortedGenerics.Add(ASortID,AGeneric);
  Result := AGeneric;
End;

Function TModule.AddPort(APort:TPort;ASortID:Integer) : TPort;
Begin
  CheckIntegerKey(ASortID,FSortedPorts);
  FPorts.Add(APort.FName,APort);
  FSortedPorts.Add(ASortID,APort);
  Result := APort;
End;

Function TModule.AddType(AType:TTypeDecl;ASortID:Integer):TTypeDecl;
Begin
  CheckIntegerKey(ASortID,FArchDecl);
  FArchDecl.Add(ASortID,AType);
  Result := AType;
End;

Function TModule.AddSignal(ASignal:TSignal;ASortID:Integer):TSignal;
Begin
  CheckIntegerKey(ASortID,FArchDecl);
  try
    FSignals.Add(ASignal.FName,ASignal);
  Except
    On E : EListError do
      Begin
        E.Message := 'Cannot add signal '+ASignal.FName+' to module '+FName+': ' + E.Message;
        WriteLn(StdErr,E.Message);
        StackTrace.DumpStack(StdErr,get_frame);
        raise;
      End;
  End;
  FArchDecl.Add(ASortID,ASignal);
  Result := ASignal;
End;

Function TModule.AddInstance(AInstance:TInstance;ASortID:Integer) : TInstance;
Begin
  CheckIntegerKey(ASortID,FArchBody);
  FInstances.Add(AInstance.FName,AInstance);
  FArchBody.Add(ASortID,AInstance);
  Result := AInstance;
End;

Function TModule.AddAssignment(ADest:TValue;AValue:TValue;ASortID:Integer):TAssignment;
Begin
  CheckIntegerKey(ASortID,FArchBody);
  Result := TAssignment.Create(ADest,AValue);
  FAssignments.Add(Result);
  FArchBody.Add(ASortID,Result);
End;

Function TModule.AddProcess(AProcess:TProcess;ASortID:Integer):TProcess;
Begin
  CheckIntegerKey(ASortID,FArchBody);
  FProcesses.Add(AProcess.FName,AProcess);
  FArchBody.Add(ASortID,AProcess);
  Result := AProcess;
End;

Function TModule.FindAssignment(ADest:String):TAssignment;
Var I : Integer;
Begin
  For I := 0 to FAssignments.Count-1 do
    if (FAssignments[I].FDest is TSignal) and ((FAssignments[I].FDest as TSignal).FName = ADest) then
      Exit(FAssignments[I]);
  Result := Nil;  // nothing found
End;

Function TModule.FindInstance(AModule:String):TInstance;
Var I : Integer;
Begin
  For I := 0 to FInstances.Count-1 do
    if FInstances.Data[I].FModule.FName = AModule then
      Exit(FInstances.Data[I]);
  Result := Nil;  // nothing found
End;

Function TModule.FindModule(AModule:String):TModule;
Var Instance : TInstance;
Begin
  Instance := FindInstance(AModule);
  if assigned(Instance) then
    Result := Instance.FModule
  else
    Result := Nil;  // nothing found
End;

Function TModule.WriteVerilogDeclaration:String;

  // use one common function for FArchDecl and FArchBody
  Procedure WriteVerilogArch(AItem:TObject);
    Procedure WriteSignal(ASignal:TSignal);
    Begin
      Result += PrefixIf(ASignal.FComment,'  // ',LineEnding) + ASignal.WriteVerilogDeclaration + ';' + LineEnding;
    End;
    Procedure WriteInstance(AInstance:TInstance);
    Begin
      AInstance.CheckGenerics;
      AInstance.CheckConnections;
      Result += LineEnding + PrefixIf(AInstance.FComment,'  // ',LineEnding) + AInstance.WriteVerilogDeclaration;
    End;
    Procedure WriteTypeDecl(AType:TTypeDecl);
    Begin
      Result += LineEnding + PrefixIf(AType.FComment,'  // ',LineEnding) + AType.GetVerilog + LineEnding;
    End;
    Procedure WriteAssignment(AAssignment:TAssignment);
    Begin
      Result += Indent(AAssignment.GetVerilog + ';' + LineEnding,2);
    End;
    Procedure WriteProcess(AProcess:TProcess);
    Begin
      Result += '// process ' +AProcess.FName+ ': TODO: processes for Verilog not yet implemented' + LineEnding;
    End;
  Begin
    if      AItem is TSignal     then WriteSignal    (AItem as TSignal)
    else if AItem.InheritsFrom(TTypeDecl) then WriteTypeDecl  (AItem as TTypeDecl)
    else if AItem is TInstance   then WriteInstance  (AItem as TInstance)
    else if AItem is TAssignment then WriteAssignment(AItem as TAssignment)
    else if AItem is TProcess    then WriteProcess   (AItem as TProcess)
    else
      raise Exception.Create(AItem.ClassName+' is not allowed here!');
  End;

Var I    : Integer;
Begin
  Result := FEntityAttributes.GetVerilog + FArchitectureAttributes.GetVerilog + 'module ' + FName + ' (' + LineEnding;
  // ports
  For I := 0 to FSortedPorts.Count-1 do
    Begin
      Result += PrefixIf(FSortedPorts.Data[I].FComment,'  // ',LineEnding);
      Result += FSortedPorts.Data[I].WriteVerilogDeclaration;
      if I < FSortedPorts.Count-1 then
        Result += ',';
      Result += LineEnding;
    End;
  Result += ');' + LineEnding + LineEnding;

  // parameters
  For I := 0 to FSortedGenerics.Count-1 do
    Result += PrefixIf(FSortedGenerics.Data[I].FComment,'  // ',LineEnding) + FSortedGenerics.Data[I].WriteVerilogDeclaration + ';' + LineEnding;
  if FSortedGenerics.Count > 0 then
    Result += LineEnding;
  For I := 0 to FArchDecl.Count-1 do
    WriteVerilogArch(FArchDecl.Data[I]);
  if FArchDecl.Count > 0 then
    Result += LineEnding;
  For I := 0 to FArchBody.Count-1 do
    WriteVerilogArch(FArchBody.Data[I]);
  // end module
  Result += LineEnding;
  Result += 'endmodule' + LineEnding;
End;

Function TModule.WriteVerilogInstance:String;
Begin
  Result := 'TODO: TModule.WriteVerilogInstance not yet implemented';
End;

Function TModule.GetVHDLEntityInner(WithAttributes:Boolean):String;
Var I : Integer;
Begin
  Result := '';
  // generics
  if FGenerics.Count > 0 then
    Begin
      Result += '  generic (' + LineEnding;
      For I := 0 to FSortedGenerics.Count-1 do
        Begin
          Result += PrefixIf(FSortedGenerics.Data[I].FComment,'    -- ',LineEnding) + '    ' + FSortedGenerics.Data[I].GetVHDLDeclaration;
          if I < FSortedGenerics.Count-1 then
            Result += ';';
          Result += LineEnding;
        End;
      Result += '  );' + LineEnding;
    End;
  // ports
  if FPorts.Count > 0 then
    Begin
      Result += '  port (' + LineEnding;
      For I := 0 to FSortedPorts.Count-1 do
        Begin
          Result += PrefixIf(FSortedPorts.Data[I].FComment,'    -- ',LineEnding) + '    ' + FSortedPorts.Data[I].GetVHDLDeclaration;
          if I < FSortedPorts.Count-1 then
            Result += ';';
          Result += LineEnding;
        End;
      Result += '  );' + LineEnding;
    End;
  if not WithAttributes then
    Exit;
  // attribute declarations
  For I := 0 to FEntityAttrDecls.Count-1 do
    Result += Indent(FEntityAttrDecls.Data[I].GetVHDLDeclaration,2) + LineEnding;
  // entity's attributes
  Result += Indent(FEntityAttributes.GetVHDLSpecifications(FName,'entity'),2);
  // generics' attributes
  For I := 0 to FSortedGenerics.Count-1 do
    Result += Indent(FSortedGenerics.Data[I].FAttributes.GetVHDLSpecifications(FSortedGenerics.Data[I].FName,'constant'),2);
  // ports' attributes
  For I := 0 to FSortedPorts.Count-1 do
    Result += Indent(FSortedPorts.Data[I].FAttributes.GetVHDLSpecifications(FSortedPorts.Data[I].FName,'signal'),2);
End;

Function TModule.GetVHDLEntity:String;
Begin
  Result := 'entity ' + FName + ' is' + LineEnding;
  Result += GetVHDLEntityInner(true);
  Result += 'end ' + FName + ';' + LineEnding;
End;

Function TModule.GetVHDLComponent:String;
Begin
  Result := '  component ' + FName + LineEnding;
  Result += Indent(GetVHDLEntityInner(false),2);
  Result += '  end component;' + LineEnding;
End;

Function TModule.GetVHDLArchitecture:String;
Var PrevItem : TObject;

  // use one common function for FArchDecl and FArchBody
  Procedure WriteVHDLArch(AItem:TObject);
    Procedure WriteSignal(ASignal:TSignal);
    Begin
      Result += PrefixIf(ASignal.FComment,'  -- ',LineEnding);
      Result += '  ' + ASignal.GetVHDLDeclaration + ';' + LineEnding;
      Result += Indent(ASignal.FAttributes.GetVHDLSpecifications(ASignal.FName,'signal'),2);
    End;
    Procedure WriteTypeDecl(ATypeDecl:TTypeDecl);
    Begin
      Result += LineEnding + PrefixIf(ATypeDecl.FComment,'  -- ',LineEnding) + '  ' + ATypeDecl.GetVHDL + ';' + LineEnding;
    End;
    Procedure WriteInstance(AInstance:TInstance);
    Begin
      AInstance.CheckGenerics;
      AInstance.CheckConnections;
      Result += LineEnding + PrefixIf(AInstance.FComment,'  -- ',LineEnding) + AInstance.GetVHDL + LineEnding;
    End;
    Procedure WriteAssignment(AAssignment:TAssignment);
    Begin
      Result += Indent(AAssignment.GetVHDL + ';',2) + LineEnding;
    End;
    Procedure WriteProcess(AProcess:TProcess);
    Begin
      Result += Indent(AProcess.GetVHDL,2) + LineEnding;
    End;
  Begin
    if assigned(PrevItem) and (TypeOf(AItem) <> TypeOf(PrevItem)) and
       ((PrevItem is TAssignment) or (PrevItem is TSignal) or (PrevItem.InheritsFrom(TTypeDecl))) then
      Result += LineEnding;
    if      AItem is TSignal     then WriteSignal    (AItem as TSignal)
    else if AItem.InheritsFrom(TTypeDecl) then WriteTypeDecl  (AItem as TTypeDecl)
    else if AItem is TInstance   then WriteInstance  (AItem as TInstance)
    else if AItem is TAssignment then WriteAssignment(AItem as TAssignment)
    else if AItem is TProcess    then WriteProcess   (AItem as TProcess)
    else
      raise Exception.Create(AItem.ClassName+' is not allowed here!');

    PrevItem := AItem;
  End;

Var I : Integer;
    ModuleList : TModuleList;
Begin
  Result := 'architecture ' + FArchitectureName + ' of ' + FName + ' is' + LineEnding + LineEnding;
  // attribute declarations
  For I := 0 to FArchitectureAttrDecls.Count-1 do
    Result += Indent(FArchitectureAttrDecls.Data[I].GetVHDLDeclaration,2) + LineEnding;
  // architecture attributes
  Result += Indent(FArchitectureAttributes.GetVHDLSpecifications(FArchitectureName,'architecture'),2);
  if FArchitectureAttributes.FAttrValList.Count > 0 then Result += LineEnding;
  // components
  // first collect all instantiated modules
  ModuleList := TModuleList.Create;
  For I := 0 to FInstances.Count-1 do
    if ModuleList.IndexOf(FInstances.Data[I].FModule.FName) < 0 then
      ModuleList.Add(FInstances.Data[I].FModule.FName,FInstances.Data[I].FModule);
  For I := 0 to ModuleList.Count-1 do
    Result += ModuleList.Data[I].GetVHDLComponent + LineEnding;
  ModuleList.Free;
  // architecture declaration
  PrevItem := Nil;
  For I := 0 to FArchDecl.Count-1 do
    WriteVHDLArch(FArchDecl.Data[I]);
  Result += LineEnding;
  // architecture body
  Result += 'begin' + LineEnding;
  PrevItem := Nil;
  For I := 0 to FArchBody.Count-1 do
    WriteVHDLArch(FArchBody.Data[I]);
  // end architecture
  Result += LineEnding;
  Result += 'end ' + FArchitectureName + ';' + LineEnding;
End;

Function TModule.GetVHDLHeader:String;
Begin
  Result := 'library ieee;' + LineEnding;
  Result += 'use ieee.std_logic_1164.all;' + LineEnding;
  Result += 'use ieee.numeric_std.all;' + LineEnding;
  Result += LineEnding;
End;

Function TModule.GetVHDL:String;
Begin
  Result := GetVHDLHeader;
  Result += GetVHDLEntity;
  Result += LineEnding;
  Result += GetVHDLArchitecture;
End;

Function TModule.GetILang:String;
Var I    : Integer;
Begin
  Result := '# Generated by netlist.pas' + LineEnding + LineEnding;
  Result += FEntityAttributes.GetILang + FArchitectureAttributes.GetILang + 'module ' + GetILangIdentifier(FName) + LineEnding;
  // ports
  For I := 0 to FSortedPorts.Count-1 do
    Begin
      Result += LineEnding;
      Result += PrefixIf(FSortedPorts.Data[I].FComment,'  # ',LineEnding);
      Result += FSortedPorts.Data[I].GetILangDeclaration(FSortedPorts.Keys[I]) + LineEnding;
    End;
  // signals
  For I := 0 to FSignals.Count-1 do
    Begin
      Result += LineEnding;
      Result += PrefixIf(FSignals.Data[I].FComment,'  # ',LineEnding);
      Result += FSignals.Data[I].GetILangDeclaration + LineEnding;
    End;
  // instances
  For I := 0 to FInstances.Count-1 do
    Begin
      Result += LineEnding;
      Result += PrefixIf(FInstances.Data[I].FComment,'  # ',LineEnding);
      Result += FInstances.Data[I].GetILang;
    End;
  // assignments
  Result += LineEnding;
  For I := 0 to FAssignments.Count-1 do
    Begin
      Result += PrefixIf(FAssignments[I].FComment,'  # ',LineEnding);
      Result += FAssignments[I].GetILang + LineEnding;
    End;
  if FProcesses.Count <> 0 then
    raise Exception.Create('ILang writer doesn''t yet support processes');
  // end module
  Result += 'end' + LineEnding;
End;

Function TModule.GetSignal(AName:String;ASignalPort:TSignalPortSet):TSignal;
Var I : Integer;
Begin
  if (spPort   in ASignalPort) and FPorts.  Find(AName,I) then Exit(FPorts.  Data[I]);
  if (spSignal in ASignalPort) and FSignals.Find(AName,I) then Exit(FSignals.Data[I]);
  Result := Nil;
End;

Function TModule.FindSignal(RegEx:String;ASignalPort:TSignalPortSet):TSignal;
Var I : Integer;
Begin
  RegEx := '^'+RegEx+'$';
  if spPort   in ASignalPort then
    For I := 0 to FPorts.Count-1 do
      if ExecRegExpr(RegEx,FPorts.Keys[I]) then
        Exit(FPorts.Data[I]);
  if spSignal in ASignalPort then
    For I := 0 to FSignals.Count-1 do
      if ExecRegExpr(RegEx,FSignals.Keys[I]) then
        Exit(FSignals.Data[I]);
  Result := Nil;
End;

Function TModule.FindSignals(RegEx:String;Var List:TSignalList;ASignalPort:TSignalPortSet):Integer;
Var OldCount : Integer;
    I        : Integer;
Begin
  RegEx := '^'+RegEx+'$';
  if not assigned(List) then
    List := TSignalList.Create;
  OldCount := List.Count;
  if spPort   in ASignalPort then
    For I := 0 to FPorts.Count-1 do
      if ExecRegExpr(RegEx,FPorts.Keys[I]) then
        List.Add(FPorts.Keys[I],FPorts.Data[I]);
  if spSignal in ASignalPort then
    For I := 0 to FSignals.Count-1 do
      if ExecRegExpr(RegEx,FSignals.Keys[I]) then
        List.Add(FSignals.Keys[I],FSignals.Data[I]);
  Result := List.Count - OldCount;
End;

Function TModule.GetDriver(ASignal:String;AllowNonPort:Boolean;TTL:Integer):TValue;
Var I,J : Integer;
Begin
  // is it an input port?
  I := FPorts.IndexOf(ASignal);
  if (I >= 0) and (FPorts.Data[I].FDir = dirIn) then
    Exit(FPorts.Data[I]);
  // is it driven by an instantiated cell?
  For I := 0 to FInstances.Count-1 do
    With FInstances.Data[I] do
      For J := 0 to FConnections.Count-1 do
        if assigned(FConnections.Data[J]) and (FConnections.Data[J] is TSignal) and ((FConnections.Data[J] as TSignal).FName = ASignal) and (FModule.FPorts[FConnections.Keys[J]].FDir = dirOut) then
          Exit(TConnection.Create(FInstances.Data[I],FModule.FPorts[FConnections.Keys[J]]));
  if TTL <= 0 then
    raise Exception.Create('Too deep recursion, can''t find the signal driver');
  // is it driven by an assignment?
  For I := 0 to FAssignments.Count-1 do
    With FAssignments[I] Do
      if (FDest is TSignal) and ((FDest as TSignal).FName = ASignal) then
        Begin
          if not (FValue is TSignal) then
            Begin
              if AllowNonPort then
                Exit(FValue)
              else
                raise Exception.Create('Can''t handle assignments from anything different than signals: '+FValue.ClassName+' '+FValue.GetVHDLValue);
            End;
          Exit(GetDriver((FValue as TSignal).FName,AllowNonPort,TTL-1));
        End;
  Result := Nil;
End;

Function TModule.GetDriver(AValue:TValue;AllowNonPort:Boolean;TTL:Integer):TValue;
Var I,J : Integer;
Begin
  // is it an input port?
  if (AValue is TPort) then
    Begin
      I := FPorts.IndexOf((AValue as TPort).FName);
      if (I >= 0) and (FPorts.Data[I].FDir = dirIn) then
        Exit(FPorts.Data[I]);
      // not found
      Exit(Nil);
    End;
  // is it driven by an instantiated cell?
  For I := 0 to FInstances.Count-1 do
    With FInstances.Data[I] do
      For J := 0 to FConnections.Count-1 do
        if assigned(FConnections.Data[J]) and (FConnections.Data[J] = AValue) and (FModule.FPorts[FConnections.Keys[J]].FDir = dirOut) then
          Exit(TConnection.Create(FInstances.Data[I],FModule.FPorts[FConnections.Keys[J]]))
        else if assigned(FConnections.Data[J]) and (AValue is TValueIndex) and (FConnections.Data[J] = (AValue as TValueIndex).FValue) and (FModule.FPorts[FConnections.Keys[J]].FDir = dirOut) then
          Begin
            // found driver of TValueIndex, which itself drives all signals at onece
            Exit(TConnection.Create(FInstances.Data[I],FModule.FPorts[FConnections.Keys[J]],(AValue as TValueIndex).FIndex))
          End;
  if TTL <= 0 then
    raise Exception.Create('Too deep recursion, can''t find the signal driver');
  // is it driven by an assignment?
  For I := 0 to FAssignments.Count-1 do
    With FAssignments[I] Do
      if (FDest = AValue) then
        Exit(GetDriver(FValue,AllowNonPort,TTL-1));
  Result := Nil;
End;

Function TModule.GetSinks(ASignal:String;All:Boolean;List:TValueList;TTL:Integer):TValueList;
Var I,J : Integer;
Begin
  if assigned(List) then
    Result := List
  else
    Result := TValueList.Create;
  // is it an output port?
  I := FPorts.IndexOf(ASignal);
  if (I >= 0) and (FPorts.Data[I].FDir = dirOut) then
    Result.Add(FPorts.Data[I]);
  // is it used by an instantiated cell?
  For I := 0 to FInstances.Count-1 do
    With FInstances.Data[I] do
      For J := 0 to FConnections.Count-1 do
        if assigned(FConnections.Data[J]) and (FConnections.Data[J] is TSignal) and ((FConnections.Data[J] as TSignal).FName = ASignal) and (FModule.FPorts[FConnections.Keys[J]].FDir = dirIn) then
          Result.Add(TConnection.Create(FInstances.Data[I],FModule.FPorts[FConnections.Keys[J]]));
  if TTL <= 0 then
    raise Exception.Create('Too deep recursion, can''t find the signal driver');
  // is it used by an assignment?
  For I := 0 to FAssignments.Count-1 do
    With FAssignments[I] Do
      if (FValue is TSignal) and ((FValue as TSignal).FName = ASignal) then
        Begin
          if not (FDest is TSignal) then
            raise Exception.Create('Can''t handle assignments to anything different than signals');
          GetSinks((FDest as TSignal).FName,All,List,TTL-1);
        End;
End;

Begin
  TypeInt    := TType.Create('integer');
  TypeReal   := TType.Create('real');
  TypeTime   := TType.Create('time');
  TypeBit    := TType.Create('std_logic');
  TypeString := TType.Create('string');
End.

