Program TestNetlist;

{$mode objfpc}{$H+}

Uses Classes, Netlist;

Procedure Test1;
Var Module : TModule;
    AttrTest : TAttribute;
    Sig1   : TSignal;
    Sig2   : TSignal;
    Inst   : TInstance;
    Proc   : TProcess;
    Cond1  : TCondition;
    Cond2  : TCondition;

Begin
  AttrTest := TAttribute.Create('TestAttr',TypeString);

  Module := TModule.Create('MyModule');
  Module.FArchitectureName := 'MyStruct';
  Module.AddGeneric(TGeneric.Create('MyFirstGeneric', TypeInt,TValueInteger.Create( 5)),0);
  Module.AddGeneric(TGeneric.Create('MySecondGeneric',TypeInt,TValueInteger.Create(10)),1);
  Module.AddGeneric(TGeneric.Create('MyThirdGeneric', TypeInt,TValueInteger.Create(15)),2);
  Module.AddGeneric(TGeneric.Create('MyFourthGeneric',TType.Create('std_logic_vector',dirDown,15,0),TValueVector.Create(16,$AA55)),3);
  Module.AddPort(TPort.Create('In1_i', dirIn, TypeBit),3);
  Module.AddPort(TPort.Create('In2_i', dirIn, 'std_logic_vector',dirDown,TValueOperatorMinus.Create(Module.FGenerics['MyFirstGeneric'],TValueInteger.Create(1)),TValueInteger.Create(0)),2);
  Module.AddPort(TPort.Create('Out1_o',dirOut,TypeBit),1);
  Module.AddPort(TPort.Create('Out2_o',dirOut,TypeBit),0);

  Module.FPorts['In1_i'].FComment := 'One-line comment for port';
  Module.FPorts['In1_i'].FAttributes.Add(AttrTest,TValueString.Create('TestAttr@port'));
  Module.FPorts['In1_i'].FAttributes.FAttrValList['TestAttr'].FComment := 'One-line comment for port-attribute';
  Module.FGenerics['MyThirdGeneric'].FComment := 'One-line comment for generic';
  Module.FGenerics['MyThirdGeneric'].FAttributes.Add(AttrTest,TValueString.Create('TestAttr@generic'));
  Module.FGenerics['MyThirdGeneric'].FAttributes.FAttrValList['TestAttr'].FComment :=
    'Multi-Line Comment for this generic-attribute'+LineEnding+
    ' - 1st point'+LineEnding+
    ' - 2nd point';
  Module.FEntityAttributes.Add(AttrTest,TValueString.Create('TestAttr@entity'));
  Module.FArchitectureAttributes.Add(AttrTest,TValueString.Create('TestAttr@arch'));

  Sig1 := Module.AddSignal(TSignal.Create('MySig1',TypeBit),0);
  Sig2 := Module.AddSignal(TSignal.Create('MySig2',TypeBit),1);

  Sig1.FComment :=
    'Multi-Line Comment for this signal'+LineEnding+
    ' - 1st point'+LineEnding+
    ' - 2nd point';
  Sig1.FAttributes.Add(AttrTest,TValueString.Create('TestAttr@signal'));
  Sig1.FAttributes.FAttrValList['TestAttr'].FComment := 'One-line comment for this signal-attribute';

  Inst := TInstance.Create('MyModule_0',Module);  // okok, in reality a module can't instantiate itself :-)
  Inst.SetGeneric('MyFirstGeneric',TValueIntegerHex.Create(13));
  Inst.SetGeneric('MyFourthGeneric',TValueVector.Create(16,$AFFE));
  Inst.ConnectPort('In1_i', Sig1);
  Inst.ConnectPort('Out2_o',Sig2);
  Inst.FComment := 'One-line comment for instance';

  Module.AddInstance(Inst,0);

  Module.AddAssignment(Sig1,TValueOperatorAnd.Create(Module.FPorts['In1_i'],Module.FPorts['In2_i']),0)
    .FComment := 'One-line comment for assignment';

  Proc := TProcess.Create('MyProc');
  Proc.FComment := 'Very simple process';
  Proc.AddSensitive(Sig1,0);
  Proc.AddSensitive(Sig2,1);
  Proc.AddVariabe(TVariable.Create('MyVar1',TypeBit),0);
  Proc.AddVariabe(TVariable.Create('MyVar2',TType.Create('unsigned',dirDown,15,0)),1);
  Proc.AddStatement(TAssignment.Create(Module.FPorts['Out1_o'],TValueBit.Create('1')));
  Proc.AddStatement(TAssignment.Create(Module.FPorts['Out2_o'],TValueOperatorOr.Create(Module.FPorts['In1_i'],Module.FPorts['In2_i'])));
  Cond1 := TCondition.Create(TValueOperatorUnequal.Create(Module.FPorts['In1_i'],Module.FPorts['In2_i']));
  Cond1.FComment := 'If Conditional, with a large' + LineEnding + 'multi-line comment';
  Cond1.AddStatementThen(TAssignment.Create(Module.FPorts['Out1_o'],TValueBit.Create('0')));
  Cond2 := TCondition.Create(TValueOperatorGreaterOrEqual.Create(Proc.FVariables[1],TValueInteger.Create(10000)));
  Cond2.AddStatementThen(TAssignment.Create(Module.FPorts['Out1_o'],TValueBit.Create('1')));
  Cond1.AddStatementThen(Cond2);
  Cond1.AddStatementThen(TAssignment.Create(Module.FPorts['Out2_o'],TValueBit.Create('1')));
  Cond1.AddStatementElse(TAssignment.Create(Module.FPorts['Out2_o'],TValueBit.Create('0')));
  Cond1.AddStatementElse(TVarAssignment.Create(Proc.FVariables[0],TValueBit.Create('0')));
  Proc.AddStatement(Cond1);
  Module.AddProcess(Proc,0);

  WriteLn(Module.WriteVerilogDeclaration);
  WriteLn('---------------------------------------------------------------------------');
  WriteLn(Module.GetVHDL);
  WriteLn('---------------------------------------------------------------------------');
  WriteLn(Module.GetVHDLComponent);
End;

Begin
  Test1;
End.

