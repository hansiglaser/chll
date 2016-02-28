Unit PeriphIntf;

{$mode objfpc}{$H+}
{$INTERFACES CORBA}

Interface

Uses
  Classes, SysUtils, FGL, Netlist, ReconfModule;

Type
  // forward declarations
  TPeriphInst = class;
  TPeriphInstList = specialize TFPGMap<String,TPeriphInst>;

  { TPeriphIntf }

  TPeriphIntf = class
    FInstances : TPeriphInstList;
    Constructor Create;
    Destructor  Destroy; override;
    Function  AddInstance(AInst:TPeriphInst) : TPeriphInst;
    Procedure AddToNetlist(ANetlist:TReconfModuleNetlist); virtual;  // call this one inside the overridden method _after_ the main stuff was done
  End;

  IPeriphInst = interface
    Procedure AddToNetlist(ANetlist:TReconfModuleNetlist);
  End;

  { TPeriphInst }

  TPeriphInst = class
    FName : String;
    FIntf : TPeriphIntf;
    FInst : IPeriphInst;   // object with an instance of this class, used to call back its AddToNetlist method
    Constructor Create(AName:String;AIntf:TPeriphIntf);
    Destructor  Destroy; override;
    Procedure AddToNetlist(ANetlist:TReconfModuleNetlist); virtual;
  End;

  { TPeriphIntfOpenMSP430 }

  TPeriphIntfOpenMSP430 = class(TPeriphIntf)
    FPerAddr           : TSignal;   // signals in parent
    FPerDIn            : TSignal;
    // PerDOut is specific to every instance
    FPerWr             : TSignal;
    FPerEn             : TSignal;
    Constructor Create(APerAddr,APerDIn,APerWr,APerEn:TSignal);
    Destructor  Destroy; override;
    Procedure AddToNetlist(ANetlist:TReconfModuleNetlist); override;
  End;

  { TPeriphInstOpenMSP430 }

  TPeriphInstOpenMSP430 = class (TPeriphInst)
    FBaseAddr : Integer;
    FPerDOut  : TSignal;
    Constructor Create(AName:String;AIntf:TPeriphIntf;ABaseAddr:Integer;APerDOut:TSignal);
    Destructor  Destroy; override;
    Function  GetPerAddr(ANetlist:TReconfModuleNetlist) : TSignal;
    Function  GetPerDIn (ANetlist:TReconfModuleNetlist) : TSignal;
    Function  GetPerDOut(ANetlist:TReconfModuleNetlist) : TSignal;
    Function  GetPerWr  (ANetlist:TReconfModuleNetlist) : TSignal;
    Function  GetPerEn  (ANetlist:TReconfModuleNetlist) : TSignal;
    Procedure AddToNetlist(ANetlist:TReconfModuleNetlist); override;
  End;


Implementation

{ TPeriphIntf }

Constructor TPeriphIntf.Create;
Begin
  inherited Create;
  FInstances := TPeriphInstList.Create;
End;

Destructor TPeriphIntf.Destroy;
Begin
  FInstances.Free;
  Inherited Destroy;
End;

Function TPeriphIntf.AddInstance(AInst:TPeriphInst):TPeriphInst;
Begin
  FInstances.Add(AInst.FName,AInst);
  Result := AInst;
End;

Procedure TPeriphIntf.AddToNetlist(ANetlist:TReconfModuleNetlist);
Var I : Integer;
Begin
  For I := 0 to FInstances.Count-1 do
    FInstances.Data[I].AddToNetlist(ANetlist);
End;

{ TPeriphInst }

Constructor TPeriphInst.Create(AName:String;AIntf:TPeriphIntf);
Begin
  inherited Create;
  FName := AName;
  FIntf := AIntf;
End;

Destructor TPeriphInst.Destroy;
Begin
  Inherited Destroy;
End;

Procedure TPeriphInst.AddToNetlist(ANetlist:TReconfModuleNetlist);
Begin
  // don't check if FInst <> Nil, we require that, and the programmer should get an exception if he forgot :-)
  FInst.AddToNetlist(ANetlist);
End;

{ TPeriphIntfOpenMSP430 }

Constructor TPeriphIntfOpenMSP430.Create(APerAddr,APerDIn,APerWr,APerEn:TSignal);
Begin
  inherited Create;
  FPerAddr  := APerAddr;   // signals in parent
  FPerDIn   := APerDIn;
  FPerWr    := APerWr;
  FPerEn    := APerEn;
End;

Destructor TPeriphIntfOpenMSP430.Destroy;
Begin
  Inherited Destroy;
End;

Procedure TPeriphIntfOpenMSP430.AddToNetlist(ANetlist:TReconfModuleNetlist);
Begin
  // prepare ReconfModule: create ports for OpenMSP430 interface
  ANetlist.FModule.AddPort(TPort.Create('PerAddr_i',   dirIn, TType.Create('std_logic_vector',dirDown,13,0)));
  ANetlist.FModule.AddPort(TPort.Create('PerDIn_i',    dirIn, TType.Create('std_logic_vector',dirDown,15,0)));
  // PerDOut is created by each TPeriphInstOpenMSP430
  ANetlist.FModule.AddPort(TPort.Create('PerWr_i',     dirIn, TType.Create('std_logic_vector',dirDown, 1,0)));
  ANetlist.FModule.AddPort(TPort.Create('PerEn_i',     dirIn, TypeBit));
  // prepare ReconfModule instance: connect ports for OpenMSP430 interface with signals in parent
  ANetlist.FInstance.ConnectPort('PerAddr_i',FPerAddr);
  ANetlist.FInstance.ConnectPort('PerDIn_i', FPerDIn);
  // PerDOut is connected by each TPeriphInstOpenMSP430
  ANetlist.FInstance.ConnectPort('PerWr_i',  FPerWr);
  ANetlist.FInstance.ConnectPort('PerEn_i',  FPerEn);

  // now add all peripheral instances to the netlist
  inherited AddToNetlist(ANetlist);
End;

{ TPeriphInstOpenMSP430 }

Constructor TPeriphInstOpenMSP430.Create(AName:String;AIntf:TPeriphIntf;ABaseAddr:Integer;APerDOut:TSignal);
Begin
  inherited Create(AName,AIntf);
  FBaseAddr := ABaseAddr;
  FPerDOut  := APerDOut;   // signal in parent
End;

Destructor TPeriphInstOpenMSP430.Destroy;
Begin
  Inherited Destroy;
End;

Function TPeriphInstOpenMSP430.GetPerAddr(ANetlist:TReconfModuleNetlist):TSignal;
Begin
  Result := ANetlist.FModule.FPorts['PerAddr_i'];
End;

Function TPeriphInstOpenMSP430.GetPerDIn(ANetlist:TReconfModuleNetlist):TSignal;
Begin
  Result := ANetlist.FModule.FPorts['PerDIn_i'];
End;

Function TPeriphInstOpenMSP430.GetPerDOut(ANetlist:TReconfModuleNetlist):TSignal;
Begin
  Result := ANetlist.FModule.FPorts[FName+'DOut_o'];
End;

Function TPeriphInstOpenMSP430.GetPerWr(ANetlist:TReconfModuleNetlist):TSignal;
Begin
  Result := ANetlist.FModule.FPorts['PerWr_i'];
End;

Function TPeriphInstOpenMSP430.GetPerEn(ANetlist:TReconfModuleNetlist):TSignal;
Begin
  Result := ANetlist.FModule.FPorts['PerEn_i'];
End;

Procedure TPeriphInstOpenMSP430.AddToNetlist(ANetlist:TReconfModuleNetlist);
Begin
  // prepare ReconfModule: create ports for OpenMSP430 interface
  ANetlist.FModule.AddPort(TPort.Create(FName+'DOut_o',   dirOut,TType.Create('std_logic_vector',dirDown,15,0)));
  // prepare ReconfModule instance: connect ports for OpenMSP430 interface with signals in parent
  ANetlist.FInstance.ConnectPort(FName+'DOut_o',FPerDOut);
  // call the object which instantiates this class to add its ports to the netlist
  inherited AddToNetlist(ANetlist);
End;

End.

