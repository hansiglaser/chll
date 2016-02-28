Unit StackTrace;

{$mode objfpc}{$H+}

Interface

Uses
  Classes, SysUtils;

Type
  // see proc(5)
  TProcMapsFlag = (pmfRead,pmfWrite,pmfExecute,pmfShared);
  TProcMapsFlags = set of TProcMapsFlag;
  PProcMapsEntry = ^TProcMapsEntry;
  TProcMapsEntry = record
    FromAddr : Pointer;         // starting address of the region for this process
    ToAddr   : Pointer;         // ending address (actually +1)
    Flags    : TProcMapsFlags;  // permissions
    Offset   : PtrUInt;         // if mmap()ed, this is the offset
    DevMajor : Integer;         // device major/minor if mmap()ed
    DevMinor : Integer;
    INode    : Int64;           // INode number of mmap()ed
    Pathname : String;          // empty of anonymous, special values: '[stack]', '[stack:<tid>]', '[vdso]', '[heap]'
  End;
  TProcMapsEntries = Array of TProcMapsEntry;

Procedure SetupStackTrace;
Function FindSymbol(Addr,FromAddr:Pointer;Filename:String) : ShortString;
Function FindProcMapsEntry(Addr:Pointer) : PProcMapsEntry;
Function Demangle(MangledName:String):String;
Procedure DumpStack(var f : text;bp : Pointer);

Implementation
Uses Unix,Utils;

Var OldBackTraceStrFunc : TBackTraceStrFunc;

Const ProcMapsEntries : TProcMapsEntries = Nil;

Procedure ReadProcMapsEntries;
Var FM : Byte;
    T  : Text;
    N  : Integer;
    L  : String;
    St : String;
    SA : TDynStringArray;
    I  : Integer;
Begin
  //  St := FileGetContents('/proc/self/maps');
  Assign(T,'/proc/self/maps');
  FM := FileMode;
  FileMode := 0;   // read only
  Reset(T);
  N := 0;
  While not EOF(T) do
    Begin
      ReadLn(T,L);
      SA := Split(L);
      SetLength(ProcMapsEntries,N+1);
      With ProcMapsEntries[N] do
        Begin
          St := SA[0];
          I  := Pos('-',St);
          if I = 0 then
            Begin
              WriteLn('Warning: strange format of maps line ',N+1,' ''',L,'''');
              continue;
            End;
          FromAddr := Pointer(HexToInt64(Copy(St,1,I-1)));
          ToAddr   := Pointer(HexToInt64(Copy(St,I+1,Length(St))));
          Flags := [];
          if SA[1][1] = 'r' then Flags += [pmfRead];
          if SA[1][2] = 'w' then Flags += [pmfWrite];
          if SA[1][3] = 'x' then Flags += [pmfExecute];
          if SA[1][4] = 's' then Flags += [pmfShared];   // 's'hared/'p'rivate
          Offset := HexToInt64(SA[2]);
          St := SA[3];
          I  := Pos(':',St);
          if I = 0 then
            Begin
              WriteLn('Warning: strange format of maps line ',N+1,' ''',L,'''');
              continue;
            End;
          DevMajor := HexToInt64(Copy(St,1,I-1));
          DevMinor := HexToInt64(Copy(St,I+1,Length(St)));
          INode    := StrToInt64(SA[4]);
          if Length(SA) = 6 then
            Pathname := SA[5]
          else
            Pathname := '';
        End;
      Inc(N);
    End;
  Close(T);
  FileMode := FM;   // read/write
End;

Function FindSymbol(Addr,FromAddr:Pointer;Filename:String) : ShortString;
Var St : String;
    TempFile : String;
    I  : Integer;
    T  : Text;
    SA : TDynStringArray;
    A  : Int64;
    LastAddr : Int64;
    LastSym  : String;
Begin
  St := 'nm ';
  if Pos('.so',Filename) > 0 then   // shared library
    Begin
      St += '-D ';
      Addr := Pointer(PtrUInt(Addr) - PtrUInt(FromAddr));
(*
gdb Stacktrace says: 0x00007ffff7aea8f8 in TclEvalObjvInternal
/proc/self/maps says: 7ffff7ab8000-7ffff7bd1000 r-xp 00000000 fe:00 271824                     /usr/lib/x86_64-linux-gnu/libtcl8.5.so
0x00007ffff7aea8f8-0x7ffff7ab8000 = 207096 = 0x328F8

gdb disassembles Tcl_ListMathFuncs with start at at 0x00007ffff7aea500
nm says it starts at 0000000000032500 T Tcl_ListMathFuncs
0x7ffff7ab8000+0x0000000000032500 = 0x00007ffff7aea500 <-- which is correct
*)
    End;
  TempFile := GetTempFileName;
  St += '''' + Filename + ''' > ''' + TempFile + '''';
  I := fpSystem(St);
  if I <> 0 then Exit('Error: '+IntToStr(I));   // error
  Assign(T,TempFile);
  Reset(T);
  LastAddr := 0;
  LastSym  := '*unknown*';
//WriteLn('Searching for symbol at ',HexStr(Addr));
  While not EOF(T) do
    Begin
      ReadLn(T,St);
      // e.g. '000000000040d688 T P$TRFSMGEN_TTRFSMGENAPP_$__INSERTTRFSMWRAPPER$LONGINT$PPTCL_OBJECT'
      SA := Split(St);
      if Length(SA) <> 3 then Continue;
      // SA[0] is the address
      // SA[1] is a flag which denotes the section
      // SA[2] is the symbol name
      if Upcase(SA[1]) <> 'T' then Continue;   // 'T' means: The symbol is in the text (code) section. (upper case: global)
      if (SA[1] = 't') and (Pos('.L',SA[2]) = 1) then Continue;   // local symbol, e.g. '.LA41'
      A := HexToInt64(SA[0]);
//WriteLn(St);
      if A > PtrUInt(Addr) then Continue;
      // get the nearest address below Addr
      if A < LastAddr then Continue;
//WriteLn('Found symbol ',SA[2],' at ',HexStr(Pointer(A)));
      LastAddr := A;
      LastSym  := SA[2];
    End;
  Close(T);
  DeleteFile(TempFile);
  Result := LastSym;

(*
If we don't find the symbol in the .so file, we can try to find it in the debug
symbols directory.

After installing libtcl8.5-dbg, the file
/usr/lib/debug/.build-id/89/6af4a8d82130af74a98da7108520ed613919a6.debug
is available. Note that this name corresponds to the BuildID of libtcl.so:

$ file /usr/lib/x86_64-linux-gnu/libtcl8.5.so
/usr/lib/x86_64-linux-gnu/libtcl8.5.so: ELF 64-bit LSB shared object, x86-64, version 1 (SYSV), dynamically linked, BuildID[sha1]=896af4a8d82130af74a98da7108520ed613919a6, stripped

$ readelf -n /usr/lib/x86_64-linux-gnu/libtcl8.5.so
Displaying notes found at file offset 0x000001c8 with length 0x00000024:
  Owner                 Data size	Description
  GNU                  0x00000014	NT_GNU_BUILD_ID (unique build ID bitstring)
    Build ID: 896af4a8d82130af74a98da7108520ed613919a6

Again, we can use "nm" to get the symbols with addresses
$ nm /usr/lib/debug/.build-id/89/6af4a8d82130af74a98da7108520ed613919a6.debug | grep TclEvalObjvInternal
00000000000326c0 t TclEvalObjvInternal

An even more elegant way would be to read the ELF files. Either directly (there
is a class which is used by the Resource-stuff, but this is extremely limited)
or using libbfd (which is extremely much work).

but see also exeinfo.pp GetModuleByAddr and lineinfo.pp !

More documentation: http://wiki.freepascal.org/Logging_exceptions

very interesting: http://www.yosefk.com/blog/getting-the-call-stack-without-a-frame-pointer.html

libc backtrace() http://www.gnu.org/software/libc/manual/html_node/Backtraces.html

addr2line http://linuxcommand.org/man_pages/addr2line1.html
 - uses debug info but not exported symbols
 - does not automatically search in /usr/lib/debug/.build-id/

*)
End;

Function FindProcMapsEntry(Addr:Pointer) : PProcMapsEntry;
Var I : Integer;
Begin
  if ProcMapsEntries = Nil then
    ReadProcMapsEntries;
  Result := Nil;
  For I := 0 to Length(ProcMapsEntries)-1 do
    With ProcMapsEntries[I] do
      Begin
        if (Addr < FromAddr) or (Addr >= ToAddr) then
          Continue;
        Result := @(ProcMapsEntries[I]);
        Exit;
      End;
End;

Function Demangle(MangledName:String):String;
Var St : String;
    St1 : String;
    I,N : Integer;
Begin
  // see compiler/symdef.pas make_mangledname()   (FPC 2.6.4)
  // name = [<typeprefix>_]<[P$]unit>[_<prefix>][_<suffix>]
  // <typeprefix>: from outside of make_mangledname()
  // <prefix>: [<record>_$_...][<nestedfunc>_...]<function>[$<paramtype>...][$$<returntype>], might be '$CRC<hhhhhhhh>' instead if too long
  // <suffix>: from outside of make_mangledname()
  St := MangledName;
  Result := '';
  // extract unit/program name
  I := Pos('_',St);
  if I = 0 then Exit(MangledName);   // invalid format
  St1 := Copy(St,1,I-1);
  Delete(St,1,I);
  if Pos('P$',St1) = 1 then
    Delete(St1,1,2);   // leading 'P$' for program name (instead of unit name)
  Result += St1 + '.';
  // extract object/class/record hierarchy
  I := Pos('_$_',St);
  while I > 0 do
    Begin
      St1 := Copy(St,1,I-1);
      Delete(St,1,I+2);
      Result += St1 + '.';
      // TODO: even classes can have parameters like functions, which is then for generics!
      I := Pos('_$_',St);
    End;
  // function name
  if St[1] = '_' then
    Delete(St,1,1);   // for class methods, there is '<class>_$__<name>', nested functions inside of class methods are '<class>_$_<name>_<nestedname>'
  repeat
    I := Pos('$',St);
    if I = 0 then
      Begin
        // no parameters or return type, we are done here
        Result += '.' + St;
        St := '';
        break;
      End;
    St1 := Copy(St,1,I-1);  // function name
    Delete(St,1,I);         // St = 'LONGINT$PPTCL_OBJECT'
    Result += St1;          // Result = 'TRFSMGEN.TTRFSMGENAPP.GETRESETSTATE'
    // parameters
    N := 0;
    While (St > '') and (St[1] <> '$') do
      Begin
        if N = 0 then
          Result += '('
        else
          Result += ',';
        I := Pos('$',St);
        if I = 0 then I := Length(St)+1;
        St1 := Copy(St,1,I-1);
        Delete(St,1,I);
        Result += St1;
        Inc(N);
      End;
    if N > 0 then
      Result += ')';
    // return type
    if (St > '') and (St[1] = '$') then
      Begin
        Delete(St,1,1);
        Result += ':' + St;
        Break;
      End;
  Until St = '';
End;

Function FuncInfo(Addr : Pointer): ShortString;
Var ProcMapsEntry : PProcMapsEntry;
    Symbol        : String;
Begin
  Result := OldBackTraceStrFunc(Addr);
  ProcMapsEntry := FindProcMapsEntry(Addr);
  if ProcMapsEntry = Nil then Exit;
  With ProcMapsEntry^ do
    Begin
      if not (pmfExecute in Flags) then
        Begin
          Result += ' not in executable memory region of '+ Pathname +'!';
          Exit;
        End;
      Result += ' (';
      Symbol := FindSymbol(Addr,Pointer(PtrUInt(FromAddr)+Offset),Pathname);
      if ParamStr(0) <> Pathname then
        Result += Pathname + ' '
      else
        Symbol := Demangle(Symbol);
      Result += Symbol + ')';
    End;
End;

Procedure DumpStack(var f : text;bp : Pointer);
var
  i : Longint;
  prevbp : Pointer;
  caller_frame,
  caller_addr : Pointer;
  ProcMapsEntry : PProcMapsEntry;
Begin
  try
    bp := get_frame;
    prevbp:=bp-1;
    i:=0;
    while bp > prevbp Do
     Begin
       caller_addr := get_caller_addr(bp);
       caller_frame := get_caller_frame(bp);
       if (caller_addr<>nil) then
         Writeln(f,FuncInfo(caller_addr));
       if (caller_frame=nil) then
         break;
       ProcMapsEntry := FindProcMapsEntry(caller_frame);
       if ProcMapsEntry = Nil then
         Begin
           // unknown memory region --> invalid frame pointer
           WriteLn(F,'  ... (can''t determine previous stack frame)');
           Break;
         End;
       if ProcMapsEntry^.Pathname <> '[stack]' then
         Begin
           // not pointing to stack --> invalid frame pointer
           WriteLn(F,'  ... (can''t determine previous stack frame)');
           Break;
         End;
       Inc(i);
       If (i>7{256}) Then
         Begin
           // not pointing to stack --> invalid frame pointer
           WriteLn(F,'  ... (maximum depth reached)');
           Break;
         End;
       prevbp:=bp;
       bp:=caller_frame;
     End;
   except
     { prevent endless dump if an exception occured }
   end;
End;

(*
Unfortunately, the function which calls TclOOP.CmdCaller() (extdecl!)
(TclEvalObjvInternal) itself does some strange stack manipulation instead of
"push bp; mov bp,sp". GDB is smart enough to follow this stuff, but we aren't.
OTOH, in the case of an exception it is only interesting from where within of
our own TCL command function it happened, so we don't really need to follow
up to main().

GDB Sources Version 7.6.2:
 - gdb/stack.c: creates command "backtrace"
 - calls backtrace_command
 - calls backtrace_command_1
 - calls struct frame_info* get_current_frame();
   and   struct frame_info* get_prev_frame(frame_info* );  (both in
 - both in gdb/frame.c
 - ...
 - get_prev_frame_1 () (called by get_prev_frame())
 -
 -
*)

(*
FGL-Add Duplicate Problem:
Breakpoint 1, 0x00000000004ddf8a in FGL_TFPSMAP_$__ADD$POINTER$$LONGINT ()
(gdb) bt
#0  0x00000000004ddf8a in FGL_TFPSMAP_$__ADD$POINTER$$LONGINT ()
#1  0x7075442a00000026 in ?? ()
#2  0x207365746163696c in ?? ()
#3  0x6f6c6c6120746f6e in ?? ()
#4  0x74206e6920646577 in ?? ()
#5  0x7473696c20736968 in ?? ()
#6  0x0029782530242820 in ?? ()
#7  0x00007fffffffce00 in ?? ()
#8  0x00007ffff7f341f0 in ?? ()
#9  0x0000000000000000 in ?? ()

==> not even GDB is able to follow that back

disassembly by objdump:

00000000004ddf08 <FGL_TFPSMAP_$__ADD$POINTER$$LONGINT>:
  4ddf08:       48 81 ec 18 01 00 00    sub    $0x118,%rsp
  4ddf0f:       48 89 9c 24 08 01 00    mov    %rbx,0x108(%rsp)
  4ddf16:       00
  4ddf17:       4c 89 a4 24 10 01 00    mov    %r12,0x110(%rsp)
  4ddf1e:       00
  4ddf1f:       48 89 fb                mov    %rdi,%rbx
  4ddf22:       49 89 f4                mov    %rsi,%r12

which shows that it doesn't care for the stack frame :-( Additionally, there is
no debug info available for the unit "fgl".

This function is called by the generic functions, e.g.
00000000004951e0 <UTILS_TFPGMAP$ANSISTRING$LONGINT_$__ADD$ANSISTRING$$LONGINT>:
0000000000495978 <UTILS_TFPGMAP$ANSISTRING$ANSISTRING_$__ADD$ANSISTRING$$LONGINT>:
00000000004af3e0 <NETLIST_TFPGMAP$ANSISTRING$TATTRIBUTE_$__ADD$ANSISTRING$$LONGINT>:
00000000004afb00 <NETLIST_TATTRIBUTEVALUES_$_TFPGMAP$ANSISTRING$TATTRIBUTEVALUE_$__ADD$ANSISTRING$$LONGINT>:
...
which all start with
  4afb00:       55                      push   %rbp
  4afb01:       48 89 e5                mov    %rsp,%rbp
and end with
  4afb23:       c9                      leaveq
  4afb24:       c3                      retq



*)

Procedure SetupStackTrace;
Begin
  OldBackTraceStrFunc := BackTraceStrFunc;
  BackTraceStrFunc:=@FuncInfo;
End;

initialization
  // don't automatically setup symbol lookup for stack trace, do it manually
  // with SetupStackTrace()
End.

