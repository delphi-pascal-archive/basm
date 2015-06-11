program a;
USES
windows,
SysUtils, Convert, StrTools,
      ObjCodeDump,
      Symbols, Symbols2, Segments, Asmtostr, Assemble, RelocationStack,
      Scanner, Parser, Global, PeLinker, import;

{.$i Switches.inc}

{.$APPTYPE CONSOLE}

const
  src : string =
//    '.APPTYPE GUI' + #13#10 +
//    'include "inc\winconst.inc"' + #13#10 +
//    'include "inc\winstruct.inc"' + #13#10 +
//    'IMPORT ExitProcess, lib:"kernel32.dll", Name:"ExitProcess"' + #13#10 +
//    'IMPORT MessageBoxA, lib:"user32.dll", Name:"MessageBoxA"' + #13#10 +
//    'NIL	= 0' + #13#10 +
//    '.DATA' + #13#10 +
//    ' Message db "Hello!",0' + #13#10 +
//    ' Title   db "BASM",0' + #13#10 +
//    '.CODE' + #13#10 +
//    'START:' + #13#10 +
//    ' MOV ECX, 3' + #13#10 +
//    '@loop:' + #13#10 +
//    ' PUSH ECX' + #13#10 +
//    '	PUSH	MB_OK+MB_ICONSTOP' + #13#10 +
//    '	PUSH	Title' + #13#10 +
//    '	PUSH	Message' + #13#10 +
//    '	PUSH	NIL' + #13#10 +
//    '	CALL	MessageBoxA' + #13#10 +
//    ' POP ECX' + #13#10 +
//    ' LOOP SHORT @loop' + #13#10 +
//    '; INT3' + #13#10 +
//    '	PUSH	0' + #13#10 +
//    '	CALL	ExitProcess';
    '.APPTYPE GUI' + #13#10 +
    'include "inc\winconst.inc"' + #13#10 +
    'include "inc\winstruct.inc"' + #13#10 +
    'IMPORT ExitProcess, lib:"kernel32.dll", Name:"ExitProcess"' + #13#10 +
    'IMPORT MessageBoxA, lib:"user32.dll", Name:"MessageBoxA"' + #13#10 +
    'NIL	= 0' + #13#10 +
    '.DATA' + #13#10 +
    ' Message db "Hello!",0' + #13#10 +
    ' Title   db "BASM",0' + #13#10 +
    '.CODE' + #13#10 +
    'START:' + #13#10 +
    '  CALL _MAINPROC' + #13#10 +
    '  PUSH 0' + #13#10 +
    '  CALL EXITPROCESS' + #13#10 +
    '; END START' + #13#10 +
    '_MAINPROC:' + #13#10 +
    '  PUSH EBP' + #13#10 +
    '  MOV EBP, ESP' + #13#10 +
    '  SUB ESP, 4' + #13#10 +
    '  MOV EAX, EBP' + #13#10 +
    '  SUB EAX, 4' + #13#10 +
    '  MOV [EAX], DWORD PTR 10' + #13#10 + // здесь же не правильнопо смыслу, если, должно быть MOV DWORD PTR [EAX], 10
    '_LBL_WHILE1:    ; "WHILE" START' + #13#10 +
    '  PUSH DWORD PTR [EBP - 4]' + #13#10 +
    '  POP EAX' + #13#10 +
    '  TEST EAX, EAX' + #13#10 +
    '  JZ _LBL_WHILE2' + #13#10 +
    '  MOV EAX, EBP' + #13#10 +
    '  SUB EAX, 4' + #13#10 +
    '  PUSH EAX' + #13#10 +
    '  POP EAX' + #13#10 +
    '  PUSH DWORD PTR [EAX]' + #13#10 +
    '  DEC DWORD PTR [EAX]' + #13#10 +
    '  POP EAX       ; CLEANING STACK AFTER EXPRESSION' + #13#10 +
    '  PUSHAD' + #13#10 +
    '	PUSH	MB_OK+MB_ICONSTOP' + #13#10 +
    '	PUSH	Title' + #13#10 +
    '	PUSH	Message' + #13#10 +
    '	PUSH	NIL' + #13#10 +
    '	CALL	MessageBoxA' + #13#10 +
    '  POPAD' + #13#10 +
    '  JMP _LBL_WHILE1' + #13#10 +
    '_LBL_WHILE2:    ; "WHILE" END' + #13#10 +
    '  PUSH DWORD PTR 0      ; INTEGER CONSTANT' + #13#10 +
    '  POP EAX' + #13#10 +
    '  MOV EBX, EBP  ; SETTING RETURN VALUE' + #13#10 +
    '  ADD EBX, 8' + #13#10 +
    '  MOV [EBX], EAX' + #13#10 +
    '  JMP _LBL_FUNC1' + #13#10 +
    '_LBL_FUNC1:' + #13#10 +
    '  MOV ESP, EBP' + #13#10 +
    '  POP EBP' + #13#10 +
    '  RETN' + #13#10 +
    '; END CODE';

PROCEDURE PrintSymbolLength(SymLength: TSymbolLength);

  PROCEDURE PrintSymbolStack(SymStack: TSymbolStack);

    PROCEDURE PrintSymbol(Sym: TSymbol);
     Var  SymData : pointer;
    Begin
      SymData:=Sym.Data;
      Case Sym.Kind of
        SYM_LABEL:
            if (SYM_FLAG_USED in Sym.Flag) then begin
              if SYM_FLAG_IMPORT in Sym.Flag then
                AddListing('IMPORT')
              else
              if SYM_FLAG_EXTERN in Sym.Flag then
                AddListing('EXTERN')
              else
              if SYM_FLAG_PUBLIC in Sym.Flag then
                AddListing('PUBLIC');

              AddListing(#9 + RtJustify(Sym.Name,20));
              if (SYM_FLAG_DEFINED in Sym.Flag) then
                AddListing(TDefinitionKindName[TDataLabel(SymData).Def] + #9 + Hex32( TDataLabel(SymData).OwnOfs+ tSegment(TDataLabel(SymData).OwnSeg).VirtualAddress ) )
              else
                AddListing(' UNDEFINED AND USED');

              AddListing(sLineBreak);
            end;
        SYM_VALUE:
          ;//WriteLN(fLst,#9,RtJustify(Sym.Name,20),' ',TSymbolKindName[Sym.Kind],' = ', Hex32(pDataValue(SymData).Value),'H' );
        SYM_EQUAL:
          ;//WriteLN(fLst,#9,RtJustify(Sym.Name,20),' ',TSymbolKindName[Sym.Kind], #9, OperandToStr(SIZE_NONE,pDataEqual(SymData).Expr) );
        SYM_TYPEDEF:
          ;//WriteLN(fLst,#9,RtJustify(Sym.Name,20),' ',TSymbolKindName[Sym.Kind], #9 );
      end;
    end;

  var
    Sym : tSymbol;
  Begin
    Sym := SymStack.Head;
    while Sym <> nil do
    begin
      PrintSymbol( Sym );
      Sym := Sym.Next;
    end;
  end;

var
  Len: integer;
  sym: TSymbolStack;
begin
  AddListing(sLineBreak);
  for Len := 1 to 128 do
  begin
    sym := SymLength.SymbolStack[Len];
    PrintSymbolStack(sym);
  end;
end;

Var
  Lnk : TPELinker;
  pars: TParser;
  _Scan  : TFileScanner;
  f: Text;
begin
  asm
//    pushad
//    popad
//    pushfd
//    popfd
  end;
  pars := TParser.create;

  _Scan := TFileScanner.Create;
  _Scan.LoadData(src);
  PushScanner(_Scan);
  ReadChar;

  App.Symbols := TSymbolGroupItem.Create;

  App.SecCODE := TSegment.Create(SEG_CODE, 'CODE');
  App.SecDATA := TSegment.Create(SEG_DATA, 'DATA');
  App.SecBSS := TSegment.Create(SEG_BSS, 'BSS');
  App.SecIDATA := TSegment.Create(SEG_IDATA, 'IDATA');

  App.LibStack := TLibraryStack.Create;
  App.AppType:=APP_CONSOLE;

  SymGroup := TSymbolGroup.Create; //.Init;
  SymGroup.PushItem(App.Symbols);
  CurSeg:=@App.SecCODE;

  pars.Compile;
  App.SecCODE.SetLocalFixups;
  App.SecDATA.SetLocalFixups;

  Lnk := TPELinker.Create;
  Lnk.Link('.exe');

  PrintSymbolLength(App.Symbols);

  DoneScanner;

  App.Symbols.Free;
  App.SecCODE.Free;
  App.SecDATA.Free;
  App.SecBSS.Free;
  App.SecIDATA.Free;
  Lnk.Free;
  App.LibStack.Free;
  App.EntryPoint:=0;

  AssignFile(f, '.lst');
  Rewrite(f);
  Writeln(f, Listing);
  Writeln(f);
  Writeln(f);
  Writeln(f, 'ERRORS:');
  Writeln(f, Errors);
  CloseFile(f);
END.
