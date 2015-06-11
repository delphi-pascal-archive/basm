{-*+*-*+*-*+*-*+*-*+*-*+*-*+*-*+*-*+*-*+*-*+*-*+*-*+*-*+*-*+*-*+*-*+*-*+*-*+*-}
{-*+*-*+*-*+*-*+*-*+*-*+*-*+*-*+*-* Win-Assembler *+*-*+*-*+*-*+*-*+*-*+*-*+*-}
{-*+*-*+*-*+*-*+*-*+*-*+*-*+*-*+*-*+*-*+*-*+*-*+*-*+*-*+*-*+*-*+*-*+*-*+*-*+*-}
// Written by Basssem

USES	SysUtils, Convert, StrTools,
      ObjList, ObjStack, ObjCodeDump, ObjBuffer,
      ComLine, Symbols, Symbols2, Segments, Asmtostr, Assemble, RelocationStack,
      Scanner, Parser, Global, PeLinker;

{.$i Switches.inc}

{.$APPTYPE CONSOLE}

const
  src : string = '.APPTYPE GUI' + #13#10 +
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
                 '	PUSH	MB_OK+MB_ICONINFORMATION' + #13#10 +
                 '	PUSH	Title' + #13#10 +
                 '	PUSH	Message' + #13#10 +
                 '	PUSH	NIL' + #13#10 +
                 '	CALL	MessageBoxA' + #13#10 +
                 '	PUSH	0' + #13#10 +
                 '	CALL	ExitProcess';

PROCEDURE PrintSymbol(Sym: TSymbol);
 Var  SymData : pointer;
Begin
  SymData:=Sym.Data;
  Case Sym.Kind of
    SYM_LABEL:
        if (SYM_FLAG_USED in Sym.Flag) then begin
          if SYM_FLAG_IMPORT in Sym.Flag then Write(fLst,'IMPORT')
          else if SYM_FLAG_EXTERN in Sym.Flag then Write(fLst,'EXTERN')
          else if SYM_FLAG_PUBLIC in Sym.Flag then Write(fLst,'PUBLIC');
          Write(fLst,#9,RtJustify(Sym.Name,20));
          if (SYM_FLAG_DEFINED in Sym.Flag) then
            Writeln(fLst, TDefinitionKindName[TDataLabel(SymData).Def], #9, Hex32( TDataLabel(SymData).OwnOfs+ pSegment(TDataLabel(SymData).OwnSeg).VirtualAddress ) )
          else Writeln(fLst,' UNDEFINED AND USED');
        end;
    SYM_VALUE:
      ;//WriteLN(fLst,#9,RtJustify(Sym.Name,20),' ',TSymbolKindName[Sym.Kind],' = ', Hex32(pDataValue(SymData).Value),'H' );
    SYM_EQUAL:
      ;//WriteLN(fLst,#9,RtJustify(Sym.Name,20),' ',TSymbolKindName[Sym.Kind], #9, OperandToStr(SIZE_NONE,pDataEqual(SymData).Expr) );
    SYM_TYPEDEF:
      ;//WriteLN(fLst,#9,RtJustify(Sym.Name,20),' ',TSymbolKindName[Sym.Kind], #9 );
  end;
end;

PROCEDURE PrintSymbolStack(SymStack: TSymbolStack);
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

PROCEDURE PrintSymbolLength(SymLength: tSymbolLength);
var
  Len: integer;
  sym: TSymbolStack;
begin
  for Len := 1 to 128 do
  begin
    sym := SymLength.SymbolStack[Len];
    PrintSymbolStack(sym);
  end;
end;

{##########################################################################}
				{ MAIN }
{##########################################################################}
Var Lnk : tPELinker;
  pars: TParser;
  _Scan  : TFileScanner;
BEGIN
  pars := TParser.create;
 { Initialize }
/////
  Listing:=True;
  if Listing Then
  Begin
    Assign(fLst, fLstName);
	  Rewrite(fLst);
	  Writeln(fLst,ProgVersion);
	  Writeln(fLst);
  End;

  _Scan := TFileScanner.Create;
  _Scan.LoadData(src);
  PushScanner(_Scan);
  ReadChar;

  App.Symbols := TSymbolGroupItem.Create; //.Init;
  App.SecCODE.Init(1024,SEG_CODE,'CODE');
  App.SecDATA.Init(1024,SEG_DATA,'DATA');
  App.SecBSS.Init(1024,SEG_BSS,'BSS');
  App.SecIDATA.Init(1024,SEG_IDATA,'IDATA');
  App.LibStack.Init;
  App.AppType:=APP_CONSOLE;

  SymGroup := TSymbolGroup.Create; //.Init;
  SymGroup.PushItem(App.Symbols);
  CurSeg:=@App.SecCODE;
/////
// Pass1
//  Writeln;
//  WriteLN('+Pass 1 ');
  pars.Compile;
  App.SecCODE.SetLocalFixups;
  App.SecDATA.SetLocalFixups;
  //if Not(finish) Then Writeln('Warning: Unexpected end of file!');
//  Writeln(' -> ok');

// Pass2
//  WriteLN('+Pass 2 ');

//  ComLinker;
  Lnk.Link(fExeName);
 { Terminate }
  if Listing=TRUE Then
  Begin
//    Writeln(fLst,#13#10'Symbols:');
    PrintSymbolLength(App.Symbols);
  end;
/////
  Close(fLst);
  DoneScanner;
//
  App.Symbols.Free; //.Done;
  App.SecCODE.Done;
  App.SecDATA.Done;
  App.SecBSS.Done;
  App.SecIDATA.Done;
  App.LibStack.PopAll;
  App.EntryPoint:=0;
/////
END.







