unit Parser;

interface
uses
  SysUtils, Convert, StrTools, Classes,
  Symbols, Symbols2, Global, Segments, RelocationStack, Assemble,
  Scanner, Import, Asmtostr;


type
  { Main Expression type to calculate Value and get registers and references used }
  TOperator = (
    coEXPO, {Highest precedence}
	  coMOD, coDIV, coMUL,
    coAND, coSHL, coSHR,
    coPLUS, coMINUS, coXOR, coOR,
    coEQUAL, {Lowest}
    coNONE
  );

  TParser = class(TObject)
  private
    NumArg: Integer;
    Inst: tAsmInstruction;

    FFinish: Boolean;

    procedure ParseLine;

    procedure EvalOperation(Term1, Term2: TAsmOperand; Op: TOperator;
      var Rt: TAsmOperand);
    procedure ParseAlign;
    procedure ParseApptype;
    procedure ParseDB;
    procedure ParseDD;
    procedure ParseDW;
    procedure ParseDirective;
    procedure ParseExtern;
    procedure ParseImport;
    procedure ParseInclude;
    procedure ParseIdentifier;
    procedure ParseModule;
    procedure ParseOperands;
    procedure ParseOperand(var Arg: TAsmOperand);
    procedure ParseOrg;
    procedure ParsePublic;
    procedure ParseExpression(var Term: TAsmOperand);
    procedure ReadExpression(var LastOper: TOperator; var LastTerm: TAsmOperand);
    procedure ReadFactor(var Term: TAsmOperand);
    function ReadOperator: TOperator;
    procedure CheckRelocation(const Op: TAsmOperand; top: tTableOperandLocation);
  public
    procedure Compile;
    constructor Create;
    destructor Destroy;
  end;

implementation

const
 Precedence: array [TOperator] of Byte =(
    1,
    2,2,2,
    3,3,3,
    4,4,4,4,
    5,
    6
 );

PROCEDURE SplitPath(Const Path:String;Var Dir,Name,Ext:String);
 Var i      :Byte;
     DirEndPos,PointPos:Byte;
Begin
 DirEndPos:=0;
 PointPos:=0;
 For i:=1 To Length(Path) Do
  Case Path[I] of
   '\':Begin DirEndPos:=i;PointPos:=0;End;
   '.':PointPos:=i;
  End;

 if DirEndPos>1 Then Dir:=Copy(Path,1,DirEndPos) Else Dir:='';
 if PointPos>1 Then
   Begin
     Name:=Copy(Path, DirEndPos+1,PointPos-DirEndPos-1);
     Ext:=Copy(Path, PointPos,Length(Path)-PointPos+1);
   End
   Else Begin
     Name:=Copy(Path, DirEndPos+1,Length(Path)-DirEndPos);
     Ext:='';
   End;
End;

{ TParser }

procedure TParser.CheckRelocation(const Op: TAsmOperand;
  top: tTableOperandLocation);
var
  rel: TRelocKind;
begin
  case Op.Loc of
    LOC_IMMEDIATE:
      if Op.RefKind <> REF_NONE then
      begin
        case top of
          coJRS:
            rel := REL_SHORT; { SHORT Immediate relative byte offset (for jumps)}

          coJRN:
            rel := REL_NEAR;  { NEAR Immediate relative full offset (for jumps)}
        else
          rel := REL_OFFSET;
        end;
        CurSeg.Relocs.PushReloc( rel, Op.RefSym, CurSeg.Size+DumpOfs.Immediate, Op.Value);
      end;

    LOC_MEMORY:
      if Op.RefKind <> REF_NONE then
        CurSeg.Relocs.PushReloc( REL_OFFSET, Op.RefSym, CurSeg.Size+DumpOfs.Disp, Op.Value);
  end;
end;

procedure TParser.Compile;
begin
  FFinish := False;
  ReadToken;
  while not FFinish do
    ParseLine;
end;

constructor TParser.Create;
begin
  Inst := TAsmInstruction.Create;
end;

destructor TParser.Destroy;
begin
  Inst.Free;
end;

procedure TParser.EvalOperation(Term1, Term2: TAsmOperand; Op: TOperator;
  var Rt: TAsmOperand);
begin
 Rt.Loc := LOC_NONE;
 Rt.RefKind := REF_NONE;
 Rt.Value := 0;

  case (OP) of
    coPLUS: // Addition of values, registers and references
    case Term1.Loc of
      LOC_IMMEDIATE:
      case Term2.Loc of
        LOC_IMMEDIATE:
        begin
          if Term1.RefKind <> REF_NONE then
            if Term2.RefKind <> REF_NONE then
              AddError('More than one symbol in an operand')
            else
            begin
              Rt.RefKind := Term1.RefKind;
              Rt.RefSym := Term1.RefSym;
            end
          else
          begin
            Rt.RefKind := Term2.RefKind;
            Rt.RefSym := Term2.RefSym;
          end;
          Rt.Loc := LOC_IMMEDIATE;
          Rt.Value := Term1.Value + Term2.Value;
        end;

        LOC_REGISTER:
        begin
          Rt.RefKind := Term1.RefKind;
          Rt.RefSym := Term1.RefSym;
          SetMemoryOperand(Rt, SIZE_NONE, Term2.Reg, REG_NONE, 0, Term1.Value);
        end;

        LOC_MEMORY:
        begin
          if Term1.RefKind<>REF_NONE then
            if Term2.RefKind<>REF_NONE then
              AddError('More than one symbol in an operand')
            else
            begin
              Rt.RefKind := Term1.RefKind;
              Rt.RefSym := Term1.RefSym;
            end
          else
          begin
            Rt.RefKind := Term2.RefKind;
            Rt.RefSym := Term2.RefSym;
          end;
          SetMemoryOperand(Rt, SIZE_NONE, Term2.Base, Term2.Index, Term2.Scale,
            Term1.Value + Term2.Offset);
        end;
      end;

      LOC_REGISTER:
      case Term2.Loc of
        LOC_IMMEDIATE:
        begin
          Rt.RefKind := Term2.RefKind;
          Rt.RefSym := Term2.RefSym;
          SetMemoryOperand(Rt, SIZE_NONE, Term1.Reg, REG_NONE, 0, Term2.Value);
        end;

        LOC_REGISTER:
        begin
          if RegisterSize(Term1.Reg) <> RegisterSize(Term2.Reg) then
            AddError('Different Register Sizes');

          Rt.RefKind := REF_NONE;
          Rt.RefSym := nil;
          SetMemoryOperand(Rt, SIZE_NONE, Term1.Reg, Term2.Reg, 0, 0);
        end;

        LOC_MEMORY:
        begin
          if Term2.Index <> REG_NONE then
            AddError('Too much registers');

          Rt.RefKind := Term2.RefKind;
          Rt.RefSym := Term2.RefSym;
          SetMemoryOperand(Rt, SIZE_NONE, Term1.Reg, Term2.Reg, Term2.Scale, Term2.Offset);
        end;
      end;

      LOC_MEMORY:
      case Term2.Loc of
        LOC_IMMEDIATE:
        begin
          if Term1.RefKind <> REF_NONE then
            if Term2.RefKind <> REF_NONE then
              AddError('Cant evaluate 2 references')
            else
            begin
              Rt.RefKind := Term1.RefKind;
              Rt.RefSym := Term1.RefSym;
            end
          else
          begin
            Rt.RefKind := Term2.RefKind;
            Rt.RefSym := Term2.RefSym;
          end;
          SetMemoryOperand(Rt, SIZE_NONE, Term1.Base, Term1.Index, Term1.Scale,
            Term1.Value + Term2.Offset);
        end;

        LOC_REGISTER:
        begin
          if Term1.Index <> REG_NONE then
            AddError('Too much registers');

          Rt.RefKind := Term1.RefKind;
          Rt.RefSym := Term1.RefSym;
          SetMemoryOperand(Rt, SIZE_NONE, Term2.Reg, Term1.Base, Term1.Scale,
            Term1.Offset);
        end;

        LOC_MEMORY:
        begin
          if (Term1.Index <> REG_NONE) and (Term2.Index <> REG_NONE) then
            AddError('Too much registers');

          if (Term1.Scale <> 0) and (Term2.Scale <> 0) then
            AddError('Only scaled index');

          if Term1.RefKind <> REF_NONE then
            if Term2.RefKind <> REF_NONE then
              AddError('More than one symbol in an operand')
            else
            begin
              Rt.RefKind:=Term1.RefKind;
              Rt.RefSym:=Term1.RefSym;
            end
          else
          begin
            Rt.RefKind := Term2.RefKind;
            Rt.RefSym := Term2.RefSym;
          end;

          if Term1.Scale <> 0 then
            SetMemoryOperand(Rt, SIZE_NONE, Term2.Base, Term1.Base, Term1.Scale,
              Term2.Offset)
          else
            SetMemoryOperand(Rt, SIZE_NONE, Term1.Base, Term2.Base, Term2.Scale,
              Term2.Offset)
        end;
      end;
    end;

    coMINUS:
    begin{ Substraction of values only }
      if Term2.Loc <> LOC_IMMEDIATE then
        AddError('Invalid Register operation');

      if Term2.RefKind <> REF_NONE then
        AddError('Invalid Register operation');

      case Term1.Loc of
        LOC_IMMEDIATE:
        begin
          Rt.RefKind := Term1.RefKind;
          Rt.RefSym := Term1.RefSym;
          SetImmediateOperand(Rt, Term1.Value - Term2.Value);
        end;

        LOC_REGISTER:
        begin
          Rt.RefKind := Term1.RefKind;
          Rt.RefSym := Term1.RefSym;
          SetMemoryOperand(Rt,SIZE_NONE, Term1.Reg, REG_NONE, 0, -Term2.Value);
        end;

        LOC_MEMORY:
        begin
          Rt.RefKind := Term1.RefKind;
          Rt.RefSym := Term1.RefSym;
          SetMemoryOperand(Rt,SIZE_NONE, Term1.Base, Term1.Index, Term1.Scale,
            Term1.Offset - Term2.Value);
        end;
      end;
    end;

    coNONE: Move(Term2, Rt, Sizeof(Term2));
  else
    begin
      if (Term1.Loc <> LOC_IMMEDIATE) or (Term2.Loc <> LOC_IMMEDIATE) then
        AddError('Invalid Register operation');

      if (Term1.RefKind <> REF_NONE) or (Term2.RefKind <> REF_NONE) then
        AddError('Invalid Register operation');

      case Op of
        //coEXPO:Result.Value := Expo(Term1.Value, Term2.Value);
        coEQUAL : Rt.Value := Integer(Term1.Value = Term2.Value);
        coMOD   : Rt.Value := Term1.Value mod Term2.Value;
        coDIV   : Rt.Value := Term1.Value div Term2.Value;
        coMUL   : Rt.Value := Term1.Value *   Term2.Value;
        coAND   : Rt.Value := Term1.Value and Term2.Value;
        coSHL   : Rt.Value := Term1.Value shl Term2.Value;
        coSHR   : Rt.Value := Term1.Value shr Term2.Value;
        coXOR   : Rt.Value := Term1.Value xor Term2.Value;
        coOR    : Rt.Value := Term1.Value or  Term2.Value;
      end;
    end;
  end;
end;

procedure TParser.ParseAlign;
var
  Expr: TAsmOperand;
begin
  ReadToken;
  ParseExpression(Expr);

  if Expr.Loc <> LOC_IMMEDIATE then
    AddError('Integer constant expected');

  if Expr.RefKind <> REF_NONE then
    AddError('Integer constant expected');

  if (Expr.Value > 0) and (Expr.Value < 1024) then
    CurSeg.Align(Expr.Value);

  SkipLine;
end;

procedure TParser.ParseApptype;
begin
  ReadToken;
  if Token <> TOK_IDENTIFIER then
    AddError('Identifier expected');

  if TokenUpStr = 'CONSOLE' then
    App.AppType := APP_CONSOLE
  else
  if TokenUpStr = 'GUI' then
    App.AppType := APP_GUI
  else
    AddError('Unknown App Type');
end;

procedure TParser.ParseDB;
var
  Value: Longint;
  DupValue: Longint;
begin
  repeat
    ReadToken;
    if Token = TOK_INTCONST then
    begin
      Value:=TokenInt; DupValue:=1;
      ReadToken;

      if (Token = TOK_IDENTIFIER) and (TokenUpStr = 'DUP') then
      begin
        if ReadToken <> TOK_LPAREN then
          AddError(EXPECT_LPAREN);

        if ReadToken <> TOK_INTCONST then
          AddError(EXPECT_INT_CONST);

        DupValue:=Value;

        Value:=TokenInt;

        if (ReadToken<>TOK_RPAREN) then
          AddError(EXPECT_RPAREN);
      end;

      if Value > 255 then
        AddError('Value out of range');

      CurSeg.AddByte(Value, DupValue);
    end else
    if Token = TOK_StrConst then
    begin
      CurSeg.AddString(TokenStr);
      ReadToken;
    end else
      AddError('Expect Byte Data');
  until Token <> TOK_COMMA;
end;

procedure TParser.ParseDD;
var
  Value: Longint;
  DupValue: Longint;
  Sym: TSymbol;
begin
  repeat
    case ReadToken of
      TOK_INTCONST:
      begin
        Value:=TokenInt; DupValue:=1;
        ReadToken;
        if (Token = TOK_IDENTIFIER) and (TokenUpStr = 'DUP') then
        begin
          if ReadToken <> TOK_LPAREN then
            AddError(EXPECT_LPAREN);

          if ReadToken <> TOK_INTCONST then
            AddError(EXPECT_INT_CONST);

          DupValue := Value;
          Value := TokenInt;

          if ReadToken <> TOK_RPAREN then
            AddError(EXPECT_RPAREN);
        end;
        CurSeg.AddDword(Value, DupValue);
      end;

      TOK_IDENTIFIER:
      begin
        if TokenUpStr = 'OFFSET' then
        begin
    			ReadToken;
          Sym := SymGroup.FindSymbol(TokenUpStr);
          if Sym = nil then
          begin
            Sym := TSymbol.Create(SYM_UNDEF, TokenUpStr, [SYM_FLAG_USED], nil);
            SymGroup.Head.PushSymbol(Sym);
          end else
          if Sym.Kind <> SYM_LABEL then
            Sym.Flag := Sym.Flag + [SYM_FLAG_USED];

          CurSeg.Relocs.PushReloc(REL_OFFSET, Sym, CurSeg.Size, 0);
          Value := 0;
    			CurSeg.AddDword(Value, 1);
    			ReadToken;
        end else
          AddError('Expect Byte Data');
      end;
      // else ?.
    else
      AddError('DD error');
    end;
  until ReadToken <> TOK_COMMA;
end;

procedure TParser.ParseDirective;
begin
  case tDirective(TokenInt) of
    DIR_DB:
      ParseDB;
    DIR_DW:
      ParseDW;
    DIR_DD:
      ParseDD;
    DIR_ORG:
      ParseOrg;
    DIR_END:
      FFinish:=True;
    DIR_INCLUDE:
      ParseInclude;
    DIR_EXTERN:
      ParseExtern;
    DIR_PUBLIC:
      ParsePublic;
    DIR_IMPORT:
      ParseImport;
    DIR_ALIGN:
      ParseAlign;
  end;

  SkipLine;
end;

procedure TParser.ParseDW;
var
  value: Longint;
  dupValue: Longint;
begin
  repeat
    ReadToken;
    if Token = TOK_INTCONST then
    begin
      value := TokenInt;
      dupValue := 1;
      ReadToken;
      if (Token = TOK_IDENTIFIER) and (TokenUpStr = 'DUP') then
      begin
        if ReadToken <> TOK_LPAREN then
          AddError(EXPECT_LPAREN);

        if ReadToken <> TOK_INTCONST then
          AddError(EXPECT_INT_CONST);

        dupValue:=value;
        value:=TokenInt;

        if ReadToken <> TOK_RPAREN then
          AddError(EXPECT_RPAREN);
      end;
      if Cardinal(value) > $FFFF then
        AddError('Out of range');

      CurSeg.AddWord(value, dupValue);
    end else
      AddError('DW error');

  until ReadToken <> TOK_COMMA;
end;

procedure TParser.ParseExpression(var Term: TAsmOperand);
var
  oper: TOperator;
Begin
  oper := coNONE;
  ReadExpression(oper, Term);
  Term.Size := SIZE_NONE;
end;

procedure TParser.ParseExtern;
var
  labl: string;
  sym: TSymbol;
Begin
  ReadToken;

  if not IsTokenIdent then
    AddError('Identifier expected');

  labl := TokenUpStr;{ Get The Name }
  sym := SymGroup.FindSymbol(labl);

  if sym = nil then
  begin
    sym := TSymbol.Create(SYM_UNDEF, labl, [SYM_FLAG_EXTERN], nil);
    SymGroup.Head.PushSymbol(sym);
  end else
  if sym.Kind = SYM_LABEL then
    if SYM_FLAG_DEFINED in sym.Flag then
      AddError('External Label Defined in current module')
    else
      sym.Flag := sym.Flag + [SYM_FLAG_EXTERN]
  else
    AddError('External Label Defined in current module');
end;

procedure TParser.ParseIdentifier;
var
  Labl: string;
  Expr: TAsmOperand;
  Sym: TSymbol;
  SymDef: TSymbol;
  SaveCurSeg: TSegment;
  tmp: TObject;
begin
  Labl := TokenUpStr;
  Sym := SymGroup.Head.FindSymbol(Labl);

  if Sym <> nil then
    if SYM_FLAG_DEFINED in Sym.Flag then
      AddError('Redeclared Identifier ' + Labl);

  ReadToken;
  case Token of
    TOK_IDENTIFIER:
    begin
      if TokenUpStr = 'EQU' then
      Begin { Symbol EQU Expression }
        ReadToken;
        ParseExpression(Expr);
        if (Expr.Loc = LOC_IMMEDIATE) and (Expr.RefKind = REF_NONE) then
        begin
          if Sym = nil then
          begin
            Sym := TSymbol.Create(SYM_UNDEF, Labl, [SYM_FLAG_DEFINED], nil);
            SymGroup.Head.PushSymbol(Sym);
          end;
          Sym.Kind := SYM_VALUE;
          Sym.Flag := Sym.Flag + [SYM_FLAG_DEFINED];
          Sym.Data := TDataValue.Create(Expr.Value);
        end else
        begin
          if Sym = nil then
          begin
            Sym := TSymbol.Create(SYM_UNDEF, Labl, [SYM_FLAG_DEFINED], nil);
            SymGroup.Head.PushSymbol(Sym);
          end;

          Sym.Kind := SYM_EQUAL;
          Sym.Flag := Sym.Flag + [SYM_FLAG_DEFINED];
          Sym.Data := TDataEqual.Create(Expr);
        end;
        SkipLine;
      end else
      if TokenUpStr = 'DB' then
      begin { Symbol DB ? }
        if Sym = nil then
        begin
          Sym := TSymbol.Create(SYM_UNDEF, Labl, [SYM_FLAG_DEFINED], nil);
          SymGroup.Head.PushSymbol(Sym);
        end;

        Sym.Kind := SYM_LABEL;
        Sym.Flag := Sym.Flag+ [SYM_FLAG_DEFINED];
        Sym.Data := TDataLabel.Create(DEF_BYTE, nil, CurSeg, CurSeg.Size);
	      ParseDB;
      end else
      if TokenUpStr = 'DW' then
      begin { Symbol DW ? }
        if Sym = nil then
        begin
          Sym := TSymbol.Create(SYM_UNDEF, Labl, [SYM_FLAG_DEFINED], nil);
          SymGroup.Head.PushSymbol(Sym);
        end;

        Sym.Kind := SYM_LABEL;
        Sym.Flag := Sym.Flag + [SYM_FLAG_DEFINED];
        Sym.Data := TDataLabel.Create(DEF_WORD, nil, CurSeg, CurSeg.Size);

	      ParseDW;
      end else
      if TokenUpStr = 'DD' then
      begin { Symbol DW ? }
        if Sym = nil then
        begin
          Sym := TSymbol.Create(SYM_UNDEF, Labl, [SYM_FLAG_DEFINED], nil);
          SymGroup.Head.PushSymbol(Sym);
        end;

        Sym.Kind := SYM_LABEL;
        Sym.Flag := Sym.Flag + [SYM_FLAG_DEFINED];
        Sym.Data := TDataLabel.Create(DEF_DWORD, nil, CurSeg, CurSeg.Size);
	      ParseDD;
      end else
      if TokenUpStr = 'STRUCT' then
      begin
        if Sym <> nil then
          AddError('Redeclared Structure ' + Sym.Name);
        // New Struct : Segment, SymList
        tmp := TDataStruct.Create(Labl);
        Sym := TSymbol.Create(SYM_TYPEDEF, Labl, [SYM_FLAG_DEFINED], tmp);

        SymGroup.Head.PushSymbol(Sym);

        // Save Current Segment
        SaveCurSeg := CurSeg;
        CurSeg := TDataStruct(tmp).Segment;
        SymGroup.PushItem(TDataStruct(tmp).Symbols);
        SkipLine;

        while Token <> TOK_EOF do
        begin
          if (Token = TOK_IDENTIFIER) then
            if (Labl = TokenUpStr) or (TokenUpStr = 'ENDS') then
            begin // it's the end of the struct
              TDataStruct(tmp).Size := CurSeg.Size;
              CurSeg := SaveCurSeg;
              SymGroup.PopItem;
              SkipLine;

              Break;
            end;
          ParseLine;
        end;
      end else
      begin
        SymDef := SymGroup.FindSymbol(TokenUpStr);
        if (SymDef <> nil) and (SymDef.Kind = SYM_TYPEDEF) then
          case TDataTypeDef(SymDef.Data).Def of
            DEF_STRUCT:
            begin
              // ADD SYMBOL
              if Sym = nil then
              begin
                Sym := TSymbol.Create(SYM_LABEL, Labl, [SYM_FLAG_DEFINED], nil);
                SymGroup.Head.PushSymbol(Sym);
              end;

              Sym.Kind := SYM_LABEL;
              Sym.Flag := Sym.Flag+ [SYM_FLAG_DEFINED];
              Sym.Data := TDataLabel.Create(DEF_STRUCT, SymDef, CurSeg, CurSeg.Size);

              // WRITE DATA TO SEGMENT
              CurSeg.AddBuffer(TDataStruct(SymDef.Data).Segment.Data[0], TDataStruct(SymDef.Data).Segment.Size, 1);
              SkipLine;
            end;
          else
            AddError(EXPECT_COLON)
          end
        else
          AddError(EXPECT_COLON);
      end;
    end;

    TOK_EQUAL:
    begin { Symbol = NumberValue }
      ReadToken;
      ParseExpression(Expr);

      if (Expr.Loc <> LOC_IMMEDIATE) or (Expr.RefKind <> REF_NONE) then
        AddError(EXPECT_CONSTANT);

      if Sym = nil then
      begin
        Sym := TSymbol.Create(SYM_VALUE, Labl, [SYM_FLAG_DEFINED], nil);
        SymGroup.Head.PushSymbol(Sym);
      end;

      Sym.Kind := SYM_VALUE;
      Sym.Flag := Sym.Flag + [SYM_FLAG_DEFINED];
      Sym.Data := TDataValue.Create(Expr.Value);

      SkipLine;
    end;

    TOK_COLON:
    begin { Label : }
      if Sym = nil then
      begin
        Sym := TSymbol.Create(SYM_LABEL, Labl, [SYM_FLAG_DEFINED], nil);
        SymGroup.Head.PushSymbol(Sym);
      end;

      Sym.Kind := SYM_LABEL;
      Sym.Flag := Sym.Flag+ [SYM_FLAG_DEFINED];
      Sym.Data := TDataLabel.Create(DEF_LABEL, nil, CurSeg, CurSeg.Size);

     	ReadToken;
    end;
  else
    AddError(EXPECT_COLON);
  end;
end;

procedure TParser.ParseImport;
var
  Labl: string;
  LibName: string;
  ImportName: string;
  LibItem: TLibraryItem;
  ImportItem: TImportItem;
  Sym: TSymbol;
begin
  ReadToken;

  if not IsTokenIdent Then
    AddError('Identifier expected');

  Labl := TokenUpStr;
  ImportName := TokenStr;

  if ReadToken <> TOK_COMMA then
    AddError('"," expected');

  if ReadToken <> TOK_IDENTIFIER then
    AddError('Identifier expected');

  if TokenUpStr <> 'LIB' then
    AddError('');

  if ReadToken <> TOK_COLON then
    AddError('":" expected');

  if ReadToken <> TOK_STRCONST then
    AddError('');

  LibName := TokenStr;

  if ReadToken = TOK_COMMA then
  begin
    if ReadToken <> TOK_IDENTIFIER then
      AddError('Identifier expected');

    if TokenUpStr <> 'NAME' then
      AddError('');

    if ReadToken <> TOK_COLON then
      AddError('":" expected');

    if ReadToken <> TOK_STRCONST then
      AddError('');

    ImportName := TokenStr;
  end;

  LibItem := App.LibStack.FindLib(LibName);
  if LibItem = nil then
  begin
    App.LibStack.PushLib(LibName);
    LibItem := App.LibStack.Head;
  end;

  ImportItem := LibItem.FindImport(ImportName);
  if ImportItem = nil then
  begin
    Sym := TSymbol.Create(SYM_UNDEF, Labl, [SYM_FLAG_IMPORT], nil);
    SymGroup.Head.PushSymbol(Sym);
    LibItem.PushImport(ImportName, -1, Sym);
  end;
  SkipLine;
end;

procedure TParser.ParseInclude;
var
  dir, name, ext: string;
  _fScan: TFileScanner;
  _f: TStringStream;
Begin
  if ReadToken = TOK_STRCONST then
    SplitPath(TokenStr, dir, name, ext);

  if ext = '' then
    ext := '.INC';

  if FileExists(DirSOURCE + dir + name + ext) then
    dir := DirSOURCE+dir
  else
    dir := DirINCLUDE+dir;

  _fScan := TFileScanner.Create;

  _f := TStringStream.Create;
  _f.LoadFromFile(dir + name + ext);
  try
    if _fScan.LoadData(_f.DataString) then
    begin
      PushScanner(_fScan);
      ReadToken;
    end;
  finally
    _f.Free;
  end;
end;

procedure TParser.ParseLine;
begin
  case Token of
    TOK_EOLN:
      ReadToken;

    TOK_EOF:
      FFinish := True;

    TOK_POINT:
      ParseModule;

    TOK_IDENTIFIER:
    begin
      case TokenFlag([TOK_FLAG_OPCODE, TOK_FLAG_DIRECTIVE, TOK_FLAG_PREFIX]) of
        TOK_OPCODE:
        begin
          Inst.Opcode:=tAsmOpcode(TokenInt);
          ParseOperands;
          Inst.SearchTable;
          if Inst.TableRow <> -1 then
          begin
            Inst.DumpCode;

            CheckRelocation(Inst.Op1, INSTR_TABLE[Inst.TableRow].O1);
            CheckRelocation(Inst.Op2, INSTR_TABLE[Inst.TableRow].O2);
            CheckRelocation(Inst.Op3, INSTR_TABLE[Inst.TableRow].O3);

            AddListing(Format('%s: %s %s DUMP = %s'#13#10, [
                CurSeg.Name,
                RtJustify(Hex16(CurSeg.Size), 8),
                RtJustify(InstructionToStr(Inst), 40),
                BytesToHex(Inst.Code.Dump, Inst.Code.Size, ' ')
              ])
            );

            if Inst.Code.Size > 0 Then
              CurSeg.AddBuffer(Inst.Code.Dump, Inst.Code.Size, 1);

            Inst.Reset;
            SkipLine;
          end else
            AddError('unknown instruction');
        end;

        TOK_DIRECTIVE:
          ParseDirective;

        TOK_PREFIX:
        begin
          Case tAsmPrefix(TokenInt) of
            PRE_LOCK..PRE_REPZ:
              Inst.PrefixLockRepeat := tAsmPrefix(TokenInt);

            PRE_SegES..PRE_SegGS:
              Inst.PrefixSegment := tAsmPrefix(TokenInt);
          end;
          ReadToken;
        end;
      else
        ParseIdentifier;
      end;
    end;
  else
    begin
      ReadToken;
      AddError('Special Character ' + TokenStr);
    end;
  end;
end;

procedure TParser.ParseModule;
begin
  ReadToken;
  if TokenFlag([TOK_FLAG_MODULE]) <> TOK_MODULE then
    AddError('Module Expected');

  case tModule(TokenInt) of
    MOD_MODEL:
    begin
      ReadToken;
      { get Model Type }
      ReadToken;
    end;
    MOD_CODE: CurSeg := App.SecCODE;
    MOD_DATA: CurSeg := App.SecDATA;
    MOD_APPTYPE: ParseApptype;
  else
    AddError('');
  end;

  SkipLine;
end;

procedure TParser.ParseOperand(var Arg: TAsmOperand);
type
  TDatSiz = packed record
    StrConst: string;
    SizeConst: tDataSize;
  end;

const
  DATA_SIZES: array[0..4] of TDatSiz = (
    (StrConst: 'BYTE' ; SizeConst: SIZE_BYTE ),
    (StrConst: 'WORD' ; SizeConst: SIZE_WORD ),
    (StrConst: 'DWORD'; SizeConst: SIZE_DWORD),
    (StrConst: 'SHORT'; SizeConst: SIZE_SHORT),
    (StrConst: 'NEAR' ; SizeConst: SIZE_NEAR )
  );

var
  size: tDataSize;
  i: integer;
begin
  size := SIZE_NONE;

  if Token = TOK_IDENTIFIER then
    for i := 0 to 4 do
      if TokenUpStr = DATA_SIZES[i].StrConst then
      begin
        size := DATA_SIZES[i].SizeConst;

        if (ReadToken = TOK_IDENTIFIER) and (TokenUpStr = 'PTR') then
          ReadToken;

        Break;
      end;

  ParseExpression(Arg);

  if size <> SIZE_NONE then
    Arg.size := size;
end;

procedure TParser.ParseOperands;
begin
  NumArg := 0;
  ReadToken;

  if Token = TOK_EOLN Then
    Exit;

  ParseOperand(Inst.Op1);

  if Inst.Op1.Loc <> LOC_NONE then
  begin
    Inc( NumArg );
    if Token = TOK_COMMA then
    begin
      ReadToken;

      ParseOperand(Inst.Op2);

      if Inst.Op2.Loc = LOC_NONE then
        AddError('Argument Syntax Error');

      Inc(NumArg);

      if Token = TOK_COMMA then
      begin
        ReadToken; ParseOperand(Inst.Op3);

        if (Inst.Op3.Loc=LOC_NONE) then
          AddError('Argument Syntax Error');

        Inc( NumArg );
      end;
    end;
  end;
end;

procedure TParser.ParseOrg;
begin
  ReadToken;
  if Token = TOK_INTCONST then
  begin
    if CurSeg.Size > TokenInt then
      AddError('Address Already Used');

    CurSeg.Size := TokenInt;
  end else
    AddError(EXPECT_INT_CONST);
end;

procedure TParser.ParsePublic;
var
  labl: string;
  sym: TSymbol;
begin
  ReadToken;

  if not IsTokenIdent then
    AddError('Identifier expected');

  labl := TokenUpStr;{ Get The Name }
  sym := SymGroup.FindSymbol(labl);
  if sym = nil then
  begin
    sym := TSymbol.Create(SYM_UNDEF, labl, [SYM_FLAG_PUBLIC], nil);
    SymGroup.Head.PushSymbol(sym);
  end else
  if sym.Kind = SYM_LABEL then
    sym.Flag := sym.Flag + [SYM_FLAG_PUBLIC]
  else
    AddError('Error in symbol declaration');
end;

procedure TParser.ReadExpression(var LastOper: TOperator;
  var LastTerm: TAsmOperand);
var
  CurTerm: TAsmOperand;
	CurOper: TOperator;
begin
  ReadFactor(CurTerm);

  CurOper := ReadOperator;

  while Precedence[CurOper] < Precedence[LastOper] do
    ReadExpression(CurOper, CurTerm);

  EvalOperation(LastTerm, CurTerm, LastOper , LastTerm);
  LastOper := CurOper;
end;

procedure TParser.ReadFactor(var Term: TAsmOperand);
var
  Sym: TSymbol;
  Sym2: TSymbol;
  Val: Integer;
begin
  Term.Loc := LOC_NONE;
  Term.RefKind := REF_NONE;

  case Token of
    TOK_INTCONST: SetImmediateOperand(Term, TokenInt);

    TOK_LFRAME:
    begin  // Memory
      ReadToken;
      ParseExpression(Term);
      case Term.Loc of // change any operand location to memory location.
        LOC_IMMEDIATE: SetMemoryOperand(Term, SIZE_NONE, REG_NONE, REG_NONE, 0,
          Term.Value);

        LOC_REGISTER:
        begin
          SetMemoryOperand(Term, SIZE_NONE, Term.Reg, REG_NONE, 0, 0);
          Term.RefKind := REF_NONE;
        end;
        LOC_MEMORY: ; // nothing to do
      end;

      if Token <> TOK_RFRAME then
        AddError(EXPECT_RFRAME);

      // now check for structure type.
      if Readtoken <> TOK_POINT then
        Exit; // no used structure.

      Readtoken;
      if not IsTokenIdent then
        AddError('Identifier Expected but found ' + TokenStr);

      Sym := SymGroup.FindSymbol(TokenUpStr);
      if Sym = nil then
        AddError('Undeclared Structure: ' + TokenStr);

      if (Sym.Kind = SYM_TYPEDEF) and (TDataTypeDef(Sym.Data).Def = DEF_STRUCT) then
      begin // is a structure
        if Readtoken <> TOK_POINT then
          AddError('"." expected');

        ReadToken;
        Sym2 := TDataStruct(Sym.Data).Symbols.FindSymbol(TokenUpStr);
        if Sym2 = nil then
          AddError('Field Expected of the Structure: ' + Sym.Name);

        Term.Offset := Term.Offset + TDataLabel(Sym2.Data).OwnOfs;
      end else
        AddError('Structure Type Expected');
    end;

    TOK_MINUS:
    begin{ Function NEG }
      ReadToken;
      if (Token <> TOK_INTCONST) then
        AddError(EXPECT_INT_CONST);

      SetImmediateOperand(Term, -TokenInt);
    end;

    TOK_EXCLAMATION:
    begin{ Function NOT }
      ReadToken;
      if (Token <> TOK_INTCONST) then
        AddError(EXPECT_INT_CONST);

      SetImmediateOperand(Term, not(TokenInt));
    end;

    TOK_LPAREN:
    begin
      ReadToken;
      ParseExpression(Term);
      if (Token <> TOK_RPAREN) then
        AddError(EXPECT_RPAREN);
    end;

    TOK_IDENTIFIER:
    case TokenFlag([TOK_FLAG_REGISTER, TOK_FLAG_ROUTINE, TOK_FLAG_SPECIFIER]) of
      TOK_REGISTER:
      begin // check for segment register???
        SetRegisterOperand(Term, tAsmRegister(TokenInt));
        if ReadToken <> TOK_COLON then
          Exit;
        case tAsmRegister(TokenInt) of
          REG_ES: Inst.PrefixSegment := PRE_SEGES;
          REG_CS: Inst.PrefixSegment := PRE_SEGCS;
          REG_SS: Inst.PrefixSegment := PRE_SEGSS;
          REG_DS: Inst.PrefixSegment := PRE_SEGDS;
        else
          AddError('Register segment Expected');
        end;
        ReadToken;
        ParseExpression(Term);
        Exit;
      end;

      TOK_SPECIFIER:
      begin
        Val := TokenInt;
        ReadToken;
        if not IsTokenIdent then
          AddError('Expect Symbol after NEAR');

        Sym := SymGroup.FindSymbol(TokenUpStr);
        if (Sym = nil) then
        begin
          Sym := TSymbol.Create(SYM_UNDEF,TokenUpStr,[SYM_FLAG_USED], nil);
          SymGroup.Head.PushSymbol(Sym);
        end else
          Sym.Flag:= Sym.Flag + [SYM_FLAG_USED];

        Term.RefSym := Sym;
        SetImmediateOperand(Term,0);
        case tSizeSpecifier(Val) of
          SZS_NEAR:  Term.RefKind := REF_NEAR;
          SZS_SHORT: Term.RefKind := REF_SHORT;
        end;
      end;
      TOK_ROUTINE:
      case tRoutine(TokenInt) of
        ROUT_ADDR, ROUT_OFFSET:
        begin // Offset <Ref> -> exOFS
          ReadToken;
          if not IsTokenIdent then
            AddError('Expect Symbol');

          Sym := SymGroup.FindSymbol(TokenUpStr);
          if Sym = nil then
          begin
            Sym := TSymbol.Create(SYM_UNDEF, TokenUpStr, [SYM_FLAG_USED], nil);
            SymGroup.Head.PushSymbol(Sym);
          end else
            Sym.Flag:= Sym.Flag + [SYM_FLAG_USED];

          Term.RefSym := Sym;
          Term.RefKind := REF_OFS;
          SetImmediateOperand(Term,0);
        end;

        ROUT_SIZEOF:
        begin // Offset <Ref> -> exOFS
          Term.RefKind := REF_NONE;
          ReadToken;
          if not IsTokenIdent then
            AddError('Expect Symbol');

          Sym := SymGroup.FindSymbol(TokenUpStr);
          if Sym = nil then
            AddError('Undeclared symbol');

          case Sym.Kind of
            SYM_LABEL: SetImmediateOperand(Term, 4);
            SYM_TYPEDEF: SetImmediateOperand(Term, TDataTypeDef(Sym.Data).Size);
          end;
        end;
      end;

      TOK_IDENTIFIER:
      begin{ <Ref> -> exFIXUP (default type) }
        if not IsTokenIdent then
          AddError('Identifier Expected');

        Term.RefKind := REF_FIXUP;
        Sym := SymGroup.FindSymbol(TokenUpStr);
        if Sym = nil then
        begin
          Sym := TSymbol.Create(SYM_UNDEF, TokenUpStr, [SYM_FLAG_USED], nil);
          SymGroup.Head.PushSymbol(Sym);
          SetImmediateOperand(Term, 0);
          Term.RefSym := Sym;
        end else
        begin
          Sym.Flag:= Sym.Flag + [SYM_FLAG_USED];
          case Sym.Kind of
            SYM_LABEL:
            begin
              Sym.Flag := Sym.Flag + [SYM_FLAG_USED];
              SetImmediateOperand(Term,0);
              Term.RefSym := Sym;
              if TDataLabel(Sym.Data).Def = DEF_STRUCT then
                if ReadToken = TOK_POINT then
                begin
                  ReadToken;
                  Sym2 := TDataLabel(Sym.Data).DefSym;
                  Sym := TDataStruct(Sym2.Data).Symbols.FindSymbol(TokenUpStr);

                  if Sym = nil then
                    AddError('Field Struct Expected');

                  Term.Offset := TDataLabel(Sym.Data).OwnOfs;
                end else
                  Exit;
            end;

            SYM_VALUE:
            begin
              SetImmediateOperand(Term, TDataValue(Sym.Data).Value);
              Term.RefKind := REF_NONE;
            end;

            SYM_EQUAL: Move(TDataEqual(Sym.Data).Expr, Term, Sizeof(term));
          else
            begin
              SetImmediateOperand(Term, 0);
              Term.RefSym := Sym;
            end;
          end;
        end;
      end;
    end;
  else
    AddError('Expression Error');
  end;
  ReadToken;
end;

function TParser.ReadOperator: TOperator;
begin
  case Token of
    TOK_EQUAL : Result := coEQUAL;
    TOK_PLUS	: Result := coPLUS ;
    TOK_MINUS	: Result := coMINUS;
    TOK_SLASH	: Result := coDIV  ;
    TOK_STAR	: Result := coMUL  ;
    TOK_CARET	: Result := coEXPO ;
    TOK_IDENTIFIER:
      if TokenFlag([TOK_FLAG_OPERATOR]) = TOK_OPERATOR then
        case tTokOperator(TokenInt) of
          OPER_SHR: Result := coSHR;
          OPER_SHL: Result := coSHL;
          OPER_MOD: Result := coDIV;
          OPER_DIV: Result := coDIV;
          OPER_AND: Result := coAND;
          OPER_OR : Result := coOR ;
          OPER_XOR: Result := coXOR;
        end
      else
        Exit(coNONE);
  else
    Exit(coNONE);
  end;
  ReadToken;
end;

end.


