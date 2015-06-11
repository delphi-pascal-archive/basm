unit AsmToStr;

interface

uses
  Convert, ObjCodeDump, Symbols, Assemble;

function OperandToStr(Size: tDataSize; const Op: tAsmOperand): string;
function InstructionToStr(const Inst: tAsmInstruction): string;

implementation

function Typecast(Size: TDataSize): string;
begin
  Result := '';
  case Size of
    SIZE_BYTE:
      Result := 'Byte Ptr ';
    SIZE_WORD:
      Result := 'Word Ptr ';
    SIZE_DWORD:
      Result := 'Dword Ptr ';
  end;
end;

function jmptype(Size: tDataSize): string;
begin
  jmptype := '';
  case Size of
    SIZE_BYTE:
      jmptype := 'Short Ptr ';
    SIZE_WORD:
      jmptype := 'Near Ptr ';
    SIZE_DWORD:
      jmptype := 'Far Ptr ';
  end;
end;

function RefToStr(Ref: tRefKind): string;
begin
  case Ref of
    REF_NONE, REF_FIXUP:
      RefToStr := '';
    REF_OFS:
      RefToStr := 'Offset ';
    REF_NEAR:
      RefToStr := 'Near ';
    REF_SHORT:
      RefToStr := 'Short ';
    REF_FAR:
      RefToStr := 'Far ';
  end;
end;

function OperandToStr(Size: tDataSize; const Op: tAsmOperand): string;
var
  s: string;
  First: Boolean;
begin
  s := '';
  Case Op.Loc Of
    LOC_REGISTER:
      OperandToStr := AsmRegisterName[Op.Reg];

    LOC_IMMEDIATE:
      begin
        if Op.RefKind <> REF_NONE then
        begin
          s := RefToStr(Op.RefKind) + TSymbol(Op.RefSym).Name;
          if Op.Value <> 0 then
            s := s + '+ 0' + Hex(Op.Value) + 'h';
        end
        else
          s := '0' + Hex(Op.Value) + 'h';
        OperandToStr := s;
      end;

    LOC_MEMORY:
      begin // [ BASE + INDEX + Disp ]
        s := Typecast(Size) + '[';
        First := True;
        if (Op.Base <> REG_NONE) Then
        begin
          s := s + AsmRegisterName[Op.Base];
          First := False;
        end;
        if (Op.Index <> REG_NONE) Then
        begin
          if Not First then
            s := s + '+' + AsmRegisterName[Op.Index]
          else
            s := s + AsmRegisterName[Op.Index];
          if (Op.Scale <> 0) Then
            s := s + '*' + Hex8(Op.Scale);
          First := False;
        end;
        if Op.RefKind <> REF_NONE then
        begin
          if Not First then
            s := s + '+';
          s := s + RefToStr(Op.RefKind) + TSymbol(Op.RefSym).Name;
          First := False;
        end;
        if (Op.Value <> 0) or First Then
        begin
          if Not First then
            s := s + '+';
          s := s + '0' + Hex(Op.Value) + 'h';
          // First:=False;
        end;
        OperandToStr := s + ']';
      end;
  else
    OperandToStr := '';
  end;
end;

function InstructionToStr(const Inst: tAsmInstruction): string;
var
  s: string;
Begin
  Result := '';
  with Inst do
  begin
    if OpCode = OP_NONE Then
      Exit;
    s := ''; // Justify(Dump, 12);
    if PrefixLockRepeat <> PRE_NONE then
      s := s + ASM_PREFIX_NAME[PrefixLockRepeat] + ' ';

    s := s + ASM_OPCODE_NAME[OpCode];
    if Op1.Loc <> LOC_NONE then
      s := s + ' ' + OperandToStr(DataSize, Op1);

    if Op2.Loc <> LOC_NONE then
      s := s + ', ' + OperandToStr(DataSize, Op2);

    if Op3.Loc <> LOC_NONE then
      s := s + ', ' + OperandToStr(DataSize, Op3);
  end;
  Result := s;
end;

end.
