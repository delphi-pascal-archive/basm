Unit Symbols;

INTERFACE

USES	SysUtils, Assemble;

TYPE { Symbols definition }

  TSymbolName = string; // Max Allocation size of an identifier
  TSymbolHash = Cardinal;

TYPE
 { Here All Symbols definitions that we can find in a source file }
  TSymbolKind =(
      SYM_UNDEF,      // ???
      SYM_PROGRAM,    // Program
      SYM_EQUAL,      // for EQU definitions
      SYM_VALUE,      // for '=' definitions
	    SYM_LABEL,      // Label definitions
      SYM_TYPEDEF     // Type Definition
  );

TYPE
  TDefinitionKind = (
	    DEF_LABEL,  // UNKNOWN definition
	    DEF_NEAR,
	    DEF_FAR,
	    DEF_BYTE,
	    DEF_WORD,
	    DEF_DWORD,
      DEF_STRUCT
  );

CONST
  TDefinitionKindName : Array[TDefinitionKind] of String[6] =(
	    'LABEL',  // UNKNOWN definition
	    'NEAR',
	    'FAR',
	    'BYTE',
	    'WORD',
	    'DWORD',
      'STRUCT'
  );

TYPE
  TSymbolFlag = set of (
 	    SYM_FLAG_EXTERN,  // External Symbol
	    SYM_FLAG_PUBLIC,    // Symbol is public
      SYM_FLAG_IMPORT,
	    SYM_FLAG_DEFINED,   // Symbol is defined in current module
      SYM_FLAG_USED       // Symbol was called or used in an instruction
  );

  TSymbol = class(TObject)  { Symbol Structure Header }
    Next: TSymbol;      // next symbol
    Prev: TSymbol;
    Kind: TSymbolKind;
    Name: TSymbolName;
    Hash: TSymbolHash; { Hash calculated for fastest searching }
    Flag: TSymbolFlag;
    Data: Pointer;

    constructor Create(sKind: TSymbolKind; const sName: string;
      sFlag: TSymbolFlag; aData: Pointer);

    destructor Destroy;
  end;

  // Symbols Data
  TDataLabel = class(TObject)
    Def     : TDefinitionKind;
    DefSym  : TSymbol;
    OwnSeg  : Pointer; // Own Segment
    OwnOfs  : Integer; // Address within Own Segment

    constructor Create(ADef: TDefinitionKind; aDefSym: TSymbol; sSeg: Pointer;
      sOfs: Integer);
  end;

  TDataValue = class(TObject)
    Value : Integer;

    constructor Create(Val: Integer);
  end;

  TDataEqual = class(TObject)
    Expr    : tAsmOperand;

    constructor Create(const Op: tAsmOperand);
  end;

  TDataTypeDef = class(TObject)
    Def     : TDefinitionKind;
    Size: Integer;

    constructor Create(aDef: TDefinitionKind);
  end;

  TSymbolStack = class(TObject)
    Head    : TSymbol;
    Parent  : TSymbolStack;
    procedure PushSymbol(NewSym: TSymbol);
    procedure PopSymbol; // and destroy
    function FindSymbol(Const S:tSymbolName): pointer;
    procedure DelSymbol(Sym:TSymbol);
    procedure ReplaceSym(OldSym, NewSym:TSymbol);

    constructor Create;
    destructor Destroy;
  end;

  // Done only for Fast Search
  TSymbolLength = class(TObject)
  private
    FSymbolStack: array [1..128] of TSymbolStack;
    function GetSymbolStack(const aIndex: Integer): TSymbolStack;
    procedure SetSymbolStack(const aIndex: Integer; const Value: TSymbolStack);
  public
    Parent  : TSymbolLength;

    procedure PushSymbol(NewSym: TSymbol);
    function FindSymbol(const S: tSymbolName): pointer;

    property SymbolStack[const aIndex: Integer]: TSymbolStack read GetSymbolStack write SetSymbolStack;

    constructor Create;
    destructor Destroy;
  end;

  TSymbolGroupItem  =  TSymbolLength;

  TSymbolGroup = class(TObject)
    Head    : TSymbolGroupItem;
    procedure PushItem(NewItem: TSymbolGroupItem);
    procedure PopItem;
    function FindSymbol(Const S:tSymbolName):pointer;

    constructor Create;
    destructor Destroy;
  end;

implementation

{$ifdef Win32}
function CalcHash (const S: ShortString): TSymbolHash; assembler;
asm // in assembler to get more speed
        PUSH    ESI
        LEA     ESI, [S]
        LODSB
        XOR     EDX, EDX
        XOR     ECX, ECX
        MOV     CL , AL
        JECXZ   @Exit
        XOR     EAX, EAX
@Ret:   LODSB
        CMP     AL , 'a'
        JB      @Up
        CMP     AL , 'z'
        JA      @Up
        SUB     AL , 'a'-'A'  // there is no difference between lowercase and uppercase
@Up:    ROL     EDX, 1
        ADD     EDX, ECX
        ADD     EDX, EAX
        LOOP    @Ret
@Exit:  MOV     EAX, EDX
        POP     ESI
end;
{$else}
{$endif}
//int __fastcall sub_426D90(unsigned __int8 *a1)
//{
//  unsigned __int8 *v1; // esi@1
//  unsigned __int8 v2; // al@1
//  int v3; // esi@1
//  int v4; // edx@1
//  int v5; // ecx@1
//  int v6; // eax@2
//  int v7; // edx@6
//
//  v1 = a1;
//  v2 = *a1;
//  v3 = (int)(v1 + 1);
//  v4 = 0;
//  v5 = v2;
//  if ( v2 )
//  {
//    v6 = 0;
//    do
//    {
//      LOBYTE(v6) = *(_BYTE *)v3++;
//      if ( (unsigned __int8)v6 >= 0x61u && (unsigned __int8)v6 <= 0x7Au )
//        LOBYTE(v6) = v6 - 32;
//      v7 = __ROL__(v4, 1);
//      v4 = v6 + v5-- + v7;
//    }
//    while ( v5 );
//  }
//  return v4;
//}
constructor TSymbol.Create(sKind: TSymbolKind; const sName: string;
  sFlag: TSymbolFlag; aData: Pointer);
begin
  Kind := sKind;
  Hash := CalcHash(sName);
  Name := sName;
  Flag := sFlag;
  Data := aData;
end;

procedure TSymbolStack.PushSymbol( NewSym:TSymbol );
begin
  if Head <> nil then
    Head.Prev := NewSym;

  NewSym.Next := Head;
  NewSym.Prev := nil;
  Head := NewSym;
end;

procedure TSymbolStack.PopSymbol;
var
  Item: TSymbol;
begin
  if Head = NIL then
    Exit;

  Item := Head.Next;
  //Head^.SelfDestroy;
  Head.Free;
  Head := Item;
  if Head <> nil then
    Head.Prev := nil;
end;

function TSymbolStack.FindSymbol(Const S:tSymbolName):pointer;
var
  Item: TSymbol;
  Hash: tSymbolHash;
begin
  Item := Head;
  Hash := CalcHash(S);
  while Item <> nil do
  begin
    if Item.Hash = Hash then
      if Item.Name = S then
        Exit(Item);

    Item := Item.Next;
  end;
  Result := nil;
end;

constructor TSymbolStack.Create;
begin
  Head := nil;
end;

procedure TSymbolStack.DelSymbol(Sym:TSymbol);
var
  Prev, Next: TSymbol;
begin
  if Sym = nil then
    Exit;

  Prev := Sym.Prev;
  Next := Sym.Next;
  if Prev <> nil then
    Prev.Next := Next;

  if Next <> nil then
    Next.Prev := Prev;

  if Head = Sym then
    Head := Next;

  FreeAndNil(Sym); //Sym.SelfDestroy;
end;

destructor TSymbolStack.Destroy;
begin
  while Head <> nil do
    PopSymbol;
end;

procedure TSymbolStack.ReplaceSym(OldSym, NewSym: TSymbol);
var
  Prev, Next: TSymbol;
begin
  if (OldSym = nil) or (NewSym = nil) then
    Exit;

  Prev := OldSym.Prev;
  Next := OldSym.Next;
  NewSym.Prev := Prev;
  NewSym.Next := Next;

  if Prev <> NIL then
    Prev.Next := NewSym;

  if Next <> NIL then
    Next.Prev := NewSym;

  if Head = OldSym then
    Head := NewSym;

  OldSym.Free; //OldSym.SelfDestroy;
end;

constructor TSymbolLength.Create;
var
  Len: Integer;
begin
  for Len := 1 to 128 do
    SymbolStack[Len] := TSymbolStack.Create;
end;

destructor TSymbolLength.Destroy;
var
  Len: Integer;
begin
  for Len := 1 to 128 do
    FreeAndNil(FSymbolStack[Len]);
end;

procedure TSymbolLength.PushSymbol( NewSym:TSymbol );
var
  Len: Integer;
begin
  Len := Length(NewSym.Name);

  if SymbolStack[Len] = nil then
    SymbolStack[Len] := TSymbolStack.Create;

  SymbolStack[Len].PushSymbol(NewSym);
end;

procedure TSymbolLength.SetSymbolStack(const aIndex: Integer;
  const Value: TSymbolStack);
begin
  FSymbolStack[aIndex] := Value;
end;

function TSymbolLength.FindSymbol(const S: TSymbolName): pointer;
var
  Len: Integer;
begin
  Len := Length(S);
  if SymbolStack[Len] = nil then
    Exit(nil);
  Result := SymbolStack[Len].FindSymbol(S);
end;

function TSymbolLength.GetSymbolStack(const aIndex: Integer): TSymbolStack;
begin
  Result := FSymbolStack[aIndex];
end;

constructor TSymbolGroup.Create;
begin
  Head := nil;
end;

destructor TSymbolGroup.Destroy;
begin
  while Head <> nil do
    PopItem;
end;

procedure TSymbolGroup.PushItem(NewItem: TSymbolGroupItem);
begin
  NewItem.Parent := Head;
  Head := NewItem;
end;

procedure TSymbolGroup.PopItem;
 Var    Item : TSymbolGroupItem;
begin
  if Head = nil then
    Exit;

  Item := Head.Parent;
  //delete(head)
  Head := Item;
end;

function TSymbolGroup.FindSymbol(const S: TSymbolName): Pointer;
var
  Item  : TSymbolGroupItem;
  Sym   : TSymbol;
begin
  Item := Head;
  while Item <> nil do
  begin
    Sym:=Item.FindSymbol(S);
    if Sym <> nil then
      Exit(Sym);
    Item := Item.Parent;
  end;
  Result := nil;
end;

{ TDataLabel }

constructor TDataLabel.Create(ADef: TDefinitionKind; aDefSym: TSymbol;
  sSeg: Pointer; sOfs: Integer);
begin
  Def :=ADef;
  DefSym := aDefSym;
  OwnSeg := sSeg;
  OwnOfs := sOfs;
end;

{ TDataValue }

constructor TDataValue.Create(Val: Integer);
begin
  Value := Val;
end;

{ TDataEqual }

constructor TDataEqual.Create(const Op: tAsmOperand);
begin
  //Move(Op, Expr, Sizeof(tAsmOperand) );
  Expr := Op;
end;

{ TDataTypeDef }

constructor TDataTypeDef.Create(aDef: TDefinitionKind);
begin
  Def := aDef;
end;

destructor TSymbol.Destroy;
begin
  FreeAndNil(TObject(Data));
end;

end.
