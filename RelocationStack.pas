Unit RelocationStack;

INTERFACE

TYPE
  TRelocKind = (REL_NONE, REL_OFFSET, REL_SHORT, REL_NEAR, REL_FAR);

  { * Relocation entries * }
  PRelocationItem = ^TRelocationItem;

  TRelocationItem = packed record
    Next: PRelocationItem;
    Prev: PRelocationItem;
    Kind: TRelocKind;
    OwnSec: Pointer; // symbol own section
    Sym: Pointer; // Symbols that doesn't have yet an offset
    PatchOfs: Integer; // Offset within the target segment
    Shift: LongInt; // Effective Address Adjuster of the result value
  End;

  TRelocationStack = class(TObject)
    Head: PRelocationItem;
    procedure PushReloc(aKind: TRelocKind; aSym: Pointer;
      aPatchOfs, AShift: Integer);
    procedure PopReloc;
    procedure DelReloc(var aRel: PRelocationItem);

    constructor Create;
    destructor Destroy;
  end;

implementation

destructor TRelocationStack.Destroy;
begin
  while Head <> nil do
    PopReloc;
end;

procedure TRelocationStack.PushReloc(aKind: TRelocKind; aSym: Pointer;
  aPatchOfs, AShift: Integer);
var
  NewItem: PRelocationItem;
begin
  GetMem(NewItem, Sizeof(TRelocationItem));
  with NewItem^ do
  begin
    Kind := aKind;
    Sym := aSym;
    PatchOfs := aPatchOfs;
    Shift := AShift;
  end;

  if Head <> nil then
    Head^.Prev := NewItem;

  NewItem^.Next := Head;
  NewItem^.Prev := nil;
  Head := NewItem;
end;

procedure TRelocationStack.PopReloc;
var
  Item: PRelocationItem;
begin
  if Head = nil then
    Exit;

  Item := Head.Next;
  FreeMem(Head, Sizeof(TRelocationItem));
  Head := Item;
  if Head <> nil then
    Head.Prev := nil;
end;

constructor TRelocationStack.Create;
begin
  Head := nil;
end;

procedure TRelocationStack.DelReloc(var aRel: PRelocationItem);
var
  Prev, Next: PRelocationItem;
begin
  if aRel = nil then
    Exit;

  Prev := aRel.Prev;
  Next := aRel.Next;

  if Prev <> nil then
    Prev.Next := Next;

  if Next <> nil then
    Next.Prev := Prev;

  if Head = aRel then
    Head := Next;

  FreeMem(aRel, Sizeof(TRelocationItem));
  aRel := Next;
end;

end.
