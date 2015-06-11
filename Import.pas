Unit Import;

interface

uses
  Symbols, Symbols2;

TYPE
  TImportItem = class(TSymbol)
    Index: Integer;
    Jmp_Sym: TSymbol;
    constructor Create(const aName: string; aIndex: Integer; aSym: TSymbol);
    function IsCalled: Boolean;
  end;

  TLibraryItem = class(TObject)
    Head: TImportItem;
    Next: TLibraryItem;
    Name: TSymbolName;

    procedure PushImport(Const aName: String; aIndex: Integer; aSym: TSymbol);
    procedure PopImport;
    procedure PopAll;
    function FindImport(Const S: String): TImportItem;
    function CountCalledImports: Integer;
    function CountImports: Integer;
    function IsCalled: Boolean;

    constructor Create(Const aName: String);
    destructor Destroy;
  end;

  TLibraryStack = class(TObject)
    Head: TLibraryItem;
    procedure PushLib(Const aName: String);
    procedure PopLib;
    procedure PopAll;
    function FindLib(Const S: String): TLibraryItem;
    function CountCalledLibs: Integer;
    function CountLibs: Integer;
    function CountCalledImports: Integer;
    function CountImports: Integer;

    constructor Create;
    destructor Destroy;
  end;

implementation

/// ////////////////////////////////////////////////////////////////////////////
//
/// ////////////////////////////////////////////////////////////////////////////

constructor TImportItem.Create(const aName: string; aIndex: Integer;
  aSym: TSymbol);
begin
  inherited Create(SYM_LABEL, aName, [SYM_FLAG_IMPORT], nil);
  Next := nil;
  Index := aIndex;
  Jmp_Sym := aSym;
end;

// Constructor TImportItem.Init(Const AName:String; AIndex:Integer; ASym:TSymbol);
// begin
// Inherited Init(SYM_LABEL, AName, [SYM_FLAG_IMPORT],NIL);
// Next:=NIL;
// Index:=AIndex;
// Jmp_Sym:=ASym;
// end;

function TImportItem.IsCalled: Boolean;
begin
  if SYM_FLAG_USED in Jmp_Sym.Flag then
    Result := True
  else
    Result := False;
end;

/// ////////////////////////////////////////////////////////////////////////////
// TLibraryItem
/// ////////////////////////////////////////////////////////////////////////////

constructor TLibraryItem.Create(Const aName: String);
begin
  Head := NIL;
  Next := NIL;
  Name := aName;
end;

procedure TLibraryItem.PushImport(Const aName: String; aIndex: Integer;
  aSym: TSymbol);
var
  NewItem: TImportItem;
begin
  // New( NewItem, Init(AName, AIndex, ASym) );
  NewItem := TImportItem.Create(aName, aIndex, aSym);
  NewItem.Next := Head;
  Head := NewItem;
end;

procedure TLibraryItem.PopImport;
Var
  Item: TImportItem;
begin
  if Head = nil then
    Exit;

  Item := TImportItem(Head.Next);
  Head.Free; // Dispose( Head );
  Head := Item;
end;

procedure TLibraryItem.PopAll;
begin
  while (Head <> NIL) do
    PopImport;
end;

function TLibraryItem.FindImport(Const S: String): TImportItem;
var
  Item: TImportItem;
begin
  Item := Head;
  while Item <> nil do
  begin
    if (Item.Name = S) then
      Exit(Item);

    Item := TImportItem(Item.Next);
  end;
  Result := nil;
end;

function TLibraryItem.CountCalledImports: Integer;
var
  Item: TImportItem;
  Num: Integer;
begin
  Item := Head;
  Num := 0;
  while Item <> nil do
  begin
    if Item.IsCalled then
      Inc(Num);

    Item := TImportItem(Item.Next);
  end;
  Result := Num;
end;

function TLibraryItem.CountImports: Integer;
var
  Item: TImportItem;
  Num: Integer;
begin
  Item := Head;
  Num := 0;
  while Item <> nil do
  begin
    Inc(Num);
    Item := TImportItem(Item.Next);
  end;
  Result := Num;
end;

destructor TLibraryItem.Destroy;
begin
  PopAll;
end;

function TLibraryItem.IsCalled: Boolean;
var
  Item: TImportItem;
begin
  Item := Head;
  while Item <> nil do
  begin
    if Item.IsCalled then
      Exit(True);

    Item := TImportItem(Item.Next);
  end;
  Result := False;
end;

/// ////////////////////////////////////////////////////////////////////////////
// TLibraryStack
/// ////////////////////////////////////////////////////////////////////////////

procedure TLibraryStack.PushLib(Const aName: String);
var
  NewItem: TLibraryItem;
begin
  NewItem := TLibraryItem.Create(aName);
  NewItem.Next := Head;
  Head := NewItem;
end;

procedure TLibraryStack.PopLib;
var
  Item: TLibraryItem;
begin
  if Head = NIL then
    Exit;

  Item := Head.Next;
  Head.Free;
  Head := Item;
end;

procedure TLibraryStack.PopAll;
begin
  while (Head <> NIL) do
    PopLib;
end;

function TLibraryStack.FindLib(Const S: String): TLibraryItem;
var
  Item: TLibraryItem;
begin
  Result := nil;
  Item := Head;
  while Item <> nil do
  begin
    if Item.Name = S then
      Exit(Item);

    Item := Item.Next;
  end;
end;

function TLibraryStack.CountCalledLibs: Integer;
var
  Item: TLibraryItem;
  Num: Integer;
begin
  Item := Head;
  Num := 0;
  while Item <> nil do
  begin
    if Item.IsCalled then
      Inc(Num);

    Item := Item.Next;
  end;
  Result := Num;
end;

function TLibraryStack.CountLibs: Integer;
var
  Item: TLibraryItem;
  Num: Integer;
begin
  Item := Head;
  Num := 0;
  while Item <> nil do
  begin
    Inc(Num);
    Item := Item.Next;
  end;
  Result := Num;
end;

constructor TLibraryStack.Create;
begin
  Head := nil;
end;

destructor TLibraryStack.Destroy;
begin
  PopAll;
end;

function TLibraryStack.CountCalledImports: Integer;
var
  Item: TLibraryItem;
  Num: Integer;
begin
  Item := Head;
  Num := 0;
  while Item <> nil do
  begin
    Inc(Num, Item.CountCalledImports);
    Item := Item.Next;
  end;
  Result := Num;
end;

function TLibraryStack.CountImports: Integer;
var
  Item: TLibraryItem;
  Num: Integer;
begin
  Item := Head;
  Num := 0;
  while Item <> nil do
  begin
    Inc(Num, Item.CountImports);
    Item := Item.Next;
  end;
  Result := Num;
end;

end.
