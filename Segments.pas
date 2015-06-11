unit Segments;

interface

uses
  Symbols, RelocationStack, PEHeader;

type
  TSegmentKind = (
    SEG_NONE,
    SEG_CODE,
    SEG_IDATA,
    SEG_CONST,
    SEG_BSS,
    SEG_DATA,
    SEG_RES
  );

type
  TSegment = class(TObject)
  private
    procedure AllocData(NewSize:Integer);
  public

    Data    : Array of Byte;
    Size    : Longint;

    Kind    : TSegmentKind;
    Name    : String; { Segment name }
    Relocs  : TRelocationStack;  // Own Relocations
  // Address in the memory
    VirtualAddress : Integer;

    procedure Align( a:Integer);

    procedure SetFixups;
    procedure SetLocalFixups;

    procedure AddBuffer(Const Buf; BufSize,Times:Integer);
    procedure AddString(Const S:String);
    procedure AddByte( b:byte; Times:Integer);
    procedure AddWord( w:Word; Times:Integer);
    procedure AddDword( d:Cardinal; Times:Integer);
    procedure SetBuffer(Loc:Integer;Const Buf; BufSize,Times:Integer);
    procedure SetString(Loc:Integer;Const S:String;Times:Integer);
    procedure SetByte(Loc:Integer; B:Byte; Times:Integer);
    procedure SetWord(Loc:Integer; W:Word; Times:Integer);
    procedure SetDword(Loc:Integer; D:Cardinal; Times:Integer);

    constructor Create(AKind:TSegmentKind; AName:TSymbolName);
    destructor Destroy;
  end;

implementation

procedure TSegment.AllocData(NewSize:Integer);
var
  NewData: array of Byte;
begin
  //NewSize:= ((NewSize div Delta)+1)*Delta;
  SetLength(NewData, NewSize); //Delta );
  Move( Data[0], NewData[0], Size);
  SetLength(Data,0);
  Data:=Pointer(NewData);
End;

procedure TSegment.AddBuffer(Const Buf; BufSize,Times:Integer);
 Var
      NewSize : Integer;
      i       : Integer;
Begin { add a buffer at the end of the section }
  NewSize:=Size+(BufSize*Times);
  if NewSize>Length(Data) then AllocData(NewSize);
  for i:=1 To Times Do begin
    Move(Buf,Data[Size], BufSize);
    Inc(Size, BufSize);
  end;
End;

procedure TSegment.AddString(Const S:String);
var
  _s: AnsiString;
Begin { Add a string at the end of the section }
  _s := s;
 AddBuffer(_S[1],Length(S), 1);
End;

procedure TSegment.AddByte( b:byte; Times:Integer);
Begin
 AddBuffer(B,1, Times);
End;

procedure TSegment.AddWord( w:Word; Times:Integer);
Begin
 AddBuffer(W,2, Times);
End;

procedure TSegment.AddDword( d:Cardinal; Times:Integer);
Begin
 AddBuffer(D,4, Times);
End;

procedure TSegment.SetBuffer(Loc:Integer;Const Buf; BufSize,Times:Integer);
 Var
      i       : Integer;
      NewSize : Integer;
Begin
  NewSize:=Loc+(BufSize*Times);
  if NewSize>Length(Data) then AllocData(NewSize);
  for i:=1 To Times Do begin
    Move(Buf,Data[Loc], BufSize);
    Inc(Loc, BufSize);
  end;
  if NewSize>Size then Size:=NewSize;
End;

procedure TSegment.SetString(Loc:Integer;Const S:String;Times:Integer);
Begin { set a string at the location within the section }
  SetBuffer(Loc, s[1], Length(S), Times);
End;

procedure TSegment.SetByte(Loc:Integer; B:Byte; Times:Integer);
Begin { set a string at the location within the section }
  SetBuffer(Loc, B, 1, Times);
End;
procedure TSegment.SetWord(Loc:Integer; W:Word; Times:Integer);
Begin { set a string at the location within the section }
  SetBuffer(Loc, W, 2, Times);
End;
procedure TSegment.SetDword(Loc:Integer; D:Cardinal; Times:Integer);
Begin { set a string at the location within the section }
  SetBuffer(Loc, D, 4, Times);
End;

constructor TSegment.Create(AKind:TSegmentKind; AName:TSymbolName);
begin
 SetLength(Data,0);
 Size:=0;

 Kind:=AKind;
 Name:=AName;
 Relocs := TRelocationStack.Create;
 VirtualAddress:=0;
end;

destructor TSegment.Destroy;
begin
 Kind:=SEG_NONE;
 Name:='';
 Relocs.Free;
 if (Data<>NIL) Then SetLength(Data,0);
 Data:=NIL;
 Size:=0;
end;

procedure TSegment.Align( a:Integer);
 Var i:Integer;
begin
  i := a - ( Size mod a );
  AddByte( 0,i );
end;

procedure TSegment.SetFixups;
var
  Sym     : TSymbol;
  Sec     : TSegment;
  Item    : pRelocationItem;
  Address : Integer;
  Diff    : Integer;
  SymData : TDataLabel;
begin
  Item := Relocs.Head;
  while Item <> nil do
  begin
    Sym := Item^.Sym;
    if (Sym.Kind = SYM_LABEL) and (SYM_FLAG_DEFINED in Sym.Flag) then
    begin
      SymData := Sym.Data;
      Sec := SymData.OwnSeg;
      case Item^.Kind of
        REL_OFFSET:
        begin
          Address:= Sec.VirtualAddress + SymData.OwnOfs + Item^.Shift;
          SetDword(Item^.PatchOfs ,Address, 1);
        end;
        REL_SHORT:
        begin
          Diff:= (Sec.VirtualAddress+SymData.OwnOfs+Item^.Shift) - (VirtualAddress+Item^.PatchOfs+1); // Destination - Source
          //if (n>127)or(n<-128) Then MakeError('Short Jump Size to '+Sym.Name+' at '+hex16(Item^.PatchOfs));
          SetByte(Item^.PatchOfs,Diff,1);
        end;
        REL_NEAR:
        begin
          Diff:= (Sec.VirtualAddress+SymData.OwnOfs+Item^.Shift) - (VirtualAddress+Item^.PatchOfs+4); // Destination - Source
          // if (N<-32768)OR(N>32767) Then MakeError('Invalid Near Jump to Target Label '+Sym.Name);
          SetDword(Item^.PatchOfs,Diff,1);
        end;
      End;{Case}
      Relocs.DelReloc( Item );
    end else
      Item:=Item^.Next;
  end;
end;

procedure TSegment.SetLocalFixups;
var
  Sym     : TSymbol;
  SymData : TDataLabel;
  Item    : pRelocationItem;
  Diff    : Integer;
  SelfSeg : TSegment;
Begin
  SelfSeg := Self;
  Item := Relocs.Head;
  while Item <> NIL do
  begin
    Sym:=Item^.Sym;
    SymData:=Sym.Data;
    if (Item^.Kind <> REL_OFFSET) and
       (Sym.Kind = SYM_LABEL) and
       (SYM_FLAG_DEFINED in Sym.Flag) and
       (SymData.OwnSeg = SelfSeg) then
    begin
      case Item^.Kind of
        REL_SHORT:
        begin
          Diff:= (SymData.OwnOfs+Item^.Shift) - (Item^.PatchOfs+1); // Destination - Source
          //if (n>127)or(n<-128) Then MakeError('Short Jump Size to '+Sym.Name+' at '+hex16(Item^.PatchOfs));
          SetByte(Item^.PatchOfs,Diff,1);
        end;

        REL_NEAR:
        begin
          Diff:= (SymData.OwnOfs+Item^.Shift) - (Item^.PatchOfs+4); // Destination - Source
          // if (N<-32768)OR(N>32767) Then MakeError('Invalid Near Jump to Target Label '+Sym.Name);
          SetDword(Item^.PatchOfs,Diff,1);
        end;
      end;{case}
      // delete fixup
      Relocs.DelReloc(Item);
    end else
      Item:=Item^.Next;
  end; {while}
END;

end.

