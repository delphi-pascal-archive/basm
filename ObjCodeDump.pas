unit ObjCodeDump;

interface

type
  TCodeDump = class(TObject)
  public
    Size: Integer;
    Dump: array [0..31] of Byte;

    Procedure AddByte(const aValue: Byte);
    PROCEDURE AddWord(const aValue: Word);
    procedure AddDword(const aValue: Integer);
    procedure AddBuffer(const aBuf; const aCount: Integer);
    procedure AddCodeDump(const aCodeDump: TCodeDump);

    constructor Create;
  end;

implementation

constructor TCodeDump.Create;
begin
  Size := 0;
end;

procedure TCodeDump.AddByte(const aValue: Byte);
begin
  Move(aValue, Dump[Size], 1);
  Inc(Size, 1);
end;

procedure TCodeDump.AddWord(const aValue: Word);
begin
  Move(aValue, Dump[Size], 2);
  Inc(Size, 2);
end;

procedure TCodeDump.AddDword(const aValue: Integer);
begin
  Move(aValue, Dump[Size], 4);
  Inc(Size, 4);
end;

procedure TCodeDump.AddBuffer(const aBuf; const aCount: Integer);
begin
  Move(aBuf, Dump[Size], aCount);
  Inc(Size, aCount);
end;

procedure TCodeDump.AddCodeDump(const aCodeDump: TCodeDump);
begin
  Move(aCodeDump.Dump[0], Dump[Size], aCodeDump.Size);
  Inc(Size, aCodeDump.Size);
end;

end.
