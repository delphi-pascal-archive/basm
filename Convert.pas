UNIT Convert;

INTERFACE

{$H+}
FUNCTION Bin2Hex(Const b: array of byte; n: integer; Spacer: String): String;
procedure Hex2Bin(Const s: String; var n: integer; var tab: Array of byte);
{$H-}
{ Integer to Hexadecimal Convertion }
FUNCTION Hex8(n: byte): String;
FUNCTION Hex16(n: Word): String;
FUNCTION Hex32(n: LongInt): String;
FUNCTION Hex(n: LongInt): String;

{ Integer to Binary Convertion }
FUNCTION Bin8(n: byte): String;
FUNCTION Bin16(n: Word): String;
FUNCTION Bin32(n: LongInt): String;

FUNCTION Hex2Long(Const h: String): LongInt;
FUNCTION Dec2Long(Const h: String): LongInt;
FUNCTION Bin2Long(Const h: String): LongInt;
FUNCTION Oct2Long(Const h: String): LongInt;

{ Hex <--> Array of byte }
function BytesToHex(Const b: Array of byte; Len: integer;
  Const Spacer: String): String;
procedure HexToBytes(Str: String; Var tab: Array of byte; Var Len: integer);

{ String <--> Array of Byte }
procedure StrToBytes(Const Str: String; Var tab: Array of byte;
  Var Len: integer);
function BytesToStr(Const b: Array of byte; Len: integer): String;

IMPLEMENTATION

Const
  HexChars: Array [0 .. 15] OF char = '0123456789ABCDEF';

FUNCTION Hex8(n: byte): String;
Begin
  SetLength(Result, 2);
  Result[1] := HexChars[n shr 4];
  Result[2] := HexChars[n and $F];
End;

FUNCTION Hex16(n: Word): String;
Begin
  SetLength(Result, 4);
  Result[1] := HexChars[(n shr 12)];
  Result[2] := HexChars[(n shr 8) and $F];
  Result[3] := HexChars[(n shr 4) and $F];
  Result[4] := HexChars[(n) and $F];
End;

FUNCTION Hex32(n: LongInt): String;
Begin
  Hex32 := Hex16((n shr 16) and $FFFF) + Hex16(n and $FFFF);
End;

FUNCTION Hex(n: LongInt): String;
Var
  Shift, a: byte;
  Rt: String;
Begin
  Shift := 32 - 4;
  Rt := '';
  Repeat
    a := (n shr Shift) and $F;
    if (Rt <> '') or (a <> 0) Then
      Rt := Rt + HexChars[a];
    Dec(Shift, 4);
  Until Shift = 0;
  Rt := Rt + HexChars[n and $F];
  Hex := Rt;
End;

{$H+}

FUNCTION Bin2Hex(Const b: array of byte; n: integer; Spacer: String): String;
var
  i: integer;
  s: string;
begin
  s := '';
  for i := 0 to (n - 1) do
  begin
    if s <> '' then
      s := s + Spacer;
    s := s + Hex8(b[i]);
  End;
  Result := s;
end;

procedure Hex2Bin(Const s: String; var n: integer; var tab: Array of byte);

  function Hex2Dec(c: char): byte;
  begin
    Case c of
      '0' .. '9':
        Result := ord(c) - ord('0');
      'a' .. 'f':
        Result := ord(c) - ord('a') + 10;
      'A' .. 'F':
        Result := ord(c) - ord('A') + 10;
    else
      Result := 0;
    End;
  end;

Var
  i: byte;
Begin
  i := 1;
  n := 0;
  while (i <= length(s)) do
  begin
    if (s[i] in ['0' .. '9', 'a' .. 'z', 'A' .. 'Z']) then
    begin
      if (s[i + 1] in ['0' .. '9', 'a' .. 'z', 'A' .. 'Z']) then
      begin
        tab[n] := Hex2Dec(s[i]) * $10 + Hex2Dec(s[i + 1]);
        inc(i, 2);
        inc(n);
      end
      else
      begin
        tab[n] := Hex2Dec(s[i]);
        inc(i);
        inc(n);
      end;
    end
    else
      inc(i);
  end;
End;

{$H-}

FUNCTION Bin8(n: byte): String;
Var
  s: String;
  i: byte;
Begin
  s := '';
  For i := 7 downto 0 do
    s := s + Chr(ord('0') + ((n shr i) and 1));
  Bin8 := s;
End;

FUNCTION Bin16(n: Word): String;
Begin
  Bin16 := Bin8(n shr 8) + Bin8(n and $0F);
End;

FUNCTION Bin32(n: LongInt): String;
Begin
  Bin32 := Bin16(n shr 16) + Bin16(n and $FF);
End;

{ ########################################################################### }
{ ########################################################################## }
FUNCTION Hex2Long(Const h: String): LongInt;
Var
  Rt: LongInt;
  i: byte;
Begin
  Rt := 0;
  For i := 1 to length(h) do
    Case h[i] of
      '0' .. '9':
        Rt := Rt shl 4 + ord(h[i]) - ord('0');
      'a' .. 'f':
        Rt := Rt shl 4 + ord(h[i]) - ord('a') + 10;
      'A' .. 'F':
        Rt := Rt shl 4 + ord(h[i]) - ord('A') + 10;
    else
      begin
        Result := -1;
        Exit;
      End;
    End;
  Result := Rt;
End;

FUNCTION Dec2Long(Const h: String): LongInt;
Var
  Rt: LongInt;
  Count: byte;
  Signed: Boolean;
Begin
  Rt := 0;
  Count := 1;
  Signed := False;
  While Count <= length(h) do
  Begin
    Case h[Count] of
      '0' .. '9':
        Rt := Rt * 10 + ord(h[Count]) - ord('0');
      '-':
        if Count = 1 Then
          Signed := True;
    End;
    inc(Count);
  End;
  if Signed Then
    Rt := -Rt;
  Dec2Long := Rt;
End;

FUNCTION Bin2Long(Const h: String): LongInt;
Var
  Rt: LongInt;
  i: byte;
Begin
  Rt := 0;
  For i := 1 to length(h) do
    if h[i] in ['0', '1'] Then
      Rt := Rt * 2 + ord(h[i]) - ord('0');
  Bin2Long := Rt;
End;

FUNCTION Oct2Long(Const h: String): LongInt;
{ Converts an octal string to longint }
Var
  Rt: LongInt;
  i: byte;
Begin
  Rt := 0;
  For i := 1 to length(h) do
    if h[i] in ['0' .. '7'] then
      Rt := (Rt shl 3) + (byte(h[i]) - byte('0'));
  Oct2Long := Rt;
End;

function BytesToHex(Const b: Array of byte; Len: integer;
  Const Spacer: String): String;
Var
  s: String;
  i: integer;
Begin
  if Len > 0 then
  begin
    s := Hex8(b[0]);
    for i := 1 to (Len - 1) do
      s := s + Spacer + Hex8(b[i]);
  end
  else
    s := '';
  Result := s;
end;

PROCEDURE HexToBytes(Str: String; Var tab: Array of byte; Var Len: integer);
Var
  s: String[4];
  Index: integer;
Begin
  Len := 0;
  Index := 1;
  while Index <= length(Str) do
  begin
    Case Str[Index] of
      '0' .. '9', 'A' .. 'F', 'a' .. 'f':
        begin
          s := Str[Index];
          if (Str[Index + 1] in ['0' .. '9', 'A' .. 'F', 'a' .. 'f']) then
          begin
            inc(Index);
            s := s + Str[Index];
          end;
          tab[Len] := Hex2Long(s);
          inc(Len);
        end;
      ' ', #9, #13, #10:
        ;
    else
      Break;
    end;
    inc(Index);
  end;
end;

procedure StrToBytes(Const Str: String; Var tab: Array of byte;
  Var Len: integer);
begin
  Len := length(Str);
  Move(Str[1], tab, Len);
end;

function BytesToStr(Const b: Array of byte; Len: integer): String;
Var
  s: String;
begin
  SetLength(s, Len);
  Move(b, s[1], Len);
end;

END.
