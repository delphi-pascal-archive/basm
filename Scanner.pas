unit Scanner;

interface

uses
  SysUtils, Convert, Assemble, classes;

type
  TRoutine = (
    ROUT_ADDR,     // Addr <Sym Label> ==> return the offset of the symbol
    ROUT_OFFSET,   // Offset <Sym Label> ==> return the offset of the symbol
    ROUT_SIZEOF    // Sizeof <Sym TypeDef> ==> return the size of the symbol
  );

  TDirective = (
    DIR_DB,
    DIR_DW,
    DIR_DD,
    DIR_ORG,
    DIR_END,
    DIR_INCLUDE,
    DIR_EXTERN,
    DIR_PUBLIC,
    DIR_IMPORT,
    DIR_ALIGN
  );

  TModule = (
      MOD_MODEL,
      MOD_CODE,
      MOD_DATA,
      MOD_APPTYPE
  );

  TSizeSpecifier = (
    SZS_BYTE,
    SZS_WORD,
    SZS_DWORD,
    SZS_NEAR,
    SZS_SHORT
  );

  TTokOperator = (
      OPER_SHR,
      OPER_SHL,
      OPER_MOD,
      OPER_DIV,
      OPER_AND,
      OPER_OR,
      OPER_XOR
  );

  TAsmToken = (
    TOK_UNKNOWN, TOK_EOF, TOK_EOLN,
    { constants }
    TOK_REALCONST, TOK_STRCONST, TOK_INTCONST,
    { special characters }
    TOK_POINT, TOK_COMMA, TOK_COLON, TOK_DOLLAR,
    TOK_RPAREN,TOK_LPAREN, TOK_RFRAME, TOK_LFRAME,
    { operators }
    TOK_EQUAL, TOK_PLUS, TOK_MINUS, TOK_STAR, TOK_SLASH,
    TOK_CARET, TOK_EXCLAMATION,
    { symbols }
    TOK_IDENTIFIER, TOK_PREFIX, TOK_OPCODE,
    TOK_REGISTER, TOK_ROUTINE, TOK_DIRECTIVE, TOK_MODULE,
    TOK_SPECIFIER, TOK_OPERATOR
  );

  TTokenFlag = (
    TOK_FLAG_PREFIX,
    TOK_FLAG_OPCODE,
    TOK_FLAG_REGISTER,
    TOK_FLAG_ROUTINE,
    TOK_FLAG_DIRECTIVE,
    TOK_FLAG_MODULE,
    TOK_FLAG_SPECIFIER,
    TOK_FLAG_OPERATOR
  );

  TTokenFlagSet = set of TTokenFlag;

  TFileScanner = class(TObject)
  private
    Next    : TFileScanner;
    LastRow : Integer;
    LastCol : Integer;
    LastChar: Char;

    buf     : pchar;
    Start   : Longint;    // File Postion for the Start of the Buffer
    Size    : Longint;    // Data Size within the Buffer
    Index   : Longint;    // Current Cursor Position

    FStream: TStringStream;

    procedure ReloadBuf;
    function ReadChar: Char;
    function NextChar: Char;
  public
    function LoadData(const fn:string):boolean;

    procedure AfterConstruction; override;
    procedure BeforeDestruction; override;
  end;

procedure DoneScanner;
procedure PushScanner(const aItem: TFileScanner);
procedure PopScanner;
function ReadChar: Char;
procedure SkipComment;
procedure SkipSpace;
procedure SkipLine;
procedure Readstring;
procedure ReadNumber;
function ReadFileName: string;
function ReadToken: TAsmToken;
function TokenFlag(Flags: TTokenFlagSet): TAsmToken;
function IsTokenIdent: Boolean;

var
  LastChar  : Char;
  Token		  : TAsmToken;
  TokenStr	: string;
  TokenUpStr: string;
  TokenInt	: Longint;
  TokenRow  : Integer;
  TokenCol  : Integer;

implementation

uses Global;

const
  END_OF_LINE = #13;
  END_OF_ALL  = #26;

  BUFFER_LENGTH = 4096;

  TOK_OPERATOR_NAME: array [TTokOperator] of string = (
      'SHR',  'SHL',
      'MOD',  'DIV',
      'AND',  'OR',
      'XOR'
  );

  SIZE_SPECIFIER_NAME: array [TSizeSpecifier] of string = (
    'BYTE',
    'WORD',
    'DWORD',
    'NEAR',
    'SHORT'
  );

  MODULE_NAME: array [TModule] of string = (
    'MODEL', 'CODE', 'DATA' , 'APPTYPE'
  );

  DIRECTIVE_NAME: array [TDirective] of string = (
    'DB', 'DW', 'DD',
    'ORG','end',
    'INCLUDE',
    'EXTERN','PUBLIC', 'IMPORT',
    'ALIGN'
  );

  ROUTINE_NAME: array [TRoutine] of string = (
    'ADDR',
    'OFFSET',
    'SIZEOF'
  );

var
  ScanItem: TFileScanner = nil;

function IsPrefix(const aData: string): Integer;
var
  i: TAsmPrefix;
begin
  for i := Low(TAsmPrefix) to High(TAsmPrefix) do
    if aData = ASM_PREFIX_NAME[i] then
      Exit(Ord(i));

  Result := -1;
end;

function IsOpcode(const aData: string): Integer;
var
  i: TAsmOpcode;
begin
  for i := Low(tAsmOpcode) to High(tAsmOpcode) do
    if aData = ASM_OPCODE_NAME[i] then
      Exit(Ord(i));

  Result := -1;
end;

function IsRegister(const aData: string): Integer;
var
  i: TAsmRegister;
begin
  for i:=Low(TAsmRegister) to High(TAsmRegister) do
    if aData = AsmRegisterName[i] then
      Exit(Ord(i));

  Result := -1;
end;

function IsRoutine(const aData: string): Integer;
var
  i: TRoutine;
begin
  for i := Low(TRoutine) to High(TRoutine) do
    if aData = ROUTINE_NAME[i] then
      Exit(Ord(i));

  Result := -1;
end;

function IsDirective(const aData: string): Integer;
var
  i: TDirective;
begin
  for i:=Low(TDirective) to High(TDirective) do
    if aData = DIRECTIVE_NAME[i] then
      Exit(Ord(i));

  Result := -1;
end;

function IsModule(const aData: string): Integer;
var
  i: TModule;
begin
  for i:=Low(TModule) to High(TModule) do
    if aData = MODULE_NAME[i] then
      Exit(Ord(i));

  Result := -1;
end;

function IsSpecifier(const aData: string): Integer;
var
  i: TSizeSpecifier;
begin
  for i:=Low(TSizeSpecifier) to High(TSizeSpecifier) do
    if aData = SIZE_SPECIFIER_NAME[i] then
      Exit(Ord(i));

  Result := -1;
end;

function IsOperator(const aData: string): Integer;
var
  i: TTokOperator;
begin
  IsOperator:=-1;
  for i:=Low(TTokOperator) to High(TTokOperator) do begin
    if (aData=TOK_OPERATOR_NAME[i]) then begin
      IsOperator:=Ord(i);
      Exit;
    end;
  end;
end;

function TFileScanner.LoadData(const fn:string):boolean;
begin
  FStream := TStringStream.Create(fn, TUnicodeEncoding.Create);

  LastChar:=#0;
  LastRow:=1;
  LastCol:=0;

 //reset buffer
  Start:=0;
  Size:=0;
  Index:=0;

  Result:=true;
end;


procedure TFileScanner.AfterConstruction;
begin
  inherited;
  GetMem( buf,BUFFER_LENGTH * SizeOf(Char));
end;

procedure TFileScanner.BeforeDestruction;
begin
  FStream.Free;
  if assigned(buf) then FreeMem(buf,BUFFER_LENGTH* SizeOf(Char));
  inherited;
end;

procedure TFileScanner.ReloadBuf;
begin
  inc( Start, Size );
  Size := FStream.Read(buf^, BUFFER_LENGTH* SizeOf(Char)) div SizeOf(Char);
  Index:=0;
end;

function TFileScanner.ReadChar:Char;
begin
 if (Index<Size) then
 begin
    LastChar:=buf[Index];
    Inc( index );
 end
 else begin
    ReloadBuf;
    if (Size>0) then begin
        LastChar:=Buf[0];
        Index:=1;
    end
    else LastChar:=END_OF_ALL;
 end;

 if (LastChar=#10) then begin
    Inc(LastRow);
    LastCol:=0;
 end
 else if (LastChar<>#13) Then Inc(LastCol);
 Result:=LastChar;
end;

function TFileScanner.NextChar:Char;
begin
  if Index<Size then
    NextChar:= buf[Index]
  else
  begin
    ReloadBuf;
    if (Size>0) then
    begin
      NextChar:=Buf[0];
      Index:=0;
    end else
      NextChar:=END_OF_ALL;
  end;
end;


procedure DoneScanner;
begin
  while ScanItem <> nil do
    PopScanner;
end;

procedure PushScanner(const aItem:TFileScanner);
begin
  aItem.Next := ScanItem;
  ScanItem := aItem;
end;

procedure PopScanner;
 var    Item : TFileScanner;
begin
  if ScanItem=NIL then Exit;
  Item:=ScanItem.Next;
  ScanItem.Free;
  ScanItem:=Item;
end;


function ReadChar:Char;
 var  C:Char;
begin
  if ScanItem<>NIL then begin
    C:=ScanItem.ReadChar;
    if C=END_OF_ALL then begin
      PopScanner;
      if ScanItem<>NIL then C:=ReadChar;
    end;
  end else C:=END_OF_ALL;
  LastChar:=C;
  ReadChar:=C;
end;

function NextChar:Char;
 var  C:Char;
begin
  if (ScanItem<>NIL)
    then C:=ScanItem.NextChar
    else C:=END_OF_ALL;
  NextChar:=C;
end;

///////////////////////////////////////////////////////////////////////////////
procedure SkipSpace;
begin
 while (LastChar in [' ', #9, #10]) do ReadChar;
end;

///////////////////////////////////////////////////////////////////////////////
procedure SkipLine;
begin
 if  Token=TOK_EOLN then Exit;
 while not(LastChar in [END_OF_ALL,END_OF_LINE]) do ReadChar;
 ReadChar;
 Token:=TOK_EOLN;
end;

///////////////////////////////////////////////////////////////////////////////
procedure Readstring;
 var    St  : String;
begin
  St:=LastChar;
  while ReadChar in ['$','0'..'9','@'..'Z','_','a'..'z'] Do begin
     St:=St+LastChar;
  end;
  TokenStr:=St;
  if St='$' Then Token:=TOK_DOLLAR
  else begin
    TokenUpStr:=UpperCase(TokenStr);
    Token:=TOK_IDENTIFIER;
  end;
end;

procedure ReadNumber;
var
  ValType: (Val_Bin, Val_Oct, Val_Dec, Val_Hex);
begin
  TokenStr := '';
  // Check for binary number
  ValType := Val_Bin;
  repeat
    case LastChar of
      '0'..'1': ;
      '2'..'7': if ValType<Val_Oct then ValType:=Val_Oct;
      '8'..'9': if ValType<Val_Dec then ValType:=Val_Dec;
      'a'..'f','A'..'F':
      begin
        if (not (NextChar in ['0'..'9','a'..'f','A'..'F'])) and
          ((Lastchar in['b','B']) and (ValType=Val_Bin)) then
        begin
          TokenStr := TokenStr + LastChar;
          ReadChar;
          Token := TOK_INTCONST;
          TokenInt := Bin2Long(TokenStr);
          Exit;
        end;
        ValType := Val_Dec;
      end;
    else
      Break;
    end;
    TokenStr := TokenStr + LastChar;
    ReadChar;
  until False;

  case LastChar of
    'b','B': // Binary Value?
    if ValType = Val_Bin then
    begin
      Token := TOK_INTCONST;
      TokenInt := Bin2Long(TokenStr);
      TokenStr := TokenStr + LastChar;
      ReadChar;
      Exit;
    end else
      AddError('Error parsing Binary value');

    'o','O': // Octal Value?
    if (ValType<=Val_Oct) then
    begin
      Token:=TOK_INTCONST;
      TokenInt:=Oct2Long(TokenStr);
      TokenStr := TokenStr + LastChar;
      ReadChar;
      Exit;
    end else
      AddError('Error parsing Octal value');
    'h','H': // Hexadecimal value
        begin
            Token:=TOK_INTCONST;
            TokenInt:=Hex2Long(TokenStr);
            TokenStr := TokenStr + LastChar;
            ReadChar;
            Exit;
        end;
    '.': ; // Real Value
  else
    begin
      if (ValType<=Val_Dec) then
      begin // Decimal value
          Token:=TOK_INTCONST;
          TokenInt:=Dec2Long(TokenStr);
          Exit;
      end else
        AddError('Error parsing Decimal value');
    end;
  end;
end;


function ReadFileName:String;
begin
 SkipSpace;
 Result:='';
 while NextChar in ['.','$','0'..'z','_'] do Result:=Result+ UPCASE( ReadChar );
end;

procedure SkipComment;
begin
 while not(ReadChar in[END_OF_LINE,END_OF_ALL]) do;
 case LastChar of
   END_OF_LINE:
      begin
        ReadChar;
        Token:=TOK_EOLN;
      end;
   END_OF_ALL: Token:=TOK_EOF;
 end;
end;

FUNCTION ReadToken: TAsmToken;
 var    C   : Char;
 i: Integer;
begin{ Readtoken }
 SkipSpace;
 TokenStr := LastChar;

  if ScanItem<>NIL then begin
    TokenRow:=ScanItem.LastRow;
    TokenCol:=ScanItem.LastCol;
  end;

 case LastChar of

  '$','@'..'Z','_','a'..'z': ReadString;

  '0'..'9': ReadNumber;

  '''','"':{ String constant }
    begin
        C:=LastChar;
        i := 0;
        //TokenStr[0]:=#0;
        while (ReadChar<>C) do begin
            if LastChar in[END_OF_ALL,END_OF_LINE] then break;// ERROR
            Inc( i );
            SetLength(TokenStr, i);
            TokenStr[ i ]:=LastChar;
        end;
        case i of
            1: begin Token:=TOK_INTCONST; TokenInt:=Byte(TokenStr[1]); end;
            2: begin Token:=TOK_INTCONST; TokenInt:=Byte(TokenStr[1])+256*Byte(TokenStr[2]); end;
            else Token:=TOK_STRCONST;
        end;
        ReadChar;
    end;

  END_OF_LINE :{ end of Line }
    begin
        ReadChar;
        Token:=TOK_EOLN;
    end;

  END_OF_ALL :{ end of Line }
    begin
        Token:=TOK_EOF;
    end;

  ';' : SkipComment;

  '.':
    begin
        TokenStr := LastChar;
        Token := TOK_POINT;
        ReadChar;
    end;

  ':':{ Label: , SEG : [OFS] }
	begin
        TokenStr:=LastChar;
        Token:=TOK_COLON;
        ReadChar;
    end;
  ',':{ Opcode Arg,Arg }
    begin
        TokenStr:=LastChar;
        Token:=TOK_COMMA;
        ReadChar;
    end;
{ Les operateurs }
  '+':
    begin
        TokenStr:=LastChar;
        Token:=TOK_PLUS;
        ReadChar;
    end;
  '-':
    begin
        TokenStr:=LastChar;
        Token:=TOK_MINUS;
        ReadChar;
    end;
  '*':
    begin
        TokenStr:=LastChar;
        Token:=TOK_STAR;
        ReadChar;
    end;
  '/':
    begin
        if NextChar=END_OF_LINE then begin
          ReadChar; ReadChar; ReadToken;
        end
        else begin
          TokenStr:=LastChar;
          Token:=TOK_SLASH;
          ReadChar;
        end;
    end;

  '=':
    begin
	    TokenStr:=LastChar;
	    Token:=TOK_EQUAL;
      ReadChar;
    end;

  '(':
    begin
	    TokenStr:=LastChar;
	    Token:=TOK_LPAREN;
      ReadChar;
    end;
  ')':
    begin
        TokenStr:=LastChar;
        Token:=TOK_RPAREN;
        ReadChar;
    end;

  '[':
    begin
	    TokenStr:=LastChar;
	    Token:=TOK_LFRAME;
      ReadChar;
    end;
  ']':
    begin
        TokenStr:=LastChar;
        Token:=TOK_RFRAME;
        ReadChar;
    end;

  '^':
    begin
	    TokenStr:=LastChar;
	    Token:=TOK_CARET;
      ReadChar;
    end;

  '!':
    begin
	    TokenStr:=LastChar;
	    Token:=TOK_EXCLAMATION;{Not Operand}
      ReadChar;
    end;

  else
    begin
      ReadChar;
	    Token:=TOK_UNKNOWN;
    end;
 end;{case}
 ReadToken:=Token;
end;{ ReadToken }

function TokenFlag( Flags:TTokenFlagSet ) : TAsmToken;
begin
  Result:=Token;
  if Token=TOK_IDENTIFIER then
  begin
    if TOK_FLAG_PREFIX in Flags then
    begin
      TokenInt := IsPrefix(TokenUpStr);
      if TokenInt <> -1 then
      begin
        Token:=TOK_PREFIX;
        Exit(Token);
      end;
    end;
    if TOK_FLAG_OPCODE in Flags then
    begin
      TokenInt:=IsOPCODE(TokenUpStr);
      if TokenInt<>-1 then
      begin
        Token:=TOK_OPCODE;
        Exit(Token);
      end;
    end;
    if TOK_FLAG_REGISTER in Flags then
    begin
      TokenInt:=IsRegister(TokenUpStr);
      if TokenInt<>-1 then
      begin
        Token:=TOK_REGISTER;
        Exit(Token);
      end;
    end;
    if TOK_FLAG_ROUTINE in Flags then
    begin
      TokenInt:=IsROUTINE(TokenUpStr);
      if TokenInt<>-1 then
      begin
        Token:=TOK_ROUTINE;
        Exit(Token);
      end;
    end;
    if TOK_FLAG_DIRECTIVE in Flags then
    begin
      TokenInt:=IsDIRECTIVE(TokenUpStr);
      if TokenInt<>-1 then
      begin
        Token:=TOK_DIRECTIVE;
        Exit(Token);
      end;
    end;
    if TOK_FLAG_MODULE in Flags then
    begin
      TokenInt:=IsMODULE(TokenUpStr);
      if TokenInt<>-1 then
      begin
        Token:=TOK_MODULE;
        Exit(Token);
      end;
    end;
    if TOK_FLAG_SPECIFIER in Flags then
    begin
      TokenInt:=IsSPECIFIER(TokenUpStr);
      if TokenInt<>-1 then
      begin
        Token:=TOK_SPECIFIER;
        Exit(Token);
      end;
    end;
    if TOK_FLAG_OPERATOR in Flags then
    begin
      TokenInt:=IsOPERATOR(TokenUpStr);
      if TokenInt<>-1 then
      begin
        Token:=TOK_OPERATOR;
        Exit(Token);
      end;
    end;
  end;
end;

function IsTokenIdent:Boolean;
begin
  Result:= TokenFlag([Low(TTokenFlag)..High(TTokenFlag)] ) = TOK_IDENTIFIER;
end;

end.


