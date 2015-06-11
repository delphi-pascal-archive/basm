unit Global;

interface

uses
  Symbols,
  Assemble,
  RelocationStack,
  Segments,
  Import,
  Scanner,
  SysUtils;

type
  TOutFile = packed record
    EntryPoint: Integer;  // Offset within SecCODE
    Symbols   : TSymbolGroupItem;
    SecCODE   : TSegment;
    SecDATA   : TSegment; //initialized data segment
    SecBSS    : TSegment; //uninitialized data segment
    SecIDATA  : TSegment; // Imported Data Segment
    LibStack  : TLibraryStack;
    AppType   : ( APP_CONSOLE, APP_GUI );
  end;

const
  // Error Messages
  	EXPECT_CONSTANT : string = 'Constant expected';
  	EXPECT_INT_CONST: string = 'Integer constant expected';
  	EXPECT_REGISTER : string = 'Expect Register';
  	EXPECT_LPAREN	  : string = 'Expect "("';
  	EXPECT_RPAREN	  : string = 'Expect ")"';
  	EXPECT_LFRAME	  : string = 'Expect "["';
  	EXPECT_RFRAME	  : string = 'Expect "]"';
  	EXPECT_COMMA	  : string = 'Expect ","';
  	EXPECT_COLON	  : string = 'Expect ":"';
  	TYPE_MISMATCH	  : string = 'Type mismatch';
  	INV_REGISTER	  : string = 'Invalid register combination';
  	ERR_ARG_COUNT	  : string = 'Invalid argument number';

var
  Listing: string = '';
  Errors: string = '';
  SymGroup  : TSymbolGroup;
  CurSeg    : TSegment;
  App       : TOutFile;
  DirEXEC   : String; { Executable directory }
  DirSOURCE : String;
  DirOUTPUT : String;
  DirINCLUDE: String;

procedure AddError(const aData: string);
procedure AddListing(const aData: string);

implementation

procedure AddError(const aData: string);
begin
  Errors := Errors + Format('Error: %s'#13#10 +
                            '  Line = %d'#13#10 +
                            '  Col  = %d'#13#10,
                            [aData,
                             TokenRow,
                             TokenCol]
                            );
end;

PROCEDURE MakeError(Const ProcName, n:String);
Begin
  WriteLN('ERROR: ',N);
  Writeln(' + LINE = ',TokenRow);
  Writeln(' + COL = ',TokenCol);
  Writeln(' + MODULE = ',ProcName);
  Readln;
End;

procedure AddListing(const aData: string);
begin
  Listing := Listing + aData;
end;

end.
