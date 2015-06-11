unit Symbols2;

interface

uses
  SysUtils, Assemble, Symbols, Segments;

type
  TDataStruct = class(TDataTypeDef)
  private
    FSymbols: TSymbolGroupItem;
    FSegment: TSegment;
  public
    property Symbols: TSymbolGroupItem read FSymbols write FSymbols;
    property Segment: TSegment read FSegment write FSegment;

    constructor Create(const sName: string);
    destructor Destroy;
  end;

implementation

constructor TDataStruct.Create(const sName: string);
begin
  inherited Create(DEF_STRUCT);
  FSymbols := TSymbolGroupItem.Create;
  FSegment := TSegment.Create(SEG_DATA, sName);
end;

destructor TDataStruct.Destroy;
begin
  FSegment.Free; // заменить на free
  inherited;
end;

end.
