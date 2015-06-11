unit PeLinker;

interface
{ RVA : Relative Virtual Address
  VA  : Virtual Address = RVA + ImageBase   : Offset in the memory when process is loaded }
uses
  Global, Segments, Symbols, PEHeader, Import, RelocationStack, Convert, classes;

type
  THeader = record
    SecCODE : TPESection;
    SecDATA : TPESection;
    SecIDATA: TPESection; // Imported DATA
    SecBSS  : TPESection;
  end;

  TPELinker = class(TObject)
  private
    FRelVirtualAddr: Integer; // Current RVA Address
    FPtrtoRawData: Integer; // Current File (Raw Data) Pointer
    FExeFile: TMemoryStream;
    FPEHeader: TPEheader;
    FOptHeader: TPEoptheader;
    FHeader: THeader;

    procedure InitHeaders;
    procedure FixImportSection;
    procedure FixBSSSection;
    procedure FixDATASection;
    procedure FixCodeSection;

    procedure Align_File;
    procedure WritePE;
  public
    procedure Link(const aFileName: string);
  end;

implementation

const
  SEC_FLAG_CODE  = $60000020;
  SEC_FLAG_IDATA = $C0000040;
  SEC_FLAG_RSRC  = $50000040;
  SEC_FLAG_RELOC = $42000040;

const
  DOSStub: array[0..$7F] of AnsiChar = (
    'M', 'Z',
    #$6C, #$00, #$01, #$00, #$00, #$00, #$04, #$00, #$00, #$00, #$FF, #$FF,
    #$03, #$00, #$00, #$01, #$00, #$00, #$00, #$00, #$00, #$00, #$40, #$00,
    #$00, #$00, #$00, #$00, #$00, #$00, #$00, #$00, #$00, #$00, #$00, #$00,
    #$00, #$00, #$00, #$00, #$00, #$00, #$00, #$00, #$00, #$00, #$00, #$00,
    #$00, #$00, #$00, #$00, #$00, #$00, #$00, #$00, #$00, #$00, #$80, #$00,
    #$00, #$00,
    {dos executable code}
    #$0E, #$1F, #$BA, #$0E, #$00, #$B4, #$09, #$CD, #$21, #$B8, #$00, #$4C,
    #$CD, #$21,
    'T','h','i','s',' ','p','r','o','g','r','a','m',' ','c','a','n','n','o','t',
    ' ','b','e',' ','r','u','n',' ','i','n',' ','d','o','s',' ','m','o','d','e',
    '.',#$0D,#$0A,'$','-','B','A','S','S','E','M','-'
  );

procedure TPELinker.InitHeaders;
begin
  FillChar(FPEHeader, SizeOf(FPEHeader), 0);
  with FPEHeader do
  begin
    Magic := 'PE'#0#0;
    Machine   := $014C;
    NumberOfSections:= 0;
    TimeDateStamp := 0;
    PointerToSymbolTable := 0;
    NumberOfSymbols := 0;
    SizeOfOptionalHeader := SizeOf(FOptHeader);
    Characteristics := $818E {or IMAGE_FILE_RELOCS_STRIPPED};
  end;

  FillChar( FOptHeader, Sizeof(tPEoptHeader), 0);
  with FOptHeader do
  begin
    Magic           := $010B;
    MajorLinkerVersion := 0;
    MinorLinkerVersion := 0;
    SizeOfCode      := 0;{ Set Later }
    SizeOfInitializedData   := 0;
    SizeOfUninitializedData   := 0;
    AddressOfEntryPoint   := 0;
    BaseOfCode      := 0;
    BaseOfData      := 0;
    ImageBase       := $400000;
    SectionAlignment:= $1000;
    FileAlignment := $200;
    MajorOperatingSystemVersion := 1;
    MinorOperatingSystemVersion := 0;
    MajorImageVersion:= 0;
    MajorImageVersion:= 0;
    MajorSubsystemVersion := 4;
    Win32VersionValue := 0;
    SizeOfImage     := { $1000 + $3000;{ (address of first section) + }(1+FPEHeader.NumberOfSections)*FOptHeader.SectionAlignment;
    SizeOfHeaders   := $200; {//FilePos(F) + SizeOf(OptHeader)}
    CheckSum        := 0;
    if App.AppType = APP_GUI then
      Subsystem := SUBSYSTEM_WINDOWS_GUI
    else
      Subsystem := SUBSYSTEM_WINDOWS_CUI;
    DllCharacteristics := 0;
    SizeOfStackReserve := $100000;
    SizeOfStackCommit  := $4000;
    SizeOfHeapReserve  := $100000;
    SizeOfHeapCommit   := $1000;
    LoaderFlags     := 0;
    NumberOfRvaAndSizes := 16;
  end;
end;

procedure TPELinker.FixImportSection;
{
 DIRECTORY TABLE
 NULL DIR ENTRY

 DLL 1 LOOKUP TABLE
 DLL 2 LOOKUP TABLE
 ...
 NULL(4bytes)

 HINT - NAME TABLE
 NULL(4bytes)
}
var
  SecRVA_IDATA: Integer;
  ofsDLL1LookupTable: Integer;  // Address for the offset table within import section
  ofsHintNameTable: Integer;    // Address for the name table within import section
  LibraryCount: Integer;        // Number of Impported Libraries
  ImportCount: Integer;        // Number of All Impported functions
  ImportDir: TImportDirectory;
  Name: string;
  Rva_Name: LONGINT;
  LibItem: TLibraryItem;
  ImportItem: TImportItem;
  //SymData: TDataLabel;
  Sym: TSymbol;
begin
  SecRVA_IDATA :=FRelVirtualAddr;
  FHeader.SecIDATA.RelVirtualAddr:=FRelVirtualAddr;
  App.SecIDATA.VirtualAddress:=FOptHeader.ImageBase+FRelVirtualAddr;

// get The number of librairies and imported Data
  LibraryCount:=App.LibStack.CountCalledLibs;
  ImportCount:=App.LibStack.CountCalledImports;
//  Writeln(' + Used Libraries Number = ',LibraryCount);
//  Writeln(' + Called Imports Count = ',ImportCount);

// get the offset of the first lirary lookup table
  ofsDLL1LookupTable:= (Sizeof(tImportDirectory)*(LibraryCount+1)); // +1 for null import dir

// get the offset of the (HInt/Name) table
  ofsHintNameTable   := ofsDLL1LookupTable+( 4*(ImportCount+LibraryCount) );

// Set buffer to be filled with import dirs and address table
  App.SecIDATA.AddByte(0, ofsHintNameTable);

  LibraryCount := 0;
  ImportCount := 0;
  FillChar(ImportDir,SizeOf(ImportDir),0);

  LibItem := App.LibStack.Head;
  while (LibItem <> nil) do
  begin
    if LibItem.IsCalled then
    begin
     // Set Import Directory
      ImportDir.Characteristics := SecRVA_IDATA + ofsDLL1LookupTable + (ImportCount*4);
      ImportDir.LookupTableRVA  := SecRVA_IDATA + ofsDLL1LookupTable + (ImportCount*4);
      ImportDir.NameRVA         := SecRVA_IDATA + App.SecIDATA.Size;
      App.SecIDATA.SetBuffer( Sizeof(tImportDirectory)*LibraryCount, ImportDir, Sizeof(tImportDirectory), 1);
     // Set Library Name
      Name := LibItem.Name + #0#0#0; // for alignemnt
      if odd(Length(Name)) then
        SetLength(Name, Length(Name) - 1);
        //Dec(Name[0]);

      App.SecIDATA.AddString(Name);  // Add lib name within name table
     // Set Address Table & functions names for current library
      ImportItem := LibItem.Head;
      while ImportItem <> nil do
      begin
        if ImportItem.IsCalled then
        begin
          RVA_Name := SecRVA_IDATA+App.SecIDATA.Size;
          { 00000000h - Import by name }
          { 80000000h - Import by ordinal }
          App.SecIDATA.SetDword( ofsDLL1LookupTable+Sizeof(Longint)*ImportCount, RVA_Name, 1);
          Name := #0#0 + ImportItem.Name + #0#0;
          //if odd( byte(Name[0]) ) Then Dec(Name[0]);
          if odd(Length(Name)) then
            SetLength(Name, Length(Name) - 1);
          App.SecIDATA.AddString(Name);

          // Set Import Function Jump Code address
          ImportItem.Kind := SYM_LABEL;
          ImportItem.Flag := ImportItem.Flag + [SYM_FLAG_DEFINED];
          ImportItem.Data := TDataLabel.Create(DEF_LABEL, nil, App.SecIDATA, ofsDLL1LookupTable + (ImportCount * 4));

          // Set Jmp_Sym Address : FF 25 <00 00 00 00>
          App.SecCODE.Align(8);
          Sym := ImportItem.Jmp_Sym;
          Sym.Kind := SYM_LABEL;
          Sym.Flag := Sym.Flag + [SYM_FLAG_DEFINED];
          Sym.Data := TDataLabel.Create(DEF_LABEL, nil, App.SecCODE, App.SecCODE.Size);

          App.SecCODE.AddString(#$FF#$25);
          // Set Relocation
          App.SecCODE.Relocs.PushReloc(REL_OFFSET,ImportItem,App.SecCODE.Size,0);
          App.SecCODE.AddByte(0,4);

          Inc(ImportCount);
        end;
        ImportItem:=Pointer(ImportItem.Next);
      end;
      Inc(ImportCount);
      Inc(LibraryCount);
    end;
    LibItem:=LibItem.Next;
  end;

// Update Headers
// we are using an import directory
 FOptHeader.Directory[image_Directory_Entry_Import].RVA :=SecRVA_IDATA;
 FOptHeader.Directory[image_Directory_Entry_Import].Size:=App.SecIDATA.Size;
  with FHeader.SecIDATA do
  begin
    Name:='.idata'#0#0;
    VirtualSize := ( ((App.SecIDATA.Size+FOptHeader.SectionAlignment-1)div FOptHeader.SectionAlignment) * FOptHeader.SectionAlignment);
    RelVirtualAddr := SecRVA_IDATA;
    SizeofRawData := ( ((App.SecIDATA.Size+FOptHeader.FileAlignment-1)div FOptHeader.FileAlignment) * FOptHeader.FileAlignment) ;  { must be multiple of file alignment }
    PtrtoRawData := FPtrtoRawData; // Last File Pos
    Characteristics := SEC_FLAG_IDATA;
  end;

 FRelVirtualAddr:= FRelVirtualAddr + FHeader.SecIDATA.VirtualSize;
 FPtrtoRawData:=FPtrtoRawData + FHeader.SecIDATA.SizeofRawData;
End;


///////////////////////////////////////////////////////////////////////////////
// BSS SECTION
///////////////////////////////////////////////////////////////////////////////
procedure TPELinker.FixBSSSection;
begin
 App.SecBSS.VirtualAddress:=FOptHeader.ImageBase+FRelVirtualAddr;

 With FHeader.SecBSS do begin
    Name:='.bss'#0#0#0#0;
    VirtualSize :=FOptHeader.SectionAlignment;//( ((SectionBSS.Size+OptHeader.SectionAlignment-1)div OptHeader.SectionAlignment) * OptHeader.SectionAlignment);
    RelVirtualAddr := FRelVirtualAddr;
    SizeOfRawData := 0;  { must be multiple of file alignment }
    PtrToRawData := FPtrtoRawData;
    Characteristics := Section_BSS + Section_READ + Section_WRITE ;
 End;
 FOptHeader.SizeOfUninitializedData:=FHeader.SecBSS.VirtualSize;

 FRelVirtualAddr:= FRelVirtualAddr + FHeader.SecBSS.VirtualSize;
 FPtrtoRawData:=FPtrtoRawData+FHeader.SecBSS.SizeofRawData;
end;

procedure TPELinker.FixDATASection;
begin
 App.SecDATA.VirtualAddress:=FOptHeader.ImageBase+FRelVirtualAddr;

 With FHeader.SecDATA do begin
    Name:='.data'#0#0#0;
    VirtualSize :=FOptHeader.SectionAlignment;//( ((SectionBSS.Size+OptHeader.SectionAlignment-1)div OptHeader.SectionAlignment) * OptHeader.SectionAlignment);
    RelVirtualAddr := FRelVirtualAddr;
    SizeofRawData := ( ((App.SecDATA.Size+FOptHeader.FileAlignment-1)div FOptHeader.FileAlignment) * FOptHeader.FileAlignment) ;  { must be multiple of file alignment }
    PtrToRawData := FPtrtoRawData;
    Characteristics := Section_BSS + Section_READ + Section_WRITE ;
 End;

 FRelVirtualAddr:= FRelVirtualAddr + FHeader.SecDATA.VirtualSize;
 FPtrtoRawData:=FPtrtoRawData+FHeader.SecDATA.SizeofRawData;
end;

procedure TPELinker.FixCodeSection;
var
  SizeOfCode: Integer;
  SecRVA_CODE: Integer;
begin
  SecRVA_CODE := FRelVirtualAddr;
  App.SecCODE.VirtualAddress := FOptHeader.ImageBase + FRelVirtualAddr;

// Get Total Section Code Size
  SizeofCode:= App.SecCODE.Size;
//  Writeln(' + Total Code size = ',SizeofCode);

// ApplyFixups Code Sections fixup

// Update Headers
  with FHeader.SecCODE do
  begin
    Name := '.text'#0#0#0;

    VirtualSize := (((SizeofCode + FOptHeader.SectionAlignment - 1) div
                      FOptHeader.SectionAlignment) * FOptHeader.SectionAlignment);

    RelVirtualAddr := FRelVirtualAddr;

    SizeOfRawData := (((SizeofCode + FOptHeader.FileAlignment-1) div
                        FOptHeader.FileAlignment) * FOptHeader.FileAlignment);  { must be multiple of file alignment }

    PtrToRawData := FPtrtoRawData;
    Characteristics := SEC_FLAG_CODE;
  end;

  FOptHeader.SizeOfCode := SizeofCode;
  FOptHeader.AddressOfEntryPoint := SecRVA_CODE+App.EntryPoint;
  FOptHeader.BaseOfCode := SecRVA_CODE;

  FRelVirtualAddr:= FRelVirtualAddr + FHeader.SecCODE.VirtualSize;
  FPtrtoRawData:=FPtrtoRawData+ FHeader.SecCODE.SizeofRawData;
end;

procedure TPELinker.Align_File;
var
  i: Integer;
  b: Byte;
//  TempBuf: Pointer;
begin
  i := FOptHeader.FileAlignment - (FExeFile.Position mod FOptHeader.FileAlignment);
  b := 0;
  while i > 0 do
  begin
    FExeFile.Write(b, SizeOf(Byte));
    Dec(i);
  end;
//  if i > 0 then
//  begin
//    GetMem(TempBuf, i);
//    FillChar(TempBuf^, i, #0);
//
//    FExeFile.Write(TempBuf^, i);
//    //BlockWrite(FExeFile, TempBuf^, i);
//    Freemem(TempBuf, i);
//  end;
end;

procedure TPELinker.WritePE;
Begin
  // PE Header
  FExeFile.Write(DOSStub, SizeOf(DOSStub));
  FExeFile.Write(FPEHeader, SizeOf(FPEHeader));
  FExeFile.Write(FOptHeader, SizeOf(FOptHeader));
  if (App.SecBSS.Size>0) then
    FExeFile.Write(FHeader.SecBSS, SizeOf(TPESection));
  FExeFile.Write(FHeader.SecIDATA, SizeOf(TPESection));
  if (App.SecDATA.Size>0) then
    FExeFile.Write(FHeader.SecDATA, SizeOf(TPESection));
  FExeFile.Write(FHeader.SecCODE, SizeOf(TPESection));
  Align_File;
  // IMPORT
  FExeFile.Write(App.SecIDATA.Data[0], App.SecIDATA.Size);
  Align_File;
 // Section DATA
  if (App.SecDATA.Size>0) then
  begin
    FExeFile.Write(App.SecDATA.Data[0], App.SecDATA.Size);
    Align_File;
  end;
 // Code Section
  FExeFile.Write(App.SecCODE.Data[0], App.SecCODE.Size);
  Align_File;
End;


procedure TPELinker.Link(const aFileName: string);
var
  SectionCount: Integer;
  HeaderSize: Integer;
Begin
  InitHeaders;

  // Calculate the Number of Sections to Write
  SectionCount := 0;
  if App.SecBSS.Size > 0 then
    Inc(SectionCount);

  {if (App.SecIDATA.Size>0) then}
    Inc(SectionCount);

  if App.SecDATA.Size > 0 then
    Inc(SectionCount);

  {if (App.SecCODE.Size>0) then}
    Inc(SectionCount);

  FPEHeader.NumberOfSections := SectionCount;

  // Calculate All Headers Size
  HeaderSize:= SizeOf(DOSStub) + SizeOf(FPEHeader) + SizeOf(FOptHeader) + SectionCount*Sizeof(TPESection);
  FOptHeader.SizeOfHeaders:=HeaderSize;

  FRelVirtualAddr :=( ((HeaderSize+FOptHeader.SectionAlignment-1)div FOptHeader.SectionAlignment) * FOptHeader.SectionAlignment);
  FPtrtoRawData:=( ((HeaderSize+FOptHeader.FileAlignment-1)div FOptHeader.FileAlignment) * FOptHeader.FileAlignment);

 // Fix All Sections ( Addresses and sizes )
  if App.SecBSS.Size > 0 then
    FixBSSSection;

  FixImportSection;

  if App.SecDATA.Size > 0 then
    FixDATASection;

  FixCodeSection;

  App.SecCODE.SetFixups;
  App.SecDATA.SetFixups;

  if (App.SecCODE.Relocs.Head = nil) and (App.SecDATA.Relocs.Head = nil) then
  begin
    FOptHeader.SizeOfImage:= FRelVirtualAddr;
    FExeFile := TMemoryStream.Create; //TFileStream.Create(aFileName, fmCreate);
    WritePE; // Creating EXE file
    FExeFile.SaveToFile(aFileName);
    FExeFile.Free;
//    Writeln(' + BSS RVA   = $', Hex32(Header.SecBSS.RelVirtualAddr));
//    Writeln(' + IDATA RVA = $', Hex32(Header.SecIDATA.RelVirtualAddr));
//    Writeln(' + DATA RVA  = $', Hex32(Header.SecDATA.RelVirtualAddr));
//    Writeln(' + CODE RVA  = $', Hex32(Header.SecCODE.RelVirtualAddr));
//    WriteLN(' -> ',fExeName);
  end else
  begin // Create Object File??????????
//    if (App.SecCODE.Relocs.Head <> nil) then
//      Writeln('Undefined Symbol: ', TSymbol(App.SecCODE.Relocs.Head.Sym).Name);
//    if (App.SecDATA.Relocs.Head <> nil) then
//      Writeln('Undefined Symbol: ', TSymbol(App.SecCODE.Relocs.Head.Sym).Name);
  end;
End;

END.


