unit PEHeader;

interface

{
tSectionTEXT = '.text'#0#0#0
tSectionDATA ='.data'#0#0#0
tSectionRDATA = '.rdata'#0#0
tSectionIDATA = '.idata'#0#0
'.edata'#0#0
'.reloc'#0#0
'.rsrc'#0#0#0
}
{ EXE file Structure:
³
³DOS exe header
ÀÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄ
 ³PE exe header
 ÀÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄ
  ³PE optional header
  ÀÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄ
   ³ALL Sections headr
   ÀÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄ
    ³ <*FILE ALIGNEMENT*>
    ³    Section1 Data
    ÀÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄ
     ³ <*FILE ALIGNEMENT*>
     ³    Section2 Data
     ÀÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄ
      ³ <*FILE ALIGNEMENT*>
      ³    Section3 Data
      ÀÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄ
       ...
}
type
  TEXEheader = packed record
{00}Signature : array[1..2]Of ansiChar;  {contains EXEID }
{02}LengthRem : Word; {# of Bytes in last page}
{04}LengthPages   : Word; {# of pages in whole file}
{06}NumReloc      : Word; {# of relocation entrys}
{08}HeaderSize  : Word; {size of header, in paragraphs}
{0A}MinHeap, MaxHeap: Word; {min and max # of free paragraphs needed}
    InitSSeg,InitSPtr : Word;
    CheckSum  : Word;
    InitIPtr,InitCSeg : Word;
    OfsReloc  : Word;
    OverlayNum    : Word;
    {Reserved: Array[1..32] of Byte;}
    Reserved  : Array[0..3] of word;
    OEMid         : word;
    OEMinfo   : word;
    Reserved2   : Array[0..9] of word;
    {3C:far Offset to PE header}
    OfstPEheader: LongInt;
  end;

{ Directory format }
  TPEdirectory = record
{00}Rva: LongInt;   { RVA du r‚pertoire                   }
{04}Size: LongInt;{ Taille (en octets) de la section    }
  end;

 TPEheader = record
    {00}Magic : Array[1..4]Of ansichar; { 'PE'+#0#0}
    {04}Machine : word;     { Processeur recquis pour l'exécution }
    {06}NumberOfSections : word;  {~4}
    {08}TimeDateStamp : longint;
    {0C}PointerToSymbolTable : longint;
    {10}NumberOfSymbols : longint;
    {14}SizeOfOptionalHeader : word; {~0E0h}
    {16}Characteristics : word;{
            0001: Relocation info stripped from file.
            0002: File is executable  (i.e. no unresolved externel references).
            0004: Line nunbers stripped from file.
            0008: Local symbols stripped from file.
            0080: Bytes of machine word are reversed.
            0100: 32 bit word machine.
            0200: Debugging info stripped from file in .DBG file
            0400: If Image is on removable media, copy and run from the swap file.
            0800: If Image is on Net, copy and run from the swap file.
            1000: System File.
            2000: File is a DLL.
            4000: File should only be run on a UP machine
            8000: Bytes of machine word are reversed.}
 End;


{Optional header format}
 TPEoptheader = record {Optional header}
    {Standard fields}
    {00}Magic   : Word;{Signature = $010B}
    {02}MajorLinkerVersion : byte;
    {03}MinorLinkerVersion : byte;
    {04}SizeOfCode    : longint;{Memory Size to Allocate for code }
    {08}SizeOfInitializedData     :longint;{Memory Size for init.data}
    {0C}SizeOfUninitializedData :longint;{memory size for uninit.data}
    {10}AddressOfEntryPoint : longint;{entry point relative to imagebase}
    {14}BaseOfCode    :Longint;{offset of code Relative to imagebase}
    {18}BaseOfData    :Longint;{offset of Data Relative to imagebase}
    {NT additional fields}
    {1C}ImageBase     :Longint;{Offset of the image file in RAM (00400000)}
    {20}SectionAlignment:Longint;{RAM Alignement between two adjacent Sections(4096)}
            {This must be a power of 2 between 512 and 256M inclusive. The default is 64K.}
    {24}FileAlignment :Longint;{FILE Alignement between two adjacent Sections }
            {This value should be a power of 2 between 512 and 64K inclusive.}
    {28}MajorOperatingSystemVersion : word;
    {2A}MinorOperatingSystemVersion : word;
    {2C}MajorImageVersion : word;
    {2E}MinorImageVersion : word;
    {30}MajorSubsystemVersion : word;
    {32}MinorSubsystemVersion : word;
    {34}Win32VersionValue : longint;{ ??Win32VersionValue}
    {38}SizeOfImage   : longint;{Total Memory requested for creating the image}
    {3C}SizeOfHeaders : longint;{Size of all headers in the file }
            {The combined size of the Dos Header, PE Header Optional Header and Section Table.}
    {40}CheckSum  : longint;{CRC}
    {44}Subsystem : word;{Subsytem of the Operating System
            0000: Unknown
            0001: Native
            0002: (Gui) Graphic Windows';
            0003: (Cui) Character Console Windows
            0005: OS/2 Character
            0007: POSIX Character }
    {46}DllCharacteristics : word; {Indicates special loader requirements
            0001: Per-Process Library Initialization.
            0002: Per-Process Library Termination.
            0004: Per-Thread Library Initialization.
            0008: Per-Thread Library Termination. }
    {48}SizeOfStackReserve : longint;{ Max Stack Size to Set on reserve }
    {4C}SizeOfStackCommit: longint;{ Initial Committed Stack Size }
    {50}SizeOfHeapReserve: longint;
    {54}SizeOfHeapCommit: longint;
    {58}LoaderFlags   : longint;
    {5C}NumberOfRvaAndSizes: longint; { size of data directory ~ 16 }
    {60}Directory     : Array[0..15] of tPEdirectory;{ 16 directory }
    {E0}
 End;

CONST{ Subsytem Types }
      SUBSYSTEM_WINDOWS_GUI = 2;
      SUBSYSTEM_WINDOWS_CUI = 3;

CONST { Directory Entries}
      IMAGE_DIRECTORY_ENTRY_EXPORT          = 0;  {// Export Directory}
      IMAGE_DIRECTORY_ENTRY_IMPORT          = 1;  {// Import Directory}
      IMAGE_DIRECTORY_ENTRY_RESOURCE        = 2;  {// Resource Directory}
      IMAGE_DIRECTORY_ENTRY_EXCEPTION       = 3;  {// Exception Directory}
      IMAGE_DIRECTORY_ENTRY_SECURITY        = 4;  {// Security Directory}
      IMAGE_DIRECTORY_ENTRY_BASERELOC       = 5;  {// Base Relocation Table}
      IMAGE_DIRECTORY_ENTRY_DEBUG           = 6;  {// Debug Directory}
      IMAGE_DIRECTORY_ENTRY_COPYRIGHT       = 7;  {// Description String}
      IMAGE_DIRECTORY_ENTRY_GLOBALPTR       = 8;  {// Machine Value (MIPS GP)}
      IMAGE_DIRECTORY_ENTRY_TLS             = 9;  {// TLS Directory}
      IMAGE_DIRECTORY_ENTRY_LOADCONFIG      = 10; {// Load Configuration Directory}
      IMAGE_DIRECTORY_ENTRY_BOUNDIMPORT     = 11; {// Bound Import Directory in headers}
      IMAGE_DIRECTORY_ENTRY_IAT             = 12; { Import Address Table}


TYPE
{Section Header}
 PPEsection = ^TPEsection;
 TPEsection = record{Size:$28}
    {00}Name: Array[1..8] of ansiChar;{Section Name, it can be any name}
    {08}VirtualSize :LongInt;{Virtual memory size of the section when it
                          will be loaded, Any difference between PHYSICAL
                          SIZE and VIRTUAL SIZE is zero filled}
    {0C}RelVirtualAddr  :LongInt;{Virtuel Adress of section Relative to imagebase}
    {10}SizeofRawData   :LongInt;{File size of initialized data}
    {14}PtrtoRawData    :LongInt;{Offset in the file to RAW data Physical}
                        {si =0, alors les Donn‚es ne seront}
                        {accessible que Lors de l'ex‚cution}
    {18}PtrtoRelocations:LongInt;
    {1C}PtrtoLineNumbers:LongInt;
    {20}NbrRelocations    :Word;
    {22}NbrLineNumbers    :Word;
    {24}Characteristics :CARDINAL;{
            00000000: Type_Reg
            00000001: Type_DSect
            00000002: Type_NoLoad
            00000004: Type_Group
            00000008: Type_No_Pad
            00000010: Type_Copy
            00000020: Section contains code.
            00000040: Section contains initialized data.
            00000080: Section contains uninitialized data.
            00000100: Lnk_Other
            00000200: Section contains comments or some other type of information.
            00000400: Reserved(Type_Over)
            00000800: Section contents will not become part of image(Lnk_Remove)
            00001000: Section contents comdat.Lnk_ComDat
            00002000: Reserved.
            00004000: Mem_Protected - Obsolete
            00008000: Mem_FarData
            00010000: Mem_SYSHEAP  - Obsolete
            00020000: Mem_Purgeable
            00020000: Mem_16Bit
            00040000: Mem_Locked
            00080000: Mem_Preload
            00100000: Align_1Bytes
            00200000: Align_2Bytes
            00300000: Align_4Bytes
            00400000: Align_8Bytes
            00500000: Default alignment if no others are specified. Align_16Bytes
            00600000: Align_32Bytes
            00700000: Align_64Bytes
            00800000: Unused
            01000000: Section contains extended relocations.
            02000000: Section can be discarded.
            04000000: Section is not cachable.
            08000000: Section is not pageable.
            10000000: Section is shareable.
            20000000: Section is executable.
            40000000: Section is readable.
            80000000: Section is writeable.
            }{
            60000020: Contains code,Executable,Readable = 'code'
            40000040: Contains Initialized Data, Readable ='donn‚es'
            42000040: Contains InitData,discarded,readable .reloc
            50000040: 'donn‚es partag‚es'
            C0000040: 'variables'
            C0000000: 'Lecture + ‚criture'
            E0000020: 'CODE (+ variables)'     }
 End;

CONST{ Section flags }
      SECTION_CODE    = $00000020;{Contains code}
      SECTION_DATA    = $00000040;{Contains initialized data}
      SECTION_BSS     = $00000080;{Contains uninitialized data}
      SECTION_DISCARD = $02000000;{Section Can be discarded}
      SECTION_EXEC    = $20000000;{Section is Executable}
      SECTION_READ    = $40000000;{Section is Readable}
      SECTION_WRITE   = $80000000;{Section is Writeable}

TYPE
{Import Format}

 pImportDirectory = ^tImportDirectory;
 tImportDirectory = record
    {00}Characteristics : LongInt;
    {04}TimeDateStamp   : LongInt;{ 0 }
    {08}ForwarderChain  : LongInt;{ 0 }
    {0C}NameRVA         : LongInt;{ RVA of the Dll asciiz Name }
    {10}LookupTableRVA  : LongInt;{ RVA to the start of the import lookup table }
           { The Import Lookup Table is an array of ordinal or hint/name RVA's for each DLL.
             The last entry is empty (NULL) which indicates the end of the table. }
 End;

 PImportByName = ^TImportByName;
 TImportByName = packed record
    Hint: Word;{ NULL }
    Name: packed array[0..0] of ansiChar;{ Asciiz Name }
 end;

{
  PImageThunkData = ^TImageThunkData;
  TImageThunkData = packed record
    case Integer of
      0: (ForwarderString: PByte);
      1: (_Function:       PDWord); // renamed from Function
      2: (Ordinal:         DWord);
      3: (AddressOfData:   PImageImportByName);
  end;

const
  image_Ordinal_Flag                    = $80000000;

type
  PImageImportDescriptor = ^TImageImportDescriptor;
  TImageImportDescriptor = packed record
    OriginalFirstThunk: PImageThunkData; // RVA to original unbound IAT
    TimeDateStamp:      DWord;           // 0 if not bound,
                                         // -1 if bound, and real date\time stamp
                                         //     in image_Directory_Entry_Bound_Import (new BIND)
                                         // O.W. date/time stamp of DLL bound to (Old BIND)
    ForwarderChain:     DWord;           // -1 if no forwarders
    Name:               DWord;
    FirstThunk:         PImageThunkData; // RVA to IAT (if bound this IAT has actual addresses)
  end;
}




{Export Format}
 tExportDirectory = RECORD
    {00}Characteristics     : LongInt;{ Caract‚ristiques (inutilis‚es = 0)  }
    {04}TimeDateStamp   : LongInt;
    {08}MajorVersion  : Word;
    {0A}MinorVersion  : Word;
    {0C}Name            :Longint;
    {10}Base            :Longint;
    {14}NumFunctions  :Longint;
    {18}NumNames    :Longint;
    {1C}AddrFunctions   :Longint;
    {20}AddrNames     :Longint;
    {24}AddrOrdinals  :Longint;
   end;


{ Relocation format.}
TYPE
 pRelocation = ^tRelocation;
 tRelocation = RECORD
    Case Integer of
      0:( VirtualAddress:Longint;
        SymbolTblIndex:Longint;
        _Type   :Longint ); {renamed form Type}
      1:( RelocCount  :Longint ); {Set to the real count when image_Scn_Lnk_NReloc_Ovfl is set}
 End;




////////////////////////////////////////////////////////////////////////////////
// Resource Format
////////////////////////////////////////////////////////////////////////////////
//
// Resource directory consists of two counts, following by a variable length
// array of directory entries.  The first count is the number of entries at
// beginning of the array that have actual names associated with each entry.
// The entries are in ascending order, case insensitive strings.  The second
// count is the number of entries that immediately follow the named entries.
// This second count identifies the number of entries that have 16-bit integer
// Ids as their name.  These entries are also sorted in ascending order.
//
// This structure allows fast lookup by either name or number, but for any
// given resource entry only one form of lookup is supported, not both.
// This is consistant with the syntax of the .RC file and the .RES file.
//
 tResourceDirectory =  packed record
    {00}Characteristics     : CARDINAL;{ Caract‚ristiques (inutilis‚es = 0)  }
    {04}TimeDateStamp   : CARDINAL;
    {08}MajorVersion  : Word;
    {0A}MinorVersion  : Word;
    {0C}NbrNamedEntry   : Word;{ Nombre d'entr‚es nomm‚es }
    {0E}NbrIndexEntry   : Word;{ Nombre d'entr‚es index‚es }
    // DirectoryEntries: array [0..x] of TResourceDirectoryEntry;
 End;

// Each directory contains the 32-bit Name of the entry and an offset,
// relative to the beginning of the resource directory of the data associated
// with this directory entry. If the name of the entry is an actual text
// string instead of an integer Id, then the high order bit of the name field
// is set to one and the low order 31-bits are an offset, relative to the
// beginning of the resource directory of the string, which is of type
// IMAGE_RESOURCE_DIRECTORY_STRING.  Otherwise the high bit is clear and the
// low-order 16-bits are the integer Id that identify this resource directory
// entry. If the directory entry is yet another resource directory (i.e. a
// subdirectory), then the high order bit of the offset field will be
// set to indicate this.  Otherwise the high bit is clear and the offset
// field points to a resource data entry.
//

  tResourceDirectoryEntry = packed record
    case Integer of
      0: (Name: Cardinal; OffsetToData: Cardinal);
      1: (Index : Word);
  end;


// For resource directory entries that have actual string names, the Name
// field of the directory entry points to an object of the following type.
// All of these string objects are stored together after the last resource
// directory entry and before the first resource data object.  This minimizes
// the impact of these variable length objects on the alignment of the fixed
// size directory entry objects.

  TResourceDirectoryString = packed record
    Length: Word;
    NameString: array of ansiChar;
  end;

  TResourceDirectoryStringU = packed record
    Length: Word;
    NameString: array of Word;
  end;

// Each resource data entry describes a leaf node in the resource directory
// tree.  It contains an offset, relative to the beginning of the resource
// directory of the data for the resource, a size field that gives the number
// of bytes of data at that offset, a CodePage that should be used when
// decoding code point values within the resource data.  Typically for new
// applications the code page would be the unicode code page.

  TResourceDataEntry = packed record
    OffsetToData: Cardinal;
    Size:         Cardinal;
    CodePage:     Cardinal;
    Reserved:     Cardinal;
  end;

{ Predefined resource types }
const
  RESOURCE_CURSOR       = $0001;  // Cursor image (one entry in a cursor group)
  RESOURCE_BITMAP       = $0002;  // Bitmap (Windows or OS/2 BMP format)
  RESOURCE_ICON         = $0003;  // Icon image (one entry in an icon group)
  RESOURCE_MENU         = $0004;
  RESOURCE_DIALOG       = $0005;  // Dialog box
  RESOURCE_STRING       = $0006;  // String table (must have numeric identifier, not textual)
  RESOURCE_FONTDIR      = $0007;  // Font directory
  RESOURCE_FONT         = $0008;  // Font entry
  RESOURCE_ACCELERATORS = $0009;  // Keyboard accelerator table
  RESOURCE_RCDATA       = $000A;  // Application-defined data
  RESOURCE_MESSAGETABLE = $000B;  // (win32)Message table
  RESOURCE_GROUP_CURSOR = $000C;  // Group header for a cursor
  RESOURCE_GROUP_ICON   = $000E;  // Group header for an icon
  RESOURCE_VERSION      = $0010;  // (win32)Version information
  RESOURCE_DLGINCLUDE   = $0011;  // (win32)Dialog include
  RESOURCE_PLUGPLAY     = $0013;  // (win32)Plug and play
  RESOURCE_VXD          = $0014;  // (win32)VxD
  RESOURCE_ANICURSOR    = $0015;  // (win32)Animated cursor
  RESOURCE_NEWRESOURCE  = $2000;
  RESOURCE_ERROR        = $7FFF;
  RESOURCE_NEWBITMAP    = (RESOURCE_BITMAP or RESOURCE_NEWRESOURCE);
  RESOURCE_NEWMENU      = (RESOURCE_MENU or RESOURCE_NEWRESOURCE);
  RESOURCE_NEWDIALOG    = (RESOURCE_DIALOG or RESOURCE_NEWRESOURCE);

implementation

end.
