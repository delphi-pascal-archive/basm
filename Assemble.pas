unit Assemble;

interface

uses
  Convert, ObjCodeDump;

type
  TDataSize = (
    SIZE_NONE,
    SIZE_SBYTE,
    SIZE_BYTE,
    SIZE_SWORD,
    SIZE_WORD,
    SIZE_SDWORD,
    SIZE_DWORD,

    SIZE_QWORD,
    SIZE_TWORD,
    SIZE_SHORT,
    SIZE_NEAR,
    SIZE_FAR
  );

  TAsmPrefix = (
    PRE_NONE,
    PRE_LOCK,
    PRE_REPNZ, PRE_REPNE, PRE_REP, PRE_REPE, PRE_REPZ,
    PRE_SEGES, PRE_SEGCS, PRE_SEGSS, PRE_SEGDS, PRE_SEGFS, PRE_SEGGS
  );

  TAsmOpcode = (
    OP_NONE,
    OP_MOV,
    OP_ADD, OP_OR, OP_ADC, OP_SBB, OP_AND, OP_SUB, OP_XOR, OP_CMP,
    OP_ROL, OP_ROR, OP_RCL, OP_RCR, OP_SHL, OP_SHR, OP_SAL, OP_SAR,
    OP_NOT, OP_NEG, OP_MUL, OP_IMUL, OP_DIV, OP_IDIV, OP_TEST,
    OP_INC, OP_DEC, OP_CALL, OP_JMP, OP_PUSH, OP_POP,
    OP_JO, OP_JNO, OP_JB, OP_JC, OP_JNAE, OP_JNB, OP_JNC, OP_JAE, OP_JZ, OP_JE, OP_JNZ,
    OP_JNE, OP_JNA, OP_JBE, OP_JA, OP_JNBE, OP_JS, OP_JNS, OP_JP, OP_JPE, OP_JNP,
    OP_JPO, OP_JNGE, OP_JL, OP_JNL, OP_JGE, OP_JNG, OP_JLE, OP_JG, OP_JNLE,
    OP_RET, OP_RETN, OP_RETF, OP_INT3, OP_INT, OP_INTO, OP_IRET,
    OP_DAA, OP_DAS, OP_AAA, OP_AAS, OP_AAD, OP_AAM,
    OP_HLT, OP_CMC, OP_CLC, OP_STC, OP_CLI, OP_STI, OP_CLD, OP_STD,
    OP_MOVSB, OP_MOVSW, OP_CMPSB, OP_CMPSW,
    OP_STOSB, OP_STOSW, OP_LODSB, OP_LODSW,
    OP_SCASB, OP_SCASW,
    OP_NOP, OP_XCHG, OP_CBW, OP_CWD, OP_WAIT, OP_SAHF, OP_LAHF,
    OP_PUSHA, OP_POPA, OP_PUSHF, OP_POPF,
    OP_LEA, OP_LES, OP_LDS,
    OP_LOOPNZ, OP_LOOPNE, OP_LOOPE, OP_LOOPZ, OP_LOOP, OP_JCXZ, OP_IN, OP_OUT,
    OP_XLAT,
    // '286 instructions
    OP_BOUND, OP_ARPL, OP_LEAVE, OP_ENTER,
    OP_INS, OP_INSB, OP_INSW, OP_OUTS, OP_OUTSB, OP_OUTSW,
    // '386 instructions
    OP_SHLD, OP_SHRD, OP_IRETD,
    OP_MOVSD, OP_CMPSD, OP_STOSD, OP_LODSD, OP_SCASD, OP_INSD, OP_OUTSD,
    OP_CWDE, OP_CDQ,
    OP_PUSHAD, OP_POPAD, OP_PUSHFD, OP_POPFD,
    OP_LFS, OP_LGS, OP_JECXZ,
    OP_VERR, OP_VERW,
    OP_LAR, OP_LSL,
    OP_BSF, OP_BSR, OP_BT, OP_BTC, OP_BTR, OP_BTS
  );

const
  ASM_PREFIX_NAME: array [TAsmPrefix] of string = (
    '----', 'LOCK',
    'REPNZ', 'REPNE', 'REP', 'REPE', 'REPZ',
    'SEGES', 'SEGCS', 'SEGSS', 'SEGDS', 'SEGFS', 'SEGGS'
  );

  ASM_PREFIX_CODE: array [TAsmPrefix] of Byte = (
    $90,
    $F0, //LOCK prefix.
    $F2, $F2, //REPNE/REPNZ prefix
    $F3, $F3, $F3, //REP/REPE/REPZ prefix
    $26, //ES segment override prefix.
    $2E, //CS segment override prefix.
    $36, //SS segment override prefix.
    $3E, //DS segment override prefix.
    $64, //FS segment override prefix.
    $65  //GS segment override prefix.
  );

  ASM_OPCODE_NAME: array [TAsmOpcode] of string = (
    '',
    'MOV',
    'ADD', 'OR', 'ADC', 'SBB', 'AND', 'SUB', 'XOR', 'CMP',
    'ROL', 'ROR', 'RCL', 'RCR', 'SHL', 'SHR', 'SAL', 'SAR',
    'NOT', 'NEG', 'MUL', 'IMUL', 'DIV', 'IDIV', 'TEST',
    'INC', 'DEC', 'CALL', 'JMP', 'PUSH', 'POP',
    'JO', 'JNO', 'JB', 'JC', 'JNAE', 'JNB', 'JNC', 'JAE', 'JZ', 'JE', 'JNZ',
    'JNE', 'JNA', 'JBE', 'JA', 'JNBE', 'JS', 'JNS', 'JP', 'JPE', 'JNP',
    'JPO', 'JNGE', 'JL', 'JNL', 'JGE', 'JNG', 'JLE', 'JG', 'JNLE',
    'RET', 'RETN', 'RETF', 'INT3', 'INT', 'INTO', 'IRET',
    'DAA', 'DAS', 'AAA', 'AAS', 'AAD', 'AAM',
    'HLT', 'CMC', 'CLC', 'STC', 'CLI', 'STI', 'CLD', 'STD',
    'MOVSB', 'MOVSW', 'CMPSB', 'CMPSW',
    'STOSB', 'STOSW', 'LODSB', 'LODSW',
    'SCASB', 'SCASW',
    'NOP', 'XCHG', 'CBW', 'CWD', 'WAIT', 'SAHF', 'LAHF',
    'PUSHA', 'POPA', 'PUSHF', 'POPF',
    'LEA', 'LES', 'LDS',
    'LOOPNZ', 'LOOPNE', 'LOOPE', 'LOOPZ', 'LOOP', 'JCXZ', 'IN', 'OUT',
    'XLAT',
    {286}
    'BOUND', 'ARPL', 'LEAVE', 'ENTER',
    'INS', 'INSB', 'INSW', 'OUTS', 'OUTSB', 'OUTSW',
    {386}
    'SHLD', 'SHRD', 'IRETD',
    'MOVSD', 'CMPSD', 'STOSD', 'LODSD', 'SCASD', 'INSD', 'OUTSD',
    'CWDE', 'CDQ',
    'PUSHAD', 'POPAD', 'PUSHFD', 'POPFD',
    'LFS', 'LGS', 'JECXZ',
    'VERR', 'VERW',
    'LAR', 'LSL',
    'BSF', 'BSR', 'BT', 'BTC', 'BTR', 'BTS'
 );

type
  TAsmRegister = (
    REG_NONE,
    REG_AL, REG_CL, REG_DL, REG_BL, REG_AH, REG_CH, REG_DH, REG_BH,
    REG_AX, REG_CX, REG_DX, REG_BX, REG_SP, REG_BP, REG_SI, REG_DI,
    REG_EAX,REG_ECX,REG_EDX,REG_EBX,REG_ESP,REG_EBP,REG_ESI,REG_EDI,
    REG_ES, REG_CS, REG_SS, REG_DS,
    REG_ST0, REG_ST1, REG_ST2, REG_ST3, REG_ST4, REG_ST5, REG_ST6, REG_ST7
  );

const
  AsmRegisterName : Array[TAsmRegister] of String[3] = (
    '',
    'AL', 'CL', 'DL', 'BL', 'AH', 'CH', 'DH', 'BH',
    'AX', 'CX', 'DX', 'BX', 'SP', 'BP', 'SI', 'DI',
    'EAX', 'ECX', 'EDX', 'EBX', 'ESP', 'EBP', 'ESI', 'EDI',
    'ES', 'CS', 'SS', 'DS',
    'ST0', 'ST1', 'ST2', 'ST3', 'ST4', 'ST5', 'ST6', 'ST7'
  );

  AsmRegisterValue : Array[TAsmRegister] of Byte = (
    0,
    0, 1, 2, 3, 4, 5, 6, 7,
    0, 1, 2, 3, 4, 5, 6, 7,
    0, 1, 2, 3, 4, 5, 6, 7,
    0, 1, 2, 3,
    0, 1, 2, 3, 4, 5, 6, 7
  );

type
  TRefKind = (REF_NONE, REF_FIXUP, REF_OFS, REF_NEAR, REF_SHORT, REF_FAR);
  TOperandLocation = (
    LOC_NONE,
    LOC_REGISTER,
    LOC_IMMEDIATE,
    LOC_MEMORY
  );

  TAsmOperand = packed record
    Size    : TDataSize;
    RefKind	: TRefKind;
    RefSym  : Pointer;
    case Loc: TOperandLocation of
      LOC_IMMEDIATE : ( Value : Longint );
      LOC_REGISTER  : ( Reg   : TAsmRegister );
      LOC_MEMORY    : ( Base  : TAsmRegister;
                        Index : TAsmRegister;
                        Scale : byte;
                        Offset: longint;
                       );
  end;

type { Instruction Flags }
  TInstructionFlags = (
    FLAG_NONE,   // No flags }
    FLAG_OPCODE1=FLAG_NONE,   // Opcode Size = 1 byte
    FLAG_OPCODE2,   // Opcode Size = 2 bytes
    FLAG_OPCODRM,   // Add data opcode to ModRM byte

    // Data Size Flags
    FLAG_BYTE,      // 1 byte size
    FLAG_BYTE2,     // 2 bytes size
    FLAG_BYTE4,     // 4 bytes size

    FLAG_SIZE0,     // Set flag Size of operand, Bit[0] of the 'first' opcode byte:( 0=byte size, 1=word size )
    FLAG_SIZE3,     // Set flag Size of operand, Bit[3] of the 'first' opcode byte:( 0=byte size, 1=word size )

    FLAG_SIGN1,     // Sign extention of immediate, Bit[1] of the opcode byte, 0=unsigned/1=signed

    FLAG_SIZEOVERRIDE,   // Use Operand-size override. Control the dump of PrefixOperandSize
    FLAG_ADDROVERRIDE    // Use Address-size override. Control the dump of PrefixAddressSize
  );

  TTableOperandLocation = (
    coNONE, {No operand}
    // Register
    coREG,    //   Integer register in Reg field used with MRG(?? XXX ???) in ModRM byte
    coREGCMD, // Integer register in Opcode byte (?? ??? XXX)
    coWRGCMD, // D-Word register in Opcode byte (?? ??? XXX)
    coRACC,   // Accumulator (AL/E-AX)
    coWRACC,  // Word Size Accumulator (E-AX)
    coREGAX,  // Word Size Accumulator
    coREGDX,  // Register DX (16-bit implicit port address)
    coREGCL,  // Implicit CL register (for shifts)
    coSEG,    // Sergment register in Reg field used with MRG(?? XXX ???) in ModRM byte
    coSEGES,  // Segment ES
    coSEGCS,  // Segment CS
    coSEGSS,  // Segment SS
    coSEGDS,  // Segment DS
    coSEGFS,  // Segment DS
    coSEGGS,  // Segment DS
    // Memory/Register
    coMEM,    // Memory reference in ModRM byte '[...]'
    coMEMREG, {Memory/register in ModRM byte '[...]' }
    coMEMREGB, {Memory/register in ModRM byte '[...]' }
    coMEMREGW, {Memory/register in ModRM byte '[...]' }
    coMOFFS,
    // Immediate
    coIMM,    // Immediate data (8 or 16/32) flag dependent
    coIMM8,   // Immediate byte
    coIMM16,
    coIMMW,   // Immediate word (16/32)
    coCONST1, {Implicit constant 1 (for shifts)}
    // JUMPS
    coJMR, {Memory/reg in ModRM as JUMP target}
    coJAN, { NEAR Immediate absolute near data address}
    coJRS, { SHORT Immediate relative byte offset (for jumps)}
    coJRN, { NEAR Immediate relative full offset (for jumps)}
    coJAF  { FAR Immediate absolute far jump/call addr}

  );

  TTableInstruction = packed record
    C   : Word;  // Opcode
    RM  : Byte;
    O   : TAsmOpcode; { Opcode Name Value }
    O1,
    O2,
    O3  : TTableOperandLocation; // Operands
    F   : set of TInstructionFlags; { Instruction flags }
  end;

const
  INSTR_COUNT = 220;
  INSTR_TABLE: array [0..INSTR_COUNT] of TTableInstruction = (
    ( C: $0000; RM: $00; O: OP_ADD;   O1: coMEMREG; O2: coREG;    O3: coNONE;  F: [FLAG_SIZE0, FLAG_SIZEOVERRIDE] ),
    ( C: $0002; RM: $00; O: OP_ADD;   O1: coREG;    O2: coMEMREG; O3: coNONE;  F: [FLAG_SIZE0, FLAG_SIZEOVERRIDE] ),
    ( C: $0004; RM: $00; O: OP_ADD;   O1: coRACC;   O2: coIMM;    O3: coNONE;  F: [FLAG_SIZE0, FLAG_SIZEOVERRIDE] ),
    ( C: $0006; RM: $00; O: OP_PUSH;  O1: coSEGES;  O2: coNONE;   O3: coNONE;  F: [FLAG_NONE] ),
    ( C: $0007; RM: $00; O: OP_POP;   O1: coSEGES;  O2: coNONE;   O3: coNONE;  F: [FLAG_NONE] ),
    ( C: $0008; RM: $00; O: OP_OR;    O1: coMEMREG; O2: coREG;    O3: coNONE;  F: [FLAG_SIZE0, FLAG_SIZEOVERRIDE] ),
    ( C: $000A; RM: $00; O: OP_OR;    O1: coREG;    O2: coMEMREG; O3: coNONE;  F: [FLAG_SIZE0, FLAG_SIZEOVERRIDE] ),
    ( C: $000C; RM: $00; O: OP_OR;    O1: coRACC;   O2: coIMM;    O3: coNONE;  F: [FLAG_SIZE0, FLAG_SIZEOVERRIDE] ),
    ( C: $000E; RM: $00; O: OP_PUSH;  O1: coSEGCS;  O2: coNONE;   O3: coNONE;  F: [FLAG_NONE]; ),
  // 0f
    ( C: $800F; RM: $00; O: OP_JO;    O1: coJRN;    O2: coNONE;   O3: coNONE;  F: [FLAG_OPCODE2] ),
    ( C: $810F; RM: $00; O: OP_JNO;   O1: coJRN;    O2: coNONE;   O3: coNONE;  F: [FLAG_OPCODE2] ),
    ( C: $820F; RM: $00; O: OP_JB;    O1: coJRN;    O2: coNONE;   O3: coNONE;  F: [FLAG_OPCODE2] ),
    ( C: $820F; RM: $00; O: OP_JC;    O1: coJRN;    O2: coNONE;   O3: coNONE;  F: [FLAG_OPCODE2] ),
    ( C: $820F; RM: $00; O: OP_JNAE;  O1: coJRN;    O2: coNONE;   O3: coNONE;  F: [FLAG_OPCODE2] ),
    ( C: $830F; RM: $00; O: OP_JNB;   O1: coJRN;    O2: coNONE;   O3: coNONE;  F: [FLAG_OPCODE2] ),
    ( C: $830F; RM: $00; O: OP_JNC;   O1: coJRN;    O2: coNONE;   O3: coNONE;  F: [FLAG_OPCODE2] ),
    ( C: $840F; RM: $00; O: OP_JZ;    O1: coJRN;    O2: coNONE;   O3: coNONE;  F: [FLAG_OPCODE2] ),
    ( C: $840F; RM: $00; O: OP_JE;    O1: coJRN;    O2: coNONE;   O3: coNONE;  F: [FLAG_OPCODE2] ),
    ( C: $850F; RM: $00; O: OP_JNZ;   O1: coJRN;    O2: coNONE;   O3: coNONE;  F: [FLAG_OPCODE2] ),
    ( C: $850F; RM: $00; O: OP_JNE;   O1: coJRN;    O2: coNONE;   O3: coNONE;  F: [FLAG_OPCODE2] ),
    ( C: $860F; RM: $00; O: OP_JBE;   O1: coJRN;    O2: coNONE;   O3: coNONE;  F: [FLAG_OPCODE2] ),
    ( C: $860F; RM: $00; O: OP_JNA;   O1: coJRN;    O2: coNONE;   O3: coNONE;  F: [FLAG_OPCODE2] ),
    ( C: $870F; RM: $00; O: OP_JA;    O1: coJRN;    O2: coNONE;   O3: coNONE;  F: [FLAG_OPCODE2] ),
    ( C: $870F; RM: $00; O: OP_JNBE;  O1: coJRN;    O2: coNONE;   O3: coNONE;  F: [FLAG_OPCODE2] ),
    ( C: $880F; RM: $00; O: OP_JS;    O1: coJRN;    O2: coNONE;   O3: coNONE;  F: [FLAG_OPCODE2] ),
    ( C: $890F; RM: $00; O: OP_JNS;   O1: coJRN;    O2: coNONE;   O3: coNONE;  F: [FLAG_OPCODE2] ),
    ( C: $8A0F; RM: $00; O: OP_JP;    O1: coJRN;    O2: coNONE;   O3: coNONE;  F: [FLAG_OPCODE2] ),
    ( C: $8A0F; RM: $00; O: OP_JPE;   O1: coJRN;    O2: coNONE;   O3: coNONE;  F: [FLAG_OPCODE2] ),
    ( C: $8B0F; RM: $00; O: OP_JPO;   O1: coJRN;    O2: coNONE;   O3: coNONE;  F: [FLAG_OPCODE2] ),
    ( C: $8B0F; RM: $00; O: OP_JNP;   O1: coJRN;    O2: coNONE;   O3: coNONE;  F: [FLAG_OPCODE2] ),
    ( C: $8C0F; RM: $00; O: OP_JL;    O1: coJRN;    O2: coNONE;   O3: coNONE;  F: [FLAG_OPCODE2] ),
    ( C: $8C0F; RM: $00; O: OP_JNGE;  O1: coJRN;    O2: coNONE;   O3: coNONE;  F: [FLAG_OPCODE2] ),
    ( C: $8D0F; RM: $00; O: OP_JGE;   O1: coJRN;    O2: coNONE;   O3: coNONE;  F: [FLAG_OPCODE2] ),
    ( C: $8D0F; RM: $00; O: OP_JNL;   O1: coJRN;    O2: coNONE;   O3: coNONE;  F: [FLAG_OPCODE2] ),
    ( C: $8E0F; RM: $00; O: OP_JLE;   O1: coJRS;    O2: coNONE;   O3: coNONE;  F: [FLAG_OPCODE2] ),
    ( C: $8E0F; RM: $00; O: OP_JNG;   O1: coJRS;    O2: coNONE;   O3: coNONE;  F: [FLAG_OPCODE2] ),
    ( C: $8F0F; RM: $00; O: OP_JG;    O1: coJRS;    O2: coNONE;   O3: coNONE;  F: [FLAG_OPCODE2] ),
    ( C: $8F0F; RM: $00; O: OP_JNLE;  O1: coJRS;    O2: coNONE;   O3: coNONE;  F: [FLAG_OPCODE2] ),

    ( C: $A00F; RM: $00; O: OP_PUSH;  O1: coSEGFS;  O2: coNONE;   O3: coNONE;  F: [FLAG_NONE, FLAG_OPCODE2];  ),
    ( C: $AF0F; RM: $00; O: OP_PUSH;  O1: coSEGGS;  O2: coNONE;   O3: coNONE;  F: [FLAG_NONE, FLAG_OPCODE2];  ),

    ( C: $0010; RM: $00; O: OP_ADC;   O1: coMEMREG; O2: coREG;    O3: coNONE;  F: [FLAG_SIZE0, FLAG_SIZEOVERRIDE] ),
    ( C: $0012; RM: $00; O: OP_ADC;   O1: coREG;    O2: coMEMREG; O3: coNONE;  F: [FLAG_SIZE0, FLAG_SIZEOVERRIDE] ),
    ( C: $0014; RM: $00; O: OP_ADC;   O1: coRACC;   O2: coIMM;    O3: coNONE;  F: [FLAG_SIZE0, FLAG_SIZEOVERRIDE] ),
    ( C: $0016; RM: $00; O: OP_PUSH;  O1: coSEGSS;  O2: coNONE;   O3: coNONE;  F: [FLAG_NONE];  ),
    ( C: $0017; RM: $00; O: OP_POP;   O1: coSEGSS;  O2: coNONE;   O3: coNONE;  F: [FLAG_NONE] ),
    ( C: $0018; RM: $00; O: OP_SBB;   O1: coMEMREG; O2: coREG;    O3: coNONE;  F: [FLAG_SIZE0, FLAG_SIZEOVERRIDE] ),
    ( C: $001A; RM: $00; O: OP_SBB;   O1: coREG;    O2: coMEMREG; O3: coNONE;  F: [FLAG_SIZE0, FLAG_SIZEOVERRIDE] ),
    ( C: $001C; RM: $00; O: OP_SBB;   O1: coRACC;   O2: coIMM;    O3: coNONE;  F: [FLAG_SIZE0, FLAG_SIZEOVERRIDE] ),
    ( C: $001E; RM: $00; O: OP_PUSH;  O1: coSEGDS;  O2: coNONE;   O3: coNONE;  F: [FLAG_NONE];  ),
    ( C: $001F; RM: $00; O: OP_POP;   O1: coSEGDS;  O2: coNONE;   O3: coNONE;  F: [FLAG_NONE] ),
    ( C: $0020; RM: $00; O: OP_AND;   O1: coMEMREG; O2: coREG;    O3: coNONE;  F: [FLAG_SIZE0, FLAG_SIZEOVERRIDE] ),
    ( C: $0022; RM: $00; O: OP_AND;   O1: coREG;    O2: coMEMREG; O3: coNONE;  F: [FLAG_SIZE0, FLAG_SIZEOVERRIDE] ),
    ( C: $0024; RM: $00; O: OP_AND;   O1: coRACC;   O2: coIMM;    O3: coNONE;  F: [FLAG_SIZE0, FLAG_SIZEOVERRIDE] ),
    ( C: $0027; RM: $00; O: OP_DAA;   O1: coNONE;   O2: coNONE;   O3: coNONE;  F: [FLAG_NONE] ),
    ( C: $0028; RM: $00; O: OP_SUB;   O1: coMEMREG; O2: coREG;    O3: coNONE;  F: [FLAG_SIZE0, FLAG_SIZEOVERRIDE] ),
    ( C: $002A; RM: $00; O: OP_SUB;   O1: coREG;    O2: coMEMREG; O3: coNONE;  F: [FLAG_SIZE0, FLAG_SIZEOVERRIDE] ),
    ( C: $002C; RM: $00; O: OP_SUB;   O1: coRACC;   O2: coIMM;    O3: coNONE;  F: [FLAG_SIZE0, FLAG_SIZEOVERRIDE] ),
    ( C: $002F; RM: $00; O: OP_DAS;   O1: coNONE;   O2: coNONE;   O3: coNONE;  F: [FLAG_NONE] ),
    ( C: $0030; RM: $00; O: OP_XOR;   O1: coMEMREG; O2: coREG;    O3: coNONE;  F: [FLAG_SIZE0, FLAG_SIZEOVERRIDE] ),
    ( C: $0032; RM: $00; O: OP_XOR;   O1: coREG;    O2: coMEMREG; O3: coNONE;  F: [FLAG_SIZE0, FLAG_SIZEOVERRIDE]),
    ( C: $0034; RM: $00; O: OP_XOR;   O1: coRACC;   O2: coIMM;    O3: coNONE;  F: [FLAG_SIZE0, FLAG_SIZEOVERRIDE] ),
    ( C: $0037; RM: $00; O: OP_AAA;   O1: coNONE;   O2: coNONE;   O3: coNONE;  F: [FLAG_NONE] ),
    ( C: $0038; RM: $00; O: OP_CMP;   O1: coMEMREG; O2: coREG;    O3: coNONE;  F: [FLAG_SIZE0, FLAG_SIZEOVERRIDE] ),
    ( C: $003A; RM: $00; O: OP_CMP;   O1: coREG;    O2: coMEMREG; O3: coNONE;  F: [FLAG_SIZE0, FLAG_SIZEOVERRIDE] ),
    ( C: $003C; RM: $00; O: OP_CMP;   O1: coRACC;   O2: coIMM;    O3: coNONE;  F: [FLAG_SIZE0, FLAG_SIZEOVERRIDE] ),
    ( C: $003F; RM: $00; O: OP_AAS;   O1: coNONE;   O2: coNONE;   O3: coNONE;  F: [FLAG_NONE] ),
    ( C: $0040; RM: $00; O: OP_INC;   O1: coWRGCMD; O2: coNONE;   O3: coNONE;  F: [FLAG_SIZEOVERRIDE] ),
    ( C: $0048; RM: $00; O: OP_DEC;   O1: coWRGCMD; O2: coNONE;   O3: coNONE;  F: [FLAG_SIZEOVERRIDE] ),
    ( C: $0050; RM: $00; O: OP_PUSH;  O1: coWRGCMD; O2: coNONE;   O3: coNONE;  F: [FLAG_SIZEOVERRIDE] ),
    ( C: $0058; RM: $00; O: OP_POP;   O1: coWRGCMD; O2: coNONE;   O3: coNONE;  F: [FLAG_SIZEOVERRIDE] ),
    ( C: $0060; RM: $00; O: OP_PUSHA; O1: coNONE;   O2: coNONE;   O3: coNONE;  F: [FLAG_NONE] ),
    ( C: $0060; RM: $00; O: OP_PUSHAD;O1: coNONE;   O2: coNONE;   O3: coNONE;  F: [FLAG_NONE] ),
    ( C: $0061; RM: $00; O: OP_POPA;  O1: coNONE;   O2: coNONE;   O3: coNONE;  F: [FLAG_NONE] ),
    ( C: $0061; RM: $00; O: OP_POPAD; O1: coNONE;   O2: coNONE;   O3: coNONE;  F: [FLAG_NONE] ),
    ( C: $006A; RM: $00; O: OP_PUSH;  O1: coIMM8;   O2: coNONE;   O3: coNONE;  F: [FLAG_NONE]),
    ( C: $0068; RM: $00; O: OP_PUSH;  O1: coIMMW;   O2: coNONE;   O3: coNONE;  F: [FLAG_SIZEOVERRIDE] ),
  //( C: $0069; RM: $00; O:OP_IMUL; O1:coREG;   O2:coMEMREG;O3:coIMM ),
  //( C: $006B; RM: $00; F:FLAG_NONE; O1:coREG;O2:coMEMREG;O3:coIMM; O:OP_IMUL ),
    ( C: $0070; RM: $00; O: OP_JO;    O1: coJRS;    O2: coNONE;   O3: coNONE;  F: [FLAG_NONE] ),
    ( C: $0071; RM: $00; O: OP_JNO;   O1: coJRS;    O2: coNONE;   O3: coNONE;  F: [FLAG_NONE] ),
    ( C: $0072; RM: $00; O: OP_JB;    O1: coJRS;    O2: coNONE;   O3: coNONE;  F: [FLAG_NONE] ),
    ( C: $0072; RM: $00; O: OP_JC;    O1: coJRS;    O2: coNONE;   O3: coNONE;  F: [FLAG_NONE] ),
    ( C: $0072; RM: $00; O: OP_JNAE;  O1: coJRS;    O2: coNONE;   O3: coNONE;  F: [FLAG_NONE] ),
    ( C: $0073; RM: $00; O: OP_JNB;   O1: coJRS;    O2: coNONE;   O3: coNONE;  F: [FLAG_NONE] ),
    ( C: $0073; RM: $00; O: OP_JNC;   O1: coJRS;    O2: coNONE;   O3: coNONE;  F: [FLAG_NONE] ),
    ( C: $0074; RM: $00; O: OP_JZ;    O1: coJRS;    O2: coNONE;   O3: coNONE;  F: [FLAG_NONE] ),
    ( C: $0074; RM: $00; O: OP_JE;    O1: coJRS;    O2: coNONE;   O3: coNONE;  F: [FLAG_NONE] ),
    ( C: $0075; RM: $00; O: OP_JNZ;   O1: coJRS;    O2: coNONE;   O3: coNONE;  F: [FLAG_NONE] ),
    ( C: $0075; RM: $00; O: OP_JNE;   O1: coJRS;    O2: coNONE;   O3: coNONE;  F: [FLAG_NONE] ),
    ( C: $0076; RM: $00; O: OP_JBE;   O1: coJRS;    O2: coNONE;   O3: coNONE;  F: [FLAG_NONE] ),
    ( C: $0076; RM: $00; O: OP_JNA;   O1: coJRS;    O2: coNONE;   O3: coNONE;  F: [FLAG_NONE] ),
    ( C: $0077; RM: $00; O: OP_JA;    O1: coJRS;    O2: coNONE;   O3: coNONE;  F: [FLAG_NONE] ),
    ( C: $0077; RM: $00; O: OP_JNBE;  O1: coJRS;    O2: coNONE;   O3: coNONE;  F: [FLAG_NONE] ),
    ( C: $0078; RM: $00; O: OP_JS;    O1: coJRS;    O2: coNONE;   O3: coNONE;  F: [FLAG_NONE] ),
    ( C: $0079; RM: $00; O: OP_JNS;   O1: coJRS;    O2: coNONE;   O3: coNONE;  F: [FLAG_NONE] ),
    ( C: $007A; RM: $00; O: OP_JP;    O1: coJRS;    O2: coNONE;   O3: coNONE;  F: [FLAG_NONE] ),
    ( C: $007A; RM: $00; O: OP_JPE;   O1: coJRS;    O2: coNONE;   O3: coNONE;  F: [FLAG_NONE] ),
    ( C: $007B; RM: $00; O: OP_JPO;   O1: coJRS;    O2: coNONE;   O3: coNONE;  F: [FLAG_NONE] ),
    ( C: $007B; RM: $00; O: OP_JNP;   O1: coJRS;    O2: coNONE;   O3: coNONE;  F: [FLAG_NONE] ),
    ( C: $007C; RM: $00; O: OP_JL;    O1: coJRS;    O2: coNONE;   O3: coNONE;  F: [FLAG_NONE] ),
    ( C: $007C; RM: $00; O: OP_JNGE;  O1: coJRS;    O2: coNONE;   O3: coNONE;  F: [FLAG_NONE] ),
    ( C: $007D; RM: $00; O: OP_JGE;   O1: coJRS;    O2: coNONE;   O3: coNONE;  F: [FLAG_NONE] ),
    ( C: $007D; RM: $00; O: OP_JNL;   O1: coJRS;    O2: coNONE;   O3: coNONE;  F: [FLAG_NONE] ),
    ( C: $007E; RM: $00; O: OP_JLE;   O1: coJRS;    O2: coNONE;   O3: coNONE;  F: [FLAG_NONE] ),
    ( C: $007E; RM: $00; O: OP_JNG;   O1: coJRS;    O2: coNONE;   O3: coNONE;  F: [FLAG_NONE] ),
    ( C: $007F; RM: $00; O: OP_JG;    O1: coJRS;    O2: coNONE;   O3: coNONE;  F: [FLAG_NONE] ),
    ( C: $007F; RM: $00; O: OP_JNLE;  O1: coJRS;    O2: coNONE;   O3: coNONE;  F: [FLAG_NONE] ),
    ( C: $0080; RM: $00; O: OP_ADD;   O1: coMEMREG; O2: coIMM;    O3: coNONE;  F: [FLAG_SIZE0, FLAG_SIZEOVERRIDE, FLAG_OPCODRM] ),
    ( C: $0880; RM: $08; O: OP_OR;    O1: coMEMREG; O2: coIMM;    O3: coNONE;  F: [FLAG_SIZE0, FLAG_SIZEOVERRIDE, FLAG_OPCODRM] ),
    ( C: $1080; RM: $10; O: OP_ADC;   O1: coMEMREG; O2: coIMM;    O3: coNONE;  F: [FLAG_SIZE0, FLAG_SIZEOVERRIDE, FLAG_OPCODRM] ),
    ( C: $1880; RM: $18; O: OP_SBB;   O1: coMEMREG; O2: coIMM;    O3: coNONE;  F: [FLAG_SIZE0, FLAG_SIZEOVERRIDE, FLAG_OPCODRM] ),
    ( C: $2080; RM: $20; O: OP_AND;   O1: coMEMREG; O2: coIMM;    O3: coNONE;  F: [FLAG_SIZE0, FLAG_SIZEOVERRIDE, FLAG_OPCODRM] ),
    ( C: $2880; RM: $28; O: OP_SUB;   O1: coMEMREG; O2: coIMM;    O3: coNONE;  F: [FLAG_SIZE0, FLAG_SIZEOVERRIDE, FLAG_OPCODRM] ),
    ( C: $3080; RM: $30; O: OP_XOR;   O1: coMEMREG; O2: coIMM;    O3: coNONE;  F: [FLAG_SIZE0, FLAG_SIZEOVERRIDE, FLAG_OPCODRM] ),
    ( C: $3880; RM: $38; O: OP_CMP;   O1: coMEMREG; O2: coIMM;    O3: coNONE;  F: [FLAG_SIZE0, FLAG_SIZEOVERRIDE, FLAG_OPCODRM] ),
    ( C: $0084; RM: $00; O: OP_TEST;  O1: coMEMREG; O2: coREG;    O3: coNONE;  F: [FLAG_SIZE0, FLAG_SIZEOVERRIDE] ),
    ( C: $0086; RM: $00; O: OP_XCHG;  O1: coREG;    O2: coMEMREG; O3: coNONE;  F: [FLAG_SIZE0, FLAG_SIZEOVERRIDE] ),
    ( C: $0086; RM: $00; O: OP_XCHG;  O1: coMEMREG; O2: coREG;    O3: coNONE;  F: [FLAG_SIZE0, FLAG_SIZEOVERRIDE] ),
    ( C: $0088; RM: $00; O: OP_MOV;   O1: coMEMREG; O2: coREG;    O3: coNONE;  F: [FLAG_SIZE0, FLAG_SIZEOVERRIDE] ),
    ( C: $008A; RM: $00; O: OP_MOV;   O1: coREG;    O2: coMEMREG; O3: coNONE;  F: [FLAG_SIZE0, FLAG_SIZEOVERRIDE] ),
    ( C: $008C; RM: $00; O: OP_MOV;   O1: coMEMREG; O2: coSEG;    O3: coNONE;  F: [FLAG_SIZEOVERRIDE] ),
    ( C: $008D; RM: $00; O: OP_LEA;   O1: coREG;    O2: coMEM;    O3: coNONE;  F: [FLAG_SIZEOVERRIDE] ),

    ( C: $008D; RM: $05; O: OP_LEA;   O1: coREG;    O2: coJAN;    O3: coNONE;  F: [FLAG_ADDROVERRIDE, FLAG_OPCODRM] ),

    ( C: $008E; RM: $00; O: OP_MOV;   O1: coSEG;    O2: coMEMREG; O3: coNONE;  F: [FLAG_SIZEOVERRIDE] ),
    ( C: $008F; RM: $00; O: OP_POP;   O1: coMEMREG; O2: coNONE;   O3: coNONE;  F: [FLAG_SIZEOVERRIDE, FLAG_OPCODRM] ),
    ( C: $0090; RM: $00; O: OP_NOP;   O1: coNONE;   O2: coNONE;   O3: coNONE;  F: [FLAG_NONE] ),
    ( C: $0090; RM: $00; O: OP_XCHG;  O1: coWRACC;  O2: coWRGCMD; O3: coNONE;  F: [FLAG_SIZEOVERRIDE] ),
    ( C: $0090; RM: $00; O: OP_XCHG;  O1: coWRGCMD; O2: coWRACC;  O3: coNONE;  F: [FLAG_SIZEOVERRIDE] ),
    ( C: $0098; RM: $00; O: OP_CBW;   O1: coNONE;   O2: coNONE;   O3: coNONE;  F: [FLAG_NONE] ),
    ( C: $0099; RM: $00; O: OP_CWD;   O1: coNONE;   O2: coNONE;   O3: coNONE;  F: [FLAG_NONE] ),
    //( C:$009A; RM:$00; F:FLAG_NONE; O1:coJAF;O2:coNONE;O3:coNONE; O:OP_CALL ),
    ( C: $009B; RM: $00; O: OP_WAIT;  O1: coNONE;   O2: coNONE;   O3: coNONE;  F: [FLAG_NONE] ),
    ( C: $009C; RM: $00; O: OP_PUSHF; O1: coNONE;   O2: coNONE;   O3: coNONE;  F: [FLAG_NONE] ),
    ( C: $009C; RM: $00; O: OP_PUSHFD;O1: coNONE;   O2: coNONE;   O3: coNONE;  F: [FLAG_NONE] ),
    ( C: $009D; RM: $00; O: OP_POPF;  O1: coNONE;   O2: coNONE;   O3: coNONE;  F: [FLAG_NONE] ),
    ( C: $009D; RM: $00; O: OP_POPFD; O1: coNONE;   O2: coNONE;   O3: coNONE;  F: [FLAG_NONE] ),

    ( C: $009E; RM: $00; O: OP_SAHF;  O1: coNONE;   O2: coNONE;   O3: coNONE;  F: [FLAG_NONE] ),
    ( C: $009F; RM: $00; O: OP_LAHF;  O1: coNONE;   O2: coNONE;   O3: coNONE;  F: [FLAG_NONE] ),
    ( C: $00A0; RM: $00; O: OP_MOV;   O1: coRACC;   O2: coMOFFS;  O3: coNONE;  F: [FLAG_SIZE0, FLAG_SIZEOVERRIDE] ),

    ( C: $00A2; RM: $00; O: OP_MOV;   O1: coMOFFS;  O2: coRACC;   O3: coNONE;  F: [FLAG_SIZE0, FLAG_SIZEOVERRIDE] ),
    ( C: $00A4; RM: $00; O: OP_MOVSB; O1: coNONE;   O2: coNONE;   O3: coNONE;  F: [FLAG_NONE] ),
    ( C: $00A5; RM: $00; O: OP_MOVSW; O1: coNONE;   O2: coNONE;   O3: coNONE;  F: [FLAG_NONE] ),
    ( C: $00A6; RM: $00; O: OP_CMPSB; O1: coNONE;   O2: coNONE;   O3: coNONE;  F: [FLAG_NONE] ),
    ( C: $00A7; RM: $00; O: OP_CMPSW; O1: coNONE;   O2: coNONE;   O3: coNONE;  F: [FLAG_NONE] ),
    ( C: $00A8; RM: $00; O: OP_TEST;  O1: coRACC;   O2: coIMM;    O3: coNONE;  F: [FLAG_SIZE0, FLAG_SIZEOVERRIDE] ),
    ( C: $00AA; RM: $00; O: OP_STOSB; O1: coNONE;   O2: coNONE;   O3: coNONE;  F: [FLAG_NONE] ),
    ( C: $00AB; RM: $00; O: OP_STOSW; O1: coNONE;   O2: coNONE;   O3: coNONE;  F: [FLAG_NONE] ),
    ( C: $00AC; RM: $00; O: OP_LODSB; O1: coNONE;   O2: coNONE;   O3: coNONE;  F: [FLAG_NONE] ),
   // ( C:$00AD; RM:$00; O:OP_LODSW; O1:coNONE;   O2:coNONE;  O3:coNONE;  F:[FLAG_NONE] ),
    ( C: $00AE; RM: $00; O: OP_SCASB; O1: coNONE;   O2: coNONE;   O3: coNONE;  F: [FLAG_NONE] ),

    ( C: $00B0; RM: $00; O: OP_MOV;   O1: coREGCMD; O2: coIMM;    O3: coNONE;  F: [FLAG_SIZE3, FLAG_SIZEOVERRIDE] ),
    ( C: $00C0; RM: $00; O: OP_ROL;   O1: coMEMREG; O2: coIMM8;   O3: coNONE;  F: [FLAG_SIZE0, FLAG_SIZEOVERRIDE, FLAG_OPCODRM] ),
    ( C: $08C0; RM: $08; O: OP_ROR;   O1: coMEMREG; O2: coIMM8;   O3: coNONE;  F: [FLAG_SIZE0, FLAG_SIZEOVERRIDE, FLAG_OPCODRM] ),
    ( C: $10C0; RM: $10; O: OP_RCL;   O1: coMEMREG; O2: coIMM8;   O3: coNONE;  F: [FLAG_SIZE0, FLAG_SIZEOVERRIDE, FLAG_OPCODRM] ),
    ( C: $18C0; RM: $18; O: OP_RCR;   O1: coMEMREG; O2: coIMM8;   O3: coNONE;  F: [FLAG_SIZE0, FLAG_SIZEOVERRIDE, FLAG_OPCODRM] ),
    ( C: $20C0; RM: $20; O: OP_SHL;   O1: coMEMREG; O2: coIMM8;   O3: coNONE;  F: [FLAG_SIZE0, FLAG_SIZEOVERRIDE, FLAG_OPCODRM] ),
    ( C: $28C0; RM: $28; O: OP_SHR;   O1: coMEMREG; O2: coIMM8;   O3: coNONE;  F: [FLAG_SIZE0, FLAG_SIZEOVERRIDE, FLAG_OPCODRM] ),
    ( C: $38C0; RM: $38; O: OP_SAR;   O1: coMEMREG; O2: coIMM8;   O3: coNONE;  F: [FLAG_SIZE0, FLAG_SIZEOVERRIDE, FLAG_OPCODRM] ),
    ( C: $00C2; RM: $00; O: OP_RETN;  O1: coIMM16;  O2: coNONE;   O3: coNONE;  F: [FLAG_NONE] ),
    ( C: $00C2; RM: $00; O: OP_RET;   O1: coIMM16;  O2: coNONE;   O3: coNONE;  F: [FLAG_NONE] ),
    ( C: $00C3; RM: $00; O: OP_RETN;  O1: coNONE;   O2: coNONE;   O3: coNONE;  F: [FLAG_NONE] ),
    ( C: $00C3; RM: $00; O: OP_RET;   O1: coNONE;   O2: coNONE;   O3: coNONE;  F: [FLAG_NONE] ),
    ( C: $00C6; RM: $00; O: OP_MOV;   O1: coMEMREG; O2: coIMM;    O3: coNONE;  F: [FLAG_SIZE0, FLAG_SIZEOVERRIDE, FLAG_OPCODRM] ),
    ( C: $00C8; RM: $00; O: OP_ENTER; O1: coIMM16;  O2: coIMM8;   O3: coNONE;  F: [FLAG_NONE] ),
    ( C: $00C9; RM: $00; O: OP_LEAVE; O1: coNONE;   O2: coNONE;   O3: coNONE;  F: [FLAG_NONE] ),
    ( C: $00CA; RM: $00; O: OP_RETF;  O1: coIMM16;  O2: coNONE;   O3: coNONE;  F: [FLAG_NONE] ),
    ( C: $00CB; RM: $00; O: OP_RETF;  O1: coNONE;   O2: coNONE;   O3: coNONE;  F: [FLAG_NONE] ),
    ( C: $00CC; RM: $00; O: OP_INT3;  O1: coNONE;   O2: coNONE;   O3: coNONE;  F: [FLAG_NONE] ),
    ( C: $00CD; RM: $00; O: OP_INT;   O1: coIMM8;   O2: coNONE;   O3: coNONE;  F: [FLAG_NONE] ),
    ( C: $00CE; RM: $00; O: OP_INTO;  O1: coNONE;   O2: coNONE;   O3: coNONE;  F: [FLAG_NONE] ),
    ( C: $00CF; RM: $00; O: OP_IRET;  O1: coNONE;   O2: coNONE;   O3: coNONE;  F: [FLAG_NONE] ),

    ( C: $00D0; RM: $00; O: OP_ROL;   O1: coMEMREG; O2: coCONST1; O3: coNONE;  F: [FLAG_SIZE0, FLAG_SIZEOVERRIDE, FLAG_OPCODRM] ),
    ( C: $08D0; RM: $08; O: OP_ROR;   O1: coMEMREG; O2: coCONST1; O3: coNONE;  F: [FLAG_SIZE0, FLAG_SIZEOVERRIDE, FLAG_OPCODRM] ),
    ( C: $10D0; RM: $10; O: OP_RCL;   O1: coMEMREG; O2: coCONST1; O3: coNONE;  F: [FLAG_SIZE0, FLAG_SIZEOVERRIDE, FLAG_OPCODRM] ),
    ( C: $18D0; RM: $18; O: OP_RCR;   O1: coMEMREG; O2: coCONST1; O3: coNONE;  F: [FLAG_SIZE0, FLAG_SIZEOVERRIDE, FLAG_OPCODRM] ),
    ( C: $20D0; RM: $20; O: OP_SHL;   O1: coMEMREG; O2: coCONST1; O3: coNONE;  F: [FLAG_SIZE0, FLAG_SIZEOVERRIDE, FLAG_OPCODRM] ),
    ( C: $28D0; RM: $28; O: OP_SHR;   O1: coMEMREG; O2: coCONST1; O3: coNONE;  F: [FLAG_SIZE0, FLAG_SIZEOVERRIDE, FLAG_OPCODRM] ),
    ( C: $38D0; RM: $38; O: OP_SAR;   O1: coMEMREG; O2: coCONST1; O3: coNONE;  F: [FLAG_SIZE0, FLAG_SIZEOVERRIDE, FLAG_OPCODRM] ),
    ( C: $00D2; RM: $00; O: OP_ROL;   O1: coMEMREG; O2: coREGCL;  O3: coNONE;  F: [FLAG_SIZE0, FLAG_SIZEOVERRIDE, FLAG_OPCODRM] ),
    ( C: $08D2; RM: $08; O: OP_ROR;   O1: coMEMREG; O2: coREGCL;  O3: coNONE;  F: [FLAG_SIZE0, FLAG_SIZEOVERRIDE, FLAG_OPCODRM] ),
    ( C: $10D2; RM: $10; O: OP_RCL;   O1: coMEMREG; O2: coREGCL;  O3: coNONE;  F: [FLAG_SIZE0, FLAG_SIZEOVERRIDE, FLAG_OPCODRM] ),
    ( C: $18D2; RM: $18; O: OP_RCR;   O1: coMEMREG; O2: coREGCL;  O3: coNONE;  F: [FLAG_SIZE0, FLAG_SIZEOVERRIDE, FLAG_OPCODRM] ),
    ( C: $20D2; RM: $20; O: OP_SHL;   O1: coMEMREG; O2: coREGCL;  O3: coNONE;  F: [FLAG_SIZE0, FLAG_SIZEOVERRIDE, FLAG_OPCODRM] ),
    ( C: $28D2; RM: $28; O: OP_SHR;   O1: coMEMREG; O2: coREGCL;  O3: coNONE;  F: [FLAG_SIZE0, FLAG_SIZEOVERRIDE, FLAG_OPCODRM] ),
    ( C: $38D2; RM: $38; O: OP_SAR;   O1: coMEMREG; O2: coREGCL;  O3: coNONE;  F: [FLAG_SIZE0, FLAG_SIZEOVERRIDE, FLAG_OPCODRM] ),
    ( C: $0AD4; RM: $00; O: OP_AAM;   O1: coNONE;   O2: coNONE;   O3: coNONE;  F: [FLAG_OPCODE2] ),
    ( C: $00D4; RM: $00; O: OP_AAM;   O1: coIMM8;   O2: coNONE;   O3: coNONE;  F: [FLAG_NONE] ),
    ( C: $0AD5; RM: $00; O: OP_AAD;   O1: coNONE;   O2: coNONE;   O3: coNONE;  F: [FLAG_OPCODE2] ),
    ( C: $00D5; RM: $00; O: OP_AAD;   O1: coIMM8;   O2: coNONE;   O3: coNONE;  F: [FLAG_NONE] ),
    // ?? ( C:$00D6; RM:$00; O1:coNONE;O2:coNONE;O3:coNONE; O:OP_SAL ),
    ( C: $00E0; RM: $00; O: OP_LOOPNE;O1: coJRS;    O2: coNONE;   O3: coNONE;  F: [FLAG_NONE] ),
    ( C: $00E0; RM: $00; O: OP_LOOPNZ;O1: coJRS;    O2: coNONE;   O3: coNONE;  F: [FLAG_NONE] ),
    ( C: $00E1; RM: $00; O: OP_LOOPE; O1: coJRS;    O2: coNONE;   O3: coNONE;  F: [FLAG_NONE] ),
    ( C: $00E1; RM: $00; O: OP_LOOPZ; O1: coJRS;    O2: coNONE;   O3: coNONE;  F: [FLAG_NONE] ),
    ( C: $00E2; RM: $00; O: OP_LOOP;  O1: coJRS;    O2: coNONE;   O3: coNONE;  F: [FLAG_NONE] ),
    ( C: $00E3; RM: $00; O: OP_JCXZ;  O1: coJRS;    O2: coNONE;   O3: coNONE;  F: [FLAG_NONE] ),
    ( C: $00E4; RM: $00; O: OP_IN;    O1: coRACC;   O2: coIMM8;   O3: coNONE;  F: [FLAG_SIZE0, FLAG_SIZEOVERRIDE] ),
    ( C: $00E6; RM: $00; O: OP_OUT;   O1: coIMM8;   O2: coRACC;   O3: coNONE;  F: [FLAG_SIZE0, FLAG_SIZEOVERRIDE] ),
    ( C: $00E8; RM: $00; O: OP_CALL;  O1: coJRN;    O2: coNONE;   O3: coNONE;  F: [FLAG_ADDROVERRIDE] ),
    ( C: $00E9; RM: $00; O: OP_JMP;   O1: coJRN;    O2: coNONE;   O3: coNONE;  F: [FLAG_ADDROVERRIDE] ),
    ( C: $00EA; RM: $00; O: OP_JMP;   O1: coJAF;    O2: coNONE;   O3: coNONE;  F: [FLAG_ADDROVERRIDE] ),

    ( C: $00EB; RM: $00; O: OP_JMP;   O1: coJRS;    O2: coNONE;   O3: coNONE;  F: [FLAG_NONE] ),
    ( C: $00EC; RM: $00; O: OP_IN;    O1: coRACC;   O2: coREGDX;  O3: coNONE;  F: [FLAG_SIZE0, FLAG_SIZEOVERRIDE] ),
    ( C: $00EE; RM: $00; O: OP_OUT;   O1: coREGDX;  O2: coRACC;   O3: coNONE;  F: [FLAG_SIZE0, FLAG_SIZEOVERRIDE] ),
    ( C: $00F4; RM: $00; O: OP_HLT;   O1: coNONE;   O2: coNONE;   O3: coNONE;  F: [FLAG_NONE] ),
    ( C: $00F5; RM: $00; O: OP_CMC;   O1: coNONE;   O2: coNONE;   O3: coNONE;  F: [FLAG_NONE] ),

    ( C: $00F6; RM: $00; O: OP_TEST;  O1: coMEMREG; O2: coIMM;    O3: coNONE;  F: [FLAG_SIZE0, FLAG_SIZEOVERRIDE, FLAG_OPCODRM] ),

    ( C: $00FF; RM: $E0; O: OP_JMP;   O1: coREG;    O2: coNONE;   O3: coNONE;  F: [FLAG_SIZE0, FLAG_SIZEOVERRIDE, FLAG_OPCODRM] ),
    ( C: $00FF; RM: $20; O: OP_JMP;   O1: coMEMREG; O2: coNONE;   O3: coNONE;  F: [FLAG_SIZE0, FLAG_SIZEOVERRIDE, FLAG_OPCODRM] ),
    ( C: $00FF; RM: $25; O: OP_JMP;   O1: coMEM;    O2: coNONE;   O3: coNONE;  F: [FLAG_SIZE0, FLAG_SIZEOVERRIDE, FLAG_OPCODRM] ),

    ( C: $10F6; RM: $10; O: OP_NOT;   O1: coMEMREG; O2: coNONE;   O3: coNONE;  F: [FLAG_SIZE0, FLAG_SIZEOVERRIDE, FLAG_OPCODRM] ),
    ( C: $18F6; RM: $18; O: OP_NEG;   O1: coMEMREG; O2: coNONE;   O3: coNONE;  F: [FLAG_SIZE0, FLAG_SIZEOVERRIDE, FLAG_OPCODRM]),
    ( C: $20F6; RM: $20; O: OP_MUL;   O1: coMEMREG; O2: coNONE;   O3: coNONE;  F: [FLAG_SIZE0, FLAG_SIZEOVERRIDE, FLAG_OPCODRM] ),
    ( C: $28F6; RM: $28; O: OP_IMUL;  O1: coMEMREG; O2: coNONE;   O3: coNONE;  F: [FLAG_SIZE0, FLAG_SIZEOVERRIDE, FLAG_OPCODRM] ),
    ( C: $30F6; RM: $30; O: OP_DIV;   O1: coMEMREG; O2: coNONE;   O3: coNONE;  F: [FLAG_SIZE0, FLAG_SIZEOVERRIDE, FLAG_OPCODRM] ),
    ( C: $38F6; RM: $38; O: OP_IDIV;  O1: coMEMREG; O2: coNONE;   O3: coNONE;  F: [FLAG_SIZE0, FLAG_SIZEOVERRIDE, FLAG_OPCODRM] ),

    ( C: $00F8; RM: $00; O: OP_CLC;   O1: coNONE;   O2: coNONE;   O3: coNONE;  F: [FLAG_NONE] ),
    ( C: $00F9; RM: $00; O: OP_STC;   O1: coNONE;   O2: coNONE;   O3: coNONE;  F: [FLAG_NONE] ),
    ( C: $00FA; RM: $00; O: OP_CLI;   O1: coNONE;   O2: coNONE;   O3: coNONE;  F: [FLAG_NONE] ),
    ( C: $00FB; RM: $00; O: OP_STI;   O1: coNONE;   O2: coNONE;   O3: coNONE;  F: [FLAG_NONE] ),
    ( C: $00FC; RM: $00; O: OP_CLD;   O1: coNONE;   O2: coNONE;   O3: coNONE;  F: [FLAG_NONE] ),
    ( C: $00FD; RM: $00; O: OP_STD;   O1: coNONE;   O2: coNONE;   O3: coNONE;  F: [FLAG_NONE] ),

    ( C: $00FE; RM: $00; O: OP_INC;   O1: coMEMREG; O2: coNONE;   O3: coNONE;  F: [FLAG_SIZE0, FLAG_SIZEOVERRIDE, FLAG_OPCODRM] ),
    ( C: $08FE; RM: $08; O: OP_DEC;   O1: coMEMREG; O2: coNONE;   O3: coNONE;  F: [FLAG_SIZE0, FLAG_SIZEOVERRIDE, FLAG_OPCODRM] ),
    //( C:$10FF; RM:$02; O1:coMEMREGW;O2:coNONE;O3:coNONE; O:OP_CALL;  F:[FLAG_SIZE0,FLAG_SIZEOVERRIDE,FLAG_OPCODRM] ),
    ( C: $20FF; RM: $20; O: OP_JMP;   O1: coMEMREGW;O2: coNONE;   O3: coNONE;  F: [FLAG_SIZEOVERRIDE, FLAG_OPCODRM] ),
    ( C: $30FF; RM: $30; O: OP_PUSH;  O1: coMEMREG; O2: coNONE;   O3: coNONE;  F: [FLAG_SIZEOVERRIDE, FLAG_OPCODRM] )
  );

type
  TAsmInstruction = class(TObject)
    DataSize        : TDataSize;
    PrefixLockRepeat: TAsmPrefix;
    PrefixSegment   : TAsmPrefix;
    OverrideOperandSize: Boolean;
    OverrideAddressSize: Boolean;
    OpCode          : TAsmOpcode;
    Op1,Op2,Op3     : TAsmOperand;
    Code            : tCodeDump;
    TableRow        : Integer;

    function GetDataSize:Boolean;
    function CompatibleOperand( Const Op:TAsmOperand; OT: TTableOperandLocation ): Boolean;
    function SearchTable: Boolean;
    procedure DumpCode;

    procedure Set_op_reg(Opc:TAsmOpcode; Reg:TAsmRegister);
    procedure Set_op_reg_reg(Opc:TAsmOpcode; Reg1,Reg2:TAsmRegister);
    procedure Set_op_reg_mem(Opc:TAsmOpcode; Reg,Base,Index:TAsmRegister; Scale,Offset:Integer);

    procedure Reset;

    constructor Create;
  end;


procedure SetRegisterOperand( Var Op:TAsmOperand; Reg:TAsmRegister );
procedure SetImmediateOperand( Var Op:TAsmOperand; Val:Integer );
procedure SetMemoryOperand( Var Op:TAsmOperand; Size:TDataSize; Base,Index:TAsmRegister; Scale,Offset:Integer);
procedure SetMOFFSOperand( Var Op:TAsmOperand; Size:TDataSize; Offset:Integer);

function RegisterSize(Reg:TAsmRegister):TDataSize;
function ImmediateSize(Imm:Integer): TDataSize;
function DataSizeValue(Size:TDataSize):Integer;


var
  OverrideSize: Integer = 2;  // win32

  Dump: packed record
    Prefix    : tCodeDump;
    Opcode    : tCodeDump;
    AddModRM  : Boolean;
    ModRM     : Byte;
    AddSibByte: Boolean;
    SibByte   : Byte;
    Disp      : tCodeDump;
    Immediate : tCodeDump;
  end;

  DumpOfs: packed record // to Know the offset of each dump in the code
    Prefix    : Byte;
    Opcode    : Byte;
    ModRM     : Byte;
    SibByte   : Byte;
    Disp      : Byte;
    Immediate : Byte;
  end;

implementation

procedure SetRegisterOperand( Var Op:TAsmOperand; Reg:TAsmRegister );
begin
 Op.Loc:=LOC_REGISTER;
 Op.Reg:=Reg;
end;

procedure SetImmediateOperand( Var Op:TAsmOperand; Val:Integer );
begin
 Op.Loc:=LOC_IMMEDIATE;
 Op.Value:=Val;
end;

procedure SetMemoryOperand( Var Op:TAsmOperand; Size:TDataSize; Base,Index:TAsmRegister; Scale,Offset:Integer);
begin
 Op.Size:=Size;
 Op.Loc:=LOC_MEMORY;
 Op.Base:=Base;
 Op.Index:=Index;
 Op.Scale:=Scale;
 Op.Offset:=Offset;
end;

procedure SetMOFFSOperand( Var Op:TAsmOperand; Size:TDataSize; Offset:Integer);
begin
 Op.Size:=Size;
 Op.Loc:=LOC_MEMORY;
 Op.Base:=REG_NONE;
 Op.Index:=REG_NONE;
 Op.Scale:=0;
 Op.Offset:=Offset;
end;


function RegisterSize(Reg:TAsmRegister):TDataSize;
begin
  Case Reg of
    REG_AL..REG_BH  : RegisterSize:=SIZE_BYTE;
    REG_AX..REG_DI  : RegisterSize:=SIZE_WORD;
    REG_EAX..REG_EDI: RegisterSize:=SIZE_DWORD;
    REG_ES..REG_DS  : RegisterSize:=SIZE_WORD;
    else RegisterSize:=SIZE_NONE;
  end;
end;

function ImmediateSize(Imm:Integer): TDataSize;
begin
  if (Imm<0) then
    if (Imm>=-128) then ImmediateSize:=SIZE_SBYTE
    else if (Imm>=-32768) then ImmediateSize:=SIZE_SWORD
    else if ( Cardinal(Imm)>=$80000000) then ImmediateSize:=SIZE_SDWORD
    else ImmediateSize:=SIZE_DWORD
  else
    if (Imm<=$7F) then ImmediateSize:=SIZE_SBYTE
    else if (Imm<=$FF) then ImmediateSize:=SIZE_BYTE
    else if (Imm<=$7FFF) then ImmediateSize:=SIZE_SWORD
    else if (Imm<=$FFFF) then ImmediateSize:=SIZE_WORD
    else if ( Cardinal(Imm)<=$7FFFFFFF) then ImmediateSize:=SIZE_SDWORD
    else ImmediateSize:=SIZE_DWORD;
End;



function DataSizeValue(Size:TDataSize):Integer;
begin
  Case Size of
    SIZE_SBYTE,SIZE_BYTE : DataSizeValue:=1;
    SIZE_SWORD,SIZE_WORD : DataSizeValue:=2;
    SIZE_SDWORD,SIZE_DWORD : DataSizeValue:=4;
    else DataSizeValue:=0;
  end;
end;

function OperandSize(Const Op:TAsmOperand): TDataSize;
begin
  OperandSize:=Op.Size;
  if Op.Size=SIZE_NONE then
    if (Op.Loc=LOC_REGISTER) then OperandSize:= RegisterSize(Op.Reg);
end;



function IsOverrideAddress(Op:TAsmOperand):Boolean;
begin
 IsOverrideAddress:=True;
 if (Op.Loc=LOC_MEMORY) then begin
    if (Op.Base<>REG_NONE) and ( DataSizeValue(RegisterSize(Op.Base))=OverrideSize ) then Exit;
    if (Op.Index<>REG_NONE) and ( DataSizeValue(RegisterSize(Op.Index))=OverrideSize ) then Exit;
 end;
 IsOverrideAddress:=False;
end;

constructor TAsmInstruction.Create;
begin
  Reset;
end;

procedure TAsmInstruction.Reset;
begin
  PrefixLockRepeat :=PRE_NONE;
  PrefixSegment := PRE_NONE;
  OverrideOperandSize :=False;
  OverrideAddressSize :=False;
  OpCode := OP_NONE;
  Op1.Loc := LOC_NONE; Op1.Size := SIZE_NONE;
  Op2.Loc := LOC_NONE; Op2.Size := SIZE_NONE;
  Op3.Loc := LOC_NONE; Op3.Size := SIZE_NONE;
  Code := TCodeDump.Create;
  TableRow := -1;
end;

function TAsmInstruction.GetDataSize: Boolean;
var
  i, j, k: TDataSize;
begin
  GetDataSize := False;
  DataSize := SIZE_NONE;
  i := OperandSize(Op1);
  j := OperandSize(Op2);
  k := OperandSize(Op3);
  if i <> SIZE_NONE then
  begin
    if (j<>SIZE_NONE) and (i <> j) then
      Exit;

    if (k<>SIZE_NONE) and (i <> k) then
      Exit;

    DataSize:=i;
  end else
  if j <> SIZE_NONE then
  begin
    if (k <> SIZE_NONE) and (j <> k) then
      Exit;

    DataSize:=j;
  end else
    DataSize:=k;
  GetDataSize:=TRUE;
end;

function TAsmInstruction.CompatibleOperand( Const Op:TAsmOperand; OT: TTableOperandLocation ): Boolean;
begin
  CompatibleOperand := False;
  case Op.Loc of
    LOC_NONE: CompatibleOperand := OT = coNONE;
    LOC_REGISTER:
      case OT of
        coMEMREGB: CompatibleOperand := RegisterSize(Op.Reg)in[SIZE_SBYTE,SIZE_BYTE];
        coREG,coREGCMD,coMEMREG,coWRGCMD: CompatibleOperand:= Op.Reg in[REG_AL..REG_EDI];
	      coRACC: CompatibleOperand := Op.Reg in[REG_AL,REG_AX,REG_EAX];
        coREGCL: CompatibleOperand := Op.Reg=REG_CL;
        coREGAX: CompatibleOperand := Op.Reg=REG_AX;
        coWRACC: CompatibleOperand := Op.Reg in[REG_AX,REG_EAX];
	      coREGDX: CompatibleOperand := Op.Reg=REG_DX;
        coSEGES: CompatibleOperand := Op.Reg=REG_ES;
        coSEGSS: CompatibleOperand := Op.Reg=REG_SS;
        coSEGDS: CompatibleOperand := Op.Reg=REG_DS;
        coSEGCS: CompatibleOperand := Op.Reg=REG_CS;
        coSEG:  CompatibleOperand := Op.Reg in[REG_ES..REG_DS];
      end;

    LOC_IMMEDIATE:
	    case OT of
  	    coIMM: CompatibleOperand := True;
  	    coCONST1: CompatibleOperand := Op.Value=1;
  	    coIMM8: CompatibleOperand := (ImmediateSize(Op.Value)=SIZE_SBYTE)and(Op.RefKind in[REF_NONE,REF_SHORT]);//<<
  	    coIMM16: CompatibleOperand := DataSizeValue(ImmediateSize(Op.Value))<=2;
  	    coIMMW: CompatibleOperand := True;    //<<
  	    coJRS: CompatibleOperand := Op.Size=SIZE_SHORT; //ImmediateSize(Op.Value)=SIZE_SBYTE;
        coJRN: CompatibleOperand := Op.Size in[SIZE_NONE,SIZE_NEAR]; //ImmediateSize(Op.Value)<=SIZE_SDWORD;
  	    coJAN: CompatibleOperand := True; // Address Size ???
  	    //unsed ,coJAF: CompatibleOperand := Size=Size_Far;
      end;

    LOC_MEMORY:
      case OT of
        coMEMREG,coMEM: CompatibleOperand := True;
        coMOFFS: CompatibleOperand := (Op.Index=REG_NONE) and (Op.Base=REG_NONE);
      end;
  end;
end;

function TAsmInstruction.SearchTable : Boolean;
var
  i: Integer;
  Tab: TTableInstruction;
Begin
  Result :=False;
  TableRow := -1;
  for i := 0 to (INSTR_COUNT) do
  begin
    Tab := INSTR_TABLE[i];
    if (Tab.O = Opcode) and
        CompatibleOperand(Op1, Tab.O1) and
        CompatibleOperand(Op2, Tab.O2) and
        CompatibleOperand(Op3, Tab.O3) then
    begin
      TableRow := i;
      Exit(True);
    end;
  end;
end;

procedure TAsmInstruction.DumpCode;
var
  C: Integer;

  procedure DumpMemReg(A:TAsmOperand);
   Var  RM,ModRM: Byte;
        // Sib Byte Var
        Scale,Base,Index: Byte;

  begin
    if A.Loc=LOC_REGISTER then begin
      Dump.AddModRM:=True;
      Dump.ModRM := Dump.ModRM OR $C0 OR (AsmRegisterValue[A.Reg] and $7);
    end
    else if A.Loc=LOC_MEMORY then
      if OverrideAddressSize then begin
        Case A.Base of
            REG_BX:
              Case (A.Index) of
                REG_SI: RM:=0;   // BX+SI
                REG_DI: RM:=1;   // BX+DI
                Else RM:=7; // BX
              End;
            REG_BP:
              Case (A.Index) of
                REG_SI: RM:=2;   // BP+SI
                REG_DI: RM:=3;   // BP+DI
                Else RM:=6;   // BP
              End;
        Else
          Case (A.Index) of
            REG_SI: RM:=4;   // SI
            REG_DI: RM:=5;   // DI
          End;
        End;//CASE
        if (A.Value=0)AND(RM<>6) Then ModRM:=$00
        else if (A.Value<=127)AND(A.Value>=-128) Then ModRM:=$40
        else ModRM:=$80;
        Dump.AddModRM:=True;
        Dump.ModRM := Dump.ModRM OR RM OR ModRM;
        Case ModRM of
          $40: Dump.Immediate.AddByte( A.Value );
          $80: Dump.Immediate.AddWord( A.Value );
        End;
      end
      else begin
        if (A.Index=REG_NONE) then begin// No SIB byte
          if (A.Base=REG_NONE) then begin//MOFFS
            RM:=5;
            ModRM:=0;
            Dump.AddModRM:=True;
            Dump.ModRM := Dump.ModRM OR RM OR ModRM;
            Dump.Disp.AddDword(A.Offset);
          end
          else begin
            RM:=AsmRegisterValue[A.Base];
            // CALC Mod
            if A.Offset=0 then ModRM:=0
            else
              Case ImmediateSize(A.Offset) of
                SIZE_SBYTE: ModRM:=$40;
                else ModRM:=$80;
              end;
            // DUMP ModRM
            Dump.AddModRM:=True;
            Dump.ModRM := Dump.ModRM OR RM OR ModRM;
            // DUMP Displacement
            Case ModRM of
              $40: Dump.Disp.AddByte(A.Offset);
              $80: Dump.Disp.AddDword(A.Offset);
            end;
          end;
        end
        else Begin // SIB byte
          RM:=4;
          // CALC Mod
          if A.Offset=0 then ModRM:=0
          else
            Case ImmediateSize(A.Offset) of
              SIZE_SBYTE: ModRM:=$40;
              else ModRM:=$80;
            end;
          // DUMP ModRM
          Dump.AddModRM:=True;
          Dump.ModRM := Dump.ModRM OR RM OR ModRM;
          // DUMP Displacement
          Case ModRM of
            $40: Dump.Disp.AddByte(A.Offset);
            $80: Dump.Disp.AddDword(A.Offset);
          end;
          // DUMP SIB
          Case A.Scale of
            0: Scale:=$00;
            2: Scale:=$40;
            4: Scale:=$80;
            8: Scale:=$C0;
          else
            Scale:=$00;
          end;
          if A.Base=REG_NONE then base:=5 else base:=AsmRegisterValue[A.Base];
          Index:=AsmRegisterValue[A.Index];
          Dump.AddSibByte:=True;
          Dump.SibByte := Scale OR (Index*8) OR Base;
        end;
      end;
  end;


  procedure DumpOperand(Const A: TAsmOperand; ot:TTableOperandLocation );
  begin
    Case ot of
      coNONE: ; {No operand}
     // Immediate
      coIMM: {Immediate data (8 or 16/32)}
        Case DataSize of
          SIZE_SBYTE,SIZE_BYTE: Dump.Immediate.AddByte(A.Value);
          SIZE_SWORD,SIZE_WORD: Dump.Immediate.AddWord(A.Value);
          SIZE_SDWORD,SIZE_DWORD: Dump.Immediate.AddDword(A.Value);
        end;
      coIMM8: Dump.Immediate.AddByte(A.Value);
      coJRS:  Dump.Immediate.AddByte(A.Value);
      coJRN,coJAN: Dump.Immediate.AddDword(A.Value);
      coIMM16: Dump.Immediate.AddWord(A.Value);
      coIMMW:
        Case DataSize of
          SIZE_SWORD,SIZE_WORD: Dump.Immediate.AddWord(A.Value);
          else Dump.Immediate.AddDword(A.Value);
        end;
    // Integer register in first Opcode byte (?? ??? XXX)
      coREGCMD: Dump.Opcode.Dump[0]:=Dump.Opcode.Dump[0] OR AsmRegisterValue[A.Reg];
    // D-Word register in Command byte (?? ??? XXX)
      coWRGCMD: Dump.Opcode.Dump[0]:=Dump.Opcode.Dump[0] OR AsmRegisterValue[A.Reg];
    // Integer register in Reg field (?? XXX ???) in ModRM Byte
      coREG: begin
        Dump.AddModRM:=True;
        Dump.ModRM:= Dump.ModRM OR ( (AsmRegisterValue[A.Reg] AND $7) *8);
      end;

    // Memory/Register
      coMEMREG: {Memory/register in ModRM byte '[...]' }
          DumpMemReg(A);

      coMOFFS:
        if OverrideAddressSize
          then Dump.Immediate.AddWord(A.Offset)
          else Dump.Immediate.AddDword(A.Offset);
    end;//CASE
  end;


Begin
 Dump.Prefix := TCodeDump.Create;
 Dump.Opcode := TCodeDump.Create;
 Dump.Disp := TCodeDump.Create;
 Dump.Immediate := TCodeDump.Create;
 Dump.AddModRM:=False; Dump.ModRM:=0;
 Dump.AddSibByte:=False; Dump.SibByte:=0;

 GetDataSize;
 if TableRow=-1 then SearchTable;
 if TableRow=-1 then Exit;

// Check Prefix Control Flags   // For Win32 OverrideSize=16
  if (FLAG_SIZEOVERRIDE in INSTR_TABLE[TableRow].F) then
    if DataSizeValue(DataSize) in[2,4] then
      if DataSizeValue(DataSize)=OverrideSize then OverrideOperandSize:=TRUE;
//  if (FLAG_ADDROVERRIDE in InstTable[TableRow].F) then
  if IsOverrideAddress(Op1) or
     IsOverrideAddress(Op2) or
     IsOverrideAddress(Op3) then
    OverrideAddressSize := TRUE;

// DUMP PREFIXES
  if PrefixLockRepeat<>PRE_NONE then Dump.Prefix.AddByte( ASM_PREFIX_CODE[PrefixLockRepeat] );
  if PrefixSegment<>PRE_NONE then Dump.Prefix.AddByte( ASM_PREFIX_CODE[PrefixSegment] );
  if OverrideOperandSize then Dump.Prefix.AddByte( $66 );
  if OverrideAddressSize then Dump.Prefix.AddByte( $67 );

// MODRM
  if (FLAG_OPCODRM in INSTR_TABLE[TableRow].F) then begin
    Dump.AddModRM:=True;
    Dump.ModRM:= Dump.ModRM OR INSTR_TABLE[TableRow].RM;
  end;

 Code.Create;

// Set Opcode flags
  C:=INSTR_TABLE[TableRow].C;
  if (FLAG_SIZE0 in INSTR_TABLE[TableRow].F) then if DataSizeValue(DataSize)in[2,4] then C:=C or 1;
  if (FLAG_SIZE3 in INSTR_TABLE[TableRow].F) then if DataSizeValue(DataSize)in[2,4] then C:=C or (1 shl 3);

// DUMP OPCODE bytes
  if FLAG_OPCODE2 in INSTR_TABLE[TableRow].F
    then Dump.Opcode.AddWord( C )
    else Dump.Opcode.AddByte( C );

// Encode Operands
  DumpOperand(Op1, INSTR_TABLE[TableRow].O1);
  DumpOperand(Op2, INSTR_TABLE[TableRow].O2);
  DumpOperand(Op3, INSTR_TABLE[TableRow].O3);

  FillChar(DumpOfs,Sizeof(DumpOfs),$FF);
// DUMP ALL CODE
  if Dump.Prefix.Size>0 then begin DumpOfs.Prefix:=Code.Size; Code.AddCodeDump(Dump.Prefix); End;
  if Dump.Opcode.Size>0 then begin DumpOfs.Opcode:=Code.Size; Code.AddCodeDump(Dump.Opcode); End;
  if Dump.AddModRM then begin DumpOfs.ModRM:=Code.Size; Code.AddByte(Dump.ModRM); End;
  if Dump.AddSibByte then begin DumpOfs.SibByte:=Code.Size; Code.AddByte(Dump.SibByte); End;
  if Dump.Disp.Size>0 then begin DumpOfs.Disp:=Code.Size; Code.AddCodeDump(Dump.Disp); End;
  if Dump.Immediate.Size>0 then begin DumpOfs.Immediate:=Code.Size; Code.AddCodeDump(Dump.Immediate); End;
End;


procedure TAsmInstruction.Set_op_reg(Opc:TAsmOpcode; Reg:TAsmRegister);
begin
  Opcode:=Opc;
  SetRegisterOperand(Op1,Reg);
  Op2.Loc:=LOC_NONE;
  Op3.Loc:=LOC_NONE;
end;
procedure TAsmInstruction.Set_op_reg_reg(Opc:TAsmOpcode; Reg1,Reg2:TAsmRegister);
begin
  Opcode:=Opc;
  SetRegisterOperand(Op1,Reg1);
  SetRegisterOperand(Op2,Reg2);
  Op3.Loc:=LOC_NONE;
end;

procedure TAsmInstruction.Set_op_reg_mem(Opc:TAsmOpcode; Reg,Base,Index:TAsmRegister; Scale,Offset:Integer);
begin
  Opcode:=Opc;
  SetRegisterOperand(Op1,Reg);
  SetMemoryOperand(Op2,SIZE_NONE,Base,Index,Scale,Offset);
  Op3.Loc:=LOC_NONE;
end;

end.



