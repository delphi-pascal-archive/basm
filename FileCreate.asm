; Title 'TestFile'
.APPTYPE GUI

include "inc\winconst.inc"
include 'inc\Kernel32.def'
include 'inc\User32.def'


.DATA
; Variables

  FileHandle	DD 0 ;THandle
  BytesWritten	DD 0 ;DWORD
  FileName	db 'TestFile',0
ALIGN 4
  Buffer	db 'Win Asm Test',0

.CODE
;  FileHandle := CreateFileA(FileName, , 0, 0, 0, 0);
    PUSH 0
    PUSH 0
    PUSH CREATE_ALWAYS
    PUSH 0
    PUSH 0
    PUSH GENERIC_READ + GENERIC_WRITE
    PUSH FileName ; Address
    CALL CreateFileA
    MOV  [FileHandle], EAX

;  WriteFile(FileHandle, Buffer,strlen(buffer), BytesWritten, 0);
    PUSH 0
    PUSH BytesWritten

    push buffer
    call strlen
    push eax

    PUSH Buffer
    PUSH [FileHandle]
    CALL WriteFile

;  CloseHandle(FileHandle);
    PUSH [FileHandle]
    CALL CloseHandle

;  ExitProcess(0);
    PUSH 0
    CALL ExitProcess

include 'inc\strings.inc'
include 'inc\file.inc'

end

