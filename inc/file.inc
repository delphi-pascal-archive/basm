;
; function openfile(fname:pchar):handle;
;
openfile:
	push	ebp
	mov	ebp,esp
	push	0
	push	0
	push	OPEN_EXISTING
	push	0
	push	FILE_SHARE_READ
	push	GENERIC_READ
	push	[ebp+08]
	call	CreateFile
	pop	ebp
	ret	4


;
; function createfile(fname:pchar):handle;
;
createfile:
	push	ebp
	mov	ebp,esp
	push	0
	push	0
	push	CREATE_ALWAYS
	push	0
	push	FILE_SHARE_READ
	push	GENERIC_WRITE
	push	[ebp+08]
	call	CreateFile
	pop	ebp
	ret	4

;
; closefile(hfile:handle)
;
closefile:
	push	ebp
	mov	ebp,esp
	push	[ebp+08]
	call	CloseHandle
	pop	ebp
	ret	4
