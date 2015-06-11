;
; function StrLen(Str:pChar):Integer;
;
strlen:
	push	ebp
	mov	ebp, esp
	mov	edi, [ebp+08]	
	cld
	xor	eax, eax
	mov	ecx, -1
	repne	scasb
	neg	ecx
	sub	ecx,2
	xchg	eax, ecx
	pop	ebp
	ret	4
