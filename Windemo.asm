.APPTYPE GUI

NULL	= 0

include 'inc\winconst.inc'
include 'inc\winstruct.inc'
include 'inc\Kernel32.def'
include 'inc\User32.def'


.data

ClassName db "SimpleWinClass",0
AppName   db "Our First Window",0
WinTitle  db 'Window Creation with BASM',0

wc	WNDCLASSEX
Msg	TagMSG
hwnd	dd 0
hInst   dd 0


.CODE

START:	

; GetModuleHandle
	push	0
	CALL	GetModuleHandleA
	mov    [hInst],eax

; Set WindowClass
	mov   dword ptr[wc.wcxSize],SIZEOF WNDCLASSEX
	mov   dword ptr[wc.wcxStyle], CS_HREDRAW+CS_VREDRAW
	mov   dword ptr[wc.wcxWndProc], WndProc
	mov   dword ptr[wc.wcxClsExtra],NULL
	mov   dword ptr[wc.wcxWndExtra],NULL
	mov   eax, [hInst]
	mov   [wc.wcxInstance], eax
	mov   dword ptr[wc.wcxBkgndBrush],COLOR_WINDOW+1
	mov   dword ptr[wc.wcxMenuName],NULL
	mov   dword ptr[wc.wcxClassName],OFFSET ClassName
	mov   dword ptr[wc.wcxIcon], NULL
	mov   dword ptr[wc.wcxSmallIcon], NULL
; LoadCursor(hInstance,lpCursorName)
	Push	IDC_ARROW
	PUSH	NULL
	CALL	LoadCursorA 
	mov   dword ptr[wc.wcxCursor], EAX

	push  wc
	CALL  RegisterClassExA

; Create Window
	push	NULL
	push	[hInst]
	push	NULL
	push	NULL
	push	200
	push	300
	push	CW_USEDEFAULT
	push	CW_USEDEFAULT
	push	WS_VISIBLE+WS_SYSMENU+WS_MINIMIZEBOX+WS_MAXIMIZEBOX
	push	WinTitle
	push	[wc.wcxClassName]
	push	NULL ;WS_EX_TOPMOST 
	CALL	CreateWindowExA
	mov	[hWnd], EAX

; ShowWindow(hwnd, nCmdShow);
	push	SW_SHOWNORMAL
	push	[hwnd]
	CALL	ShowWindow

; UpdateWindow(hwnd);
	push	[hwnd]
	CALL	UpdateWindow	

; Boucle de message

StartLoop:
;     while (GetMessage(Msg, 0, 0, 0)) do
	push	NULL
	push	NULL
	push	NULL
	push	Msg
	CALL	GetMessageA
	OR	EAX,EAX
	JZ	Short ExitLoop
	
; TranslateMessage(Msg);
	push	Msg
	call	TranslateMessage

; DispatchMessage (Msg);
	push	Msg
	call	DispatchMessageA

	JMP	StartLoop
ExitLoop:


; ExitProcess
	PUSH	0
	CALL	ExitProcess

; Window Procedure

WndProc:
; LRESULT CALLBACK WindowProc(HWND hwnd, UINT uMsg, WPARAM wParam, LPARAM lParam)
	mov eax, ESP
	CMP dword ptr[eax+8], WM_DESTROY
	JNZ WndProcEnd
	push 0
	call PostQuitMessage
WndProcEnd:
	JMP	DefWindowProcA
	RETN

include 'strings.inc'
END
