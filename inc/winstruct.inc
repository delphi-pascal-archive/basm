;==============================================================================
;
; WIN32 STRUCTURES
;
;==============================================================================


WNDCLASS struct		; comments show the C/C++ name
	wcStyle		dd 0	; style
	wcWndProc	dd 0	; lpfnWndProc
	wcClsExtra	dd 0	; cbClsExtra
	wcWndExtra	dd 0	; cbWndExtra
	wcInstance	dd 0	; hInstance
	wcIcon		dd 0	; hIcon
	wcCursor	dd 0	; hCursor
	wcBkgndBrush	dd 0	; hbrBackground
	wcMenuName	dd 0	; lpszMenuName
	wcClassName	dd 0	; lpszClassName
WNDCLASS ends

WNDCLASSEX struct		; comments show the C/C++ name
	wcxSize		dd 0	; cbSize, size of WNDCLASSEX
	wcxStyle	dd 0	; style
	wcxWndProc	dd 0	; lpfnWndProc
	wcxClsExtra	dd 0	; cbClsExtra
	wcxWndExtra	dd 0	; cbWndExtra
	wcxInstance	dd 0	; hInstance
	wcxIcon		dd 0	; hIcon
	wcxCursor	dd 0	; hCursor
	wcxBkgndBrush	dd 0	; hbrBackground
	wcxMenuName	dd 0	; lpszMenuName
	wcxClassName	dd 0	; lpszClassName
	wcxSmallIcon	dd 0	; hIconSm
WNDCLASSEX ends


tagPOINT struct
    x	dd 0
    y   dd 0 
ENDS POINT

TagMSG struct 
    hwnd    dd 0	 
    message dd 0 
    wParam  dd 0
    lParam  dd 0 
    time    dd 0 
    pt	    tagPOINT 
ENDS MSG; 
