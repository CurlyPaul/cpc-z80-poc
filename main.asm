UseHardwareKeyMap equ 1 	; enable the Keyboard map
BuildCPC equ 1			; required for some of the Chibi resources	
ScreenSize equ &4000

org &8000

read ".\libs\Multiplatform_ReadJoystick_Header.asm"
ld a,1
call &BC0E	;SCR SET MODE 1
call KeyboardScanner_Init

MainLoop:
	
	call Player_ReadControlsDual	;Read in the controllers
	ld a,h
	and l
	ld h,a			;Read both controllers, and merge them together into H	

	ld de,(CursorCurrentPosXY)		;Update Last cursor Data
	call Cursor_ProcessDirections		;Process the Movement directions
	ld (CursorCurrentPosXY),de		;Save the new Cursor Position
	; Todo add a wait frame so that we can reduce the flickering 
	; Todo draw stuff somewhere else and then bank switch it into screen memory
	call DrawBackground
	call DrawPlayer
jp MainLoop

ClearScreen:
	ld hl,&C000
	ld de,&C000+1
	ld bc,ScreenSize-1
	ld (hl),0
	ldir
ret

ret

DrawBackground:
	; for now, just draw an empty background
	call ClearScreen
ret

DrawPlayer:

	ld bc,(CursorCurrentPosXY)
	call GetScreenPos

	ld de,TestSprite
	ld b,48		; * lines

	SpriteNextLine:
		push hl
			ld c,12		; bytes per line
	SpriteNextByte:
			ld a,(de)	; Sourcebyte	
			ld (hl),a	; Screen desintation

			inc de
			inc hl
			dec c
			jr nz,SpriteNextByte
		pop hl
	call &bc26			; screen next line? TODO attempt to get rid of this
	djnz SpriteNextLine 		; djnz - decreases b and jumps when it's not zero
ret

CursorCurrentPosXY:	dw &0101	;Player xy pos
CursorMinX: 	db 1			;Player Move limits
CursorMaxX: 	db 68 			; screen width 80 bytes - player width (12)
CursorMinY: 	db 1			
CursorMaxY: 	db 152			; screen height 200 pixels - player height (48)

CursorMoveSpeedXY: dw &0103		;Player Move speed

read ".\libs\CA_Cursor_ProcessDirections.asm"

align32	
KeyMap equ KeyMap2+16			;wsad bnm p
KeyMap2:				;Default controls
	db &F7,&03,&7f,&05,&ef,&09,&df,&09,&f7,&09,&fB,&09,&fd,&09,&fe,&09 ;p2-pause,f3,f2,f1,r,l,d,u
	db &f7,&03,&bf,&04,&bf,&05,&bf,&06,&df,&07,&df,&08,&ef,&07,&f7,&07 ;p1-pause,f3,f2,f1,r,l,d,u

KeyboardScanner_KeyPresses: ds 16 	; define 16 bytes

read ".\libs\Multiplatform_ScanKeys.asm"
read ".\libs\CPC_V1_KeyboardDriver.asm"
read ".\libs\Multiplatform_ReadJoystickKeypressHandler.asm"

KeyboardScanner_Init:								;Init the screen buffer.
	ld hl,KeyboardScanner_KeyPresses
	ld d,h
	ld e,l
	inc de
	ld bc,15
	ld (hl),255
	z_ldir
	ret

; Converts X,Y into screen memory locations
; It expect x to to stored in b, y in c and the result is stored in hl 
GetScreenPos:
	; BC - X Y
	push bc
		ld b,0			; b is xpos, which we can ignore for now
		ld hl,scr_addr_table	; load the address of the label into h1
		add hl,bc		; add the value of c (ypos) to hl twice
		add hl,bc		; as each value in the look up is 2 bytes
		ld a,(hl)		; load the value at address in hl into a
		inc l			; increment the address we are looking at
		ld h,(hl)		; load the in the address at h into h
		ld l,a			; now put l into a
	pop bc				; reset to BC to the original values
	ld c,b				; load the value of b into c, we need b to be the low byte
	ld b,&C0			; and &C0 is the high byte as &C000 is the start of the screen space
	add hl,bc			; 
ret

align 2
scr_addr_table:
    defb &00,&00, &00,&08, &00,&10, &00,&18, &00,&20, &00,&28, &00,&30, &00,&38;1
    defb &50,&00, &50,&08, &50,&10, &50,&18, &50,&20, &50,&28, &50,&30, &50,&38;2
    defb &A0,&00, &A0,&08, &A0,&10, &A0,&18, &A0,&20, &A0,&28, &A0,&30, &A0,&38;3
    defb &F0,&00, &F0,&08, &F0,&10, &F0,&18, &F0,&20, &F0,&28, &F0,&30, &F0,&38;4
    defb &40,&01, &40,&09, &40,&11, &40,&19, &40,&21, &40,&29, &40,&31, &40,&39;5
    defb &90,&01, &90,&09, &90,&11, &90,&19, &90,&21, &90,&29, &90,&31, &90,&39;6
    defb &E0,&01, &E0,&09, &E0,&11, &E0,&19, &E0,&21, &E0,&29, &E0,&31, &E0,&39;7
    defb &30,&02, &30,&0A, &30,&12, &30,&1A, &30,&22, &30,&2A, &30,&32, &30,&3A;8
    defb &80,&02, &80,&0A, &80,&12, &80,&1A, &80,&22, &80,&2A, &80,&32, &80,&3A;9
    defb &D0,&02, &D0,&0A, &D0,&12, &D0,&1A, &D0,&22, &D0,&2A, &D0,&32, &D0,&3A;10
    defb &20,&03, &20,&0B, &20,&13, &20,&1B, &20,&23, &20,&2B, &20,&33, &20,&3B;11
    defb &70,&03, &70,&0B, &70,&13, &70,&1B, &70,&23, &70,&2B, &70,&33, &70,&3B;12
    defb &C0,&03, &C0,&0B, &C0,&13, &C0,&1B, &C0,&23, &C0,&2B, &C0,&33, &C0,&3B;13
    defb &10,&04, &10,&0C, &10,&14, &10,&1C, &10,&24, &10,&2C, &10,&34, &10,&3C;14
    defb &60,&04, &60,&0C, &60,&14, &60,&1C, &60,&24, &60,&2C, &60,&34, &60,&3C;15
    defb &B0,&04, &B0,&0C, &B0,&14, &B0,&1C, &B0,&24, &B0,&2C, &B0,&34, &B0,&3C;16
    defb &00,&05, &00,&0D, &00,&15, &00,&1D, &00,&25, &00,&2D, &00,&35, &00,&3D;17
    defb &50,&05, &50,&0D, &50,&15, &50,&1D, &50,&25, &50,&2D, &50,&35, &50,&3D;18
    defb &A0,&05, &A0,&0D, &A0,&15, &A0,&1D, &A0,&25, &A0,&2D, &A0,&35, &A0,&3D;19
    defb &F0,&05, &F0,&0D, &F0,&15, &F0,&1D, &F0,&25, &F0,&2D, &F0,&35, &F0,&3D;20
    defb &40,&06, &40,&0E, &40,&16, &40,&1E, &40,&26, &40,&2E, &40,&36, &40,&3E;21
    defb &90,&06, &90,&0E, &90,&16, &90,&1E, &90,&26, &90,&2E, &90,&36, &90,&3E;22
    defb &E0,&06, &E0,&0E, &E0,&16, &E0,&1E, &E0,&26, &E0,&2E, &E0,&36, &E0,&3E;23
    defb &30,&07, &30,&0F, &30,&17, &30,&1F, &30,&27, &30,&2F, &30,&37, &30,&3F;24
    defb &80,&07, &80,&0F, &80,&17, &80,&1F, &80,&27, &80,&2F, &80,&37, &80,&3F;25

TestSprite:
	incbin ".\res\SpriteCPC.raw"