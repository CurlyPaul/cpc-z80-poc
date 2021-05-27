;****************************************
; Header
;****************************************
ScreenSize equ &4000
UseHardwareKeyMap equ 1 	; enable the Keyboard map for the input reader	
read ".\libs\Multiplatform_ReadJoystick_Header.asm"

org &8000

;;
;; Frame counters can be simpler by checking one bit
;; Remove the exx from keyboard checker as it's dangerous
;;
;;

call Screen_Init
call Palette_Init
call KeyboardScanner_Init
call InterruptHandler_Init


;****************************************
; Main Program
;****************************************
MainLoop:
	call Player_ReadControlsDual		; Read state of the controllers in hl
	ld a,h
	and l
	ld h,a					; Read both controllers, and merge them together into H	

	push hl
		call ProcessFireButton
	pop hl

	call UpdatePlayerPosition
	call UpdateBulletPosition
	di
	call DrawScreen
	ei
	;; This seems like a crazy way to signal for a frame. Hit google
	ld d,0
WaitFrame:
	ld a,1
	cp d
	jp z,MainLoop
	halt
jp WaitFrame
	


InterruptHandler:
	;exx
	;ex af,af'
		ld b,&F5 	; The PPI (Programmable Peripheral Interface) is a device which gives us info about the screen
		in a,(c)	; read a from port (bc)
		rra 		; right most bit indicates vsync, so push it into the carry
		jp nc,NoVsync
		call SwitchScreenBuffer
		ld d,1
	NoVsync: 		;; This could just return if we aren't in vsync, but I'll leave this here for expansion later
	;exx
	;ex af,af'
	ei
ret

SwitchScreenBuffer:
	; Flips all the screen buffer variables and moves the back buffer onto the screen
	ld a,(ScreenStartAddressFlag)
	sub 16
	jp nz, SetScreenBufferTwo
SetScreenBufferOne:
	ld de,48
	ld (ScreenStartAddressFlag),de
	ld de,&4000
	ld (BackBufferAddress),de
	ld de,&7FFF
	ld (ScreenOverflowAddress),de
	jp DoSwitchScreen
SetScreenBufferTwo:
	ld de,16
	ld (ScreenStartAddressFlag),de
	ld de,&C000
	ld (BackBufferAddress),de 
	ld de,&FFFF
	ld (ScreenOverflowAddress),de
DoSwitchScreen:
	ld bc,&BC0C 	; CRTC Register to change the start address of the screen
	out (c),c
	inc b
	ld a,(ScreenStartAddressFlag)
	out (c),a
ret

UpdatePlayerPosition:
	;; Inputs:	HL byte from the keyboard scanner			
	ld de,(CursorCurrentPosXY)		; Load the last player position
	call Cursor_ProcessDirections		; Process the Movement directions
	ld (CursorCurrentPosXY),de		; Save the new Cursor Position
ret

ProcessFireButton:
	;; Inputs:	HL byte from the keyboard scanner
	bit Keymap_F1,h				;Test the fire button bit in H
	ret nz
	call SpawnBullet
ret

UpdateBulletPosition:
	
	ld hl,(BulletCurrentPosXY)		;; This notation loads both values
	ld bc,(BulletMoveSpeedXY)
	add hl,bc
	ld (BulletCurrentPosXY),hl

	;; Check if we are out of bounds
	ld a,h					;; Current X pos
	ld hl,CursorMaxX
	cp (hl) 				;; Compare to limit
	ret c					;; Return if not over limit

	;; If we are here we are out of bounds, mark the bullet as dead
	ld hl,(BulletIsAlive)
	ld hl,0
	ld (BulletIsAlive),hl
ret

SpawnBullet:
	ld hl,(CursorCurrentPosXY)
	ld b,&06	;; This is just a majic number to offset it 
	ld c,&1C	;; to the correct spot relative to the player
	add hl,bc	

	ld (BulletCurrentPosXY),hl
	ld hl,BulletIsAlive
	ld (hl),1
	ld hl,BulletCurrentFrame
	ld (hl),BulletFrameOne
	ld hl,BulletFrameCounter
	ld (hl),0
ret

DrawScreen:
	call DrawBackground
	call DrawPlayer

	ld hl,(BulletIsAlive)
	ld a,1
	cp l
	jp z,DrawBullet
ret

DrawBackground:
	; for now, just draw an empty background
	call ClearScreen
ret

ClearScreen:
	ld hl,(BackBufferAddress)
	ld de,(BackBufferAddress)
	inc de
	ld bc,ScreenSize-1
	ld (hl),0
	ldir
ret

DrawBullet:
	ld a,(BulletFrameCounter)
	adc &40
	ld (BulletFrameCounter),a
	jp nc,DoBulletDrawing
		ld hl,BulletFrameOne
		ld de,(BulletCurrentFrame)
		or a				;; this is needed to clear the carry as sbc will be effected by it
		sbc hl,de
		jp nz,DoBulletDrawing		;; I just want to show the first frame once, so never switch back to it
			ld hl,BulletFrameTwo
			ld (BulletCurrentFrame),hl

	DoBulletDrawing:
		ld de,(BulletCurrentFrame)
		ld bc,(BulletCurrentPosXY)
		call GetScreenPos
		ld b,24	 ; Lines
		ld c,6   ; Bytes per line
		call DrawSprite
ret

DrawPlayer:	
	ld a,(FrameCounter)
	adc &80
	ld (FrameCounter),a
	jp nc,DoPlayerDrawing		

	ld hl,PickleFrameOne
	ld de,(PickleCurrentFrame)
	or a				;; this is needed to clear the carry as sbc will be effected by it
	sbc hl,de
	jp nz,ChangeToFrameOne		;; How would I do this with 3 frames or more?
		ld hl,PickleFrameTwo
		ld (PickleCurrentFrame),hl
		jp DoPlayerDrawing
	ChangeToFrameOne:
		ld hl,PickleFrameOne
		ld (PickleCurrentFrame),hl

	DoPlayerDrawing:
		ld bc,(CursorCurrentPosXY)
		call GetScreenPos
		ld de,(PickleCurrentFrame)
		ld b,56 ; Lines
		ld c,12 ; Bytes per line
	
DrawSprite:
	;; Inputs: 
	;; 	DE - Frame address
	;; 	HL - Screen address 	
	;; 	B  - Lines per sprite
	;; 	C  - Bytes per line
	SpriteNextLine:
		push hl
		push bc
	SpriteNextByte:
			ld a,(de)	; Sourcebyte	
			ld (hl),a	; Screen desintation

			inc de
			inc hl
			dec c
			jr nz,SpriteNextByte
		pop bc
		pop hl
	call GetNextLine 		; expected - c051, C0A1, C0F1.. last C9e1
	djnz SpriteNextLine 		; djnz - decreases b and jumps when it's not zero
ret

GetScreenPos:
	;; Inputs: BC - X Y
	;; Returns HL : screen memory locations

	;; Calculate the ypos first
	push bc				; push bc because we need to preserve the value of b (xpos)
		ld b,0			; which we must zero out because bc needs to be the y coordinate
		ld hl,scr_addr_table	; load the address of the label into h1
		add hl,bc		; as each element in the look up table is 2 bytes(&XXXX) long, so add the value of c (ypos) to hl twice
		add hl,bc		; ...to convert h into an offset from the start of the lookup table
		
		;; Now read two bytes from the address held in hl. We have to do this one at a time

		ld a,(hl)		; stash one byte from the address in hl into a
		inc l			; increment the address we are pointing at
		ld h,(hl)		; load the next byte into the address at h into h
		ld l,a			; now put the first byte we read back into l

	;; Now calculate the xpos, this is much easier as these are linear in screen
	pop bc				; reset to BC to the original XY values
					
	ld a,b				; need to stash b as the next op insists on reading 16bit - we can't to ld c,(label)
	ld bc,(BackBufferAddress)	; bc now contains either &4000 or &C000, depending which cycle we are in
	ld c,a				; bc will now contain &40{x}
	add hl,bc			; hl = hl + bc, add the x and y values together
ret

GetNextLine:
	;; Inputs: HL Current screen memory location
	;; Returns: HL updated to the start of the next line
	ld a,h				; load the high byte of hl into a
	add &08				; it's just a fact that each line is + &0800 from the last one
	ld h,a				; put the value back in h

	push hl
	push de
		ld d,h
		ld e,l
		ld hl,(ScreenOverflowAddress)
		sbc hl,de 	; (OverflowAddress - CurrentAddress)
	pop de
	pop hl
	ret p			; if top bit is set we've wrapped and ran out memory			
	push bc		
		ld bc,&C050	; if we've wrapped add this magic number nudge back to the right place
		add hl,bc
	pop bc	
ret

InterruptHandler_Init:
	; Sets the rastor interrupt to our interrupt handler
	di
		ld a,&C3		; jp op code
		ld (&0038),a		; &0038 is executed when the rastor interrupt fires
		ld hl,InterruptHandler
		ld (&0039),hl		; write jp InterruptHandler into the target address
	ei
ret

KeyboardScanner_Init:
	; Unsurprisingly initiases the keyboard scanner, which needs to know which area of memory it can store data in
	ld hl,KeyboardScanner_KeyPresses
	ld d,h
	ld e,l
	inc de
	ld bc,15
	ld (hl),255
	z_ldir
ret

;****************************************
; Variables
;****************************************
ScreenStartAddressFlag:	db 48  		; 16 = &4000 48 = &C000 
ScreenOverflowAddress: 	dw &7FFF
BackBufferAddress: 	dw &4000 
FrameCounter: 		db 0

CursorMinX: 		db 1			; Player Move limits
CursorMaxX: 		db 68 			; Screen width 80 bytes - player width (12)
CursorMinY: 		db 1			
CursorMaxY: 		db 140			; Screen height 200 pixels - player height (56)

CursorCurrentPosXY:	dw &0000		; Player xy pos
PickleCurrentFrame: 	dw PickleFrameOne	; Pointer to the current frame
CursorMoveSpeedXY: 	dw &040A		; Player Move speed

BulletCurrentPosXY:	dw &1030
BulletCurrentFrame: 	dw BulletFrameOne		
BulletMoveSpeedXY: 	dw &0202
BulletFrameCounter: 	db 0
BulletIsAlive:		db 1

align32	
KeyMap equ KeyMap2+16			; wsad bnm p
KeyMap2:				; Default controls
	db &F7,&09,&7f,&05,&ef,&09,&df,&09,&f7,&09,&fB,&09,&fd,&09,&fe,&09 ;p2-pause,f3,f2,f1,r,l,d,u
	db &f7,&09,&f7,&05,&bf,&05,&bf,&06,&df,&07,&df,&08,&ef,&07,&f7,&07 ;p1-pause,f3,f2,f1,r,l,d,u

KeyboardScanner_KeyPresses: ds 16 	; define 16 bytes to for the keyboard scanner to use

read ".\libs\Multiplatform_ScanKeys.asm"
read ".\libs\CPC_V1_KeyboardDriver.asm"
read ".\libs\Multiplatform_ReadJoystickKeypressHandler.asm"
read ".\libs\CA_Cursor_ProcessDirections.asm"
read ".\libs\CPC_V1_SimpleScreenSetUp.asm"
read ".\libs\CPC_V1_SimplePalette.asm"

;****************************************
; Resources
;****************************************
PickleFrameOne:
	incbin ".\res\pickle1.raw"
PickleFrameTwo:
	incbin ".\res\pickle2.raw"
BulletFrameOne:
	incbin ".\res\bullet1.raw"
BulletFrameTwo:
	incbin ".\res\bullet2.raw"
