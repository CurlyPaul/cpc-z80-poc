;;*****************************************************************************************
;; Origially based an example in the zip file found at https://www.chibiakumas.com/z80/
;;*****************************************************************************************

Cursor_ProcessDirections:				;We're going to process keypressses
							;H should contain the keypress bits ---FRLDU
							;DE should contain last XY pos
										
		ld bc,(CursorMoveSpeedXY)		;Load XY Move speeds into BC

		bit Keymap_D,h				;Test the down bit in H
		jr nz,OnscreenCursorNotDown
		
		ld a,e					;Read in Y pos
		exx
			ld hl,CursorMaxY		;Get Max Ypos
			Call CursorDoMaxCheck
			add c
		
							;Move down
		ld e,a					;Store new Y
OnscreenCursorNotDown:
		bit Keymap_U,h				;Test the up bit in H
		jr nz,OnscreenCursorNotUp
		ld a,e					;Read in Y pos
		exx
			ld hl,CursorMinY		;Get Min Ypos
			Call CursorDoMinCheck
		sub c	
							;Move up
		ld e,a					;Store new Y
OnscreenCursorNotUp:
		bit Keymap_R,h				;Test the right bit in H
		jr nz,OnscreenCursorNotRight
		ld a,d					;Read in the X pos
		exx
			ld hl,CursorMaxX		;Get the Max Xpos
			Call CursorDoMaxCheck
			add b
							;Move the cursor Right
		ld d,a
OnscreenCursorNotRight:
		bit Keymap_L,h				;Test the right bit in L
		jr nz,OnscreenCursorNotLeft
		ld a,d
		exx					;Read in the X pos
			ld hl,CursorMinX		;Get the Min Xpos
			Call CursorDoMinCheck
			sub b					;Move the cursor Left
		ld d,a
OnscreenCursorNotLeft:
OnscreenCursorAbandon:	
	ret			

CursorDoMaxCheck:
		cp (hl) 					; Compare to limit
		exx						; hl now contains the keypress bits again
		ret c						; Return if not over limit
		
		pop af						; Remove the return
		ret	;Failed limit check - so give up

CursorDoMinCheck:
		cp (hl) 					; Compare to limit
		exx
		ret nc						; Return if not under limit
		
		pop af						; Remove the return ??? TODO understand why this exactly
		ret	;Failed limit check - so give up

		