
Cursor_ProcessDirections:				;We're going to process keypressses
										;H should contain the keypress bits ---FRLDU
										;DE should contain last XY pos
										
		ld bc,(CursorMoveSpeedXY)		;Load XY Move speeds into BC

		bit Keymap_D,h					;Test the down bit in H
		jr nz,OnscreenCursorNotDown
		
		ld a,e
		di							;Read in Y pos
			exx
				ld hl,CursorMaxY			;Get Max Ypos
				Call CursorDoCheck
			add c
		ei							;Move down
		ld e,a							;Store new Y
OnscreenCursorNotDown:
		bit Keymap_U,h					;Test the up bit in H
		jr nz,OnscreenCursorNotUp
		ld a,e							;Read in Y pos
		di
		exx
			ld hl,CursorMinY			;Get Min Ypos
			Call CursorDoCheck
		sub c	
		ei						;Move up
		ld e,a							;Store new Y
OnscreenCursorNotUp:
		bit Keymap_R,h					;Test the right bit in H
		jr nz,OnscreenCursorNotRight
		ld a,d							;Read in the X pos
		di
		exx
			ld hl,CursorMaxX			;Get the Max Xpos
			Call CursorDoCheck
		add b
		ei							;Move the cursor Right
		ld d,a
OnscreenCursorNotRight:
		bit Keymap_L,h					;Test the right bit in L
		jr nz,OnscreenCursorNotLeft
		ld a,d							;Read in the X pos
		di
		exx
			ld hl,CursorMinX			;Get the Min Xpos
			Call CursorDoCheck
		ei
		sub b							;Move the cursor Left
		ld d,a
OnscreenCursorNotLeft:
OnscreenCursorAbandon:	
	ret			

CursorDoCheck:
		cp (hl) 					;Compare to limit
		exx
		ret nz						;Return if not over limit
		
		pop af						;Remove the return
		ret	;Failed limit check - so give up

		