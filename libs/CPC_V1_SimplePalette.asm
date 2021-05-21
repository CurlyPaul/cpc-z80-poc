;***************************************
; Taken from an example at www.cpcwiki.eu/index.php/Programming An_example_loader
;***************************************
scr_set_border		equ &bc38
scr_set_ink		equ &BC32

; TODO This definetly doesn't work when using customer interrupts
; Need to figure out which bits to set and out (c),c &7F https-//www.cpcwiki.eu/index.php/Gate_Array#Controlling_the_Gate_Array

ColourPalette:
defb 0,&12,&09,&0C,&02,&1A,&12,&12,&12,&12,&12,&12,&12,&12,&12,&12,0



setup_colours:
di
exx
ld b,16			  ;; 16 colours
xor a			  ;; start with pen 0

do_colours:
push bc
push af
	ld c,(hl)		  ;; colour value
	inc hl
	ld b,c			  ;; B=C so colours will not flash
	push hl
	;; A = pen number
	;; B,C = colour
	call &BC32		  ;; set colour for pen
	pop hl
pop af
pop bc
;; increment pen number
inc a
djnz do_colours

;; set border colour

ld c,(hl)
ld b,c
call scr_set_border	
exx
ei
ret