; GAVDAN-ROVER for the GAVDAN-2 6502 computer
;
; vim syntax asm_ca65 (https://github.com/maxbane/vim-asm_ca65)
; for vasm6502_oldstyle assembler (http://sun.hasenbraten.de/vasm/)

; setup pointers for the 6522 Versatile Interface Adapter (VIA)

VIA_PortB	= $BFF0		; port b data register on 6522 VIA
VIA_PortA	= $BFF1		; port a data register on 6522 VIA
VIA_DDRB	= $BFF2		; data direction register for port b on the 6522 VIA 
VIA_DDRA	= $BFF3		; data direction register for port a on the 6522 VIA


; set base location of code

  .org $6000

; motor control routines

forward:
	lda #%00010001
	sta VIA_PortA
	jsr delay
        lsr VIA_PortA
	jsr delay
	lsr VIA_PortA
	jsr delay
	lsr VIA_PortA
	jsr delay
	rts

backward:
	lda #%10001000
	sta VIA_PortA
	jsr delay
	asl VIA_PortA
	jsr delay
	asl VIA_PortA
	jsr delay
	asl VIA_PortA
	jsr delay
	rts
delay:
	ldx #3
loop2:
	ldy #128
loop3:
	dey
	bne loop3
	dex
	bne loop2
	rts
