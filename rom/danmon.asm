; DANMON monitor ROM for the GAVDAN-2 6502 computer
;
; vim syntax asm_ca65 (https://github.com/maxbane/vim-asm_ca65)
; for vasm6502_oldstyle assembler (http://sun.hasenbraten.de/vasm/)

; include EhBasic 2.22p5 from basic.6502

  .include "ehbasic.asm"

; for EhBasic, put the IRQ and MNI code in RAM so that it can be changed

IRQ_vec     	= VEC_SV+2	; IRQ code vector
NMI_vec     	= IRQ_vec+$0A	; NMI code vector


; setup pointers for the 6522 Versatile Interface Adapter (VIA)

VIA_PortB	= $BFF0		; port b data register on 6522 VIA
VIA_PortA	= $BFF1		; port a data register on 6522 VIA
VIA_DDRB	= $BFF2		; data direction register for port b on the 6522 VIA 
VIA_DDRA	= $BFF3		; data direction register for port a on the 6522 VIA


; setup pointers for the 6551 Asyncronous Communication Interface Adapter (ACIA)

ACIA_Data	= $BFEC		; data register of 6551 ACIA
ACIA_Status	= $BFED		; status register of the 6551 ACIA
ACIA_Command	= $BFEE		; command register of the 6551 ACIA
ACIA_Control	= $BFEF		; control register of the 6551 ACIA


; setup variable locations, these are placed in EhBasic's unsued area of
; zero page, $E2 through $EE, and $F1 through $FF (Check?)

CmdPtr		= $E2		; location of command pointer
PrintPtr	= $E3		; location of print immediate sting, 2 bytes
MonPtr		= $E5		; location of monitor pointer, 2 bytes
MonPtrTmp	= $E7		; temporary store location for monitor pointer, 2 bytes
StrPtr		= $E9		; location of store pointer, used in store command, 2 bytes
StrLoop		= $EB		; location of store loop variable, used in store command
HexDec		= $EC		; loop varible used in the hex decoder subroutine
HexStatus	= $ED		; varilble used to track status of hex tes subroutine

; setup other pointer locations

CmdBuf		= Ibuffe+1	; location of command buffer, placed after EhBasics buffer located in page 2 ($200)
                                ; note depending on location, a full buffer could overwrite the base RAM used by EhBASIC


; set base address of code, base address of the ROM in the GAVDAN-1

  .org $EA00


RES_vec:
; lable used by EhBasic for the following routine
reset:
; start location og DANMON ROM execution on reset
  cld				; clear decimal mode
  ldx #$FF			; empty stack
  txs				; set the stack

; set up vectors and interrupt code, copy them to page 2

  ldy #END_CODE-LAB_vec		; set index/count

LAB_stlp:
  lda LAB_vec-1,Y		; get byte from interrupt code
  sta VEC_IN-1,Y		; save to RAM
  dey				; decrement index/count
  bne LAB_stlp			; loop if more to do

  jsr sub_ACIA_Init		; setup the 6551 ACIA
  jsr sub_PrintString		; print ROM welcome string
  .byte $07,$1B,"[2J","GAVDAN-2 Computer 32K",$0D,$0A,$0D,$0A,$00
  jsr sub_StartMonitor		; initialise the monitor 

cmd_Ready:
  jsr sub_MonitorPrompt		; display the monitor prompt

cmd_Input:  
  jsr sub_ACIA_GetByte		; go and get a byte from the 6551 ACIA
  bcc cmd_Input			; if no char pressed then lopp back and try again
  ldx CmdPtr			; load command pointer into x register
  cmp #$1B			; ESC pressed?
  beq cmd_Escape		; yes, execute escape code
  cmp #$08			; backspace pressed?
  beq cmd_Backspace		; yes, execute backspace code
  cmp #$0D			; CR pressed?
  beq cmd_Parse			; yes, go parse the command
  cpx #$FF			; is the pointer at $FF, command buffer full?
  beq cmd_BufferFull		; yes, execute the buffer full code
  jsr sub_ACIA_SendByte         ; echo character out to screen
  sta CmdBuf,x			; store character into the command buffer
  inc CmdPtr                    ; increment the command pointer
  jmp cmd_Input                 ; go back for more input

cmd_Escape:
  jsr sub_PrintString           ; print escaped
  .byte $0D,$0A,"Escaped",$00
  jmp cmd_Ready			; go back for more input

cmd_Backspace:
  cpx #$00			; is the buffer empty?
  beq cmd_Input			; yes, go back for more input
  dec CmdPtr			; decrement the command pointer
  lda #$08			; load ASCII of a backspace into the accumulator
  jsr sub_ACIA_SendByte		; amnd print to the terminal
  lda #" "			; load ASCII of a space into the accumulator
  jsr sub_ACIA_SendByte		; and print to the terminal
  lda #$08			; load ASCII of a backspace into accumulator
  jsr sub_ACIA_SendByte		; and print to the terminal
  jmp cmd_Input			; go back for more input

cmd_BufferFull:
  lda #$07                      ; yes, load ASCII for bell into accumulator
  jsr sub_ACIA_SendByte         ; send bell to terminal
  jmp cmd_Input                 ; go back for more input

cmd_Parse:
  cpx #$00			; check if the CmdPtr is 0
  beq cmd_Ready			; yes, nothing entered, go back for another command
  ldx #$00			; load x with 0, will be used to index the command buffer

cmd_Parse_lbl1:
  lda CmdBuf,x			; get a character from the command buffer
  cmp #$20			; is it a space?
  bne cmd_Parse_lbl2		; no, continue with parse
  inx				; yes, increment x index
  cpx CmdPtr			; are we at the end of the command buffer
  beq cmd_Ready			; yes, and only spaces, so go back for another command
  jmp cmd_Parse_lbl1		; go back for another character

cmd_Parse_lbl2:  
  and #$DF			; enforce characher to uppercase
  cmp #"B"			; B = Basic
  bne cmd_Parse_lbl3		; no, so check the next command
  jmp cmd_Basic			; yes, execute the basic code

cmd_Parse_lbl3:
  cmp #"C"			; C = Clear
  bne cmd_Parse_lbl4		; no, so check the next command
  jmp cmd_Clear			; yes, execute the clear code

cmd_Parse_lbl4:
  cmp #"E"			; E = Execute
  bne cmd_Parse_lbl5		; no, so check the next command
  jmp cmd_Execute		; yes, execute the execute code

cmd_Parse_lbl5:
  cmp #"H"			; H = Help
  bne cmd_Parse_lbl6		; no, so check next command
  jmp cmd_Help			; yes, execute help code

cmd_Parse_lbl6:
  cmp #"L"			; L = List
  bne cmd_Parse_lbl7		; no so check next command
  jmp cmd_List			; yes, execute list code 

cmd_Parse_lbl7:
  cmp #"M"			; M = Monitor prompt set
  bne cmd_Parse_lbl8		; no, so check next command
  jmp cmd_SetMonPrompt		; yes, exec set monitor prompt code

cmd_Parse_lbl8:
  cmp #"R"			; R = Reset
  bne cmd_Parse_lbl9			; no, so check next command
  jmp cmd_Reset			; yes, execute the reset code

cmd_Parse_lbl9:
  cmp #"S"			; S = Store
  bne cmd_error			; no, run out of commands so display and error
  jmp cmd_Store

cmd_error:
  jsr sub_PrintString		; print unknown command to the terminal
  .byte $0D,$0A,"Unknown command",$00
  jmp cmd_Ready			; go back for another command

cmd_syntax:
  jsr sub_PrintString		; print syntax error to the terminal
  .byte $0D,$0A,"Syntax error",$00
  jmp cmd_Ready			; go back for another command

cmd_Basic:
  jsr sub_PrintString		; print EhBasic start prompt to the screen
  .byte $0D,$0A,$0D,$0A,"Start 6502 EhBASIC [C]old/[W]arm ?",$00

cmd_Basic_lbl1:
  jsr sub_ACIA_GetByte          ; go and get a byte from the 6551 ACIA
  bcc cmd_Basic_lbl1		; if no char pressed then loop back and try again
  and #$DF			; force convert to upercase
  cmp #"C"			; C = Cold Start
  beq cmd_Basic_lbl2		; yes, jump to lunch EhBasic via Cold Start
  cmp #"W"			; W = Warm Start
  beq cmd_Basic_lbl3		; yes, jump to launch EhBasic via Warm Start
  jmp cmd_Basic_lbl1		; not C or W, so go back and try again

cmd_Basic_lbl2:
  jmp LAB_COLD			; EhBasic Cold Start

cmd_Basic_lbl3:
  jmp LAB_WARM			; EhBasic Warm Start

cmd_Clear:
  inx                           ; increment x, moving us forward in the command buffer
  cpx CmdPtr                    ; are we at the end of the command buffer
  beq cmd_Clear_lbl1            ; yes so go ahead an run the clear command
  lda CmdBuf,x                  ; get a character from the command buffer
  cmp #$20                      ; is it a space?
  beq cmd_Clear			; yes, go back to check the rest of the command buffer
  jmp cmd_syntax                ; no, so go and report syntax error

cmd_Clear_lbl1:
  jsr sub_PrintString		; print control codes to clear the screen
  .byte $1B,"[2J",$00
  jmp cmd_Ready			; go back for another command

cmd_Execute:
  inx                           ; increment x, moving us forward in the command buffer
  cpx CmdPtr                    ; are we at the end of the command buffer
  bne cmd_Execute_lbl1          ; no we're not. so check the rest of the command
  jmp cmd_syntax                ; no characters found so report an error

cmd_Execute_lbl1:
  lda CmdBuf,x                  ; get a character from the command buffer
  cmp #$20                      ; is it a space?
  beq cmd_Execute               ; yes, go back to check the rest of the command buffer

cmd_Execute_lbl2:
  dex                           ; decremt x, to return it back to the start of the hex to decode
  jsr sub_Hex_Decode            ; decode the hex address of the pointer
  ldy HexStatus                 ; retrieve the hex status, if set to 1 then no valid hex was found
  cpy #1                        ; check error with hex
  bne cmd_Execute_lbl3          ; no error, so carry on processing
  lda MonPtrTmp+1               ; restore monitor pointer high byte from temporary area
  sta MonPtr+1                  ; store back in monitor pointer high byte
  lda MonPtrTmp                 ; restore monitor pointer low byte from temporary area
  sta MonPtr                    ; store back in monitor pointer low byte
  jmp cmd_syntax                ; bad hex found, so report an error

cmd_Execute_lbl3:
  jmp (MonPtr)			; execute code from address sotred in to monitor pointer

cmd_Help:
  inx				; increment x, moving us forward in the command buffer
  cpx CmdPtr			; are we at the end of the command buffer
  beq cmd_Help_lbl1		; yes so go ahead an run the help command
  lda CmdBuf,x			; get a character from the command buffer
  cmp #$20			; is it a space?
  beq cmd_Help			; yes, go back to check the rest of the command buffer
  jmp cmd_syntax		; no, so go and report syntax error

cmd_Help_lbl1:
  jsr sub_PrintString		; print the following help message to the terminal
  .byte $0D,$0A,$0D,$0A
  .byte "DANMON 1.20 Commands",$0D,$0A
  .byte " B               - Launch Enhanced [B]asic",$0D,$0A
  .byte " C               - [C]lear the screen",$0D,$0A
  .byte " E <addr>        - [E]xecute code at hex address <addr>",$0D,$0A
  .byte " H               - [H]elp",$0D,$0A
  .byte " L <addr>        - [L]ist memory starting at hex address <addr>",$0D,$0A
  .byte " M <addr>        - Set the [M]onitor prompt to hex address <addr>",$0D,$0A
  .byte " R               - Monitor [R]eset",$0D,$0A
  .byte " S <da>:<da>:... - [S]tore hex data <da> at the monitor prompt address",$0D,$0A
  .byte $00
  jmp cmd_Ready			; go back for another command

cmd_List:
  inx                           ; increment x, moving us forward in the command buffer
  cpx CmdPtr                    ; are we at the end of the command buffer
  bne cmd_List_lbl1		; no we're not. so check the rest of the command
  jmp cmd_List_lbl3		; no characters found so go ahead and list memory

cmd_List_lbl1:
  lda CmdBuf,x                  ; get a character from the command buffer
  cmp #$20                      ; is it a space?
  beq cmd_List			; yes, go back to check the rest of the command buffer

cmd_List_lbl2:
  dex                           ; decremt x, to return it back to the start of the hex to decode
  jsr sub_Hex_Decode            ; decode the hex address of the pointer
  ldy HexStatus                 ; retrieve the hex status, if set to 1 then no valid hex was found
  cpy #1                        ; check error with hex
  bne cmd_List_lbl3		; no error, so carry on processing
  lda MonPtrTmp+1               ; restore monitor pointer high byte from temporary area
  sta MonPtr+1                  ; store back in monitor pointer high byte
  lda MonPtrTmp                 ; restore monitor pointer low byte from temporary area
  sta MonPtr                    ; store back in monitor pointer low byte
  jmp cmd_syntax                ; bad hex found, so report an error

cmd_List_lbl3:
  jsr sub_MonitorPrompt		; diaply the monitor prompt
  ldy #0			; set y register to zero, use to count memory locations

cmd_List_lbl4:
  lda (MonPtr),y		; get a byte from the address in the monitor pointer
  jsr sub_PrintHex		; send it to the console a hex
  lda #" "			; insert a space between output
  jsr sub_ACIA_SendByte		;
  iny				; increment y index, used to count memory locations
  cpy #$10			; have we reached 16 bytes, one too many
  bne cmd_List_lbl4		; no, so go back and list more memory
  lda MonPtr			; load the low byte on the monitor pointer
  adc #15			; add 15 to it with carry
  sta MonPtr			; and save it back to the low byte of the monitor pointer
  bcc cmd_List_lbl5		; was the carry clear, if so jump onwards
  inc MonPtr+1			; no we have a carry, so increment the high byte of the monitor pointer

cmd_List_lbl5:
  jsr sub_PrintString		; display continue prompt
  .byte $0D,$0A,"Continue (Y)/N",$00

cmd_List_lbl6:
  jsr sub_ACIA_GetByte		; get a key press
  bcc cmd_List_lbl6		; if no char received, loop back and try again
  cmp #$0D			; is it a return
  beq cmd_List_lbl7		; yes, so jump to reset the line for next output
  and #$DF			; convert to uppercase
  cmp #"Y"			; is it a Y for yes
  beq cmd_List_lbl7		; yes, so jump to clear the line for next output
  cmp #"N"			; is it an N for no
  beq cmd_List_lbl8		; yes, so jump to clear the promt and exit
  jmp cmd_List_lbl6

cmd_List_lbl7:
  lda #$0B			; load cursor up control code
  jsr sub_ACIA_SendByte		; send it to the console
  jmp cmd_List_lbl3		; loop back to list more memory

cmd_List_lbl8:
  jsr sub_PrintString		; print to clear the continue prompt from the console
  .byte $0D,"              ",$00
  jmp cmd_Ready			; exit and return to monitor prompt

cmd_SetMonPrompt:
  inx                           ; increment x, moving us forward in the command buffer
  cpx CmdPtr                    ; are we at the end of the command buffer
  bne cmd_SetMonPrompt_lbl1	; no we're not. so check the rest of the command
  jmp cmd_syntax                ; no characters found so report an error

cmd_SetMonPrompt_lbl1:
  lda CmdBuf,x                  ; get a character from the command buffer
  cmp #$20                      ; is it a space?
  beq cmd_SetMonPrompt		; yes, go back to check the rest of the command buffer

cmd_SetMonPrompt_lbl2:
  dex                           ; decremt x, to return it back to the start of the hex to decode
  jsr sub_Hex_Decode            ; decode the hex address of the pointer
  ldy HexStatus                 ; retrieve the hex status, if set to 1 then no valid hex was found
  cpy #1                        ; check error with hex
  bne cmd_SetMonPrompt_lbl3	; no error, so carry on processing
  lda MonPtrTmp+1               ; restore monitor pointer high byte from temporary area
  sta MonPtr+1                  ; store back in monitor pointer high byte
  lda MonPtrTmp                 ; restore monitor pointer low byte from temporary area
  sta MonPtr                    ; store back in monitor pointer low byte
  jmp cmd_syntax                ; bad hex found, so report an error

cmd_SetMonPrompt_lbl3:
  jmp cmd_Ready			; all done, go back for a new command

cmd_Reset:
  inx				; inxrement x, move us forward in the command buffer
  cpx CmdPtr                    ; are we at the end of the command buffer
  beq cmd_Reset_lbl1            ; yes so go ahead an run the reset command
  lda CmdBuf,x                  ; get a character from the command buffer
  cmp #$20                      ; is it a space?
  beq cmd_Reset			; yes, go back to check the rest of the command buffer
  jmp cmd_syntax                ; no, so go and report syntax error

cmd_Reset_lbl1:
  jmp reset                     ; jump to reset

cmd_Store:
  inx                           ; increment x, moving us forward in the command buffer
  cpx CmdPtr                    ; are we at the end of the command buffer
  bne cmd_Store_lbl1     	; no we're not. so check the rest of the command
  jmp cmd_syntax                ; no characters found so report an error

cmd_Store_lbl1:
  lda CmdBuf,x                  ; get a character from the command buffer
  cmp #$20                      ; is it a space?
  beq cmd_Store			; yes, go back to check the rest of the command buffer
  jmp cmd_Store_lbl5

cmd_Store_lbl5:
  cpx CmdPtr			; have we reached buffer end?
  bne cmd_Store_lbl6		; have werReached end without bytes to store
  jmp cmd_syntax		; no, go back and report error

cmd_Store_lbl6:
  lda MonPtr+1			; Copy monitor location to store routine cursor
  sta StrPtr+1			; so that store routine
  lda MonPtr			; can do it's
  sta StrPtr			; magic

cmd_Store_lbl7:
  lda #0
  sta StrLoop			; Zero the byte index.
  lda #0
  sta MonPtr			; We need a clean byte to deal with the math.

cmd_Store_lbl8:
  lda CmdBuf,x			;Get a byte
  jsr sub_Test_Hex		;Ask if it's hex
  ldy HexStatus
  bne cmd_Store_lbl12		;Not Hex.

cmd_Store_lbl9:
  ldy StrLoop			;Which nibble are we on?
  bne cmd_Store_lbl10		;Second nibble.
  jsr sub_To_Nibble		;First nibble. Shift A into MonCurLo
  inx
  cpx CmdPtr			;CmdBuf End?
  bne cmd_Store_lbl11		;Not done. Continue
  jsr cmd_Store_commit		;We're done.
  lda StrPtr+1			;put the monitor where it needs to be.
  sta MonPtr+1			;
  lda StrPtr			;
  sta MonPtr			;
  jmp cmd_Store_exit		;Go prompt

cmd_Store_lbl10:
  jsr sub_To_Nibble		;shift A into MonCurLo  
  jsr cmd_Store_commit		;Store it
  jmp cmd_Store_lbl13

cmd_Store_lbl11:
  inc StrLoop			;Advance
  jmp cmd_Store_lbl8		;Go again.

cmd_Store_lbl12:
  ldy StrLoop			;Load the Byte Index
  beq cmd_Store_lbl13		;It's Zero. Need to check again.
  jsr cmd_Store_commit		;It's one. Store previous nibble to memory.

cmd_Store_lbl13:
  lda StrPtr+1			;put the monitor where it needs to be.
  sta MonPtr+1			;
  lda StrPtr			;
  sta MonPtr			;
  inx

cmd_Store_lbl14:
  cpx CmdPtr			; have we reached buffer end
  beq cmd_Store_exit		; yes, so jumpt to exit
  lda CmdBuf,x                  ; no, so load next character
  cmp #":"			; we need a ":" to proceed
  bne cmd_Store_lbl15		; yes, so go back and keep processing
  inx
  cpx CmdPtr
  bne cmd_Store_lbl7

cmd_Store_lbl15:
  jmp cmd_syntax                ; no, go and report an error

cmd_Store_exit:
  jmp cmd_Ready			;Yup. We're done.

cmd_Store_commit:
  ldy #0
  lda MonPtr			; Grab the byte to store into A
  sta (StrPtr),y		; store at address indicated by storcurlo
  inc StrPtr			; Increment StorCurLo
  bne cmd_Store_commit_lbl1	; No rollover? Then done.
  inc StrPtr+1			; Rolled over. Increment StorCurHi

cmd_Store_commit_lbl1:
  rts

halt:
; stop here and do nothing

  jmp halt


sub_StartMonitor:
; run some code to setup the DANMON monitor ready to use
; set monitor pointer to start aty memory address #0300 in RAM

  lda #$03			; load high byte of monitor pointer 
  sta MonPtr+1			; store in monitor pointer, high byte
  lda #$00			; load low byte of monitor pointer
  sta MonPtr			; store in monitor pointer, low byte
  jsr sub_PrintString		; print monitor welcome string
  .byte "DANMON 1.20",$0D,$0A,$00
  rts


sub_MonitorPrompt:
; display the monitor prompt

  lda #0			; load a zero 
  sta CmdPtr			; and store it in the command pointer to reset it
  jsr sub_PrintString		; print CR,LF,CR,LF
  .byte $0D,$0A,$00	;
  lda MonPtr+1			; load the high byte of the monitor pointer
  jsr sub_PrintHex		; and print it out in hex
  lda MonPtr			; load the low byte of the monitor pointer
  jsr sub_PrintHex		; and print it out in hex
  jsr sub_PrintString		; print ":] " to end the prompt
  .byte ": ",$00		;
  rts


sub_PrintString:
; put the string following in-line until a NULL out to the console
; taken from http://www.6502.org/source/io/primm.htm

  pla				; get the low part of return address (data start address)
  sta PrintPtr			; store in print pointer
  pla				; get the high part of the return address (data start address)
  sta PrintPtr+1		; store in print pointer +1

sub_PrintString_lbl1:
  ldy #1			; fix that we're pointing one location short
  lda (PrintPtr),y		; get the next string character
  inc PrintPtr			; update the pointer
  bne sub_PrintString_lbl2	; if not, we're pointing to next character
  inc PrintPtr+1		; account for page crossing

sub_PrintString_lbl2:
  ora #0			; set flags according to contents of accumulator
  beq sub_PrintString_lbl3	; don't print the final NULL
  jsr sub_ACIA_SendByte		; write it out
  jmp sub_PrintString_lbl1	; back around

sub_PrintString_lbl3:
  inc PrintPtr
  bne sub_PrintString_lbl4
  inc PrintPtr+1		; account for page crossing

sub_PrintString_lbl4:
  jmp (PrintPtr)		; return to byte following final NULL


sub_Hex_Decode:
; decodes the hex value at the current buffer pointer and puts it in MonPtr
; adapted from Apple-1 monitor by steve wozniak

  lda MonPtr+1			; load high byte of monitor pointer
  sta MonPtrTmp+1		; store in temporary area for restore after error
  lda MonPtr			; load low byte of monitor pointer
  sta MonPtrTmp			; store in temporary area for restore after error
  lda #0			; load 0
  sta MonPtr+1			; and store this in the monitor pointer high byte to clear it
  sta MonPtr			; also store it in the monitor pointer low byte to clear it
  sta HexDec			; clear the hex decoder pointer to zero, used to track time we have lopped
  inx				; increment x, being used as an index for the command buffer

sub_Hex_Decode_lbl1:
  jsr sub_Test_Hex		; test current location of command buffer for hex
  ldy HexStatus			; load current hex status
  bne sub_Hex_Decode_lbl5	; no hex found, so exit, HexStatus will be 1
  jsr sub_To_Nibble		; we found some hex, so shift it into zero page
  inx				; increment x, being used as the index to the command buffer
  cpx CmdPtr                    ; are we at the end of the command buffer
  beq sub_Hex_Decode_lbl6       ; yes, no more to process so jump ahead
  lda CmdBuf,x
  cmp #$20			; is it a space
  beq sub_Hex_Decode_lbl4	; drop out to checking spaces, we can't accept more hex

sub_Hex_Decode_lbl2:
  inc HexDec			; increrment the hex decoder pointer
  lda HexDec                    ; and load it into the accumulator
  cmp #$04			; compare it with 4, have we reached 4 characters (max length hex address)
  bne sub_Hex_Decode_lbl1	; no, so go back and process more characters

sub_Hex_Decode_lbl3:
  lda CmdBuf,x			; load the next character
  cmp #$20			; is it a space
  bne sub_Hex_Decode_lbl5	; no, unexpected character so jump and return error

sub_Hex_Decode_lbl4:
  inx				; increment the x register used to index the command pointer
  cpx CmdPtr			; are we at the end of the command buffer 
  beq sub_Hex_Decode_lbl6	; yes, so exit cleanly
  jmp sub_Hex_Decode_lbl3       ; no, so go back and check more characters

sub_Hex_Decode_lbl5:
  ldy #1			; no, so load a 1 to store in the hex status
  sty HexStatus			; store the 1 in the hex status to report an error

sub_Hex_Decode_lbl6:
  rts				; We're done. Decoded address is in MonCurLo and MonCurHi


sub_Test_Hex:
;checks that a character in A is hex.
; if it is, returns with original value in A, and HexStatus = 0
; if it isn't, it returns with "uppercased" original value in A, and NotHex=1

  lda #0
  sta HexStatus			; start off assuming we will return with a good value.

sub_Test_Hex_lbl1:
; determine if it's a number nibble
  lda CmdBuf,x
  cmp #$30			; if < 0, go check it for letter
  bcc sub_Test_Hex_lbl2
  cmp #$3A			; if > 9, go check it for letter (or >= 9+1)
  bcs sub_Test_Hex_lbl2
  rts				; yes number. Return with value in A.

sub_Test_Hex_lbl2:
;determine if it's a letter nibble.
  and #$DF			; Convert to uppercase.
  cmp #$41			; if < A, skip
  bcc sub_Test_Hex_lbl3
  cmp #$47			; if > F, skip (or >= F+1)
  bcs sub_Test_Hex_lbl3
  adc #$B9			; A little binary math to make our letter suitable for bit shifting
  rts				; yes letter. Return with value in A.

sub_Test_Hex_lbl3:
  ldy #1          ; not letter or number.
  sty HexStatus
  rts

sub_To_Nibble:
;Move the nibble into position. Inspired by Woz Apple 1 Monitor.

  asl				; Move digit to MSD of A
  asl
  asl
  asl
  ldy #4			; Have to shift four times.

sub_To_Nibble_lbl1:
  asl				; Digit left, MSB to carry
  rol MonPtr			; Rotate into LSD
  rol MonPtr+1			; Rotate into MSD
  dey				; Done 4 shifts?
  bne sub_To_Nibble_lbl1	; No, loop
  rts


sub_PrintHex:
; converts the context of the accumualtor to ASCII for printing
; taken from Apple-1 monitor by steve wozniak

  pha                           ; save accumulator for least significant digit
  lsr                           ; --
  lsr                           ; shift most significant digit ...
  lsr                           ; ... to least significant digit position
  lsr                           ; --
  jsr sub_PrintHex_lbl1         ; output a hex digit
  pla                           ; restore the accumulator

sub_PrintHex_lbl1:
  and #%00001111                ; mask the least significant digit for hex printing
  ora #"0"                      ; add a "0"
  cmp #"9"+1                    ; is it a decimal digit?
  bcc sub_ACIA_SendByte         ; yes, so output it
  adc #6                        ; add offset for letter A - F
  jsr sub_ACIA_SendByte         ; output digit
  rts


sub_ACIA_Init:
; setup the 6551 ACIA for 115200 baud, 8 data bits, 1 stop bit, no parity

  lda #%00001011                ; set command bits for no parity, no echo, no interrupts
  sta ACIA_Command              ; store in ACIA command register
  lda #%00010000                ; set control bits for 115200 baud,8 data bits, 1 stop bit
  sta ACIA_Control              ; store in ACIA control register
  rts

ACIAout:
; label used by EhBASIC for the following routine
sub_ACIA_SendByte:
; output contents of accumulator as a byte via the 6551 ACIA
; registers are preserved

  pha				; put the accumulator (output byte) on the stack

sub_ACIA_SendByte_lbl1:
  lda ACIA_Status		; pull status register of ACIA into accumulator
  and #$10			; is the TX buffer full up?
  beq sub_ACIA_SendByte_lbl1	; is full, so ask again
  pla				; ins't full so restore accumulator
  sta ACIA_Data			; and put the accumulator (output byte) in the data register for the ACIA
  rts

ACIAin:
; label used by EhBASIC for the following routine
sub_ACIA_GetByte:
; get a byte from the 6551 ACIA
; returns with the byte in the accumulator

  lda ACIA_Status		; get contents of the ACIA status register
  and #$08			; was the RX buffer empty?
  beq sub_ACIA_GetByte_lbl1	; yes, empty jump to process empty
  lda ACIA_Data			; no, so pull the data register out and put it in the acculmulator 
  sec				; set carry, flag used to determine char was recevied
  rts

sub_ACIA_GetByte_lbl1:
  clc				; clear carry flag, to flag that no byte received
  rts


no_load:
; empty load vector for EhBASIC
  rts


no_save:
; empty save vector for EhBASIC
  rts


LAB_vec:
; EhBasic vector tables
  .word ACIAin			; byte in from simulated ACIA
  .word ACIAout			; byte out to simulated ACIA
  .word no_load			; null load vector for EhBASIC
  .word no_save			; null save vector for EhBASIC


IRQ_CODE:
; EhBASIC IRQ support
  pha				; save A
  lda IrqBase			; get the IRQ flag byte
  lsr				; shift the set b7 to b6, and on down ...
  ora IrqBase			; OR the original back in
  sta IrqBase			; save the new IRQ flag byte
  pla				; restore A
  rti


NMI_CODE:
; EhBASIC NMI support
  pha				; save A
  lda NmiBase			; get the NMI flag byte
  lsr				; shift the set b7 to b6, and on down ...
  ora NmiBase			; OR the original back in
  sta NmiBase			; save the new NMI flag byte
  pla				; restore A
  rti

END_CODE:

; setup values for the 6502's nmi, reset and irq vectors

  .org $FFFA			; set base address of 6502 vectors
  .word NMI_vec			; NMI vector at $FFFA/FFFB
  .word RES_vec			; Reset Vector at $FFFC/FD
  .word IRQ_vec			; IRQ/BRK Vector at $FFFE/FF
