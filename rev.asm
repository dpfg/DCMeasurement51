;LCD Registers addresses
LCD_CMD_WR	equ 0
LCD_DATA_WR	equ	1
LCD_BUSY_RD	equ	2
LCD_DATA_RD	equ	3

;LCD Commands
LCD_CLS		equ	1
LCD_HOME	equ	2
LCD_SETMODE	equ	4
LCD_SETVISIBLE	equ	8
LCD_SHIFT	equ	16
LCD_SETFUNCTION	equ	32
LCD_SETCGADDR	equ	64
LCD_SETDDADDR	equ	128

;Constants
INHIBITOR_ADDR equ 050h

org 0000h
	sjmp over

;====================================================================================
; Interrupt handler
;====================================================================================
org 0003H
	jbc tcon.4, stop 	;if TR0 is running stop it
	setb tcon.4 		;else run it
	mov TL0, #00h		; clear TR0
	mov TH0, #00h		; clear TR1
	mov TL1, #00h
	mov TH1, #00h
	setb TR1			;start TR1
	fin:
		reti
;====================================================================================
; Stop TR0 subroutine and clear counters
;====================================================================================
stop:					
	setb IP.7			;notify the main programm. the result of the measurement is ready
	mov R3, TH0			; mov TR0 and TR1 to R3-R6 registers
	mov R4, TL0
	mov R5, TH1
	mov R6, TL1
	clr TR0
	clr TR1
	mov TL0, #00h		;clear TR0 and TR1
	mov TH0, #00h
	mov TL1, #00h
	mov TH1, #00h
	reti
;====================================================================================
; Setup subroutine. Run once.
;====================================================================================
over:	
	; set timer mode
	mov DPTR, #0x0455
	mov TMOD, #19h
	mov TCON, #01h
	; set interuption
	setb EX0
	setb EA
	; clear timers
	mov TL0, #00h
	mov TH0, #00h
	mov th1, #00h
	mov tl1, #00h
	mov ie, #81h
	setb ppflag
	
	; set LCD 
	mov A,#038h
	MOV p1, #00011011b
	call wrcmd
	; set inhibitor
	mov 050h, #20
	
;====================================================================================
; Main subroutine. Loop ever. Check result status and if resuft is ready  - display it
;====================================================================================
simulate:	
	jbc IP.7, getw
	sjmp simulate
	
;====================================================================================
; Get result of measurement and disply it subroutine.
;====================================================================================	
getw: 		
	clr EA
	call init_lcd
	mov R1, #0
	;=============================================
	; backup timer0 value
	;=============================================
	mov 53h, R3 ;low
	mov 54h, R4 ;high
	
	;=============================================
	; prepare for a division
	;=============================================
	mov A, R6
	mov R0, A ;low
	mov A, R5
	mov R1, A ;high
	mov A, R4
	mov R2, A ;low
	
	;=============================================
	; call division subroutine
	;=============================================
	call DIV16
	
	;=============================================
	; call backup remainder of division subroutine
	;=============================================
	call backup_remainder
	
	;=============================================
	; prepare for a convertation to ASCII code
	;=============================================
	mov A, R1
	mov R3, A
	mov A, R0
	mov R2, A
	
	;=========================================================================
	; call convertation subroutine and display the result
	; Input: R2 (low) and R3 (high)   (binary)                               
	; Output: 30h,31h,32h,33h,34h  (internal RAM address)  (ASCII codes)            
	; 	high--^                  ^------low
	;=========================================================================
	call BINTOASC	
	call display_ascii
	
	;=============================================
	; display dot symbol from DB
	;=============================================
	mov DPTR, #string4
	call wrstr
	
	;=============================================
	; extract remainder from internal RAM
	;=============================================
	call extract_remainder
	
	;============================================================================
	; Multiply the remainder by 10000 and divide by Timer0 value 
	; to calculate a fraction part
	; For example:
	; 		A = 4999 and B = 2500
	;		DIV A,B is result Q=1 as integral part and R=2499 as remainder part
	; But we need 1.9996 
	; 9996 we can get if we devide R*10000/B
	;============================================================================
	mov R1, #27h
	mov R0, #10h
	call UMUL16
	mov R4, 54h ;low
	mov R5, 53h ;high
	call DIV32
	mov A, R0
	mov R2, ACC
	mov A, R1
	mov R3, A
	
	;=========================================================================
	; call convertation subroutine and display the result
	;=========================================================================
	call BINTOASC
	call display_ascii
	
	; return to the main loop
	setb EA
	sjmp simulate

;====================================================================================
; LCD initialisation
;====================================================================================
init_lcd:
	mov A,#LCD_SETVISIBLE+6 	;Make the display & blink visible:
	call wrcmd
	mov A,#LCD_CLS			;Clear screen
	call wrcmd
	mov A,#LCD_SETDDADDR+0
	call wrcmd
	mov DPTR, #string1a
	call wrstr
	ret

;====================================================================================
; Backup remainder to data storage
;====================================================================================	
backup_remainder:
	mov A, R2 ; R2 - low
	mov 36h, A
	mov A, R3 ; R3 - high
	mov 37h, A
	ret

;====================================================================================
; Extract remainder from data storage
;====================================================================================	
extract_remainder:
	mov A, 36h
	mov R2, A
	mov A, 37h
	mov R3, A
	ret

;====================================================================================
; Get ascii code for every number
; Skip zero value
;====================================================================================
display_ascii:
	mov R1, #30h
	setb F0
	bigloop:
		jb F0, miniloop
		ret
		miniloop:
			inc R1
			mov A, @R1
			CJNE R1, #35h, countinue
			clr F0
			jmp bigloop
		
	showDigit:
		mov 40h, R1
		call wrdata
		mov A, 40h
		mov R1, A
		jmp bigloop
		
	countinue:
		CJNE A, #30h, showDigit
		jmp bigloop
		

	

;====================================================================================
; Subroutine to write null terminated string at DPTR in program ram.
;====================================================================================
wrstr:	mov R0,#LCD_DATA_WR
wrstr1:	clr A
	movc A,@A+DPTR
	jz wrstr2
	movx @R0,A
	call wtbusy
	inc DPTR
	push DPL
	push DPH
	pop DPH
	pop DPL	
	jmp wrstr1
wrstr2:	ret

wrdata:
	mov R0, #LCD_DATA_WR
	movx @R0, A
	call wtbusy
	ret

;====================================================================================
;Sub routine to write null terminated string at DPTR in program ram. Slowly
;====================================================================================
wrslow:	mov R0,#LCD_DATA_WR
wrslw1:	clr A
	movc A,@A+DPTR
	jz wrslw2
	movx @R0,A
	call wtbusy
	inc DPTR
	push DPL
	push DPH
        mov DPTR,#100
        call wtms
	pop DPH
	pop DPL	
	jmp wrslw1
wrslw2:	ret

;====================================================================================
;Sub routine to write command:
;====================================================================================
wrcmd:	mov R0,#LCD_CMD_WR
	movx @R0,A
	jmp wtbusy

;====================================================================================
;Sub routine to write character:
;====================================================================================
wrchar:	mov R0,#LCD_DATA_WR
	movx @R0,A


;====================================================================================
;Subroutine to wait for busy clear
;====================================================================================
wtbusy: mov R1,#LCD_BUSY_RD
	movx A,@r1
	jb ACC.7,wtbusy
	ret

;====================================================================================
;Wait for number of seconds in A
;====================================================================================
wtsec:	push ACC
	call wtms
	pop ACC
	dec A
	jnz wtsec
	ret

string1a:db 'Duty cycle = '
	 db 0

string4:db '.'
	db 0

;==========================================================
; Routine to convert a 16bit binary number in ASCII                                                                 |
; Input : R2 (Lsb) and R3 (Msb)   ( binary)                                                                                                   
; Output : 30h,31h,32h,33h,34h  (internal RAM address)  (ASCII)            
;    msb--^                  ^------lsb                                       
BINTOASC:

        MOV R0,#30h                 ; R0 = POUT 
        MOV DPTR,#TAB               ; R=TAB(P)

COM1:

        CLR A                       ; P <- 0     
        MOVC A,@A+DPTR              ; R <-  TAB(P)
        MOV R7,A
        INC DPTR
        CLR A
        MOVC A,@A+DPTR
        MOV R6,A

        MOV R4,#'0'                  ; C  <- '0'


SOMA:                                ; N <-  N-R 
      CLR C        ;              
      MOV A,R2     ;              
      SUBB A,R6    ;              
      MOV R2,A     ;              
                                 
      MOV A,R3     ;               
      SUBB A,R7    ;              
      MOV R3,A     ;              
      JC SAIDA     ;    If < 0 goto  SAIDA
      INC R4       ;    If >0 then C <- C +1
      SJMP SOMA    ;    goto SOMA            
SAIDA:
      MOV A,R4                 
      MOV @R0,A              ;TABOUT (POUT) <- C

      MOV A,R2             
      ADD A,R6               ;  N=N+R
      MOV R2,A             
                           
      MOV A,R3             
      ADDC A,R7            
      MOV R3,A             

      INC R0                 ; PSAIDA=PSAIDA +1

      CLR A
      MOVC A,@A+DPTR
      CJNE A,#1,INCREMENTA   ; TAB(P) = 1 ?
      RET                    ; If yes, END

INCREMENTA:                  ; If No, P <- P+1
      INC DPTR
      LJMP COM1              ; goto COM1


TAB:
     DW 10000
     DW 1000
     DW 100
     DW 10
     DW 1

	 
;*****************************************************************
;*                                                               *
;*         Maths Subroutines for the 8051 microcontroller        *
;*                      W.G.Marshall 2002                        *
;*                                                               *
;*****************************************************************

; All parameters in Register bank 0, (r0 to r7)
; Bits 21H and 22H reserved for sign bits

;===================================================================
; subroutine Cr0r1
; 16-Bit 2's Complement -> magnitude / Sign Bit Conversion
;
; input:    r1, r0 = signed word
;
; output:   r1, r0 = magnitude
;           Bit 21H = sign (21H is set if negative number)
;
; alters:   acc, C
;===================================================================

Cr0r1:         mov     a, r1           ; high byte into accumulator
               jb      acc.7, c0a      ; negative if bit 7 is 1
               clr     21H             ; clear sign bit if 'positive'
               ret                     ; done

c0a:           setb    21H             ; set sign flag
               mov     a, r0           ; number is negative
               cpl     a               ; complement
               add     a, #1           ; and add +1
               mov     r0, a 
               mov     a, r1           ; get next byte
               cpl     a               ; complement
               addc    a, #0
               mov     r1, a
               ret


;====================================================================
; subroutine Cr2r3
; 16-Bit 2's Complement -> magnitude / Sign Bit Conversion
;
; input:    r3, r2 = signed word
;
; output:   r3, r2 = magnitude
;           Bit 22H = sign (22H is set if negative number)
;
; alters:   acc, C
;====================================================================

Cr2r3:         mov     a, r3           ; read high into accumulator
               jb      acc.7, c1a      ; negative if bit 7 is 1
               clr     22H             ; clear sign bit if 'positive'
               ret                     ; done

c1a:           setb    22H             ; set sign flag
               mov     a, r2           ; number is negative
               cpl     a               ; complement
               add     a, #1           ; and add +1
               mov     r2, a 
               mov     a, r3           ; get next byte
               cpl     a               ; complement
               addc    a, #0
               mov     r3, a
               ret


;====================================================================
; subroutine Cr4r5
; 16-Bit 2's Complement -> magnitude / Sign Bit Conversion
;
; input:    r5, r4 = signed word
;
; output:   r5, r4 = magnitude
;           Bit 22H = sign (22H is set if negative number)
;
; alters:   acc, C
;====================================================================

Cr4r5:         mov     a, r5           ; read high into accumulator
               jb      acc.7, c3a      ; negative if bit 7 is 1
               clr     22H             ; clear sign bit if 'positive'
               ret                     ; done

c3a:           setb    22H             ; set sign flag
               mov     a, r4           ; number is negative
               cpl     a               ; complement
               add     a, #1           ; and add +1
               mov     r4, a 
               mov     a, r5           ; get next byte
               cpl     a               ; complement
               addc    a, #0
               mov     r5, a
               ret


;====================================================================
; subroutine Cr0r3
; 32-Bit 2's Complement -> magnitude / Sign Bit Conversion
;
; input:    r3, r2, r1, r0 = signed word
;
; output:   r3, r2, r1, r0 = magnitude
;           Bit 21H = sign (21H is set if negative number)
;
; alters:   acc
;====================================================================

Cr0r3:         mov     a, r3           ; read high into accumulator
               jb      acc.7, c2a      ; negative if bit 7 is 1
               clr     21H             ; clear sign flag if 'positive'
               ret                     ; done

c2a:           setb    21H             ; set sign flag
               mov     a, r0           ; number is negative
               cpl     a               ; complement
               add     a, #1           ; and add +1
               mov     r0, a 
               mov     a, r1           ; get next byte
               cpl     a               ; complement
               addc    a, #0
               mov     r1,a
               mov     a, r2           ; get next byte
               cpl     a               ; complement
               addc    a, #0
               mov     r2,a
               mov     a, r3           ; get next byte
               cpl     a               ; complement
               addc    a, #0
               mov     r3, a
               ret                     ; done

;====================================================================
; subroutine Mr0r1
; 16-Bit magnitude / Sign Bit -> 2's Complement Conversion
;
; input:    r1, r0 = magnitude
;           Bits 21H & 22H = sign bits of operands X and Y
;           (set if negative)
;
; output:   r1, r0 = signed word
;
; alters:   acc, C
;====================================================================

Mr0r1:         jb      21H, Mr0r1b     ; test X sign
               jb      22H, Mr0r1a     ; test Y sign
               ret

Mr0r1b:        jnb     22H, Mr0r1a
               ret

Mr0r1a:        mov     a, r0           ; negate number
               cpl     a               ; complement
               add     a, #1           ; and add +1
               mov     r0, a 
               mov     a, r1           ; get next byte
               cpl     a               ; complement
               addc    a, #0
               mov     r1, a
               ret


;====================================================================
; subroutine Mr0r3
; 32-Bit magnitude / Sign Bit -> 2's Complement Conversion
;
; input:    r3, r2, r1, r0 = magnitude
;           Bits 21H & 22H = sign bits of operands X and Y
;           (set if negative)
;
; output:   r3, r2, r1, r0 = signed word
;
; alters:   acc, C
;====================================================================

Mr0r3:         jb      21H, Mr0r3b     ; test X sign
               jb      22H, Mr0r3a     ; test Y sign
               ret

Mr0r3b:        jnb     22H, Mr0r3a
               ret

Mr0r3a:        mov     a, r0           ; negate number
               cpl     a               ; complement
               add     a, #1           ; and add +1
               mov     r0, a 
               mov     a, r1           ; get next byte
               cpl     a               ; complement
               addc    a, #0
               mov     r1, a
               mov     a, r2           ; get next byte
               cpl     a               ; complement
               addc    a, #0
               mov     r2, a
               mov     a, r3           ; get next byte
               cpl     a               ; complement
               addc    a, #0
               mov     r3, a
               ret                     ; done

;====================================================================
; subroutine MUL16
; 16-Bit x 16-Bit to 32-Bit Product Signed Multiply
; 2's Complement format
;
; input:    r1, r0 = multiplicand X
;           r3, r2 = multiplier Y
;
; output:   r3, r2, r1, r0 = product P = X x Y
;
; calls:    UMUL16, Cr0r1, Cr2r3, Mr0r3
;
; alters:   acc, C, Bits 21H & 22H
;====================================================================

MUL16:         anl     PSW, #0E7H      ; Register Bank 0
               acall   Cr0r1           ; 2's comp -> Mag/Sign
               acall   Cr2r3           ; 2's comp -> Mag/Sign
               acall   UMUL16
               acall   Mr0r3           ; Mag/Sign -> 2's Comp
               ret


;====================================================================
; subroutine UMUL16
; 16-Bit x 16-Bit to 32-Bit Product Unsigned Multiply
;
; input:    r1, r0 = multiplicand X
;           r3, r2 = multiplier Y
;
; output:   r3, r2, r1, r0 = product P = X x Y
;
; alters:   acc, C
;====================================================================

UMUL16:        push    B
               push    dpl
               mov     a, r0
               mov     b, r2
               mul     ab              ; multiply XL x YL
               push    acc             ; stack result low byte
               push    b               ; stack result high byte
               mov     a, r0
               mov     b, r3
               mul     ab              ; multiply XL x YH
               pop     00H
               add     a, r0
               mov     r0, a
               clr     a
               addc    a, b
               mov     dpl, a
               mov     a, r2
               mov     b, r1
               mul     ab              ; multiply XH x YL
               add     a, r0
               mov     r0, a
               mov     a, dpl
               addc    a, b
               mov     dpl, a
               clr     a
               addc    a, #0
               push    acc             ; save intermediate carry
               mov     a, r3
               mov     b, r1
               mul     ab              ; multiply XH x YH
               add     a, dpl
               mov     r2, a
               pop     acc             ; retrieve carry
               addc    a, b
               mov     r3, a
               mov     r1, 00H
               pop     00H             ; retrieve result low byte
               pop     dpl
               pop     B
               ret

;====================================================================
; subroutine DIV16
; 16-Bit / 16-Bit to 16-Bit Quotient & remainder signed Divide
; 2's Complement Format
;
; input:    r1, r0 = Dividend X
;           r3, r2 = Divisor Y
;
; output:   r1, r0 = quotient Q of division Q = X / Y
;           r3, r2 = remainder 
;           Carry C is set if Y = 0, i.e. divide by 0 attempted
;
; calls:    UDIV16, Cr0r1, Cr2r3, Mr0r1
;
; alters:   acc, r4, r5, r6, r7, flags, Bits 21H & 22H
;====================================================================

DIV16:         anl     PSW, #0E7H      ; Register Bank 0
               mov     a, r3           ; get divisor high byte
               orl     a, r2           ; OR with low byte
               jnz     div_OK          ; divisor OK if not 0
               setb    C               ; else, overflow
               ret

div_OK:        push    dpl
               push    dph
               push    b
               acall   Cr0r1           ; 2's comp -> Mag/Sign
               acall   Cr2r3           ; 2's comp -> Mag/Sign
               acall   UDIV16
               acall   Mr0r1           ; Mag/Sign -> 2's Comp
               clr     C
               pop     b
               pop     dph
               pop     dpl
               ret                     ; done


;====================================================================
; subroutine UDIV16
; 16-Bit / 16-Bit to 16-Bit Quotient & Remainder Unsigned Divide
;
; input:    r1, r0 = Dividend X
;           r3, r2 = Divisor Y
;
; output:   r1, r0 = quotient Q of division Q = X / Y
;           r3, r2 = remainder 
;
; alters:   acc, B, dpl, dph, r4, r5, r6, r7, flags
;====================================================================

UDIV16:        mov     r7, #0          ; clear partial remainder
               mov     r6, #0
               mov     B, #16          ; set loop count

div_loop:      clr     C               ; clear carry flag
               mov     a, r0           ; shift the highest bit of
               rlc     a               ; the dividend into...
               mov     r0, a
               mov     a, r1
               rlc     a
               mov     r1, a
               mov     a, r6           ; ... the lowest bit of the
               rlc     a               ; partial remainder
               mov     r6, a
               mov     a, r7
               rlc     a
               mov     r7, a
               mov     a, r6           ; trial subtract divisor
               clr     C               ; from partial remainder
               subb    a, r2
               mov     dpl, a
               mov     a, r7
               subb    a, r3
               mov     dph, a
               cpl     C               ; complement external borrow
               jnc     div_1           ; update partial remainder if
                                       ; borrow
               mov     r7, dph         ; update partial remainder
               mov     r6, dpl
div_1:         mov     a, r4           ; shift result bit into partial
               rlc     a               ; quotient
               mov     r4, a
               mov     a, r5
               rlc     a
               mov     r5, a
               djnz    B, div_loop
               mov     a, r5           ; put quotient in r0, and r1
               mov     r1, a
               mov     a, r4
               mov     r0, a
               mov     a, r7           ; get remainder, saved before the
               mov     r3, a           ; last subtraction
               mov     a, r6
               mov     r2, a
               ret


;====================================================================
; subroutine DIV32
; 32-Bit / 16-Bit to 32-Bit Quotient & remainder signed Divide
; 2's Complement Format
;
; input:    r3, r2, r1, r0 = Dividend X
;           r5, r4 = Divisor Y
;
; output:   r3, r2, r1, r0 = quotient Q of division Q = X / Y
;           r7, r6, r5, r4 = remainder
;           Carry C is set if Y = 0, i.e. divide by 0 attempted
;
; calls:    UDIV32, Cr0r3, Cr4r5, Mr0r3
;
; alters:   acc, flags, Bits 21H & 22H
;====================================================================

DIV32:         anl     PSW, #0E7H      ; Register Bank 0
               mov     a, r4           ; get divisor high byte
               orl     a, r5           ; OR with low byte
               jnz     div32_OK        ; divisor OK if not 0
               setb    C               ; else, overflow
               ret

div32_OK:      acall   Cr0r3           ; 2's comp -> Mag/Sign
               acall   Cr4r5           ; 2's comp -> Mag/Sign
               acall   UDIV32
               acall   Mr0r3           ; Mag/Sign -> 2's Comp
               clr     C               ; divisor is not 0
               ret                     ; done


;====================================================================
; subroutine UDIV32
; 32-Bit / 16-Bit to 32-Bit Quotient & Remainder Unsigned Divide
;
; input:    r3, r2, r1, r0 = Dividend X
;           r5, r4 = Divisor Y
;
; output:   r3, r2, r1, r0 = quotient Q of division Q = X / Y
;           r7, r6, r5, r4 = remainder
;;
; alters:   acc, flags
;====================================================================

UDIV32:        push    08              ; Save Register Bank 1
               push    09
               push    0AH
               push    0BH
               push    0CH
               push    0DH
               push    0EH
               push    0FH
               push    dpl
               push    dph
               push    B
               setb    RS0             ; Select Register Bank 1
               mov     r7, #0          ; clear partial remainder
               mov     r6, #0
               mov     r5, #0          
               mov     r4, #0
               mov     B, #32          ; set loop count

div_lp32:      clr     RS0             ; Select Register Bank 0
               clr     C               ; clear carry flag
               mov     a, r0           ; shift the highest bit of the
               rlc     a               ; dividend into...
               mov     r0, a
               mov     a, r1
               rlc     a
               mov     r1, a
               mov     a, r2
               rlc     a
               mov     r2, a
               mov     a, r3
               rlc     a
               mov     r3, a
               setb    RS0             ; Select Register Bank 1
               mov     a, r4           ; ... the lowest bit of the
               rlc     a               ; partial remainder
               mov     r4, a
               mov     a, r5
               rlc     a
               mov     r5, a
               mov     a, r6
               rlc     a
               mov     r6, a
               mov     a, r7
               rlc     a
               mov     r7, a
               mov     a, r4           ; trial subtract divisor from
               clr     C               ; partial remainder
               subb    a, 04
               mov     dpl, a
               mov     a, r5
               subb    a, 05
               mov     dph, a
               mov     a, r6
               subb    a, #0
               mov     06, a
               mov     a, r7
               subb    a, #0
               mov     07, a
               cpl     C               ; complement external borrow
               jnc     div_321         ; update partial remainder if
                                       ; borrow
               mov     r7, 07          ; update partial remainder
               mov     r6, 06
               mov     r5, dph
               mov     r4, dpl
div_321:       mov     a, r0           ; shift result bit into partial
               rlc     a               ; quotient
               mov     r0, a
               mov     a, r1
               rlc     a
               mov     r1, a
               mov     a, r2
               rlc     a
               mov     r2, a
               mov     a, r3
               rlc     a
               mov     r3, a
               djnz    B, div_lp32

               mov     07, r7          ; put remainder, saved before the
               mov     06, r6          ; last subtraction, in bank 0
               mov     05, r5
               mov     04, r4
               mov     03, r3          ; put quotient in bank 0
               mov     02, r2
               mov     01, r1
               mov     00, r0
               clr     RS0
               pop     B
               pop     dph
               pop     dpl
               pop     0FH             ; Retrieve Register Bank 1
               pop     0EH
               pop     0DH
               pop     0CH
               pop     0BH
               pop     0AH
               pop     09
               pop     08
               ret

end


