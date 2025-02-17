;
; AssemblerApplication1.asm
;
; Created: 12.07.2021. 11:51:27
; Author : Sloba
;


; Replace with your application code

.equ F_CPU = 3333333                           ; DEFAULT ATTINY FREQ, 20MHz/6
.equ baudRate = 9600                           ; WE SET DESIRED BAUD RATE
.equ baud_val = (f_cpu*64) / (baudrate*16) - 1 ; ATTINY817 DATASHEET PROVIDES THIS EQUATION

; REGISTERS USED, R18 AS A COUNTER AND STRING COMPARISON RESULT
; R16 and R17 FOR SENDING STORING CHARACTERS AND FOR CHARACTER COMPARATION
; R0 - R3 for converted number storage
; R4 - R7 auxiliary registers used for ascii to ingeger conversion

start:
    inc r16
	; ATTINY USES PA1 AND PA2 AS TX AND RX OR IT CAN USE PB3 AND PB2 AS AN ALTERNATIVE
	; THE BOARD IS WIRED IN A WAY TO USE ALTERNATIVE PIN PAIR SO SET PORT DIRECTION
	; set pin directions RX TX PB3 PB2
	ldi r16, 0x04
	sts PORTB_DIR, r16
	; USE ALTERNATIVE USART PINS PORTMUX CTRLB ATTINY REGISTER BIT 0 CONTROLS THIS
	ldi r16, 1
	sts PORTMUX_CTRLB, r16
	; IF PRIMARY PIN PAIR USED THEN SET PORTA PIN DIRECTION COMMENT -sts PORTMUX_CTRLB, r16- UNCOMENT TWO LINES BELOW 
;	ldi r16, 0x02
;	sts PORTA_DIR, r16
	; ENABLE RX and TX
	ldi	r16, USART_RXEN_bm | USART_TXEN_bm   ; enable RX and TX
	sts	USART0_CTRLB, r16
	; BAUD RATE IS CALCULATED ABOVE WITH -.equ baud_val = (f_cpu*64) / (baudrate*16) - 1-
	; STORE THIS VALUE TO USART_BAUD REGISTER PAIR 
	ldi r16, LOW(baud_val)
	ldi r17, HIGH(baud_val)
	sts USART0_BAUDL, r16
	sts USART0_BAUDH, r17

; FOR TESTING PURPOSES ONLY, UNCOMENT THIS PART TO SEE HOW NUMBER STORED AS ASCII STRING IS CONVERTED INTO 4byte INTEGER
; AND STORED IN REGISTERS R0 - R3, UNCOMENT -> REPLACE FIRST INSTRUCTION IN convert_number with lpm (BECAUSE WE ARE LOADING ASCII 
; NUMBER FROM PROGRAM MEMORY) -> START DEBUGNING -> RUN TO NOP INSTRUCTION -> USE REGISTER VIEW TO SEE NUMBER CONVERTED AND STORED IN R0-R4
;
;	ldi ZL, LOW(2*test_numbers)
;	ldi ZH, HIGH(2*test_numbers)
;	rcall convert_number
;	nop
; END of ATMEL STUDIO DEBUG TEST CONVERT ASCII TO INTEGER


; LOOP UNTIL CHARACTER RECEIVED IS '*', take_char FUNCTION WILL STORE RECEIVED CHARACTER IN R16
loop1:
	rcall take_char
	cpi r16, '*'
	brne loop1

; WE RESERVED TWO BYTES IN DATA MEMORY AND LABELED THEM "receive" NOW X POINTER POINTS TO THAT PLACE
; SO WE TAKE ONE CHARACTER STORE IT AT receive FIRST BYTE LOCATION
loop2:
	ldi XL, LOW(receive)
	ldi XH, HIGH(receive)
	rcall take_char    ; we are looking for command character now
	st	X+, r16        ; store character and increment address pointed by X pointer
	cpi r16, '*'       ; if received character is '*' go to begining of the loop2, no need to go to loop 1 because	                   
	breq loop2         ; loop 1 waits for '*' and we already got it here

	rcall take_char    ; repeat whole process once more to take second command character
	st	X+, r16        
	cpi r16, '*'
	breq loop2

; AT THIS POINT WE DETECTED '*' FOLOWED BY TWO CHARACTERS, IT IS NOW TIME TO TAKE IN THE NUMBERS
; WE WILL USE r18 AS A COUNTER TO MAKE SURE NO MORE THAN 10 DIGITS ARE RECEIVED
	ldi r18, 0				; initialize counter
	ldi XL, LOW(numbers)    ; point to the address in data memory where received digits are going to be stored
	ldi XH, HIGH(numbers)

; NOW RECEIVE NUMBERS UNTIL CR IS RECEIVED OR 10 DIGITS ARE RECEIVED
loop3:
	
	rcall take_char
	st	X+, r16        ; store character
	cpi r16, '*'	   ; '*' resets whole process
	breq loop2		   ; same as in loop 2, '*' received so take command characters next
	cpi r16, 0x0D	   ; if CR received break
	breq loop3_out
	inc r18
	cpi r18, 10        ; if 10 digits received go to the crlf_wait loop which only waits CR 
	breq crlf_wait
	rjmp loop3

crlf_wait:             ; max number of digits received now just wait for CR
	rcall take_char
	cpi r16, 0x0D
	brne crlf_wait

loop3_out:
	ldi r18, 0                  ; reset r18 to use it as storage for comparison result later
	st	X+, r16					; store CR at the end of numbers array

; FOR TESTING PURPOSES NEXT 3 LINES WILL ECHO BACK RECEIVED STRING OF DIGITS
; CAN BE USED TO TEST IF RECEIVING ROUTINE FOUNCTIONS PROPERLY
	ldi ZL, LOW(numbers)
	ldi ZH, HIGH(numbers)
	rcall place_string
; END OF RECEIVE ECHO TEST

; NOW WE HAVE TO FIND OUT WHICH COMMAND HAS BEEN RECEIVED, THE PROGRAM WILL COMPARE FIRST 2 CHARACTERS WITH KNOWN COMMANDS
; KNOWN COMMANDS ARE STORED IN PROGRAM MEMORY AND LABELED c1-c4
	ldi XL, LOW(receive)        ; X pointer points to recived 2 characters
	ldi XH, HIGH(receive)
	ldi ZL, LOW(2*c1)           ; Z pointer points to known commands names stored in program memory
	ldi ZH, HIGH(2*c1)
	rcall compare               ; now we compare received characters with ones stored in program memory, comparison result will go in r18
	                            ; r18 is reused here, it was counter previously.
								; c1 label marks "AB" string, R18 bit 0 will be 1 if received characters does not match "AB"
								; and case_ab function call will be skipped 
	sbrs r18, 0                 ; skip if bit 0 in r18 set 
	rcall case_ab               ; if received characters match then call case_ab function

; REPEAT WHOLE PROCESS FOR OTHER 3 CASES, THE PROCESS IS THE SAME JUST X AND Z POINTERS MUST BE UPDATED
; X TO POINT TO THE BEGINING OF THE RECEIVED CHARACTERS BECAUSE IT WAS INCREMENTED PREVIOUSLY WHILE COMPARING
; Y TO POINT TO THE C2 WHICH CONTAINS "BC" STRING
	ldi XL, LOW(receive)
	ldi XH, HIGH(receive)
	ldi ZL, LOW(2*c2)
	ldi ZH, HIGH(2*c2)
	rcall compare
	sbrs r18, 0
	rcall case_bc

; SAME FOR "CD" COMMAND CHECK
	ldi XL, LOW(receive)
	ldi XH, HIGH(receive)
	ldi ZL, LOW(2*c3)
	ldi ZH, HIGH(2*c3)
	rcall compare
	sbrs r18, 0				; skip if bit 0 in register r18 set
	rcall case_cd

; AND SAME FOR "DE" COMMAND
	ldi XL, LOW(receive)
	ldi XH, HIGH(receive)
	ldi ZL, LOW(2*c4)
	ldi ZH, HIGH(2*c4)
	rcall compare
	sbrs r18, 0				; skip if bit 0 in register r18 set
	rcall case_de

	sbrc r18, 1             ; skip next instruction if bit 1 in register r18 is cleared
	                        ; compare function sets this bit, this bit will be set if match with "AB" or "BC" or "CD" or "DE" ocured
							; if the bit is set next instruction will be relative jump to skip_unknown label thus skipping unknown case
	rjmp skip_unknown
case_unknown:               
	ldi ZL, LOW(2*td0)
	ldi ZH, HIGH(2*td0)
	rcall place_from_pmem
skip_unknown:

; RECEIVED DIGITS ARE STORED IN DATA MEMORY LABELED WITH numbers LABEL
	ldi ZL, LOW(numbers)
	ldi ZH, HIGH(numbers)
	rcall convert_number   ; Convert received string of digits, the result will be in R0-R3
	ldi r18, 0
	rjmp loop1

main:
    rjmp main

;----------------------------- TRANSMIT AND RECEIVE FUNCTIONS------------------------------------------------------------------------
take_char:
	lds		r17, USART0_STATUS			; load UCSR0A into r17
	sbrs	r17, 7						; wait for empty transmit buffer
	rjmp	take_char

	lds r16, USART0_RXDATAL
	ret

place_char:
	lds	    r17, USART0_STATUS			; load UCSR0A into r17
	sbrs	r17, 5						; wait for empty transmit buffer
	rjmp	place_char
	ldi     r16, 'c'
	sts		USART0_TXDATAL, r16			; send character
	ret



take_string:
	lds	r17, USART0_STATUS			; load UCSR0A into r17
	sbrs	r17, 7					; wait for empty transmit buffer
	rjmp	take_string

	lds r16, USART0_RXDATAL

	cpi	r16, $0D					; check if received character is CR
	breq	take_end				; branch if CR received

	st	X+, r16						; store character
	rjmp	take_string				; take another character

take_end:
	ret

; string placement
place_string:
	ld	r16,Z+				;
	cpi	r16,$0D				; check if CR
	breq place_end

single_place:
	lds	    r17, USART0_STATUS	;
	sbrs	r17, 5				; poll transmit flag
	rjmp	single_place        ; repeat until the character is sent

	sts		USART0_TXDATAL, r16
	rjmp place_string

place_end:
	ret

place_from_pmem:
	lpm	r16,Z+				;
	cpi	r16,$00				; check if null
	breq place_end

single_pmem_place:
	lds	    r17, USART0_STATUS	;
	sbrs	r17, 5				; poll transmit flag
	rjmp	single_pmem_place    ; repeat until the character is sent

	sts		USART0_TXDATAL, r16
	rjmp place_from_pmem

place_from_pmem_end:
	ret
; end of string placement

;--------------------------------------------END OF TRANSMIT RECEIVE FUNCTIONS--------------------------------------------------------------

;Before calling this function load Z with appropriate command address
; load X with received data address
compare:
	lpm r16, Z+
	ld	r17, X+
	cp r16, r17
	brne not_equal
	lpm r16, Z+
	ld	r17, X+
	cp r16, r17
	brne not_equal
	ldi r18, 2
	ret
not_equal:
	sbr r18, 1   ; logical OR between r18 and 0b00000001
	ret

;---------------------------------------------COMMAND CASES------------------------------------------------------------
case_ab:
	nop
	ldi ZL, LOW(2*td1)         ; point to "Command AB" string
	ldi ZH, HIGH(2*td1)        
	rcall place_from_pmem      ; transmit "Command AB"
	ret
case_bc:                       ; same for BC
	nop
	ldi ZL, LOW(2*td2)
	ldi ZH, HIGH(2*td2)
	rcall place_from_pmem
	ret
case_cd:                      ; same for CD
	nop
	ldi ZL, LOW(2*td3)
	ldi ZH, HIGH(2*td3)
	rcall place_from_pmem
	ret
case_de:                      ; same for DE
	nop
	ldi ZL, LOW(2*td4)
	ldi ZH, HIGH(2*td4)
	rcall place_from_pmem
	ret

;-------------------------------------------ASCII TO INTEGER FUNCTIONS--------------------------------------------------
convert_number:
	ld	r16,Z+
	cpi		r16, 0x0D       ; check for CR
	breq	convert_number_end ; Yes, end of conversion
  
	subi	r16, '0' ; ASCII decimal to binary

	rcall	multiply ; Multiply previous number by 10

	rcall	Addition1 ; Add the number in r16 to R3-R0
 
	rjmp convert_number ; Continue conversion
convert_number_end:
  ret

multiply:
  rcall copy
  rcall double ; double current number

  rcall double ; double it again, it is now multiplied by 4

  rcall addition ; Add the previous number = multiply by 5

  rcall double ; double it again and it si multiplied by 10 now

  ret

double:
  lsl R0 ; Shift 0 into LSB
  rol R1 ; Roll carry into byte 2
  rol R2
  rol R3
  ret

copy:
  mov R4,R0 ; Copy number
  mov R5,R1
  mov R6,R2
  mov R7,R3
  ret

addition:
	add R0,R4 ; Add the two numbers
	adc R1,R5
	adc R2,R6
	adc R3,R7
	ret

addition1:
	add	R0, r16 ; Add the number
	ldi r16,0 ; Clear r16 without clearing carry
	adc R1,r16 ; Add carry
	adc R2,r16
	adc R3,r16
	adc	R4,r16
	ret

; HERE ARE COMMANDS STORED IN PROGRAM MEMORY AND USED TO COMPARE RECEIVED COMMAND WITH THEM
c1: .db "AB"
c2: .db "BC"
c3: .db "CD"
c4: .db "DE"

; THIS WILL BE OUTPUT WHEN COMMAND RECKOGNIZED OR NOT
td0: .db '\r', '\n',"Unknown Command",'\r','\n', 0
td1: .db '\r','\n',"Command AB",'\r','\n', 0, 0
td2: .db '\r','\n',"Command BC",'\r','\n', 0, 0
td3: .db '\r','\n',"Command CD",'\r','\n', 0, 0
td4: .db '\r','\n',"Command DE",'\r','\n', 0, 0

; THIS IS TO TEST CONVERT ASCII TO INTEGER
test_numbers: .db "123456789", 0x0D, 00, 00

.dseg

; HERE COMMAND CHARACTERS WILL GO WHEN RECEIVED
receive: .byte 2
; HERE RECEIVED DIGITS WILL GO
numbers: .byte 12