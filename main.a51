
;*******************************************
;* Sensors & Microsystem electronics       *
;*                                         *
;* Names: Steven Peters - Thomas Lapauw    *
;*                                         *
;*                Task 9                   *
;*                                         *
;*******************************************


;Special register declarations
$include (t89c51cc01.inc)
	
;Boot code
ORG 0000h
LJMP init 

;Interrupt address vectors
;Specify tmr0 interrupt address vector 
ORG 000Bh
LJMP ISR_tmr0

ORG 001Bh
LJMP ISR_tmr1
;Address declarations

;Constants

;Initialization code
init:	

			;Initialization code here
			CLR P2.3 ;led to see if code is running
			
			;init tmr0
			MOV TMOD,#00010001b ;config tmr0 & tmr1 in 16bit mode
			MOV TH0,#0FBh ;tmr0 MSB
			MOV TL0,#099h ;tmr0 LSB
			
			MOV TH1,#0FFh ;tmr0 MSB
			MOV TL1,#0FFh ;tmr0 LSB
			
			SETB ET1
			SETB ET0 ;enable interrupt of tmr0
			SETB EA ;global interrupt enable

			MOV SP, #70h ; move stackpointer above registers
			 
			 
;Initialize ram			 
			MOV R0,#030h 
			MOV R1,#040
ramInit:
			MOV @R0,#0FFh
			INC R0
			DJNZ R1,ramInit
			
; seed of LFSR		
MOV 18h, #1101010b 
MOV R7, #01111111b ;data rows (msb = 0, others are 1) single bit zero, to enable current row

LCALL LFSR  ; generate new random data	

SETB TR0 ;run tmr0
SETB TR1

MOV R5,#20

LJMP main
			
;Main program
main:		

;;DISPLAY PART
		;CLR TR0 ;stop timer during buffer update
		;;CLR EA ;global interrupt disable
		;CPL P2.4 ; toggle led to see if working
		;MOV A,18h ; get data from MSB LFSR
		;;RL A ; multiply twice by 2 because 4 bytes per block
		;;RL A
		
		;ANL A,#0011100b ;mask for the number of blocks
		;;MOV B,#4
		;;MUL AB
		
		;MOV R2, A ; save current adress
		;;;MOV A,#0
		;MOV R3,#4 ; repeat 4 times
		;;MOV A, #0h
		;;MOV R2, #0h ; save current adress
;byteIt:
		;MOV A,R2
		;MOV DPTR, #block0		; begin bij block0
		;MOVC A,@A+DPTR

		;MOV R7,A ; stockate data in R7 for collumnshift
		;LCALL dispColShift
		
		;MOV A,R2 ; retrieve current data offset
		;INC A; advance one adress
		;MOV R2, A ; save current adress
		
		;SETB TR0 ;stop timer during buffer update
		;;SETB EA ;global interrupt disable
		;;LCALL delay ; delay before repeat
		;CLR TR0 ;stop timer during buffer update
		;;CLR EA ;global interrupt disable
		
		;DJNZ R3, byteIt ; jupmp back to te iteration
		

		;LCALL LFSR  ; generate new random data
		
		;;LCALL delay ; delay before repeat
		;;LCALL dispColShift ; shift new column in display buffer
		;SETB TR0 ;stop timer during buffer update
		;;SETB EA ;global interrupt disable

		;;MOV A,R7
		;;RR A
		;;MOV R7,A
		
		;;MOV R7,#00h
		;;CLR TR0
		;;CLR P2.4
		;;LCALL dispColShift
		;;SETB TR0 ;run tmr0
		;;LCALL delay

				
			LJMP main
		
;Interrupt handlers


ISR_tmr0: 
			CLR EA ;global interrupt disable
			CLR TR0 ;stop tmr0
			CLR TR1
			;reload timer
			MOV TH0,#0FBh ;tmr0 MSB
			MOV TL0,#099h ;tmr0 LSB
			
;DISPLAY PART
		CLR RS1 ;move to registerbank 08h to 0Fh
		SETB RS0
		MOV R1, #07 ;counting register to 8 for rows
		MOV R2, #01111111b ;data rows (msb = 0, others are 1) single bit zero, to enable current row
		MOV R0, #030h ; starting adress disp mem
rowIteration:

			MOV R7, #05 ; counter for 5 bytes of row data
lineBytes:
			MOV A,@R0 ; get data from ram
			MOV R6,A
			acall shiftR6 ; shift byte into ram
			INC R0 ; get next byte
			DJNZ R7,lineBytes ; rinse and repeat
			
			;shift R2 with the current row enable in the register
			MOV A,R2
			MOV R6, A
			Acall shiftR6 ; shift column data byte into SR
			SETB P3.2 ; cycle store clock
			CLR P3.2

			;rotate row bit to enable next row
			MOV A, R2 
			RR A
			MOV R2,A
			 ; repeat until 7 rows done
			DJNZ R1, rowIteration
			
			
			
			MOV R1, #06
lastLineComp:			 ;loop to approximate the timing of the other rows to have similar brightness
			MOV R6, #0FFh
			Acall shiftR6 ; shift collumn data byte into SR
			DJNZ R1,lastLineComp
			
			SETB P3.2 ; cycle store clock
			CLR P3.2

			CLR RS1 ;move to registerbank 00h to 08h
			CLR RS0	
			;reenable timers
			SETB TR0 ;run tmr0
			SETB TR1
			SETB EA ;global interrupt disable
			
			RETI


ISR_tmr1:
;DISPLAY PART
		CLR TR0 ;stop timer during buffer update
		CLR TR1 ;stop timer during buffer update
		CLR EA ;global interrupt disable
		
		MOV TH1,#0FFh ;tmr0 MSB
		MOV TL1,#099h ;tmr0 LSB
		
		DJNZ R5, afterRandom
		
		
		MOV A, 53h
		MOV DPTR, #block0		; begin bij block0
		MOVC A,@A+DPTR

		MOV R7,A ; stockate data in R7 for collumnshift
		LCALL dispColShift
		
		MOV A, 53h ; retrieve current data offset
		INC A; advance one adress
		MOV 53h, A ; save current adress

		DJNZ R3, afterRandom ; jupmp back to te iteration
		MOV R3,#4 ; repeat 4 times
		MOV A,18h ; get data from MSB LFSR
		ANL A,#0011100b ;mask for the number of blocks		
		MOV 53h, A ; save current adress for next block
		LCALL LFSR  ; generate new random data
		CPL P2.4 ; toggle led to see if working
		MOV R5,#20
	afterRandom:
		SETB TR1 ;stop timer during buffer update
		SETB TR0
		SETB EA

reti


;Shift byte from R6 into shift registers
shiftR6:
				push Acc
				MOV A, R6 
				MOV R6, #08 ;counting register to 8
				Reg:
				RRC A ;rotate accumulator
				MOV P4.1,C ;carry to Data
				SETB P4.0 ; cycle serial clock
				CLR P4.0
				DJNZ R6,Reg ;if 8 bits are shifted go further otherwise repeat
				pop Acc
				ret
;Last line of code


dispRowShift:
	MOV A,@R1
	RRC A
	MOV @R1,A
	DEC R1
	DJNZ R6, dispRowShift
	RET
;shift 7 MSB in framebuffer
dispColShift:
	MOV A, R7
	MOV	R5, #07 ;counter to count rows
	;RRC A ;rotate to drop LSB
	MOV R1, #52h ; start at highest address to decrease each time
dispColShiftLoop:
	RRC A ;Rotate LSB in carry to shift in row
	MOV	R6, #05 ; counter to rotate 5 horizontal bytes
	PUSH ACC ; push acc to save current data
	ACALL dispRowShift ; rotate all row bytes
	POP ACC
	DJNZ R5, dispColShiftLoop
	RET
	
;rudimentary delay for test purposes
delay:	 
		MOV R6, #09Fh
		LCALL loop
		RET

loop: 	
		MOV R7, #00h
		LCALL loop2
		DJNZ R1, loop
		RET
		
loop2:	DJNZ R2,loop2
		RET
		
		
;random number generator code trough LFSR		
LFSR: 
	SETB RS1 ;move to registerbank 08h to 0Fh
	SETB RS0
	MOV A,R0 ;Save highest byte to address
	MOV R4,A
	lcall LFSRShift
	lcall LFSRShift
	XRL A,R4
	MOV R4,A
	lcall LFSRShift
	lcall LFSRShift
	lcall LFSRShift
	lcall LFSRShift
	XRL A,R4
	MOV R4,A		
	lcall LFSRShift
	XRL A,R4
	MOV R4,A
	lcall LFSRShift
	MOV A,R4
	MOV R3,A
	
	CLR RS1 ;move to registerbank 08h to 0Fh
	CLR RS0
	
	RET
	
;shift the 32 bit registers of the LFSR
LFSRShift: 
	MOV A,R3 ; rotate shift register
	RLC A
	MOV R3,A
	MOV A,R2 ; rotate shift register
	RLC A
	MOV R2,A
	MOV A,R1 ; rotate shift register
	RLC A
	MOV R1,A
	MOV A,R0 ; rotate shift register
	RLC A
	MOV R0,A
	ret	
	

	block0:
	db 0x3e
	db 0x3e
	db 0x3e
	db 0x3e
	
	
	block1:
	
	db 0x3e
	db 0x1C
	db 0x1C
	db 0x3e
	
	block2:
	
	db 0x3e
	db 0x38
	db 0x38
	db 0x3e
		
	block3:
	
	db 0x3e
	db 0x18
	db 0x18
	db 0x3e
		
	block4:
	
	db 0x3e
	db 0x06
	db 0x06
	db 0x3e
	
	block5:
	db 0x3e
	db 0x08
	db 0x08
	db 0x3e
		
	block6:
	
	db 0x3e
	db 0x32
	db 0x32
	db 0x3e
		
	block7:
	db 0x3e
	db 0x26
	db 0x26
	db 0x3e
		
		

END

	
		