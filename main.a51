
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
;Address declarations

;Constants

;Initialization code
init:	

			;Initialization code here
			CLR P2.3 ;led to see if code is running
			
			;init tmr0
			MOV TMOD,#00000001b ;config tmr0 in 16bit mode
			MOV TH0,#0FBh ;tmr0 MSB
			MOV TL0,#099h ;tmr0 LSB
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
			
			
SETB TR0 ;run tmr0
LJMP main
			
;Main program
main:		

;DISPLAY PART
		MOV R7,18h ; get data from MSB LFSR
		CLR TR0 ;stop timer during buffer update
		CPL P2.4 ; toggle led to see if working
		LCALL LFSR  ; generate new random data
		LCALL dispColShift ; shift new column in display buffer
		SETB TR0 ;run tmr0 
		LCALL delay ; delay before repeat
		
		;MOV R7,#00h
		;CLR TR0
		;CLR P2.4
		;LCALL dispColShift
		;SETB TR0 ;run tmr0
		;LCALL delay

				
			LJMP main
		
;Interrupt handlers


ISR_tmr0: 
			CLR EA ;global interrupt disable
			CLR TR0 ;stop tmr0
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
lbl1:
			MOV A,@R0 ; get data from ram
			MOV R6,A
			acall shiftR6 ; shift byte into ram
			INC R0 ; get next byte
			DJNZ R7,lbl1 ; rinse and repeat
			
			;shift R2 with the current row enable in the register
			MOV A,R2
			MOV R6, A
			Acall shiftR6 ; shift collumn data byte into SR
			SETB P3.2 ; cycle store clock
			CLR P3.2

			;rotate row bit to enable next row
			MOV A, R2 
			RR A
			MOV R2,A
			 ; repeat until 7 rows done
			DJNZ R1, rowIteration

			CLR RS1 ;move to registerbank 00h to 08h
			CLR RS0	
			;reenable timers
			SETB TR0 ;run tmr0
			SETB EA ;global interrupt disable
			
			RETI

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
		MOV R1, #0EFh
		LCALL loop
		RET

loop: 	
		MOV R2, #00Fh
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
	

END