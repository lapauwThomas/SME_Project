
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
ORG 000Bh
LJMP ISR_tmr0

ORG 001Bh
LJMP ISR_tmr1

ORG 0043h
LJMP ISR_ADC




;Address declarations
vidMemStart EQU 030h
vidMemEnd EQU 052h
vidMemLength EQU 35

numberOfCollumns EQU 40
numberOfRows EQU 7
bytesPerRow EQU 5
	
bytesPerBlock EQU 8
	
cursorByte EQU 11011111b




;Initialization code
init:	

			;init tmr0
			MOV TMOD,#00010001b ;config tmr0 & tmr1 in 16bit mode
			MOV TH0,#0FFh ;tmr0 MSB
			MOV TL0,#0FFh ;tmr0 LSB
			
			MOV TH1,#0FFh ;tmr0 MSB
			MOV TL1,#0FFh ;tmr0 LSB
			
			SETB ET1
			SETB ET0 ;enable interrupt of tmr0


			MOV SP, #70h ; move stackpointer above registers
			 
			 
;Initialize ram			 
			MOV R0,#vidMemStart 
			MOV R1,#numberOfCollumns
ramInit:
			MOV @R0,#0FFh
			INC R0
			DJNZ R1,ramInit
			
; seed of LFSR		
MOV 18h, #1101010b 


MOV R0,#numberOfCollumns
gameInit:
		MOV R7,#03eh ; stockate data in R7 for collumnshift
		LCALL dispColShift
		DJNZ R0, gameInit


		MOV A,18h ; get data from MSB LFSR
		ANL A,#0011100b ;mask for the number of blocks		
		MOV 53h, A ; save current adress for next block


;**********************************************************************************
	; Setup for the ADC
	MOV ADCF,#0FFh ;enable the adc...
	SETB EADC
	MOV ADCON, #00101111b ; set P1.0 as ADC input


		MOV R3,#bytesPerBlock ; repeat 4 times
		MOV A,18h ; get data from MSB LFSR
		ANL A,#0111000b ;mask for the number of blocks		
		MOV 53h, A ; save current adress for next block
		LCALL LFSR  ; generate new random data
		CPL P2.4 ; toggle led to see if working

		MOV 57h,R3
	
	


SETB TR0 ;run tmr0
SETB TR1
SETB EA ;global interrupt enable

CLR P2.3 ;led to see if code is running
LJMP main
			
;Main program
main:		
				LCALL LFSR  ; generate new random data
				

				
			LJMP main
		
;Interrupt handlers


ISR_tmr0: 
			CLR EA ;global interrupt disable
			CLR TR0 ;stop tmr0
			CLR TR1
			;reload timer
			MOV TH0,#0D0h ;tmr0 MSB
			MOV TL0,#000h ;tmr0 LSB
			
;DISPLAY PART
		CLR RS1 ;move to registerbank 08h to 0Fh
		SETB RS0
		MOV R1, #numberOfRows ;counting register to 8 for rows
		MOV R2, #01111111b ;data rows (msb = 0, others are 1) single bit zero, to enable current row
		MOV R0, #vidMemStart ; starting adress disp mem
rowIteration:

			MOV R7, #bytesPerRow ; counter for 5 bytes of row data
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
			
			
			MOV R6, #cursorByte
			Acall shiftR6 ; shift collumn data byte into SR
			
			MOV R1, #04
lastLineComp:			 ;loop to approximate the timing of the other rows to have similar brightness
			MOV R6, #0FFh
			Acall shiftR6 ; shift collumn data byte into SR
			DJNZ R1,lastLineComp
			

			;MOV R6, #11101111b
			MOV A,5Ah
			ANL A, #11100000b ; mask 8 MSB 
			RL A
			RL A ;rotate so MSB become LSB
			RL A
			MOV R6, A ;stockate in R6
			MOV A,#11111110b ;cursor data in A
	locationLbl:
			RR A
			DJNZ R6,locationLbl  ;rotate cursor data equal to location
			ORL A,#10000011b ;mask data for center 
			MOV R6,A ; move cursor data to R6 for shift
			Acall shiftR6 ; shift collumn data byte into SR for row enable
			
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
		
		MOV TH1,#00h ;tmr0 MSB
		MOV TL1,#00h ;tmr0 LSB
		
		;DJNZ R5, afterRandom
		push Acc
		
		MOV A, 53h
		MOV DPTR, #block0		; begin bij block0
		MOVC A,@A+DPTR

		MOV R7,A ; stockate data in R7 for collumnshift
		LCALL dispColShift
		
		MOV A, 53h ; retrieve current data offset
		INC A; advance one adress
		MOV 53h, A ; save current adress
		MOV R3, 57h ;get current iteration from address
		DJNZ R3, afterRandom ; jupmp back to te iteration
		MOV R3,#bytesPerBlock ; repeat 4 times
		MOV A,18h ; get data from MSB LFSR
		ANL A,#0111000b ;mask for the number of blocks		
		MOV 53h, A ; save current adress for next block

		CPL P2.4 ; toggle led to see if working
		;MOV R5,#2
	afterRandom:
		MOV 57h,R3
		pop Acc
		SETB TR1 ;stop timer during buffer update
		SETB TR0
		SETB EA

reti

ISR_ADC:

		CLR TR0 ;stop timer during buffer update
		CLR TR1 ;stop timer during buffer update
		CLR EA ;global interrupt disable
		
push Acc
	MOV A, ADDH
	MOV 5Ah,A
	MOV ADCON, #00101111b ; set P1.0 as ADC input, restart conversion
pop Acc

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


detectCollision:
	






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
	MOV	R5, #numberOfRows ;counter to count rows
	;RRC A ;rotate to drop LSB
	MOV R1, #vidMemEnd ; start at highest address to decrease each time
dispColShiftLoop:
	RRC A ;Rotate LSB in carry to shift in row
	MOV	R6, #bytesPerRow ; counter to rotate 5 horizontal bytes
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
	db 0x3e
	db 0x3e
	db 0x3e
	db 0x3e
	
	
	block1:
	
	db 0x3e
	db 0x3e
	db 0x3e
	db 0x1C
	db 0x1C
	db 0x1C
	db 0x3e
	db 0x3e
	
	block2:
	
	db 0x3e
	db 0x3e
	db 0x3e
	db 0x38
	db 0x38
	db 0x38
	db 0x3e
	db 0x3e
		
	block3:
	
	db 0x3e
	db 0x3e
	db 0x3e
	db 0x18
	db 0x18
	db 0x18
	db 0x3e
	db 0x3e
		
	block4:
	
	db 0x3e
	db 0x3e
	db 0x3e
	db 0x06
	db 0x06
	db 0x3e
	db 0x3e
	db 0x3e
	
	block5:
	db 0x3e
	db 0x3e
	db 0x3e
	db 0x08
	db 0x08
	db 0x3e
	db 0x3e
	db 0x3e
		
	block6:
	
	db 0x3e
	db 0x3e
	db 0x3e
	db 0x32
	db 0x32
	db 0x3e
	db 0x3e
	db 0x3e
		
	block7:
	db 0x3e
	db 0x3e
	db 0x3e
	db 0x26
	db 0x26
	db 0x3e
	db 0x3e
	db 0x3e
		
	block8:
	db 0x3e
	db 0x3e
	db 0x3e
	db 0x26
	db 0x26
	db 0x3e
	db 0x3e
	db 0x3e
		
		

END

	
		