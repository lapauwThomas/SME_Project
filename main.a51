
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

DSEG AT 30h

	cursor DATA 54h
	blockIndex DATA 53h
	blockIteration DATA 57h
	ADCVal DATA 5Ah
CSEG	
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
	
cursorByte EQU 11111110b
cursorByteMask EQU 00000001b




;Initialization code
init:	

;**************** Init Timers **********************************
			MOV TMOD,#00010001b ;config tmr0 & tmr1 in 16bit mode
			MOV TH0,#0FFh ;tmr0 MSB
			MOV TL0,#0FFh ;tmr0 LSB
			
			MOV TH1,#0FFh ;tmr0 MSB
			MOV TL1,#0FFh ;tmr0 LSB
			
			SETB ET1
			SETB ET0 ;enable interrupt of tmr0


			MOV SP, #70h ; move stackpointer above registers
			 
			 
;***************** Initialize ram to all ones (all leds off)*******************************		 
			MOV R0,#vidMemStart 
			MOV R1,#numberOfCollumns
ramInit:
			MOV @R0,#0FFh
			INC R0
			DJNZ R1,ramInit
			
;***************** Init the game logic	*************************
MOV 18h, #1101010b 

MOV R3,#bytesPerBlock ; repeat 4 times
MOV A,18h ; get data from MSB LFSR
ANL A,#0111000b ;mask for the number of blocks		
MOV blockIndex, A ; save current adress for next block
LCALL LFSR  ; generate new random data
CPL P2.4 ; toggle led to see if working
MOV blockIteration,R3


;*********************** ADC CONFIG ***********************************************
	; Setup for the ADC
	MOV ADCF,#0FFh ;enable the adc...
	SETB EADC
	MOV ADCON, #00101111b ; set P1.0 as ADC input
	MOV ADCVal,#01100000b

	
;************* end of init: enable timers *********************

SETB TR0 ;run tmr0
SETB TR1
SETB EA ;global interrupt enable

CLR P2.3 ;led to see if code is running
LJMP main

			
;*********************************Main program***************************************
main:		
;does except update the random numbers actually everything happens timer based since the updating of either one is mutually exclusive, 
;no interrupt may happen while they are busy
	LCALL LFSR ; update ranom value
			LJMP main
		
;******************************* Interrupt handlers *********************************

; This timer refreshes the screen at a fast enough rate
; it gets the data from the ram and shifts it onto the display.
; based on the value of the ADC it also shows the cursor on the correct location.  
ISR_tmr0: 
			CLR EA ;global interrupt disable since the video memory may not be updated while the screen is updated
			CLR TR0 ;stop tmr0
			CLR TR1
			
			;reload timer0
			MOV TH0,#0D0h ;tmr0 MSB
			MOV TL0,#000h ;tmr0 LSB
			
;DISPLAY PART
		CLR RS1 ;move to registerbank 08h to 0Fh to prevent overwriting of registers
		SETB RS0
		
	;This parts displays the game background on the screen from ram
		MOV R1, #numberOfRows ;counting register to 8 for rows
		MOV R2, #01111111b ;data rows (msb = 0, others are 1) single bit zero, to enable current row
		MOV R0, #vidMemStart ; starting adress disp mem
rowIteration:

			MOV R7, #bytesPerRow ; counter for 5 bytes of row data
lineBytes:
			MOV A,@R0 ; get data from ram starting at the lowest address
			MOV R6,A ; move data to R6 to shift it into the screen
			acall shiftR6 ; shift byte into screen
			INC R0 ; increase address by one for next byte to put onto the screen
			DJNZ R7,lineBytes ; rinse and repeat for the full row
			
			;shift R2 with the current row enabled in the register
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
			
			
	;This part displays the cursor, making use of the duty cycle of the previous display part over the timerperiod,
	;the bringhtness of the cursor can be controlled
	
			MOV R1, #04  ;the first 4 bytes that are shifted into the register puts all those leds off
lastLineComp:			 ;loop to approximate the timing of the other rows to have similar brightness
			MOV R6, #0FFh
			Acall shiftR6 ; shift collumn data byte into SR
			DJNZ R1,lastLineComp
			
			MOV R6, #cursorByte ;shift this byte into the shift registers to enable a led in the eight row of the display to be matched with the cursor location
			Acall shiftR6 ; shift collumn data byte into SR
			

			;MOV R6, #11101111b
			MOV A,ADCVal ;get the last updated value of the adc to A
			MOV B,#37 
			DIV AB ;divide the ADC value to get a value between 0 and 6 to for the cursor position
			MOV R6, A ;stockate in R6
			MOV A,#11111101b ;A single pixel for the row enable

	locationLbl:
			RL A
			DJNZ R6,locationLbl  ;rotate cursor data equal to location previously calculated
			ORL A,#00000001b ;mask data over the display       ;TODO: mag evt weg
			MOV cursor,A
			MOV R6,A ; move cursor data to R6 for shift
			
			Acall shiftR6 ; shift cursor position data byte into SR for row enable
			SETB P3.2 ; cycle store clock
			CLR P3.2
			
			
			LCALL detectCollision ; check if a collision with the background happened with the current cursor position ;TODO: mag evt na de timers geplaatst worden
			

			CLR RS1 ;move to registerbank 00h to 08h
			CLR RS0	
			;reenable timers
			SETB TR0 ;run tmr0
			SETB TR1
			SETB EA ;global interrupt disable
			
			RETI

;************************** This timers updates the background of the game **********************************
ISR_tmr1:
;DISPLAY PART
		CLR TR0 ;stop timer during buffer update
		CLR TR1 ;stop timer during buffer update
		CLR EA ;global interrupt disable
		
		;reload timers
		MOV TH1,#00h ;tmr0 MSB
		MOV TL1,#00h ;tmr0 LSB
		
		push Acc
		
		;get the current block index from ram
		MOV A, blockIndex
		MOV DPTR, #block0		; start at block0 and count from there

		MOV R7,A ; stockate data in R7 for collumnshift function
		LCALL dispColShift ; shift the new collumn in
		
		MOV A, blockIndex ; retrieve current data offset
		INC A; advance one adress
		MOV blockIndex, A ; save current adress
		
		MOV R3, blockIteration ;get current iteration from address this counts the number of collumns from a certain block that have already been shifted
		
		DJNZ R3, afterRandom ; if the last collumn of the block is not updated jump over the generation of the next block
		
		MOV R3,#bytesPerBlock ; repeat 4 times
		MOV A,18h ; get data from MSB LFSR
		ANL A,#0111000b ;mask for the number of blocks	instead of dividing, this keeps 8 possible values for the 8 blocks,
						;with a distance of 8 from each other matching the number of collumns of a block
		MOV blockIndex, A ; save current adress for next block

	afterRandom:
		MOV blockIteration,R3 ;this saves the current shifted collumn count of a block in ram 
		;pop A and restart timers
		pop Acc
		SETB TR1 
		SETB TR0
		SETB EA

reti


;*********************** This interrupt updates the value from the ADC **********************************
ISR_ADC:

		CLR TR0 ;stop timer during buffer update
		CLR TR1 ;stop timer during buffer update
		CLR EA ;global interrupt disable
		
push Acc
	MOV A, ADDH ;get the highest 8 bits, low resolution is not needed,
	MOV ADCVal,A ; move the value to ram
	MOV ADCON, #00101111b ; set P1.0 as ADC input, restart conversion
pop Acc

		SETB TR1 ;stop timer during buffer update
		SETB TR0
		SETB EA
reti


;******************************************************************************************************
;********************************* HELPER FUNCTIONS *************************************************
;****************************************************************************************************


;**************************$ This function shifts the byte stockated in R6 into the shiftregisters connected to P4.1****************************************
shiftR6:
				push Acc
				MOV A, R6  ; move the value to A
				MOV R6, #08 ;counting register to 8
				Reg:
				RRC A ;rotate accumulator troug carre to get the the LSB into the carry
				MOV P4.1,C ;carry to the data pin
				SETB P4.0 ; cycle serial clock
				CLR P4.0
				DJNZ R6,Reg ;if 8 bits are shifted go further otherwise repeat
				pop Acc
				ret

;**************************$ This function detects the collisions of the cursor with the background blocks **************************************************
detectCollision:

; this moves the eight collumn of the display data into a single byte to be matched with the cursor position
	MOV A,34h ;get the byte of the first row
	RRC A ;shift the LSB into the carry, this matches the eight collumn of that row.
	MOV 67h,C ;stockate C in the MSB of the collumn
	
	MOV A,39h ;second row
	RRC A
	MOV 66h,C ;C to MSB-1
	
	MOV A,3Eh
	RRC A
	MOV 65h,C
	
	MOV A,43h
	RRC A
	MOV 64h,C
	
	MOV A,48h
	RRC A
	MOV 63h,C
	
	MOV A,4Dh
	RRC A
	MOV 62h,C
	
	MOV A,52h ; seventh row, to LSB + 1
	RRC A
	MOV 61h,C

	CLR 60h ; clear the LSB ;TODO needed?
	
	MOV A,2Ch ;move the constructed byte to A
	CPL A ; complement it because all the 0 represent walls
	MOV 2Ch,A ; move it back

	MOV A,cursor ; get the cursor data
	CPL A ; complement it since the 0 is where the cursor is, all others should be 1
	ANL A,#11111110b ; mask it so possible residual data on the invisible eight row does not influence the detection
	ANL A,2Ch ; And the cursor positon in A with the eight collumn of the screen
			  ; if the cursor is on the same location as a lit pixel (obstackle/wall) the AND will produce a 1 on a certain position
	
	JNZ dead ; if the AND action is !=0 a collision is detected , then jump to the dead routine

	ret
	
;***************** This routine is executed when a collision is detected ********************
dead:

; this clear the game field
MOV R0,#numberOfCollumns
gameReset:
		MOV R7,#0FFh ; stockate data in R7 for collumnshift
		LCALL dispColShift
		DJNZ R0, gameReset
LJMP ISR_tmr0 ;jump to restart the game


	

;*************************** This function shifts a collumn in the game to make it go forward *******************

;shift 7 MSB in framebuffer
dispColShift:
	MOV A, R7 ; move the collumn that needs to be shifted in from R7 into A
	MOV	R5, #numberOfRows ;counter to count rows 
	;RRC A ;rotate to drop LSB ;TODO check if still needed
	MOV R1, #vidMemStart ; start at lowest address to increase each time
dispColShiftLoop: ; this part is looped
	RRC A ;Rotate LSB in carry to shift into the row
	MOV	R6, #bytesPerRow ; counter to rotate 5 horizontal bytes will be used in dispRowShift to count the progress
	
	PUSH ACC ; push acc to save current data of A
	ACALL dispRowShift ; rotate all row bytes 
	POP ACC ;pop the accumulator
	DJNZ R5, dispColShiftLoop ; repeat until all rows are shifted
	RET
	
	
	;TODO move this into the dispColShiftLoop instead of calling it
dispRowShift: ; this rotates the carry into the current memory address
	MOV A,@R1 ;start at the current addres and move it from RAM into A
	RLC A ;rotate the carry into the LSB of the current byte, the LSB comes into the the carry 
	MOV @R1,A ;move the updated byte back into ram
	INC R1	 ;increase for the next byte
	DJNZ R6, dispRowShift ; do this until the current row is updated. The carry becomes the LSB, the MSB becomes the carry to become the LSB of the next byte
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
		
;**********************************************************************************		
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
	
;*****************************************************************************************************************
;************************************* DIRECT BYTES IN CODE MEMORY **********************************************
;**************************************************************************************************************$

;*********************** These are the obstackle blocks ***************************************************
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

	
		