include "P16F877A.inc"

;****************************************************************
delay1			equ 		0X30
delay2 			equ 		0X31 	;TWO REGs FOR DELAY PURPOSES
COUNT1			equ		0X32
COUNT2			equ		0X33 	;TWO REGs FOR SAVING PURPOSES
VALUE1			equ		0X34
VALUE2			equ		0X35
LEFT				equ		0X36 	;THREE REGs FOR SUBs OPERATIONS
RESULT			equ		0X37
TEMPARATURE		equ		0X38 	;SAVING THE TEMPARATURE VALUE THROUGH THE CODE
FIRST_VALUE		equ		0X39
SECOND_VALUE 	equ		0X40
HOLD_FLAG		equ		0X41 	;REG FOR CHECKING THE HOLD BUTTON
AAA				equ		0X42
AA				equ		0X43
A				equ		0X44 	;THREE REGs for separating the number
BUTTON_FLAG		equ		0X45
temp1			equ		0X46
temp2 			equ		0X47 	;temporary REGs 
temp3			equ		0X48
temp4			equ		0X49
temp5			equ		0X50
temp6			equ		0X51
temp7			equ		0X52
temp8			equ		0X53
TEMP9			equ		0X54
;****************************************************************

org			0x0000
goto		Start	
org			0x0004
goto		ISR 	;ISR WHEN ONE OF THE BUTTONS PRESSED


;***********************CONFIGURATION***********************

Start

	banksel		PR2
	movlw		0X3D					;IN DECIMAL 61
	movwf		PR2
	bsf 			ADCON0,ADCS0
	bcf  			ADCON0,ADCS1 		;CONFIGURING ADC CLOCK TO (Fosc/8)
	movlw 		b'00001111'
	movwf		TRISA				;RA0,1,2,3 INPUT
	clrf 			TRISB
	clrf			TRISC 				;PORTC OUTPUT
	clrf			TRISD 				;PORTD OUTPUT
	bsf			TRISB,4				;THESE TWO PINS ARE USED FOR THE 
	BSF 			TRISB,5				; PORTB CHANGE ISR
;****************************************************************
	BANKSEL		ADCON1 				;CONFIGURUNG ANALOG TO DIGITAL CONVERTOR
	MOVLW		0X88 				;IN BINARY 1000 1000
	MOVWF		ADCON1				;SET THE ADC AS (RIGHT-JUSTIFIED) AND A/D Acquantization Time for (2Tad) with CONVERSION CLOCK (Fosc/2)
	CALL 		DELAY 	
	CALL 		DELAY
;****************************************************************	
	BANKSEL		T2CON  																			;---------------------------------------
	BSF 			T2CON,1																			;--------CONFIGURE THE PIC
	BSF 			T2CON,2                     		  ;prescale = 16													;----------	
;****************************************************************						
	BSF			CCP1CON,2
	BSF 			CCP1CON,3 			;SET THE CCP1/RC2 FOR THE PWM MODULE
	BCF			CCP1CON,4 			
	BCF 			CCP1CON,5			;SET IT FOR THE CAPTURE MODE
;****************************************************************
	CLRF		CCPR1L 				;CLEAR THE CCPR1L REGISTER VALUE AND THE START OF THE CODE
	CLRF		PORTD				;CLEAR THE OUTPUT ON PORT-D (LCD)																	;----------------------------------------
	CLRF		PORTC				;CLEAR THE OUTPUT ON PORT-C (MOTORS OR FANS)			
	CLRF		HOLD_FLAG			;CONFIGURING THE MICRO-CONTROLLER
	clrf			BUTTON_FLAG			;CLEAR ALL THE FALGS AT THE START OF THE CODE
;****************************************************************
	MOVLW 		0X38
	CALL 		send_cmd 				;SET THE LCD TO SHOW AS (8-BIT, 2-LINES , 5x7 DOTS)
	MOVLW		0X0C
	CALL 		send_cmd 				;SET THE LCD ON AND THE CURSOR IS OFF
	MOVLW 		0x02 					
	CALL		send_cmd				;Display and cursor home
	MOVLW 		0X01
	CALL 		send_cmd 				;CLEAR THE LCD
;****************************************************************
	BSF 		INTCON,GIE
	BSF 		INTCON,RBIF				;ENABLING THE INT FLAGS 
	BSF 		INTCON,RBIE

;***********************MAIN program***********************

PPROGRAMM 
	MOVF	HOLD_FLAG,F 			;TO CHECK THE FLAG IF 0 OR NOT
	BTFSS 	STATUS,Z 			 ;BUTTON PRESSED OR NOT
	GOTO 	HOLD_BUTTON
	movf 	BUTTON_FLAG,F 		;TO CHECK THE FLAG IF 0 OR NOT
	BTFSS 	STATUS,Z 			 ;AUTO OR MANUL(PRESSED)
	goto 	MODE_1
	goto 	MODE_0

;****************************************************************
MODE_0   	;AUTO MODE DEFAULT
	CALL 	PIN_RA0 			;READ RA0
	CALL 	READ     			;READ AND CONVERT TO DIGITAL 0-255
	CALL 	CAL_TEMP 		;CONVERT THE VALUE TO (0-40)				
	CALL 	LCD
	CALL 	PWM
	GOTO	LOOPP


;****************************************************************
MODE_1
	CALL 	PIN_RA0
	CALL 	READ 			;READ THE VALUE FROM ZERO TO 1023 
	CALL 	CAL_TEMP 		;AND WE NEED TO CONVERT IT FROM 0 TO 40
	CALL 	PIN_RA1
	CALL 	READ
	CALL 	PWM
	CALL 	LCD
	GOTO	LOOPP

;****************************************************************

HOLD_BUTTON
	CLRF		CCPR1L 			;pressing the hold button leads to clear CCPR1L
	BCF 			CCP1CON,4 		;and bit 4 and 5 of CCP1CON Reg
	BCF			CCP1CON,5 		;and Calling the HOLD_LCD to display HOLD
	CALL 		HOLD_LCD
;****************************************************************
LOOPP
	CALL 		delay100ms 		;--------------------
	CALL 		delay100ms       	 	;INFINTINE DELAY UNTILL ONE OF THE
	goto			Start				;BUTTONS HAD BEEN PRESSED
								;-----------------------------------------		

;****************************************************************

HOLD_LCD
	MOVLW	0X01
	CALL	send_cmd		
	MOVLW	0X02
	CALL	send_cmd
	MOVLW	'S'
	CALL	sendchar
	MOVLW	'Y' 				;-----------------------------------
	CALL	sendchar 			;display SYSTEM-ON-HOLD message on the LCD
	MOVLW	'S' 				;
	CALL	sendchar 			;-----------------------------------
	MOVLW	'T'
	CALL	sendchar
	MOVLW	'E'
	CALL	sendchar
	MOVLW	'M'
	CALL	sendchar
	MOVLW	'-'
	CALL	sendchar
	MOVLW	'O'
	CALL	sendchar
	MOVLW	'N'
	CALL	sendchar
	MOVLW	'-'
	CALL	sendchar
	MOVLW	'H'
	CALL	sendchar
	MOVLW	'O'
	CALL	sendchar
	MOVLW	'L'
	CALL	sendchar
	MOVLW	'D'
	CALL	sendchar
RETURN




;****************************************************************

ISR                                              ;ISR Subroutine
	movwf		COUNT2 		;SAVING WORKING REGISTER IN COUNT1
	movf 		STATUS,W 	;saving the STATUS reg in COUNT1 REG
	movwf		COUNT1
	btfss 		INTCON,RBIF 	;CHECKING FOR THE PORTB CHANGE ISR
	GOTO		OUT_OF_ISR 		;ENDING THE ISR IF THE FLAGE IS 0
	MOVF		PORTB,W
	BCF			INTCON,RBIF 	;CLEAR THE FLAG IF 1
	BTFSC		PORTB,4
	GOTO		HOLD_CHANGED  ;CHECK WHAT BUTTON HAD BEEN PRESSED
	BTFSC		PORTB,5
	GOTO		MODE_CHANGED
	goto		OUT_OF_ISR
;****************************************************************
HOLD_CHANGED
comf		HOLD_FLAG	;HOLD BUTTON PRESSED AND CHANGED ITS VALUE
goto		OUT_OF_ISR	
;****************************************************************
MODE_CHANGED
movf		HOLD_FLAG,f
btfss		STATUS,Z 	;MODE BUTTON PRESSED AND CHANGED ITS VALUE
goto		OUT_OF_ISR	
comf		BUTTON_FLAG 	

;****************************************************************	
OUT_OF_ISR

	CALL 	delay400us 
	MOVF	COUNT1,w
	movwf	STATUS	   			;-------------------------------------------------------
	MOVF	COUNT2,w 			;ENDING THE ISR AND GO BACK TO THE AUTO MODE
 								;--------------------------------------------------------
	RETFIE

;****************************************************************

CAL_TEMP
	BANKSEL 		ADRESL
	MOVF		ADRESL,W 			;ADRESL HAS THE 8 BITS				 
	BANKSEL		VALUE1
	movwf		VALUE1 				;ADRESH HAS THE 2 MOST SIGNIFICANT BITS /////   VALUE1 = ADRESL
	movlw		0x19 					; 25 this number will be used to split the range from 0-40
	movwf		VALUE2 				;into 4 quarters and will use ADRESH to know 
	CALL		SPLITTER 				;which quarter will be in it
	movfw			RESULT
	movwf		TEMPARATURE 	;0-10
	movfw			ADRESH	
	movwf		SECOND_VALUE 		;ADRESH=SECOND_VALUE      ADRESH=2 RESULT=5 30 35
	movlw		d'10' 				
	movwf		FIRST_VALUE
	CALL 		MULTIPLY_THE_TEMPARATURE_VARIABLE 				;after converting the numbe will be multiplied by the 10
	movf			RESULT,w 			;since we already pointed where we are in quarters
	addwf		TEMPARATURE,f 		;the TEMPARATURE reg will be the result of summation
	RETURN							;the divison result and the multiplt result
 	

;****************************************************************


PWM
	Movf 		ADRESH ,F				;USE ADRESH TO DECIDE WHAT QUARTER WE ARE IN  0 1 (2) 3
	Btfsc 		STATUS,Z 			;FIRST WE CHECK IF WE ARE IN QUATRTER 0 BY CHECKING 
	Goto 		FIRST_QUARTER 				; THE ZERO FALG
	Movlw 		0x01 
	Subwf 		ADRESH,W 			;SUBTRACT 1 FROM IT IF THE RESULT IS 0 THEN WE GO TO FIRST
	Btfsc 		STATUS,Z 			
	Goto 		SECOND_QUARTER
	Movlw 		0x02
	Subwf 		ADRESH,W 			;SUBTRACT 2 FROM IT IF THE RESULT IS 0 THEN WE GO TO SECOND
	Btfsc 		STATUS,Z
	Goto 		THIRD_QUARTER
	Movlw 		0x03 					;SUBTRACT 3 FROM IT IF THE RESULT IS 0 THEN WE GO TO THIRD_QUARTER
	Subwf 		ADRESH,W
	Btfsc 		STATUS,Z
	Goto 		FOURTH_QUARTER
;****************************************************************

FIRST_QUARTER 		;THE SPEED IS 0 RPM FOR THE MOTORS
	Clrf 		CCPR1L 				;THE FIRST QUARTER
	Bcf 		CCP1CON,4
	Bcf 		CCP1CON,5			;0%
	RETURN
;****************************************************************
SECOND_QUARTER			;THE SPEED IS 1000 RPM FOR THE MOTORS
	Movlw 		b'00010101' 		;21
	Movwf 		CCPR1L 			;THE SECONDE QUARTER
	Bcf 		CCP1CON,4	
	Bcf 		CCP1CON,5	 	;34.1%
	RETURN
;****************************************************************
THIRD_QUARTER 		;THE SPEED IS 2000 RPM FOR THE MOTORS
	Movlw 		b'00101010'		;42
	Movwf 		CCPR1L 			;THE THIRD_QUARTER 
	Bsf 		CCP1CON,4
	Bcf 		CCP1CON,5	
	RETURN
;****************************************************************
FOURTH_QUARTER 			;THE SPEED IS 3000 RPM FOR THE MOTORS
	Movlw 		b'00111101' 		;61
	Movwf 		CCPR1L 			;THE FOURTH_QUARTER
	Bsf 		CCP1CON,4
	Bsf 		CCP1CON,5	
RETURN
;****************************************************************

PIN_RA0 		;RA0 SUB-ROUTINE
	Bcf 		ADCON0, CHS0 				;Channel0 RA0/AN0
	Bcf 		ADCON0,CHS1 				;READ FROM RA0 / AN0
	Bcf 		ADCON0,CHS2 				;0 0 0
RETURN

;****************************************************************

PIN_RA1 		;RA1 SUB-ROUTINE
	Bsf 		ADCON0, CHS0 			;Channel1 RA1/AN1
	Bcf 		ADCON0,CHS1				;READ FROM RA1/AN1
	Bcf 		ADCON0,CHS2 			;0 0 1
RETURN


;****************************************************************
READ
	Bsf 		ADCON0,ADON
	Call 		delay400us
	Bsf 		ADCON0,GO 				;startup ADC divert
DELAY1
	Btfss 		PIR1,ADIF 				;is the convert have finished? 
	Goto 		DELAY1					;wait for the convert finished
	Bcf 		PIR1,ADIF 				;Clear the A/D flag
	Bcf 		ADCON0,ADON 			;turns the A/D On (when = 1) or off (when = 0),
RETURN 									;thus saving the power it consumes.

;****************************************************************



LCD
	MOvlw 		0x01
	Call 		send_cmd
	Movf 		BUTTON_FLAG,F
	Btfsc 		STATUS,Z
	goto 		AUTO 	;BUTTON_FLAG reg = 0
	goto 		MANUAL 	;BUTTON_FLAG reg = 1
;****************************************************************

AUTO
	Movlw 	0x02
	Call 	send_cmd
	Movlw 	'M'
	Call 	sendchar
	Movlw 	'O'
	Call 	sendchar        
	Movlw 	'D'
	Call 	sendchar
	Movlw 	'E'
	Call 	sendchar
	Movlw 	'-'
	Call 	sendchar
	Movlw 	'0'
	Call 	sendchar 			;----------------------------
	Movlw 	' '			;DEISPLAY:		
	Call 	sendchar			;MODE-0 : AUTO
	Movlw 	':' 			;---------------------------
	Call 	sendchar
	Movlw 	' '
	Call 	sendchar
	Movlw 	'A'
	Call 	sendchar
	Movlw 	'U'
	Call 	sendchar        
	Movlw 	'T'
	Call 	sendchar
	Movlw 	'O'
	Call 	sendchar
goto DISPLAY_SPEED

;****************************************************************

MANUAL
	Movlw 	0x02
	Call 	send_cmd
	Movlw 	'M'
	Call 	sendchar
	Movlw 	'O'
	Call 	sendchar        
	Movlw 	'D'
	Call 	sendchar
	Movlw 	'E'
	Call 	sendchar
	Movlw 	'-'
	Call 	sendchar 					
	Movlw 	'1'				
	Call 	sendchar 				;----------------------------
	Movlw 	' ' 				;DISPLAY:
	Call 	sendchar				;MODE-1 : MANU
	Movlw 	':'				;----------------------------
	Call 	sendchar
	Movlw 	' '
	Call 	sendchar
	Movlw 	'M'
	Call 	sendchar
	Movlw  	'A'
	Call 	sendchar 		 
	Movlw 	'N'
	Call 	sendchar
	Movlw 	'U'
	Call 	sendchar

;****************************************************************
DISPLAY_SPEED			;THIS SUBROUTINE IS RESBONSIBLE FOR DISPLAYING THE SPEED VALUE ON THE LCD
	Movlw 	0xc7
	Call 	send_cmd 	;POSITION THE MESSAGE ON THE LCD
	Movlw 	'S'
	Call 	sendchar
	Movlw 	':'
	Call 	sendchar 	;WE USE ADRESH VALUE TO KNOW WHAT QUARTER WE ARE IN 
	Movf 	ADRESH,F 	;USING THE SAME REGS TO KNOW WHAT SPEED OF THE FANS
	Btfsc 	STATUS,Z 	;ADRESH=0---->SPEED=0 RPM 		;CHECK IF ITS ZERO FIRST BEFORE 
	goto 	RPM_IS_ZERO 	;ADRESH=1---->SPEED=1000RPM 	
	Movf 	ADRESH,W 	;ADRESH=2---->SPEED=2000 RPM       ;ADRESH=3---->SPEED=3000 RPM
	Addlw 	0x30 		;to display a number on the lcd we have to send the number 0x30=0, 0x31=1 and so on
	Call 	sendchar
	movlw 	'0'
	Call 	sendchar
	movlw 	'0'
	Call 	sendchar
	movlw 	'0'
	Call 	sendchar
	movlw 	'R'
	Call 	sendchar
	movlw 	'P'
	Call 	sendchar
	movlw 	'M'
	Call 	sendchar
	GOTO 	DISPLAY_TEMPARATURE 	;AFTER DISPLAYING THE SPEED WE WANT TO DISPLAY THE TEMPARATURE VALUE ON THE LCD TOO

;****************************************************************

 RPM_IS_ZERO
	movlw 	'0'
	Call 	sendchar
	movlw 	'-'
	Call 	sendchar
	movlw 	'R' 			;DISPLAY : 0 RPM
	Call 	sendchar
	movlw 	'P'
	Call 	sendchar
	movlw 	'M'
	Call 	sendchar

;****************************************************************

DISPLAY_TEMPARATURE 				;THIS SUBROUTINE IS RESBONSIBLE FOR DISPLAYING THE TEMPARATURE VALUE ON THE LCD
	Movlw 	0xc0
	Call 		send_cmd 	;POSITION THE MESSAGE ON THE LCD
	Movlw 	'T'
	Call 		sendchar
	Movlw 		'='			
	Call 		sendchar
	Movf 	TEMPARATURE, W 			;W=TEMPARATURE
	Call 		BCD    				;to separate hunderads from tens from ones 
	Movf 	TEMPARATURE,W 			;AFTER THE BCD TEMPARATURE=W
	Sublw 	D'9'
	Btfsc 	STATUS,C 				;CHECK IF THE VALUE IS LESS THAN 10 BY SIBTRACTING FROM IT 9
	goto 	LESS_THAN_OR_EQUAL_9 	;AND CHECK ON THE CARRY FLAG IS ITS NEGATIVE OR POSITIVE
	Movlw 	0x30
	Addwf 	AA,W
	Call 		sendchar 				;DISPLAY THE TENS FIRST
	Movlw 	0x30
	Addwf 	A,W
	Call 		sendchar 				;DISPLAY THE ONES
	Movlw 	'c'
	Call 		sendchar 				;WE DONT NEED TO DISPLAY THE 100 SINCE THE RANGE IS FROM 
	Movlw 	0xdf 							;ZERO --- 40
	Call 		sendchar
RETURN
;****************************************************************
LESS_THAN_OR_EQUAL_9
		movf		TEMPARATURE,w
		addlw		0x30
		call		sendchar		
		movlw		'c'
		call		sendchar
		movlw		0xdf	;----------------> C sign 
		call		sendchar	
	RETURN


;****************************************************************

BCD
	movwf		temp3
	call		THE_HUNDREDS
	movwf 		AAA
	movf		temp3 ,W				;THIS SUBROUTINE ITS GOAL TO TAKE OUT AND SPLIT THE TEMPRATURE VALUE INTO HUNDRADES
	call		THE_TENS  			;AND TENS AND ONES 
	movwf 		AA				;AND SAVE EVERY VALUE ON THE LCD	
	movf		temp3,W
	call		THE_ONES
	movwf 		A
	movf		temp3,W
	RETURN

;****************************************************************

THE_HUNDREDS
	movwf		temp2
	clrf		temp1 		;THIS SUBROUTINE SEPARATES THE VALUE INTO HUNDRADS ,TENS AND ONES
LOOP_100
	movlw 		d'100' 		;sub 100,result keep in F
	incf		temp1,f 	;judge if the result biger than 100
	subwf		temp2,f 	;no,get the ten bit result
	btfsc		STATUS,C 	;yes,result keep in TEMP
	goto		LOOP_100	;hundred bit add 1
	decf		temp1,w 	;continue to get hundred bit result
	RETURN

;****************************************************************
THE_TENS
	movwf		temp2
	clrf		temp1
LOOP_10
	movlw 		d'10' 		;sub 10,result keep in F
	incf		temp1,f 	;judge if the result biger than 10
	subwf		temp2,f 	;no,get the Entries bit result
	btfsc		STATUS,C 	;yes,result keep in TEMP
	goto		LOOP_10 	;ten bit add 1
	decf		temp1,w 	;turn to continue get ten bit
	goto		THE_ONES

;****************************************************************

THE_ONES
	movwf	temp1
	movlw 	d'10' 			;the value of Entries bit
LOOP_1
	subwf		temp1,f
	btfsc			STATUS,C
	goto			LOOP_1
	addwf		temp1,w
	RETURN

;****************************************************************

MULTIPLY_THE_TEMPARATURE_VARIABLE
	CLRW
	CLRF		RESULT 			;PRODUCE THE ACTUACL TEMPARATURE VALUE
MULT_LOOP 						;IF ITS 10 20 30 40
	MOVF		FIRST_VALUE,w 	;AND RETURN THE VALUE IN A TEMP. REGISTER
	addwf		RESULT
	decfsz		SECOND_VALUE
	goto			MULT_LOOP
	RETURN
;****************************************************************

SPLITTER
	movf		VALUE1,W
	movwf		RESULT	
	clrf			LEFT	
	movlw		d'8'
	movwf		VALUE1
SPLIT 							;THE GOAL FOR THIS SUB. IS THAT WE THAT WE DIVIDED THE 
	rlf			RESULT,f				;TEMPRATURE INTO 4 QUARTERS	
	rlf			LEFT,f				;0--255 ===>  0--10 C						
	movf		VALUE2	,W		;255-512 ===>	10-20 C
	subwf		LEFT,w			;512-778 ====> 	20-30 C
	BTFSC		STATUS,C			;778-1023 ====>  30-40 C 
	movwf		LEFT	
	decfsz		VALUE1,f
	GOTO		SPLIT
	RLF			RESULT,f
	RETURN

	

;****************************************************************

delay400us
	movlw		d'99'
	movwf		delay1
	loop1
	nop 					 	;SUBROUTINE DEALY
	decfsz		delay1,f 		;TO GENERATE 400US
	goto		loop1 			;USED FOR  GIVING TIME THE 
	nop 						;THE PIC ENOUGH TIME TO DISPLAY THE LCD
RETURN	 

;****************************************************************
delay100ms

	movlw		d'130'
	movwf		delay2
	movlw		d'221'
	movwf		delay1 			;SUBROUTINE DEALY
	loop2 						;TO GENERATE 100ms
	decfsz		delay1,f 		;USED FOR GIVING THE PIC ENOUGH TIME
	goto		loop2	 		;FOR THE MOTORS
	decfsz		delay2,f
	goto		loop2

RETURN	

;****************************************************************

send_cmd

	MOVWF	PORTD
	BCF		PORTC,3 		; Refer to table 1 on Page 5 for review of this subroutine
	BSF		PORTC,5
	NOP	 					
	BCF		PORTC,5
	BCF		PORTC,4
	CALL	delay400us
	CALL	delay400us
	CALL	delay400us

RETURN

;****************************************************************

sendchar
	movwf		PORTD 		
	BSF			PORTC,3 		; Refer to table 1 on Page 5 for review of this subroutine
	BSF			PORTC,5
	NOP
	BCF			PORTC,5
	BCF			PORTC,4
	CALL		delay400us
	CALL		delay400us
	CALL		delay400us
RETURN

;***************************Delay subroutine***********************
DELAY
MOVLW 0xFF
MOVWF TEMP9
L1
DECFSZ TEMP9,1
GOTO L1
RETURN


END












