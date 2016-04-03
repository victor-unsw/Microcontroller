.include "m2560def.inc"

; common variables defined
.def	velocity=R20				; for velocity
.def	direction=R21				; for direction 
.def	hash_pressed=R22				; for hash pressing
.def	hover_mode=R23				; for hash mode '*'
 
.equ keyport = PORTL
.equ pressed = 0


; LCD interface
.equ 	LCD_CTRL_PORT = PORTA
.equ 	LCD_CTRL_DDR  = DDRA
.equ	LCD_DATA_PORT = PORTF
.equ	LCD_DATA_DDR  = DDRF
.equ	LCD_DATA_PIN  = PINF

.def	temp=R19
.equ    lcd_Line_two         = 0x40          ; start of line 2


; locaiton variables
.def	var_x=R24
.def	var_y=R25
.def	var_z=R26

.def	count1=R27
.def	tBCD=R28
.def	distance=R29


; For keypad
.equ col1 = PINL3
.equ col2 = PINL2
.equ col3 = PINL1

.def key_value = r16			
.def temp1 = r17
.def flags = r18


;*************************
; MACRO & PROCEDURES FOR LCD OPERATIONS

.equ LCD_RS = 7
.equ LCD_E = 6
.equ LCD_RW = 5
.equ LCD_BE = 4


.macro STORE
.if @0 > 63
sts @0, @1
.else
out @0, @1
.endif
.endmacro

.macro LOAD
.if @1 > 63
lds @0, @1
.else
in @0, @1
.endif
.endmacro


.macro do_lcd_command
	push r16
	ldi r16, @0
	rcall lcd_command
	rcall lcd_wait
	pop	r16
.endmacro
; parameter is from register
.macro do_lcd_command_use_register
	push r16
	mov r16, @0
	rcall lcd_command
	rcall lcd_wait
	pop	r16
.endmacro
; paramter is from register
.macro do_lcd_data
	push r16
	ldi r16, @0
	rcall lcd_data
	rcall lcd_wait
	pop r16
.endmacro
; parameter is from register
.macro do_lcd_data_use_register
	push r16
	mov r16, @0
	rcall lcd_data
	rcall lcd_wait
	pop r16
.endmacro

;************************************


; Reset vector
.org	0x0000
		jmp RESET

RESET:
	ldi var_x,25
	ldi var_y,25
	ldi var_z,0

	jmp main

.org    INT_VECTORS_SIZE

; --------------------- Main Code ---------------------- START

main:
	; initialize stack pointer to highest RAM address
    ldi     temp,low(RAMEND)
    out     SPL,temp
    ldi     temp,high(RAMEND)
    out     SPH,temp

	ser temp
	STORE   LCD_DATA_DDR, temp
	STORE	LCD_CTRL_DDR, temp
	clr temp
	STORE 	LCD_DATA_PORT, temp
	STORE	LCD_CTRL_PORT, temp

	; initializing the LCD
;	do_lcd_command 0b00111000 ; 2x5x7
	rcall sleep_5ms
	do_lcd_command 0b00111000 ; 2x5x7
	rcall sleep_1ms
	do_lcd_command 0b00111000 ; 2x5x7
	do_lcd_command 0b00111000 ; 2x5x7
	do_lcd_command 0b00001000 ; display off
	do_lcd_command 0b00000001 ; clear display
	do_lcd_command 0b00000110 ; increment, no display shift
	do_lcd_command 0b00001110 ; Cursor on, bar, no blink

	do_lcd_data 'S'
	do_lcd_data 'T'
	do_lcd_data 'A'
	do_lcd_data 'R'
	do_lcd_data 'T'


cont:
	cpi hash_pressed,2
	breq cont
getkey:
	rcall get_key
	cpi key_value,0
	breq getkey
	cpi key_value,1
	breq K_1_operation
	cpi key_value,2
	breq K_2_operation
	cpi key_value,3
	breq K_3_operation
	cpi key_value,4
	breq K_4_operation
	cpi key_value,6
	breq reach_6
	cpi key_value,8
	breq reach_8
	cpi key_value,9
	breq K_9_operation
	cpi key_value,10
	breq go_to_star
	cpi key_value,11
	breq K_0_operation
	cpi key_value,12
	breq go_to_hash
	jmp cont

go_to_hash:
	rjmp hash_key_pressed
go_to_star:
	rjmp star_key_pressed

temp_cont:
	jmp cont

reach_6:
	jmp K_6_operation

reach_8:
	jmp K_8_operation

K_9_operation:
	cpi hover_mode,1
	breq temp_cont
	ldi direction,9
	call reset_clk
	rjmp cont

K_1_operation:
	cpi hover_mode,1
	breq jmpcont
	cpi velocity,4
	brsh jmpcont
	ldi temp,250
	call sleep_1ms
	inc velocity
	call reset_clk
	rjmp cont

K_2_operation:	
	cpi hover_mode,1
	breq cont
	ldi direction,2
	call reset_clk
	rjmp cont

K_3_operation:
	cpi hover_mode,1
	breq temp_cont
	ldi direction,3
	call reset_clk
	rjmp cont

K_0_operation:
	cpi velocity,1
	brlo jmpcont
	cpi hover_mode,1
	breq jmpcont
	ldi temp,250
	call sleep_1ms
	dec velocity
	call reset_clk
	rjmp cont

K_4_operation:
	cpi hover_mode,1
	breq temp_cont
	ldi direction,4
	call reset_clk
	rjmp cont

K_6_operation:
	cpi hover_mode,1
	breq temp_cont
	ldi direction,6
	call reset_clk
	rjmp cont

K_8_operation:
	cpi hover_mode,1
	breq temp_cont
	ldi direction,8
	call reset_clk
	rjmp cont


jmpcont:
	call reset_clk
	rjmp cont


star_key_pressed:					;toggles hover mode
	cpi hover_mode,0
	breq start_hover
	ldi hover_mode,0
	rjmp cont

hash_key_pressed:					;toggles between start and land flight
	cpi hash_pressed,0
	breq start_flight
	
	ldi hash_pressed,2
	ldi direction,9
	ldi velocity,1
	call display
	rjmp cont

start_hover:
	ldi hover_mode,1
	call show_hover
	rjmp cont

reset_clk:
	ldi temp,0x09
	sts tcnt1h,temp
	ldi temp,$0dd
	sts tcnt1l,temp
	ret


return_from_isr:
	pop temp
	out sreg,temp
	reti		
dtr:
	call flight_end
	jmp return_from_isr
hover:
	jmp return_from_isr



; Procedures & subroutines ---------------------------------------------------
start_flight:
	cli
	ldi direction,3	
	ldi hash_pressed,1
	ldi velocity,2
	call display
	ldi count1,0
	ldi temp,$09
	sts tcnt1h,temp
	ldi temp,4
	sts	tccr1b,temp
	sei
	jmp cont

	do_lcd_command_use_register temp
	do_lcd_data 'P'
	do_lcd_data 'o'
		
	;dislapy x
	mov temp,var_x
	mov temp1,temp
	swap temp		
	do_lcd_data_use_register temp
	mov temp,temp1
	do_lcd_data_use_register temp

	;display y
	do_lcd_data_use_register temp
	mov temp,var_y
	mov temp1,temp
	swap temp
	do_lcd_data_use_register temp
	mov temp,temp1
	do_lcd_data_use_register temp
	
	;display z
	do_lcd_data_use_register temp
	mov temp,var_z
	mov temp1,temp
	swap temp
	do_lcd_data_use_register temp
	mov temp,temp1
	do_lcd_data_use_register temp
	ret

; numbers to ASCII
to_ascii:
	andi temp,$0f
	ldi key_value,$30
	add temp,key_value
	ret

;updates position
update_position:
	cpi direction,3
	breq increment_z
	cpi direction,9
	breq decrement_z
	cpi direction,2
	breq increment_y
	cpi direction,4
	breq decrement_x
	cpi direction,6
	breq increment_x
	cpi direction,8
	breq decrement_y

return_2:
	add distance,velocity
	brcs crashed
	ret

increment_x:
	add var_x,velocity
	jmp return_2
decrement_x:
	sub var_x,velocity
	jmp return_2

increment_y:
	add var_y,velocity
	jmp return_2
decrement_y:
	sub var_y,velocity
	jmp return_2

increment_z:
	add var_z,velocity
	jmp return_2
decrement_z:
	sub var_z,velocity
	jmp return_2


;displays crashed 
crashed:
	do_lcd_data 'O'
	do_lcd_data 'o'
	do_lcd_data 'p'
	do_lcd_data 's'
	do_lcd_data '!'
	do_lcd_data 'C'
	do_lcd_data 'R'
	do_lcd_data 'A'
	do_lcd_data 'S'
	do_lcd_data 'H'
	do_lcd_data 'E'
	do_lcd_data 'D'
	do_lcd_data '!'
	ldi temp,$ff
	out ddrc,temp
	sbi ddrg,2
	sbi ddrg,3

loop:
	ldi temp,$ff				
	out portc,temp
	ldi temp,250
	call sleep_1ms
	ldi temp,$00				
	out portc,temp
	cbi portg,2
	cbi portg,3
	ldi temp,250
	call sleep_1ms
	jmp loop
	
;displays velocity and direction 
display:
	do_lcd_data 'S'
	do_lcd_data 'P'
	do_lcd_data 'D'
	mov temp,velocity
	call to_ascii
	do_lcd_data_use_register temp
	do_lcd_data ' '
	do_lcd_data 'D'
	cpi direction,2
	brne t_1
	rjmp forward

t_1:
	cpi direction,8
	brne t_2
	rjmp backward

t_2:
	cpi direction,4
	breq left_dir
	cpi direction,6
	breq right_dir
	cpi direction,3
	breq up_dir
	cpi direction,9
	breq down_dir
return:
	ret

left_dir:
	ldi temp,'L'
	do_lcd_data 'L'
	jmp return
right_dir:
	ldi temp,'R'
	do_lcd_data 'R'
	jmp return
up_dir:
	ldi temp,'U'
	do_lcd_data 'U'
	jmp return
down_dir:
	ldi temp,'D'
	do_lcd_data 'D'
	jmp return
forward:
	ldi temp,'F'
	do_lcd_data 'F'
	jmp return
backward:
	ldi temp,'B'
	do_lcd_data 'B'
	jmp return
show_hover:
	ldi temp,'H'
	do_lcd_data 'H'
	ret

; displays the duration and distance covered for a succesful flight
flight_end:

	do_lcd_data 'D'
	do_lcd_data 'i'
	do_lcd_data 's'
	do_lcd_data 't'
	do_lcd_data 'a'
	do_lcd_data 'n'
	do_lcd_data 'c'
	do_lcd_data 'e'
	do_lcd_data ':'
	mov temp,distance
	mov temp1,temp
	swap temp
	call to_ascii
	do_lcd_data_use_register temp
	mov temp,temp1
	call to_ascii
	do_lcd_data_use_register temp
	
	;Shift to next line
	ldi temp,lcd_Line_two
	ori temp, 0b10000000             
	do_lcd_command_use_register temp
    
	;call lcd_write_instruction_8f
	do_lcd_data 'D'
	do_lcd_data 'u'
	do_lcd_data 'r'
	do_lcd_data 'a'
	do_lcd_data 't'
	do_lcd_data 'i'
	do_lcd_data 'o'
	do_lcd_data 'n'
	do_lcd_data ':'
	mov temp,count1
	mov temp1,temp
	swap temp
	call to_ascii
	do_lcd_data_use_register temp
	mov temp,temp1
	call to_ascii
	do_lcd_data_use_register temp
	ret



;*************************************************************
;************		For Keypad		**********************

.def row  = r16      ; current row number
.def col  = r17      ; current column number
.def rmask  = r18      ; mask for current row during scan
.def cmask  = r19      ; mask for current column during scan
.def temp1  = r20
.def temp2  = r21
.def value1 = r22
.def value2 = r23
.def count  = r24
.equ PORTLDIR   = 0xF0    ; PL7-4: output, PL3-0, input
.equ INITCOLMASK  = 0xEF    ; scan from the left_dirmost column,
.equ INITROWMASK  = 0x01    ; scan from the top row
.equ ROWMASK   = 0x0F    ; for obtaining input from Port L
.equ loop_count  = 0xFF


key_init:
	ldi key_value,$F0		;Make Cols as i/p
	sts DDRL, key_value	;and Rows as o/p
	ldi key_value,$0F		;Enable pullups
	sts keyport, key_value	;on columns
	ret
 
get_key:
	ldi  cmask,INITCOLMASK    ; initialize ColMask to initial state
    clr  col           ; col = 0


colloop:
  	cpi  col,4         ; if col=4, jump to main
    breq done
  
    sts  PORTL,cmask       ; load mask to PORTL 
  
    ldi  temp1,0xFF       ; load temp1 for delay process
delay: 
	dec  temp1         		; delay for sometime ?????????????????????????????
    brne delay
  	lds  temp1,PINL       ; get input from pin L
    andi temp1,ROWMASK      ; AND with RowMask 
    cpi  temp1,0xF        ; compare ?
    breq nextcol      
  
  	ldi  rmask,INITROWMASK    ; initialize row with initial rowmask
    clr  row       ;row = 0

rowloop:
  	cpi  row,4      ;till row=3
    breq nextcol
    mov  temp2,temp1     ;move value to temp2 for comparison
    and  temp2,rmask     ;AND with rmask
    breq done      ;if found, convert the value
    inc  row       ;else increment row
    lsl  rmask      ;left_direction shift row mask
    jmp  rowloop      

nextcol:
	lsl  cmask
    inc  col
    jmp  colloop
  
done :
	ret




;;------------------ LCD DATA & INSTRUCTION PROCEDURES ------------------- START

.macro lcd_set
	sbi LCD_CTRL_PORT, @0
.endmacro
.macro lcd_clr
	cbi LCD_CTRL_PORT, @0
.endmacro


;
; Send a command to the LCD (r16)
;

lcd_command:
	STORE LCD_DATA_PORT, r16
	rcall sleep_1ms
	lcd_set LCD_E
	rcall sleep_1ms
	lcd_clr LCD_E
	rcall sleep_1ms
	ret

lcd_data:
	STORE LCD_DATA_PORT, r16
	lcd_set LCD_RS
	rcall sleep_1ms
	lcd_set LCD_E
	rcall sleep_1ms
	lcd_clr LCD_E
	rcall sleep_1ms
	lcd_clr LCD_RS
	ret


lcd_wait:
	push r16
	clr r16
	STORE LCD_DATA_DDR, r16
	STORE LCD_DATA_PORT, r16
	lcd_set LCD_RW

lcd_wait_loop:
	rcall sleep_1ms
	lcd_set LCD_E
	rcall sleep_1ms
	LOAD r16, LCD_DATA_PIN
	lcd_clr LCD_E
	sbrc r16, 7
	rjmp lcd_wait_loop
	lcd_clr LCD_RW
	ser r16
	STORE LCD_DATA_DDR, r16
	pop r16
	ret

;;------------------ LCD DATA & INSTRUCTION PROCEDURES ------------------- END


;;;;------------------- TIME DELAY PROCEDURES ------------------------------ START

.equ F_CPU = 16000000
.equ DELAY_1MS = F_CPU / 4 / 1000 - 4

; 4 cycles per iteration - setup/call-return overhead
sleep_1ms:
	push r24
	push r25
	ldi r25, high(DELAY_1MS)
	ldi r24, low(DELAY_1MS)
delayloop_1ms:
	sbiw r25:r24, 1
	brne delayloop_1ms
	pop r25
	pop r24
	ret
sleep_5ms:
	rcall sleep_1ms
	rcall sleep_1ms
	rcall sleep_1ms
	rcall sleep_1ms
	rcall sleep_1ms
	ret

