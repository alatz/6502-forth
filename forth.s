#include <stdio.s>

        lda  #<isr
        sta  $FFFE
        lda  #>isr
        sta  $FFFF
        cli

;;;;;;; temp ;;;;;;;;;;;;;;;;;;;
        lda  #1
        sta  $FF02       ; set vterm mode

        ; lda  #$30
        ; sta  $FB00
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

; NOTE... MAP ;;;;;;;;;;;;;;;;;

; /* testing... : are colons allowed inside earmuffs? YES!  */
; // testing... : are colons allowed after slashes? YES!

; NAME... working register,   INITVAL... #$00,
;   VARIABLE... w,  ADDRESS... $0021 - $0022

; NAME... interpreter pointer, INITVAL... #$02,
;   VARIABLE... ip,  ADDRESS... $0023 - $0024

; NAME... buffer offest, INITVAL... #$00,
;   VARIABLE... bufoffset, ADDRESS... $3300

; NAME... return stack pointer, INITVAL... #$00,
;   VARIABLE... rsp, ADDRESS... $0025 - $0026
;   POINTS-TO... $fb, NOTE... increments move down

; NAME... paramater stack pointer, INITVAL... #$00,
;   VARIABLE... psp, ADDRESS... $0027 - $0028, 
;   POINTS-TO... $bb, NOTE... increments move down

; ;;;;;;;;;;;;;;;;;;;;;

; INDEXED INDIRECT (adds value before dereferencing)
; only works with the x register
; (addr, val) === get value of address (addr + val)
; e.g. ($10, 5) ==> if $0015/16 contains $1000, $1000 contains $25
; ==> use the values at $0015/16 as an address (little endian)
; steps $10 + 5 ==> $15 ... ($15) ==> $1000 ... $1000 ==> $25

; INDIRECT INDEXED (adds value after dereferencing)
; only works with the y register
; (addr), val === get value of (address) + val
; e.g. ($10),5 ==> if $0010/11 contains $1000, $1005 contains $50 
; ==> uses value at location $1005, $50 in this case
; steps ($10) ==> $1000 ... $1000 + 5 ==> $1005 ... $1005 ==> $50

; INDIRECT ABSOLUTE (only JMP uses this)
; jumps to the address composed of the values at the input address
; uses low/high byte order (the address in parens is the low byte)
; e.g. jmp ($1005) when $1005 contains $03 and $1006 contains $16
; jumps to address $1603 (dwheeler.com/6502/oneelkruns/asm1step.html)

; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;


; ; macros ;;;;;;;;;;;;;;;;;;;; 

; code field address is always 7 bytes away from the ; the start 
; of a dictionay entry. use with a label e.g. square CFA == square + 7
; maybe change in the future for dynamically size dictionary entries

; note ... it must be EXACTLY like this to work (including spacing))
; #define sq(x) \
; // lda #$00 + x : \
; ldx #$02 + x
; #define square(x) ((x) * (x))


#define push_psp \
        sec          : \
        lda  psp     : \
        sbc  #2    : \
        sta  psp     : \
        ldy  #0    : \
        lda  xr,y    : \
        sta  (psp),y : \
        ldy  #1    : \
        lda  xr,y    : \
        sta  (psp),y


#define pop_psp \
        ldy  #0    : \
        lda  (psp),y : \
        sta  w,y     : \
        ldy  #1    : \
        lda  (psp),y : \
        sta  w,y     : \
        clc          : \
        lda  psp     : \
        adc  #2    : \
        sta  psp


#define push_rsp \
        sec          : \
        lda  rsp     : \
        sbc  #2      : \
        sta  rsp     : \
        ldy  #0      : \
        lda  xr      : \
        sta  (rsp),y : \
        lda  xr + 1  : \
        ldy  #1      : \
        sta  (rsp),y  


#define pop_rsp \
        ldy  #0      : \
        lda  (rsp),y : \
        sta  w       : \
        ldy  #1      : \
        lda  (rsp),y : \
        sta  w + 1   : \
        clc          : \
        lda  rsp     : \
        adc  #2      : \
        sta  rsp     : \
        lda  rsp + 1 : \
        adc  #0      : \
        sta  rsp + 1

#define xr0(x) lda x : sta xr
#define xr1(x) lda x : sta xr+1
; increment lower byte then upper byte
; decrement lower byte then upper byte

; ; end macros ;;;;;;;;;;;;;;;;;;;;;;;;;;;;

setup:
        bufaddr = $4000
        msgaddr = $4260

        ; init
        ; $0000  <- working register ($0021 & $0022)
        ; $0002  <- interpreter pointer ($0023 & $0024)

        w = $0021       ; working registers (jf eax)
        lda  #$6c
        sta  $20
        lda  #$00
        sta  $21
        lda  #$00
        sta  $22

        ip = $0023      ; interpreter pointer (jf esi)
        lda  #$02
        sta  $23
        lda  #$00
        sta  $24

        ;$25-$26 ==> $00fb => $address_in_the_ip_at_push_time

        rsp = $0025      ; return stack pointer (jf ebp)
        lda  #$fb    ; stack location
        sta  $25     
        lda  #$00    ; stack location 
        sta  $26

        psp = $0027      ; top of stack pointer
        lda  #$bb    ; paramater stack location 
        sta  $27     
        lda  #$00    ; stack location 
        sta  $28

        xr = $0029      ; X register, makes 16bit stuff easier
        lda  #$00    
        sta  $29     
        lda  #$00    
        sta  $2a

        up = $002b     
        lda  #$00    
        sta  $002b     
        lda  #$00    
        sta  $002c

        b = $002d       ; for BASE
        lda  #10
        sta  $002d
        lda  #0      ; todo... why is this 16-bit?
        sta  $002e

        h = $0030       ; for HLD
        lda  #0
        sta  $0030
        sta  $0031

        bufoffset = $4300
        lda     #$00
        sta     bufoffset

        ibp = $4302    ; input buffer pointer current position (jf currkey)
        lda  #0
        sta  ibp

        ibm = $4303    ; input buffer pointer current position (jf currkey)
        lda  #$96   ; input buffer max eq 150
        sta  ibm

        n = $4304     ; extra counter
        lda  #0    
        sta  n

        cp = $0041      ; aka here, aka dp, aka dictionary pointer
        lda  #$41
        sta  cp + 1
        lda  #$00
        sta  cp

        MODE = $0043        ; compile mode param, 1 for yes, 0 for no
        lda  #0
        sta  MODE

        tokentemp = $0046   ; compile mode param, 1 for yes, 0 for no
        lda  #0
        sta  tokentemp

        entry = $0048       ; latest defined dictionrary link 
        lda  #>l097
        sta  entry + 1
        lda  #<l097
        sta  entry

        pad = $0050       ; todo.. change. this is temporary
        lda  #$44
        sta  pad + 1
        lda  #$00
        sta  pad 

        cursor = $0062
        lda  #$FB
        sta  cursor + 1
        lda  #$00
        sta  cursor

        rpad = $5500
        lda  #0
        sta  rpad
        sta  rpad + 1


loop:
        wai
        jmp  loop


isr:                    ; isr == inturrupt service routine
        pha             ; push ac to stack


main:
        ldx  #$0A        ; <ENTR>/return
        cpx  getc
        beq  buf_parse
        ldx  #$7f        ; #$7f eq delete key
        cpx  getc
        beq  backspace
        ; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
        ldx  #$53        ; uppercase 'S'
        cpx  getc
        beq  cs_jump
        ; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
        jsr  buf_add  
        jmp  default


cs_jump:
    jmp cold_start


buf_add:
        ldx  bufoffset
        lda  getc
        sta  bufaddr,x
        inc  bufoffset
        rts

backspace: 
        ldy  #0
        lda  #$20            ; clear cursor
        sta  (cursor),y
        dec  bufoffset       ; decrement buffer pointer
        jsr  dec_cursor      ; decrement curser pointer    
        lda  #$5f
        sta  (cursor),y      ; output cursor at new position
        jmp  blank

buf_clear:
        ldx  #0
        stx  bufoffset
        ldx  #0
        stx  ibp
        rts


buf_parse:
        ; jsr     dec_cursor
        ; dec     bufoffset
        ; ldx     bufoffset

        lda     #$20
        ldy     #0
        sta     (cursor),y
        ; sta     bufaddr,x

        ldx     bufoffset       ; add space
        lda     #$20
        sta     bufaddr,x
        inc     bufoffset

        jsr     buf_add         ; add return char

        ; temp. todo... cleanup ;;;;;;;;;;;;;;;;;;;;;;;
        ldx     bufoffset       ; add another space
        lda     #$20
        sta     bufaddr,x
        inc     bufoffset
        ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

        ; jsr     newline_cursor
        jmp  cs_jump
        jmp  blank

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


; www.bradrodriguez.com/papers/moving1.htm
; (IP) -> W  fetch memory pointed by IP into "W" register
          ; ...W now holds address of the Code Field
; IP+2 -> IP advance IP, just like a program counter
          ; (assuming 2-byte addresses in the thread)
; (W) ->  X  fetch memory pointed by W into "X" register
          ; ...X now holds address of the machine code 
; JP (X)     jump to the address in the X register

next:
        ldy  #1
        lda  (ip),y
        sta  w + 1
        dey             
                        

        lda  (ip),y
        sta  w + 0

        clc              ; see (www.dwheeler.com/6502/oneelkruns/asm1step.html)
        lda  ip       ; load the ip + 0
        adc  #2       ; add 2 to the ip + 0
        sta  ip       ; store the ip + 0 (1st half)
        lda  ip + 1   ; load the ip + 1
        adc  #0       ; add 0 to carry if necessary 
        sta  ip + 1   ; store the ip + 1 (2nd half)
        jmp  w - 1


l096:   .word l095
        .byt $00,$00,$05,"ent"
enter:  sec             
        lda  rsp     
        sbc  #2    
        sta  rsp     
        lda  rsp + 1
        sbc  #0
        sta  rsp + 1
        ldy  #1    
        lda  ip + 1  
        sta  (rsp),y  
        ldy  #0      ; todo ... dey
        lda  ip      
        sta  (rsp),y 
        clc     
        lda  w       ; load w
        adc  #2      ; add 2 to the w
        sta  ip      ; store the w (1st half) as the ip
        lda  w + 1   ; load w + 1
        adc  #0      ; add 0 to carry if necessary 
        sta  ip + 1  ; store the ip + 1 (2nd half)
        jmp  next


l097:   .word l096
        .byt $00,$00,$04,"exi"
exit:  .word  *+2    ; current address + 2 bytes ie next addr
        ldy  #$00    
        lda  (rsp),y 
        sta  ip      
        ldy  #$01    
        lda  (rsp),y 
        sta  ip + 1  
        lda  rsp    
        clc
        adc  #$02    
        sta  rsp
        lda  rsp + 1
        adc  #$00    
        sta  rsp + 1
        jmp  next


msg_success:
        ldx     #0
success_loop
        ; lda bufoffset
        ; inc bufoffset
        lda  ok,x
        sta  msgaddr,x
        inx
        cmp  #0 ; todo.. lda sets zero flag check if needed
        bne  success_loop
        jsr  echo
        rts


ok: .byt $04,"OK ",$00 ; $00 needed if chekcing null end 


echo:   ldx     #1
        ldy msgaddr
        iny
        sty msgaddr

        echoloop
            lda  msgaddr,X
            sta  putc
            inx    
            cpx  msgaddr
            bne  echoloop
        rts

trace_psp:
        ldx  psp + 1
        ldy  psp + 0
        debug
        rts


trace_psp_indirect:
        ldy     #1
        lda     (psp),y
        tax
        dey
        lda     (psp),y
        tay     
        lda     #$44
        debug
        rts


trace_rsp:
        ldx  rsp + 1
        ldy  rsp + 0
        debug
        rts


trace_rsp_indirect:
        ldy  #1
        lda  (rsp),y
        tax
        dey
        lda  (rsp),y
        tay  
        lda  #$45
        debug
        rts


trace_ip_indirect:
        ldy  #1
        lda  (ip),y
        tax
        dey
        lda  (ip),y
        tay     
        lda  #$43
        debug
        rts


trace_up_indirect:
        ldy  #1
        lda  (up),y
        tax
        dey
        lda  (up),y
        tay     
        lda  #$42
        debug
        rts


trace_cp:
        ldx  cp + 1
        ldy  cp + 0
        debug
        rts


trace_cp_indirect:
        ldy  #1
        lda  (cp),y
        tax
        dey
        lda  (cp),y
        tay     
        lda  #$46
        debug
        rts


trace_w:
        ldx  w + 1
        ldy  w + 0
        debug
        rts


trace_w_indirect:
        ldy  #1
        lda  (w),y
        tax
        dey
        lda  (w),y
        tay     
        lda  #$41
        debug
        rts


trace_xr:
        ldx  xr + 1
        ldy  xr + 0
        debug
        rts


trace_ip:
        ldx  ip + 1
        ldy  ip + 0
        debug
        rts


trace_up:
        ldx  up + 1
        ldy  up + 0
        debug
        rts


; todo.. remove
l092:   .word l091
        .byt $00,$00,$01,"q"
qqq:    .word  *+2
        jsr  trace_psp_indirect
        lda  #$99
        debug
        ldx  #0
        stx  bufoffset
        ldx  #0
        stx  ibp
        jmp  quit
    

l095:   .word l094
        .byt $00,$00,$06,"dou"
double: .word  enter
        .word  dup
        .word  plus
        .word  exit


l091:   .word l090
        .byt $00,$00,$06,"dou"
dub:    .word  enter
        .word  dup
        .word  plus
        .word  exit


l084:   .word l083
        .byt $00,$00,$05,"cre"
create: .word *+2

        jsr  _word       ; pushes address, then length, to stack
        ; todo... remove
        pop_psp             ; get the word length from the stack    
        pop_psp             ; probably to clean up the stack, todo... use cp for word buffer

        ; move word to temporary buffer
        ldy  #0      
        lda  (cp),y              ; load length
        tax
        inx                         ; increase length by 1 bc we're starting at 0
        
    word_buffer_loop:
        lda  (cp),y
        sta  (pad),y
        iny
        dex
        cpx  #0
        bne  word_buffer_loop    ; copy cp word to temporary buffer (the pad)
        
        ldy  #1                  ; load link of latest word link
        lda  entry + 1
        sta  (cp),y
        dey
        lda  entry
        sta  (cp),y

        lda  cp + 1              ; update entry to latest dictionary link
        sta  entry + 1
        lda  cp
        sta  entry

        ldy  #2                  ; store smudge/precedence
        lda  #0
        sta  (cp),y       
        ldy  #3
        sta  (cp),y
        
        ldy  #0                  ; get word length from pad
        lda  (pad),y

        ldy  #4                  ; store word length
        sta  (cp),y

        clc                         ; increse cp to next open space
        lda  cp
        adc  #5
        sta  cp
        lda  cp + 1
        adc  #0
        sta  cp + 1

        ; get length if less than 3
        ldy  #0
        lda  (pad),y
        cmp  #3              ; set length to 3 if longer
        bcs  three_or_more   ; if (cp),0 greater than 3, then jump
        lda  (cp),y          ; only runs if word length eq or shorter
        sta  w               ; save length to w
        jmp  less_than_three

    three_or_more:  
        lda  #3              ; to compare length also
        sta  w               ; save length to w

    less_than_three:
        clc                 ; add 1 to pad
        lda  pad
        adc  #1
        sta  pad
        lda  pad + 1
        adc  #0
        sta  pad + 1        ; skip the word length in pad

        lda  w              ; load word length (3 or less)
        tax                 ; set x to length of word pad, w keeps value
        ldy  #0
        create_loop
        lda  (pad),y
        sta  (cp),y   
        iny
        dex
        bne  create_loop

        clc                     ; increse cp to next open space
        lda  cp
        adc  w
        sta  cp
        lda  cp + 1
        adc  #0
        sta  cp + 1

        sec                     ; add length back to pad
        lda  pad
        sbc  #1
        sta  pad
        lda  pad + 1
        sbc  #0
        sta  pad + 1
        jmp  next


l083:   .word l082
        .byt $00,$00,$01,":"
colon:  .word  enter
        .word  create
        .word  latest 
        .word  fetch   ; get the value of latest
        .word  hidden  ; hide the compiling dictionary entry 
        .word  rbrac   ; set the compile mode to 1 STATE
        .word  lit
        .word  enter
        .word  comma   ; comma "," appends a value after the cp, then increments the cp
        .word  exit
        ; ; use find to search the dictionary for the currnet token word?
        ; ; if match, that word is pushed to the stack


l082:   .word l081
        .byt $00,$00,$01,","
comma:  .word *+2   ; pop stack, append it to HERE, increment HERE
        pop_psp
        jsr  _comma
        jmp  next


_comma:  
        ldy  #1      ; todo... probably add pop psp to word
        lda  w + 1
        sta  (cp),y
        dey
        lda  w
        sta  (cp),y
        clc                 ; add two to the cp
        lda  cp
        adc  #2
        sta  cp
        lda  cp + 1
        adc  #0
        sta  cp + 1
        rts


_key:
        ldx  bufoffset
        lda  bufaddr,x
        inc  bufoffset
        rts


_word:
        ; main purpose... called by interpret to get 
        ; the current word address to lookup and its length
        lda  ibp
        sta  xr
        ldx  xr
        inx
        lda  bufaddr,x
        cmp  #$0A
        bne  push_word  ;;;;;;;;;;;;;;;;;;;;;;;

        ; ldx  bufoffset
        ; inx  ; todo...see if i actually need to add 1 here
        ; txa  ; add 1 to the buffer offset
        ; ; debug
        ; cmp  ibp
        ; ; debug
        ; bne  push_word  ;;;;;;;;;;;;;;;;;;;;;;;

    push_empty:        ;;;;;;;;;;;;;;;;;;;;;;;
        lda  #0
        sta  xr + 1
        sta  xr
        push_psp    ; push address to stack (0 in this case)
        push_psp    ; push word length to stack (0 in this case)
        ldy  #0     ; saves the length and address (for empty) to the cp
        lda  #0
        sta  (cp),y
        iny  
        sta  (cp),y
        iny  
        sta  (cp),y 
        rts
    push_word:          ;;;;;;;;;;;;;;;;;;;;;;;;
        ldx  ibp
    word_loop_1:
        lda  bufaddr,x
        cmp  #$20            ; zero flag set if eq. space ie 20
        bne  word_continue_1 ; iterate when zero flag eq 1
        inx                  ; increase ibp until first non-blank char found
        stx  ibp             
        jmp  word_loop_1
    word_continue_1:
        ldy  #0      ; keeps track of word length
        ldx  ibp
    word_loop_2:
        lda  bufaddr,x
        cmp  #$20            ; zero flag set if eq. space ie 20
        beq  word_continue_2 ; iterate when zero flag eq 1
        iny                     ; should leave cp,0 empty
        sta  (cp),y          ; use HERE as the word buffer
        inx
        stx  ibp
        jmp  word_loop_2
    word_continue_2:
        tya
        ldy  #0
        sta  (cp),y          ; save word length

        lda  #>bufaddr
        sta  xr + 1
        lda  #<bufaddr
        sta  xr

        clc                 ; add buffer pointer input buffer start
        lda  xr
        adc  ibp
        sta  xr
        lda  xr + 1
        adc  #0
        sta  xr + 1
        push_psp            ; push word address to the stack
        lda  #0
        sta  xr + 1
        ldy  #0
        lda  (cp),y
        sta  xr
        push_psp            ; push word length to stack 
        rts


l134:   .word $0000	
        .byt $00,$00,$03,"bye"           
bye:    .word  *+2 
        ext                 ; // x6502 specific command
        jmp  next


l133:   .word  l134
        .byt  $00,$00,$07,"CON"           
convert:
        .word  enter         ; aka (number)
        .word  dbg
        .word  exit


l132:   .word l133
        .byt $00,$00,$04,"0->"           ; ( d1 d2  -- d3 )
zerotoone: 
        .word  enter		; todo... test
        .word  dup
        .word  zequal
        .word  zbranch
        .word  6
        .word  drop
        .word  one
        .word  exit


l131:   .word l132
        .byt $00,$00,$02,"D-"           ; ( d1 d2  -- d3 )
dminus: .word  enter		; todo... test
        .word  dnegate
        .word  dplus
        .word  exit


_dplus:
        lda  #0
        sta  rpad + 0
        sta  rpad + 1
        sta  rpad + 2
        sta  rpad + 3
        sta  rpad + 4
        sta  rpad + 5
        sta  rpad + 6
        sta  rpad + 7

        pop_psp
        lda  w + 0
        sta  rpad + 6
        lda  w + 1
        sta  rpad + 7

        pop_psp
        lda  w + 0
        sta  rpad + 4
        lda  w + 1
        sta  rpad + 5

        pop_psp
        lda  w + 0
        sta  rpad + 2
        lda  w + 1
        sta  rpad + 3

        pop_psp
        lda  w + 0
        sta  rpad + 0
        lda  w + 1
        sta  rpad + 1

        clc
        lda  rpad + 2
        adc  rpad + 6
        sta  rpad + 6
        lda  rpad + 3
        adc  rpad + 7
        sta  rpad + 7

        lda  rpad + 0
        adc  rpad + 4
        sta  rpad + 4
        lda  rpad + 1
        adc  rpad + 5
        sta  rpad + 5

        lda  rpad + 4
        sta  xr
        lda  rpad + 5
        sta  xr + 1
        push_psp

        lda  rpad + 6
        sta  xr
        lda  rpad + 7
        sta  xr + 1
        push_psp
        rts


l130:   .word l131
        .byt $00,$00,$02,"D+"           ; ( d1 d2  -- d3 )
dplus:  .word *+2		; todo... cleanup
        jsr  _dplus
        jmp  next


l129:   .word l130
        .byt $00,$00,$02,"M*"  ; ( n1 n2  -- d3 ) all signed
mstar:  .word  enter		; todo... test
		.word  over
		.word  over
		.word  xor
		.word  tor
		.word  abs
		.word  swap
		.word  abs
		.word  ustar
		.word  fromr
		.word  dpm
		.word  exit


_ustar:
        lda #0
        sta  rpad
        sta  rpad + 1
        sta  rpad + 2
        sta  rpad + 3
        sta  rpad + 4    ; scratch
        sta  rpad + 5    ; scratch

        pop_psp
        lda  w
        sta  rpad + 4
        lda  w + 1
        sta  rpad + 5

        pop_psp
        lda  w
        sta  rpad + 0
        lda  w + 1
        sta  rpad + 1

		; //  see (forum.6502.org/viewtopic.php?t=447)

		ldy  #16  ; We'll loop 16 times.
		nxtbit: 
			asl  rpad + 2 ; Shift the entire 32 bits over one bit position.
			rol  rpad + 3
			rol  rpad + 0
			rol  rpad + 1
			bcc  mloop    	; Skip the adding-in to the result if
			clc        		; Else, add multiplier to intermediate result.
			lda  rpad + 4
			adc  rpad + 2
			sta  rpad + 2
			lda  rpad + 5
			adc	 rpad + 3
			sta	 rpad + 3
			lda	 #0			; If C eq 1, incr lo byte of hi cell
			adc  rpad + 0
			sta	 rpad + 0
		mloop:
			dey
			bne  nxtbit   ; loop again if not 16 iterations

		lda	 rpad + 2
		sta	 xr
		lda	 rpad + 3
		sta  xr + 1
		push_psp

		lda	 rpad
		sta	 xr
		lda	 rpad + 1
		sta  xr + 1
		push_psp
        rts

; 16-bit 16-bit -- 32 bit
l128:   .word l129
        .byt $00,$00,$02,"U*"  ; ( n1 n2  -- d3 ) all unsigned
ustar:  .word   *+2
        jsr  _ustar
        jmp  next


l127:   .word l128
        .byt $00,$00,$02,"U."  ; ( unsigned -- )
udot:   .word  enter
        .word  zero
        .word  ddot
        .word  exit


; print num justified right withing n chars
; 555 5 .R -->  '  555'
l126:   .word l127
        .byt $00,$00,$02,".R"  ; ( num chars -- )
dotr:   .word  enter
        .word  tor
        .word  stod
        .word  fromr
        .word  ddotr
        .word  exit


l125:   .word l126
        .byt $00,$00,$02,"D." ; ( d -- )
ddot:   .word  enter
        .word  zero
        .word  ddotr
        .word  space
        .word  exit

l124:   .word l125
        .byt $00,$00,$01,"I" ; ( -- n )
index:  .word  *+2   
        ldy  #2
        lda  (rsp),y
        sta  xr
        iny
        lda  (rsp),y
        sta  xr + 1
        push_psp
        jmp  next

l123:   .word l124
        .byt $00,$00,$02,"0>" ; ( addr n -- )
zgreater:  
        .word  enter   ; todo... cleanup
        .word  dup
        .word  zequal
        .word  swap
        .word  zless
        .word  orl
        .word  not
        .word  exit


; todo... remove
l122:   .word l123
        .byt $00,$00,$05,"FTE" ; ( addr n -- )
temp2:  .word  enter
        .word  exit

l121:   .word l122
        .byt $00,$00,$09,"TEM" ; ( addr n -- )
temp:   .word  *+2
        lda  #$49
        ldx  cursor + 1
        ldy  cursor
        debug
        jmp  next


; // $4000 + $fffc == $fffc + $4000 == ffff - 4 + 4000
; add four to fffc then add 
l120:   .word l121     ; starting from addr, output n chars 
        .byt $00,$00,$04,"TYP" ; ( addr n -- )
type:   .word  enter
        .word  dup
        .word  zgreater
        .word  zbranch     ; 3000 4
        .word  $20         ; branch to end and drop
        .word  over
        .word  plus
        .word  swap        ; - 3004 3000
        .word  tor
        .word  tor
        .word  index
        .word  fetchbyte
        .word  emit
        .word  fromr
        .word  fromr
        .word  plus1
        .word  twodup
        .word  equal
        .word  zbranch     ; 3000 4
        .word  $ffea       ; aka -22, aka $ffea + current addr
        .word  twodrop 
        .word  exit


; d is converted by base, then output with n leading spaces
l119:   .word l120    
        .byt $00,$00,$03,"D.R" ; ( d n -- )
ddotr:  .word  enter
        .word  tor
        .word  swap
        .word  over
        .word  dabs
        .word  ltsharp
        .word  sharps
        .word  sign
        .word  sharpgt
        .word  fromr
        .word  over
        .word  sub
        .word  spaces
        .word  type
        .word  exit


l118:   .word l119     ; add n spaces to the output
        .byt $00,$00,$06,"spa" ; ( n -- )
spaces: .word  enter         ; todo... cleanup
        .word  zerotoone
        .word  dup         ; temp max doesn't work right
        .word  zless
        .word  zbranch     ; temp max doesn't work right
        .word  6
        .word  drop        ; temp max doesn't work right
        ; .word   max       
        .word  one        
        .word  space       ; n -> n-1 ...
        .word  one         ; n 1 -->  n-1 1 ...
        .word  sub         ; n-1 -->  n-2 ...
        .word  dup
        .word  zequal
        .word  zbranch  ; n-1 -->  n-2 ...
        .word  $fff4
        .word  drop
        .word  exit        


l117:   .word l118
        .byt $00,$00,$04,"SIG" ; ( n1 n2 n3 -- )
sign:   .word  enter         
        .word  rot
        .word  zless
        .word  zbranch
        .word  7
        .word  blit
        .byte  $2d
        .word  hold
        .word  exit


l116:   .word l117
        .byt $00,$00,$02,"0=" ; ( n -- flag )
zequal: .word  enter         
        .word  zero
        .word  equal
        .word  exit


l115:   .word l116
        .byt $00,$00,$02,"<#" ; ( d1 -- d2 ) start numeric output
ltsharp:
        .word  enter         
        .word  padaddr
        .word  hld
        .word  store
        .word  exit


l114:   .word l115
        .byt $00,$00,$02,"#>" ; ( ud -- addr n ) end numeric output
sharpgt:   
        .word  enter
        .word  drop
        .word  drop
        .word  hld
        .word  fetch
        .word  padaddr
        ; .word   fetch
        .word  over
        .word  sub
        .word  exit


l113:   .word l114
        .byt $00,$00,$03,"PAD"
padaddr: .word *+2
        lda  #>rpad + 1
        sta  xr + 1
        lda  #<rpad
        sta  xr
        push_psp
        jmp  next


l112:   .word l113
        .byt $00,$00,$03,"HLD"
hld:    .word  *+2
        lda  #>h
        sta  xr+1
        lda  #<h
        sta  xr
        push_psp
        jmp  next


l111:   .word l112
        .byt $00,$00,$04,"HOL"  ; ( char -- ) only used inside <# #>
hold:   .word  enter         
        .word  lit
        .word  $FFFF        ; -1 
        .word  hld
        .word  addstore     ; add -1 to value at hld
        .word  hld
        .word  fetch
        .word  storebyte    ; value of hld is pad address + offset
        .word  exit


l110:   .word l111
        .byt $00,$00,$02,"-!"  ; ( num addr -- )
substore: 
        .word  enter
        .word  tor
        .word  rfetch
        .word  fetch
        .word  swap
        .word  sub
        .word  fromr
        .word  store
        .word  exit


l109:   .word l110
        .byt $00,$00,$02,"+!" ; ( num addr -- )
addstore: 
        .word  enter         
        .word  tor
        .word  rfetch
        .word  fetch
        .word  plus
        .word  fromr
        .word  store
        .word  exit


; converts one digit from an unsigned double to ascii
; and adds it to the output string buffer 
; e.g. 63 0 <# # #> TYPE
l108:   .word l109
        .byt $00,$00,$01,"#"  ; ( ud1 -- ud2 ) ; ud eq unsigned decimal
sharp:  .word  enter         
        .word  base    
        .word  fetch
        .word  mslashmod
        .word  rot
        .word  blit
        .byte  9
        .word  over
        .word  lt
        .word  zbranch
        .word  7
        .word  blit
        .byte  7
        .word  plus
        .word  blit
        .byte  $30
        .word  plus    
        .word  hold
        .word  exit


; between <# and #> converts all  unsigned 
; double number's digits to ascii chars 
; and move to output buffer. 
; alsways returns at least one digit (0)
l107:   .word l108
        .byt $00,$00,$02,"#S" ; ( ud -- 0 0 )
sharps: .word  enter  
b1071:  .word  sharp
        .word  over
        .word  over
        .word  orl
        .word  zequal
        .word  zerobranch
        .word  b1071   ; todo.. test
        .word  exit


l106:   .word l107
        .byt $00,$00,$04,"BLI" ; aka c-lit
blit:   .word *+2       ; push next byte to stack
        ldy  #0          ; store ip val in xr
        lda  (ip),y
        sta  xr
        lda  #0
        sta  xr + 1
        push_psp        ; push the ip val
        clc             ; increase the ip by 1
        lda  ip
        adc  #1
        sta  ip
        lda  ip+1
        adc  #0
        sta  ip+1
        jmp  next


; divisor is not a double, but the dividend is    
; note... this does not accept unsigned values, despite 'm' in the name
l105:   .word l106
        .byt $00,$00,$05,"M/M" ; ( und1 un2 -- unsigned-rem double-quo )  
mslashmod:      
        .word  enter
        .word  tor
        .word  zero
        .word  rfetch
        .word  uslashmod
        .word  fromr
        .word  swap
        .word  tor
        .word  uslashmod
        .word  fromr
        .word  exit


l104:   .word l105
        .byt $00,$00,$03,"MOD" ; ( n1 n2 -- rem ) e.g. n1/n2
mod:    .word  enter
        .word  slmod
        .word  drop
        .word  exit


l103:   .word l104
        .byt $00,$00,$02,"M/" ; ( d1 n2 -- rem quo ) e.g. n1/n2
mslash: .word  enter
        .word  over
        .word  tor
        .word  tor
        .word  dup
        .word  dpm
        .word  rfetch
        .word  abs
        .word  uslashmod ; U/ aka U/MOD aka UM/MOD
        .word  fromr
        .word  rfetch
        .word  xor
        .word  pm
        .word  swap
        .word  fromr
        .word  pm
        .word  swap
        .word  exit


l102:   .word l103
        .byt $00,$00,$04,"DAB"
dabs:   .word  enter
        .word  dup
        .word  dpm
        .word  exit


l101:   .word l102
        .byt $00,$00,$03,"ABS"
abs:    .word  enter
        .word  dup
        .word  pm
        .word  exit


l100:   .word l101
        .byt $00,$00,$02,"0<"
zless:  .word  *+2         ; todo... cleanup. test (n -- n)
        ; // (atariarchives.org/alp/appendix_1.php)
        ; for a byte, this also works with 16-bits by using the high byte
        ; 0 to 127 (#$7f) --> #0 to #128 
        ; 128 (#$80) to 255 (#$ff) --> -#128 to -#1
        ; 127 eq 01111111
        ; 128 eq 10000000
        ; 129 eq 10000001
        pop_psp
        ldy  #0
        asl  w + 1   ; set carray bit, by shifting hi byte 
        tya         ; load acc with 0 (set in y)
        rol         ; rotates carry bit onto the end of the acc (which is 0)
        sty  w + 1   ; store 0 in high btye
        sta  w       ; store acc in low byte
        lda  w
        sta  xr
        lda  w + 1
        sta  xr + 1
        push_psp
        ; asl 1,x  ;arithmatic shift left top high byte top of stack, most sig bit pushed to carry
        ; tya     ; load 0 into acc
        ; rol a   
        ; sty 1,x 
        ; sta 0,x 
        ; ----
        ; i think ASL sets the carry flag (or not), 
        ; we load 0 into the acc,
        ; ROL shifts that carry flag onto the end of the acc,
        ; store 0 at the high byte of stack top
        jmp  next

l000:   .word l100
        .byt $00,$00,$06,"NEG"
negate: .word   *+2  ; ( n -- -n ) leaves two complement of a number on stack      
        pop_psp
        sec
        lda  #0
        sbc  w
        sta  w
        lda  #0
        sbc  w + 1
        sta  w + 1
        lda  w
        sta  xr
        lda  w + 1
        sta  xr + 1
        push_psp
        jmp  next


        ; apply the sign of the stacktop - 1 num to sign of the stacktop num
        ; - - eq positive, + - eq negative, + + positive
l001:   .word l000      
        .byt $00,$00,$02,"+-" ; ( n1 n2 -- n3 )
pm:     .word  enter
        .word  zless
        .word  zbranch
        .word  4
        .word  negate
        .word  exit


        ; see pm (+-) above, this is that for doubles
l002:   .word l001
        .byt $00,$00,$03,"D+-"
dpm:    .word  enter  ; todo... test
        .word  zless
        .word  zbranch
        .word  4
        .word  dnegate
        .word  exit


l003:   .word l002      ; ( dnum -- -dnum )
        .byt $00,$00,$07,"DNE" ; DNEGATE aka DMINUS
dnegate:
        .word   *+2         ; todo... cleanup. change in place
        pop_psp
        lda  w + 1
        sta  up + 1
        lda  w
        sta  up 
        pop_psp
        sec
        lda  #0
        sbc  w
        sta  w
        lda  #0
        sbc  w + 1
        sta  w + 1
        lda  #0
        sbc  up
        sta  up
        lda  #0
        sbc  up + 1
        sta  up + 1
        lda  w
        sta  xr
        lda  w + 1
        sta  xr + 1
        push_psp
        lda  up
        sta  xr
        lda  up + 1
        sta  xr + 1
        push_psp
        jmp  next


; double numbers have the higher order cell on top
; TO STORE 0000 0003... 
; ---------------
; | 0003 | 0000 | <-- TOP OF STACK 
; ---------------
l004:   .word l003
        .byt $00,$00,$04,"s->"  ; transforms single into doubule ( n -- d )
stod:   .word  enter       
        .word  dup
        .word  zless
        .word  negate
        .word  exit


l005:   .word l004
        .byt $00,$00,$04,"BAS"  ; BASE (  -- addr )
base:   .word  *+2
        lda  #>b + 1 
        sta  xr + 1
        lda  #<b
        sta  xr
        push_psp
        jmp  next


l006:   .word l005
        .byt $00,$00,$05,"U/M"  ; U/MOD ( n1 n2 -- rem quo ) e.g. n1/n2
uslashmod:
        .word   *+2
        ;   ; // see ROL in (http://www.atariarchives.org/roots/chapter_9.php)
        ;    pad 1 ... divisor high byte
        ;    pad 0 ... divisor low byte
        ;    pad 3 ... high cell dividend high byte
        ;    pad 2 ... high cell dividend low byte
        ;    pad 4 ... low cell dividend high byte
        ;    pad 5 ... low cell dividend low byte
        ;    pad 6 ... scrath pad byte

        ; // 1000 / 2 = 200
        ; // #$03E8 == 1000

        ; 1st loop
        ; ----- lo cell ----- ----- hi cell -----
        ; 1111 1111 1111 1111 1111 1111 1111 1111
        ; ----- lo cell ----- ----- hi cell -----
        ; 1111 1111 1111 1111 1111 1111 1111 1110

        ; last loop
        ; ----- lo cell ----- ----- hi cell -----
        ; 1111 1111 1111 1111 1111 1111 1111 1110
        ; // modified version of (http://6502.org/source/integers/ummodfix/ummodfix.htm)
        ; u/mod and u/ are the same thing

        lda  #$00       ; setup scratch pad and 
        sta  pad + 6
        lda  #$00       ; setup CARRY byte
        sta  pad + 7
        pop_psp         ; todo... divison on the stack with offsets
        lda  w + 1
        sta  pad + 1
        lda  w
        sta  pad + 0
        pop_psp         ; todo... divison on the stack with offsets
        lda  w + 1
        sta  pad + 3
        lda  w
        sta  pad + 2
        pop_psp         ; todo... divison on the stack with offsets
        lda  w + 1
        sta  pad + 5
        lda  w
        sta  pad + 4    ; end setup --

        sec
        lda  pad + 2     ; load dividend hi cell lo byte
        sbc  pad + 0     ; sub divisor lo byte
        lda  pad + 3     ; load dividend hi cell hi byte
        sbc  pad + 1     ; sub divisor hi byte
        bcs  d_overflow  ; if borrow (grab from left), carry flag set -> break
        ; note... if divisor isn't greater than hi cell dividend, then we can't fit
        ; the result in a single cell (2 bytes), which means our result wouldn't be 16 bit
        ; this is the reason for the overflow check here
        ldx  #$11        ; loop 16 times, ROL pad + 4, ROL pad + 5 runs one extra time
                         ; at the end (why we nedd 17)
    mod_loop:
        rol  pad + 4
        rol  pad + 5
        dex
        beq  mod_end     ; when x eq 0, end routine
        rol  pad + 2     ; shift hi cell dividend left
        rol  pad + 3
        ; shift hi cell dividend left
        lda  #0
        sta  pad + 7     ; store 0 in CARRY
        rol  pad + 7     ; former high bit of hi cell in carry bit, 
        sec
        lda  pad + 2
        sbc  pad + 0
        sta  pad + 6     ; store in scratch byte
        lda  pad + 3
        sbc  pad + 1
        tay                 ; transfer to y the hi cell hi byte - divisor hi byte diff
        lda  pad + 7     ; check if divisor fits in dividend first 17 bits
        sbc  #0
        bcc  mod_loop    ; loop if it doesn't fit
        lda  pad + 6
        sta  pad + 2
        sty  pad + 3     
        jmp  mod_loop
    d_overflow:
        lda  #$ff
        sta  pad + 2
        sta  pad + 3
        sta  pad + 4
        sta  pad + 5
    mod_end:
        lda  pad + 3
        sta  xr + 1
        lda  pad + 2
        sta  xr
        push_psp
        lda  pad + 5
        sta  xr + 1
        lda  pad + 4
        sta  xr
        push_psp
        jmp  next


l007:   .word l006
        .byt $00,$00,$02,"R@"  ; ( n1 n2 -- rem quo ) e.g. n1/n2
rfetch: .word  *+2     ; copy top of return stack to parameter stack
                       
        ldy  #0
        lda  (rsp),y
        sta  xr
        iny
        lda  (rsp),y
        sta  xr + 1
        push_psp
        jmp  next


        ; put mslash in here, it will not get used again
        ; dont worry about negative for now
        ; uses u/mod
l008:   .word l007
        .byt $00,$00,$04,"/MO"  ; ( n1 n2 -- rem quo ) e.g. n1/n2
slmod:  .word  enter
        .word  tor
        .word  stod
        .word  fromr
        .word  mslash
        .word  exit


l009:   .word l008
        .byt $00,$00,$05,"spa" 
space:  .word  enter
        .word  lit
        .word  $20
        .word  emit    ; todo... uncomment
        .word  exit


l010:   .word l009
        .byt $00,$00,$05,"sta" ; ( -- addr )
state:  .word  enter
        .word  lit
        .word  MODE 
        .word  exit


l011:   .word l010
        .byt $00,$00,$04,"pag" ; ( -- ) clears output from the screen
page:   .word  *+2     ; faster version of clear written in assembly
        lda  #$FB    ; store grid 0,0 to w
        sta  w + 1
        lda  #$00
        sta  w
    pageloop:
        ldy  #0
        lda  #$20
        sta  (w),y

        clc     ; increment w by 1
        lda  w
        adc  #1
        sta  w
        lda  w + 1
        adc  #0
        sta  w + 1
        
        lda  w + 1
        cmp  #$FE
        bne  pageloop
        lda  w
        cmp  #$E8
        bne  pageloop

        lda  #$00
        sta  cursor      ; reset cursor pos to 0,0
        lda  #$FB
        sta  cursor + 1

        ldy  #0      
        lda  #$20        ; set cursor to underscore
        sta  (cursor),y

        lda  #3          ; todo... why? word length?
        sta  bufoffset

        jmp  next


l012:   .word l011
        .byt $00,$00,$02,"C@"
fetchbyte:
        .word  *+2
        pop_psp
        ldy  #1
        lda  #0 ; lda     (w),y
        sta  xr + 1
        dey
        lda  (w),y
        sta  xr
        push_psp
        jmp  next


l013:   .word l012
        .byt $00,$00,$02,"C!" ; ( n addr -- )
storebyte:
        .word  *+2
        pop_psp
        lda  w + 1
        sta  xr + 1
        lda  w
        sta  xr
        pop_psp
        ldy  #0
        lda  w
        sta  (xr),y
        jmp  next


l014:   .word l013
        .byt $00,$00,$03,"dbg"
dbg:    .word  *+2     ; debug word
        lda  #$92
        debug
        jsr  trace_psp_indirect
        jmp  next
        

l015:   .word l014
        .byt $00,$00,$05,"cle"
clear:  .word  enter
        .word  docon   ; or lit
        .word  $FB00   ; push 0,0 of vterm screen to stack
b151:   .word  dup     ; branch starting piont, brach bacwards to here
        .word  lit
        .word  $20  ; space char
        .word  swap
        .word  storebyte  ; end store logic
        .word  one
        .word  plus    ; increment by 1
        .word  dup     ; copy TOS
        .word  docon   ; or lit
        .word  $FEE8   ; push last grid position FEE7 + 1
        .word  equal   ; compare to end of grid
        .word  zerobranch
        .word  b151    ; whatever to jump to exit
        .word  drop    ; loop complete. drop address on stack
        .word  lit
        .word  $FB00   ; push cursor address to stack
        .word  lit
        .word  cursor
        .word  store   ; store 0,0 at cursor address
        .word  lit     ; fetch cursor address
        .word  cursor
        .word  fetch
        .word  lit
        .word  $5f     ; push underscore to stack
        .word  swap
        .word  store   ; store underscore at cursor address
        .word   exit


l016:   .word l015 ; todo... implement
        .byt $00,$00,$01,"'"
strend: .word  *+2       
        ; // _word
        ; // _key
        jmp  next


l017:   .word l016 ; todo... implement
        .byt $00,$00,$02,".'"
strstart:
        .word  *+2
        jmp  next


l018:   .word l017 ; todo... implement
        .byt $00,$00,$02,".s"
outputstack:
        .word  *+2
        jmp  next


l019:   .word l018
        .byt $01,$00,$01,"("
comment:                ; ignores all words inside ( ... )
        .word  *+2       ; todo... cleanup
    comment_loop:
        jsr  _word
        pop_psp         ; todo... cleanup
        pop_psp         ; todo... cleanup
        ldy  #1      ; first char, just after length
        lda  (cp),y
        cmp  #$29    ; ascii hex for ')'
        bne  comment_loop
        jmp  next
            

l020:   .word l019
        .byt $00,$00,$03,"XOR"
xor:    .word  *+2       ; todo... cleanup
        pop_psp
        lda  w       ; load the first value
        sta  xr      ; store popped value to xr
        pop_psp         ; pop updates w, why we move it to xr
        lda  xr
        eor  w       ; apply XOR to popped value
        sta  xr
        lda  #0      ; reset xr
        sta  xr + 1  ; store new XORed value
        push_psp
        jmp  next


l021:   .word l020
        .byt $00,$00,$02,"OR"
orl:    .word *+2   ; todo... rename, cleanup
        pop_psp
        lda  w       ; load the first value
        sta  xr      ; store popped value to xr
        pop_psp      ; pop updates w, why we move it to xr
        lda  xr
        ora  w       ; apply OR to popped value
        sta  xr
        lda  #0      ; reset xr
        sta  xr + 1  ; store new ORed value
        push_psp
        jmp  next


; see // http://www.atariarchives.org/roots/chapter_9.php
l022:   .word l021
        .byt $00,$00,$03,"AND"
andl:   .word  *+2   ; todo... rename, cleanup
        pop_psp
        lda  w       ; load the first value
        sta  xr      ; store popped value to xr
        pop_psp         ; pop updates w, why we move it to xr
        lda  xr
        and  w       ; apply AND to popped value
        sta  xr
        lda  #0      ; reset xr
        sta  xr + 1  ; store new ANDed value
        push_psp
        jmp  next


l023:   .word l022
        .byt $00,$00,$03,"MAX"
max:    .word  *+2       ;todo...cleanup
        pop_psp     
        lda  w
        sta  up
        lda  w + 1
        sta  up + 1
        pop_psp
        lda  w + 1
        cmp  up + 1
        bcc  max_low
        bne  max_high
        lda  w
        cmp  up
        bcc  max_low
    max_high:
        lda  w + 1
        sta  xr + 1
        lda  w
        sta  xr
        push_psp
        jmp  next
    max_low:
        lda  up + 1
        sta  xr + 1
        lda  up
        sta  xr
        push_psp
        jmp  next


l024:   .word l023
        .byt $00,$00,$03,"MIN"
min:    .word *+2       ;todo...cleanup
        pop_psp     
        lda  w
        sta  up
        lda  w + 1
        sta  up + 1
        pop_psp
        lda  w + 1
        cmp  up + 1
        bcc  min_low
        bne  min_high
        lda  w
        cmp  up
        bcc  min_low
    min_high:
        lda  up + 1
        sta  xr + 1
        lda  up
        sta  xr
        push_psp
        jmp  next
    min_low:
        lda  w + 1
        sta  xr + 1
        lda  w
        sta  xr
        push_psp
        jmp  next


l025:   .word l024
        .byt $00,$00,$01,"<"
lt:     .word  *+2   ; ( n1 n2 -- bool-flag ) 
        pop_psp     ;todo...cleanup 
        lda  w
        sta  up
        lda  w + 1
        sta  up + 1
        pop_psp     
        lda  w + 1
        cmp  up + 1
        bcc  lt_less_than
        bne  lt_greater_than_eqaul
        lda  w
        cmp  up
        bcc  lt_less_than
    lt_greater_than_eqaul:
        lda  #0
        sta  xr
        sta  xr + 1
        push_psp
        jmp  next
    lt_less_than:
        lda  #0
        sta  xr + 1
        lda  #1
        sta  xr
        push_psp
        jmp  next

        ; pop_psp
        ; lda     w
        ; sta     up
        ; lda     w + 1
        ; sta     up + 1
        ; pop_psp     
        ; sec
        ; lda w
        ; sbc up
        ; lda w + 1
        ; sbc up + 1
        ; ; lda #0
        ; ; sta up + 1
        ; bvc lt_jmp
        ; eor #$80
        ; lda #0
        ; sta xr
        ; sta xr + 1
        ; push_psp
        ; jmp next
; lt_jmp:
        ; lda #0
        ; sta xr + 1
        ; lda #1
        ; sta xr
        ; push_psp
        ; jmp next



l026:   .word l025
        .byt $00,$00,$01,">"
gt:     .word  *+2   ; ( n1 n2 -- bool-flag ) 
        pop_psp  ;todo...cleanup 
        lda  w
        sta  up
        lda  w + 1
        sta  up + 1
        pop_psp     
        lda  up + 1
        cmp  w + 1
        bcc  gt_less_than
        bne  gt_greater_than_eqaul
        lda  up
        cmp  w
        bcc  gt_less_than
    gt_greater_than_eqaul:
        lda  #0
        sta  xr
        sta  xr + 1
        push_psp
        jmp  next
    gt_less_than:
        lda  #0
        sta  xr + 1
        lda  #1
        sta  xr
        push_psp
        jmp  next


l027:   .word l026
        .byt $00,$00,$02,">="
ge:     .word  *+2   ; ( n1 n2 -- bool-flag ) 
        pop_psp     ;todo...cleanup 
        lda  w
        sta  up
        lda  w + 1
        sta  up + 1
        pop_psp     
        lda  w + 1
        cmp  up + 1
        bcc  ge_less_than
        lda  w
        cmp  up
        bcc  ge_less_than
    ge_greater_than_or_equal:
        lda  #0
        sta  xr + 1
        lda  #1
        sta  xr
        push_psp
        jmp  next
    ge_less_than:
        lda  #0
        sta  xr
        sta  xr + 1
        push_psp
        jmp  next


l028:   .word l027
        .byt $00,$00,$02,"<="
le:     .word  *+2   ; ( n1 n2 -- bool-flag ) 
        pop_psp     ; e.g. ( 1 2 <= -- -1 ) ( 2 1 <= -- 0 )
        lda  w   ;todo...cleanup 
        sta  up
        lda  w + 1
        sta  up + 1
        pop_psp
        lda  up + 1
        cmp  w + 1
        bcc  le_greater_than
        lda  up 
        cmp  w
        bcc  le_greater_than
    le_less_than_or_equal:
        lda  #0
        sta  xr + 1
        lda  #1
        sta  xr
        push_psp
        jmp  next
    le_greater_than:
        lda  #0
        sta  xr
        sta  xr + 1
        push_psp
        jmp  next


l029:   .word l028
        .byt $00,$00,$05,"CEL"
cells:  .word  enter
        ; .word  two   ; todo.. enable
        .word  one
        .word  plus     ; todo... remove
        ; .word  star     ; todo... implement with times (each cell is two bytes)
        .word  exit


l030:   .word l029
        .byt $00,$00,$02,"DP"
dp:     .word  *+2           ; pushes compiler/dictionary pointer to stck
        lda  #<cp
        sta  xr
        lda  #>cp
        sta  xr + 1
        push_psp
        jmp  next


l031:   .word l030
        .byt $00,$00,$05,"ALL"
allot:  .word  enter   ; ( n -- addr )
        .word  HERE
        .word  swap
        .word  HERE    ; add n to 'here'
        .word  plus
        .word  dp
        .word  store   ; store
        .word  exit


l032:   .word l031
        .byt $00,$00,$08,"VAR"
variable:     
        .word  enter
        .word  one
        .word  cells
        .word  allot   ; pushes to stack the cp prior to allocation
        .word  create
        .word  tick
        .word  enter
        .word  comma
        .word  tick
        .word  lit
        .word  comma
        .word  comma   ; we finally push the input to the stack here
        .word  tick
        .word  exit
        .word  comma
        .word  exit


; version of zbranch that jumps to address instead
; of adding an offset. allows jumping backwards
; todo... handle 0branch one way
l033:   .word l032
        .byt $00,$00,$07,"0BR"
zerobranch:     
        .word  *+2
        pop_psp                 ; check the stack
        lda  w                  ; check if zero
        beq  branch_to_address  ; if zero, branch to add the offset
        clc                     ; skip the offset 
        lda  ip       
        adc  #2       
        sta  ip       
        lda  ip + 1   
        adc  #0       
        sta  ip + 1   
        jmp  next
    branch_to_address:
        ldy  #1          ; get the next ip value (should be a number)
        lda  (ip),y
        sta  ip + 1      ; store it to the ip (we want to jump to this address)
        dey
        lda  (ip),y
        sta  ip
        jmp  next


l034:   .word l033
        .byt $00,$00,$05,"thr"
three:  .word  enter   
        .word  one
        .word  two
        .word  plus
        .word  exit


l035:   .word l034
        .byt $00,$00,$03,"not"
not:    .word  *+2
        pop_psp
        lda  w
        beq  invert_to_true
        lda  #0
        sta  xr
        sta  xr + 1
        push_psp
        jmp  next
    invert_to_true:
        lda  #1      ; todo... make -1
        sta  xr
        lda  #0
        sta  xr + 1
        push_psp
        jmp  next


l036:   .word l035
        .byt $00,$00,$04,"tuc"
tuck:   .word  enter   
        .word  swap
        .word  over
        .word  exit


l037:   .word l036
        .byt $00,$00,$03,"nip"
nip:    .word  enter   
        .word  swap
        .word  drop
        .word  exit


l038:   .word l037
        .byt $00,$00,$01,"="
equal:  .word  *+2     ; todo... test
        pop_psp         ; todo... make implementation better
        lda  w + 1
        sta  xr + 1
        lda  w
        sta  xr
        pop_psp
        sec
        lda  w
        sbc  xr
        sta  w
        lda  w + 1
        sbc  xr + 1
        sta  w + 1
        lda  w           ; 16-bit compare 
        bne  not_equal   ; // http://www.6502.org/tutorials/compare_beyond.html
        lda  w + 1
        bne  not_equal
        lda  #1      ; todo... make -1
        sta  xr
        lda  #0
        sta  xr + 1
        push_psp
        jmp  next
    not_equal:
        lda  #0
        sta  xr
        sta  xr + 1
        push_psp
        jmp  next


l039:   .word l038
        .byt $00,$00,$03,"two"
two:    .word  enter
        .word  docon   
        .word  2
        .word  exit


l040:   .word l039
        .byt $00,$00,$02,"2+"
plus2:  .word  enter
        .word  one
        .word  plus
        .word  one
        .word  plus
        .word  exit


l041:   .word l040
        .byt $00,$00,$02,"1+"
plus1:  .word  enter
        .word  one
        .word  plus
        .word  exit


l042:   .word l041
        .byt $00,$00,$03,"one"
one:    .word  enter   
        .word  docon   
        .word  1
        .word  exit


l043:   .word l042
        .byt $00,$00,$04,"zer"
zero:   .word  enter   
        .word  docon   
        .word  0
        .word  exit


l044:   .word l043
        .byt $00,$00,$08,"con"
constant:  
        .word  enter       ; todo... use to rbrac?
        .word  create
        .word  tick
        .word  enter
        .word  comma
        .word  tick
        .word  docon
        .word  comma
        .word  comma   ; val on stack eg 15 constant name (get 15)
        .word  tick
        .word  exit
        .word  comma
        .word  exit


l045:   .word l044
        .byt $00,$00,$05,"DOC"
docon:  .word  *+2     
; todo... this is NOT how you implement docon. 
; a weird interpreter bug occurs if you dont use enter
; for now i'm not using docon as a code word
        ldy  #1
        lda  (ip),y
        sta  xr + 1
        dey
        lda  (ip),y
        sta  xr
        push_psp
        clc
        lda  ip
        adc  #2
        sta  ip
        lda  ip + 1
        adc  #0
        sta  ip + 1
        jmp  next


l046:   .word l045
        .byt $00,$00,$04,"CHA"
char:   .word  *+2     ; pushes to stack the first char of the next word 
        jsr  _word   ; get next word, use cp as buffer
        ldy  #1      ; first char now stored at (cp),1 bc of _word
        lda  (cp),y
        sta  xr
        lda  #0      ; todo.. should this be zero? stack is 16-bit
        sta  xr + 1
        push_psp        ; push char to stack
        jmp  next


l047:   .word l046
        .byt $00,$00,$02,"R>"
fromr:  .word  *+2
        pop_rsp
        lda  w
        sta  xr
        lda  w + 1
        sta  xr + 1
        push_psp
        jmp  next


l048:   .word l047
        .byt $00,$00,$02,">R"
tor:    .word  *+2
        pop_psp
        lda  w
        sta  xr
        lda  w + 1
        sta  xr + 1
        push_rsp
        jmp  next


l049:   .word l048
        .byt $01,$00,$05,"UNT"
until:  .word  enter       ; todo --- test
        .word  tick
        .word  zbranch
        .word  comma
        .word  HERE
        .word  sub
        .word  comma
        .word  exit


l050:   .word l049
        .byt $01,$00,$06,"REP"
repeat: .word  enter       ; todo --- test
        .word  tick
        .word  branch
        .word  comma
        .word  swap
        .word  HERE
        .word  sub
        .word  comma
        .word  dup
        .word  HERE
        .word  swap
        .word  sub
        .word  swap
        .word  store
        .word  exit


l051:   .word l050
        .byt $01,$00,$05,"WHI" ; BEGIN predicate WHILE expressions REPEAT
while:  .word  enter       ; todo --- test
        .word  tick    
        .word  zbranch
        .word  comma
        .word  HERE
        .word  zero
        .word  comma 
        .word  exit


l052:   .word l051
        .byt $01,$00,$05,"BEG"
begin:  .word  enter       ; todo --- test
        .word  HERE
        .word  exit


l053:   .word l052
        .byt $01,$00,$04,"ELS"
else:   .word  enter       ; // ( needs address from branch ) ???
        .word  tick
        .word  branch
        .word  comma
        .word  HERE
        .word  zero
        .word  comma
        .word  swap
        .word  then
        .word  exit


l054:   .word l053
        .byt $01,$00,$04,"THE" 
then:   .word  enter       ; // ( needs address from branch )
        .word  HERE
        .word  over
        .word  sub
        .word  swap
        .word  store
        .word  exit


l055:   .word l054
        .byt $01,$00,$02,"IF"
if:     .word  enter       ; // ( flag -- )
        .word  tick        ; push the address of the next word
        .word  zbranch     ; todo... seems like i could remove all but zbranch?
        .word  comma       ; compile the address
        .word  HERE        ; push the offset address to the stack
        .word  tick        ; push the address of the next word
        .word  zero
        .word  comma
        .word  exit


l056:   .word l055
        .byt $00,$00,$06,"BRA"
branch: .word   *+2         ; unconditional branch
        ldy  #1          ; get the next ip value (should be a number)
        lda  (ip),y
        sta  xr + 1
        dey
        lda  (ip),y
        sta  xr

        clc             ; add value of ip to ip (we jump to this offset)
        lda  ip
        adc  xr      
        sta  ip
        lda  ip + 1
        adc  xr + 1
        sta  ip + 1
        jmp  next


l057:   .word l056
        .byt $00,$00,$07,"ZBR"
zbranch: ; branches if top of stack eq 0
        .word   *+2
        pop_psp                 ; check the stack 
        lda   w                 ; check if zero, todo... make flag -1
        beq   branch_to_offset  ; if zero, branch to add the offset
        clc              ; skip the offset 
        lda  ip       
        adc  #2       
        sta  ip       
        lda  ip + 1   
        adc  #0       
        sta  ip + 1   
        jmp  next

        branch_to_offset:

        ; jmp branch+2  ; todo... test if this works
        ldy  #1          ; get the next ip value (should be a number)
        lda  (ip),y
        sta  xr + 1
        dey
        lda  (ip),y
        sta  xr
        clc             ; add value of ip to ip (we jump to this offset)
        lda  ip
        adc  xr      
        sta  ip
        lda  ip + 1
        adc  xr + 1
        sta  ip + 1
        jmp  next


l058:   .word l057
        .byt $00,$00,$01,"/" ; ( n1 n2 -- qou )
slash:  .word  enter
        .word  slmod
        .word  swap
        .word  drop    ; drop remainder
        .word  exit


l059:   .word l058
        .byt $00,$00,$01,"*"
star:	.word  enter
		.word  ustar
		.word  drop
		.word  exit


l060:   .word l059
        .byt $00,$00,$01,"-"
sub:    .word   *+2     ; ( n1 n2 -- n3 ) n1 - n2
        pop_psp
        lda  w + 1
        sta  xr + 1
        lda  w + 0
        sta  xr + 0
        pop_psp
        sec
        lda  w
        sbc  xr
        sta  w
        lda  w + 1
        sbc  xr + 1
        sta  w + 1

        lda  w + 1
        sta  xr + 1
        lda  w
        sta  xr
        push_psp
        jmp  next


l061:   .word l060
        .byt $00,$00,$01,"+"
plus:   .word  *+2
        pop_psp
        lda  w + 1
        sta  xr + 1
        lda  w + 0
        sta  xr + 0
        pop_psp
        clc
        lda  xr + 0
        adc  w + 0
        sta  xr + 0
        lda  xr + 1
        adc  w + 1
        sta  xr + 1
        push_psp
        jmp  next


l062:   .word l061
        .byt $01,$00,$04,"LOO"
endloop: 
        .word  enter   ; todo... fix

        .word  tick
        .word  fromr
        .word  comma

        .word  tick
        .word  fromr
        .word  comma

        .word  tick
        .word  plus1
        .word  comma

        .word  tick
        .word  twodup
        .word  comma

        .word  tick
        .word  equal
        .word  comma

        .word  tick
        ; .word  zbranch ; todo... branch backwards
        .word  zerobranch ; todo... branch backwards
        .word  comma

        .word  comma   ; this is the HERE from DO

        .word  tick
        .word  twodrop
        .word  comma

        .word   exit


; end start DO some_action LOOP
l063:   .word l062
        .byt $01,$00,$02,"DO" ; ( n1 n2 -- ) n1 == end, n2 == start
do:     .word  enter  ; todo... test
        .word  HERE
        .word  tick
        .word  tor
        .word  comma

        .word  tick
        .word  tor
        .word  comma

        ; .word   tick    ;todo... remove
        ; .word   fromr   ;todo... remove
        ; .word   comma   ;todo... remove

        ; .word   tick    ;todo... remove
        ; .word   fromr   ;todo... remove
        ; .word   comma   ;todo... remove
        .word  exit


l064:   .word l063
        .byt $00,$00,$04,"OVE"
over:   .word  *+2
        ldy  #2          ; push second from top to stack
        lda  (psp),y
        sta  xr
        ldy  #3
        lda  (psp),y
        sta  xr + 1
        push_psp
        jmp  next


; todo... replace with another word, not adding twoswap
l065:   .word l064
        .byt $00,$00,$05,"2SW"
twoswap:
        .word  *+2
        jmp  next


l066:   .word l065
        .byt $00,$00,$03,"ROT"
rot:    .word  *+2
        ; rotates the third item to the top ( n1 n2 n3 -- n2 n3 n1 )
        ; todo... cleanup, implement in forth using swap, tor, fromr
        pop_psp         ; pop n3 from stack
        lda  w       ; store n3 in up
        sta  up
        lda  w + 1
        sta  up + 1
        pop_psp         ; pop n2 from stack
        lda  w + 1   ; store n2 in xr
        sta  xr + 1
        lda  w
        sta  xr
        pop_psp         ; pop n1 from stack
        push_psp        ; push n2 to stack (sitting in xr)
        lda  up + 1  ; move n3 to xr 
        sta  xr + 1
        lda  up      
        sta  xr
        push_psp        ; push n3 to stack
        lda  w + 1   ; store n1 in xr
        sta  xr + 1
        lda  w
        sta  xr
        push_psp        ; push n1 to stack
        jmp  next


l067:   .word l066
        .byt $00,$00,$04,"2DU"
twodup: .word  *+2
        lda  psp + 1     ; stare stack pointer in w 
        sta  w + 1       ; need original value of psp
        lda  psp
        sta  w
        ldy  #2          ; push second from top to the top
        lda  (w),y
        sta  xr
        ldy  #3
        lda  (w),y
        sta  xr + 1
        push_psp
        ldy  #0          ; push second from top to the top
        lda  (w),y
        sta  xr
        ldy  #1
        lda  (w),y
        sta  xr + 1
        push_psp
        jmp  next


l068:   .word l067
        .byt $00,$00,$05,"2DR"
twodrop:
        .word  *+2
        pop_psp
        pop_psp
        jmp  next


l069:   .word l068
        .byt $00,$00,$04,"DRO"
drop:   .word  *+2
        pop_psp
        jmp  next


l070:   .word l069
        .byt $00,$00,$01,"`"
tick:   .word   *+2     ; pushes to stack the address of the next word
        ; note.. does not work with immediate right now
        ; implementation is simpler
        ldy  #1
        lda  (ip),y         ; get the address of the next word
        sta  xr + 1
        dey
        lda  (ip),y
        sta  xr             ; address now stored in xr
        push_psp            ; push address to stack
        clc                 ; increment ip by 2 (to skip the next word)
        lda  ip
        adc  #2
        sta  ip
        lda  ip + 1
        adc  #0
        sta  ip + 1
        jmp  next


l071:   .word l070
        .byt $01,$00,$09,"imm"
immediate:    
        .word  *+2
        ldy  #2
        lda  (entry),y
        beq  immediate_one_toggle
        lda  #0          ; set precedence to 0
        jmp  immediate_done
    immediate_one_toggle:
        lda  #1          ; set precedence to 1
        jmp  immediate_done
    immediate_done:
        ldy  #2
        sta  (entry),y
        jmp  next


l072:   .word l071 
        .byt $00,$00,$03,"lit"
lit:    .word  *+2     ; push the value of the next ip
        ldy  #1      ; store ip val in xr
        lda  (ip),y
        sta  xr + 1
        dey
        lda  (ip),y
        sta  xr + 0
        push_psp        ; push the ip val
        clc             ; increase the ip
        lda  ip + 0
        adc  #2
        sta  ip + 0
        lda  ip + 1
        adc  #0
        sta  ip + 1
        jmp  next


l073:   .word l072
        .byt $00,$00,$06,"hid"
hidden: .word  *+2
        pop_psp         ; get link address from the stack
        clc
        lda  w
        adc  #3      ; todo... use CFA?
        sta  w
        lda  w + 1
        adc  #0
        sta  w + 1
        ldy  #0
        lda  (w),y
        cmp  #0      ; todo.. lda sets zero flag check if needed
        beq  hidden_one_toggle
        lda  #0          ; set smudge to 0
        jmp  hidden_done
    hidden_one_toggle:
        lda  #1          ; set smudge to 1
        jmp  hidden_done
    hidden_done:
        sta  (w),y
        jmp  next


l074:   .word l073
        .byt $00,$00,$01,"."
dot:    .word  enter
        .word  stod
        .word  ddot
        .word  exit


l075:   .word l074
        .byt $01,$00,$01,";"
semicolon: 
        .word  enter
        .word  lit
        .word  exit        ; append exit address to word
        .word  comma
        .word  latest 
        .word  fetch
        .word  hidden      ; untoggle hidden flag
        .word  lbrac       ; exit compile mode
        .word  exit


l076:   .word l075
        .byt $00,$00,$01,"]"
rbrac:  .word  *+2
        lda  #1
        sta  MODE
        jmp  next


l077:   .word l076
        .byt $00,$00,$01,"["
lbrac:  .word *+2
        lda  #0
        sta  MODE
        jmp  next


l078:   .word l077
        .byt $00,$00,$04,"wor"
word:   .word *+2
        jsr  _word
        jmp  next


l079:   .word l078
        .byt $00,$00,$03,"key"
key:    .word *+2
        jsr  _key
        sta  xr
        lda  #0
        sta  xr + 1
        push_psp
        jmp  next


l080:   .word l079
        .byt $00,$00,$04,"emi"
emit:   .word  *+2
        pop_psp
        ldy  #0
        lda  w
        sta  (cursor),y
        jsr  inc_cursor
        inc  bufoffset
        jmp  next


l081:   .word l080
        .byt $00,$00,$01,"?nu"
q_number: 
        .word  *+2
        jsr  _number
        jmp  next


_temp_branch_num_conversion_fail:
    jmp  num_conversion_fail


_number_temp:
        lda  #0
        sta  xr
        sta  xr + 1
        push_psp
        push_psp

        ; cp stucture...  
        ; ---------------------------------
        ; | wlength | char1 | char2 | ... | 
        ; ---------------------------------
        ldy  #0
        lda  (cp),y      
        sta  xr          ; store length in xr
        dec  xr          ; subtract 1 from length

        ldy  #1
        lda  (cp),y      ; get first char in word
        cmp  #$2d
        beq  add_dash
        dey
        inc  xr          ; add 1 to lenth if no dash
    add_dash:
        sty  rpad        ; todo... there must be a better way
        iny             ; skip '-' if necessary
        ldx  xr
    number_loop:        ;  begin number loop
        lda  (cp),y
        sec
        sbc  #$30
        bmi  _temp_branch_num_conversion_fail   ; if negative flag is set, fail
        cmp #$0A
        bmi  compare_to_base
    compare_to_base:
        cmp  b   ; val of base
        bpl  _temp_branch_num_conversion_fail ; if positive, ie greater than base --> fail

        sty  n          ; stx n + 1 ; lda xr ; debug

        sta  xr
        lda  #0
        sta  xr + 1
        push_psp

        ; // SWAP todo.. remove 
        pop_psp         ; pop item from stack
        lda  w + 1   ; store item in xr
        sta  xr + 1
        lda  w
        sta  xr
        pop_psp         ; pop item from stack
        push_psp        ; push item to stack
        lda  w + 1   ; store item in xr
        sta  xr + 1
        lda  w
        sta  xr
        push_psp        ; push item to stack
        ; // END SWAP

        lda  b
        sta  xr
        lda  #0
        sta  xr + 1
        push_psp

        jsr  _ustar

        pop_psp         ; DROP

        ; // ROT todo.. remove 
        pop_psp         ; pop n3 from stack
        lda  w       ; store n3 in up
        sta  up
        lda  w + 1
        sta  up + 1
        pop_psp         ; pop n2 from stack
        lda  w + 1   ; store n2 in xr
        sta  xr + 1
        lda  w
        sta  xr
        pop_psp         ; pop n1 from stack
        push_psp        ; push n2 to stack (sitting in xr)
        lda  up + 1  ; move n3 to xr 
        sta  xr + 1
        lda  up      
        sta  xr
        push_psp        ; push n3 to stack
        lda  w + 1   ; store n1 in xr
        sta  xr + 1
        lda  w
        sta  xr
        push_psp        ; push n1 to stack
        ; // end ROT

        lda  b
        sta  xr
        lda  #0
        sta  xr + 1
        push_psp

        jsr  _ustar
        jsr  _dplus

        ldy  n

    number_next:
        iny
        dex
        bne  temp_num_loop_jump     ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

    num_conversion_success:
        ; drop higher order double if not double
        ldy  #0      ; todo...is this ok? 
        lda  (psp),y
        bne  num_conversion_success_double
        iny
        lda  (psp),y
        bne  num_conversion_success_double
        pop_psp

    num_conversion_success_double:
        lda  #1      ; push success flag
        sta  xr
        lda  #0
        sta  xr + 1
        push_psp
        rts
    temp_num_loop_jump:
        jmp  number_loop ; todo...remove
    num_conversion_fail:
        lda  #0      ; push fail flag
        sta  xr
        sta  xr + 1
        push_psp
        rts

; todo.. remove
_number:
        ldy  #0          
        lda  (cp),y      ; get he length of the current word

        ; todo... test if hex is 30 to 39
        cmp  #3          ; todo.. add real comparison
        ; with cmp
        ; if acc eq the memory value, the carry flag is set
        ; if acc greater than the memory value, the carry flag is set
        ; if acc is less then the memory value, the carry flag is NOT set
        ; above code means if acc is eq to or greater than 3, the carry flag is set
        ; bcs (branch on carry set) if acc greater than or eq to 3, branch to fail 
        ; bcc (branch of carry clear) is the opposite of bcs 
        ; means branch if acc less than memory value

        bcs  conversion_fail     ; tests greater eq to acc (mlb ch 6)

        ; bne token 
        ; debug

    conversion_success:             ; ...branch_not_greater_than
        ; note... make this work with numbers not 2 chars in length
        ldy  #1          
        lda  (cp),y      
        sta  xr + 1
        ldy  #2
        lda  (cp),y      
        sta  xr
        push_psp
        ; one indicates successful conversion
        lda  #0
        sta  xr + 1
        lda  #1
        sta  xr
        push_psp
        rts
    conversion_fail:
        ; 1 indicates conversion falied
        lda  #0
        sta  xr + 1
        sta  xr
        push_psp
        rts
        
        ; get token length

        ; "word" returns two items on the stack, one is the address
        ; the other tis the token length 
        ; or check if it's 3 spaces (first 2 can max out at ff)
        
        ; not sure about the comparison, for now, just check the length
        ; within byte range...0 - 65535, total 65536
        ; #$FFFF
        
        ; could check the overflow flag by adding numbers?  
        ; (as long as i cleared it? command is CLV)

        ; true false flag added first
        lda  #1      ; or 0 or -1
        sta  xr
        push_psp
        ; // push_psp fail number (probably #0)
        ; // push_psp success number (probably #1)
        ; check to see what the base is
        ; is it a hex
        rts

interpret_end_jump:
        jmp     interpret_end

; //  word_not_found_jump:
        ; ; lda #$70
        ; ; debug
        ; jmp     word_not_found

interpret_addr:  .word interpret

; the outer interpreter.
interpret:  
        .word *+2
    interpret_start:

        jsr  _word       ; pushes address, then length, to stack

        pop_psp             ; get the word length from the stack
        pop_psp             ; probably to clean up the stack, todo... use cp for word buffer

        
        ; ldy     #0
        ; lda     (cp),y              ; get the length zero flag set if value is zero
        ; debug
        ; lda #$77
        ; debug

        ; this is zero when no words are left in the buffer
        ldy  #0
        lda  (cp),y              ; get the length zero flag set if value is zero
        beq  interpret_end_jump  ; if word length is zero, jump to end

        jsr  _find               ; dictionary find
        
        pop_psp

        lda  w
        bne  word_found
        jmp  word_not_found

    word_found:

        ; jsr     _cfa    ; get the code field given the link
        ; // pop_psp         ; get the dictionary word address from the stack


        lda  MODE            ; determine if compile mode
        bne  word_compile  ; when MODE = 1 (compile mode is set, run commma)

    word_execute: 
        ; todo... move to _cfa ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
        ; // pop_psp     ; probably to clean up the stack, todo... use cp for word buffer
        ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
        jsr  _cfa    ; get the code field given the link
        pop_psp         ; get the dictionary word address from the stack
        lda  #<interpret_addr    ; todo... move, set ip in setup
        sta  ip
        lda  #>interpret_addr
        sta  ip + 1
        jmp  w - 1
    word_compile:
        check_immediate:

        ldy  #0          ; peek at link address (do not increment stack pointer)
        lda  (psp),y
        sta  w
        ldy  #1
        lda  (psp),y
        sta  w + 1

        ldy  #2      ; get immediate value for word
        lda  (w),y
        beq  continue_compile    ; if zero, not immediate, so continue

        lda  #<interpret_addr        ; todo... move, set ip in setup
        sta  ip
        lda  #>interpret_addr
        sta  ip + 1

        jsr  _cfa   ; get the code field given the link
        pop_psp        ; get the dictionary word address from the stack
        jmp  w - 1

        ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

        continue_compile:

        jsr  _cfa   ; get the code field given the link
        pop_psp        ; get the dictionary word address from the stack

        ; jmp     interpret_end_jump ; todo..

        jsr  _comma

        jmp  interpret_start
    word_not_found:
        ; not dictionary word? try to convert to number
        ; jsr     _number         ; push number to stack
        jsr  _number_temp    ; todo... remove

        pop_psp     ; success/fail param on top of stack
        lda  w                   ; check successful conversion
        beq  interpret_error     ; 0 on stack if conversion fails
    
        lda  MODE            ; continue on if conversion is successful
        bne  number_compile  ; when MODE = 1
    number_execute:
        ; if number conversion successful, already on the stack, so do nothing
        ; lda  #$54 ; debug ; jsr trace_psp_indirect ; lda  w ; debug ; jmp quit
        jmp  interpret_start
    number_compile: ; comma, number, literal
        ldy  #0      ; save lower end to cp location
        lda  #<lit
        sta  (cp),y

        ldy  #1      ; save upper end to cp location
        lda  #>lit
        sta  (cp),y

        clc
        lda  cp      ; increase cp by 2
        adc  #2
        sta  cp
        lda  cp + 1
        adc  #0
        sta  cp + 1

        pop_psp         ; pop number from stack

        ldy  #1      ; save number to disctionary
        lda  w + 1
        sta  (cp),y
        dey
        lda  w
        sta  (cp),y

        clc
        lda  cp      ; increase cp by 2
        adc  #2
        sta  cp
        lda  cp + 1
        adc  #0
        sta  cp + 1
        jmp  interpret_start
    interpret_error:
        jsr  buf_clear
        jmp  quit
    interpret_end:
        jsr  msg_success
        jsr  newline_cursor
        jsr  buf_clear
        ldy  #0
        lda  #$5f
        sta  (cursor),y      ; output cursor at new position
        jmp  quit
        jmp  next


; // _question_mark: .word *+2 ; todo ...implement
        ; lda     #$99
        ; lda     cp + 1
        ; jsr trace_ip
        ; jmp     quit


l088:   .word l087
        .byt $00,$00,$01,"@"
fetch:  .word *+2       ; gets the value of the address on the stack
        pop_psp         ; get the address from the stack
        ldy  #1
        lda  (w),y
        sta  xr + 1
        dey
        lda  (w),y
        sta  xr
        push_psp
        jmp  next


l087:   .word l086
        .byt $00,$00,$01,"!"
store:  .word *+2 ;     // ( n addr -- )
        pop_psp
        lda  w + 1
        sta  xr + 1
        lda  w
        sta  xr
        pop_psp
        ldy  #1
        lda  w + 1
        sta  (xr),y
        dey
        lda  w
        sta  (xr),y
        jmp  next


l086:   .word l085
        .byt $00,$00,$03,"cfa"
CFA:    .word   *+2     ; gets the code address starting from the link
        pop_psp     ; todo... replace with _cfa
        clc
        lda  w
        adc  #9
        sta  xr
        lda  w + 1
        adc  #0
        sta  xr + 1
        push_psp
        jmp  next


l085:   .word l084
        .byt $00,$00,$03,"dfa"
DFA:    .word   *+2     ; gets the data field starting from the link + 2
        pop_psp
        clc
        lda  w
        adc  #11
        sta  xr
        lda  w + 1
        adc  #0
        sta  xr + 1
        push_psp
        jmp  next


l090:   .word l089
        .byt $00,$00,$06,"LAT"
latest: .word *+2
        lda  #>entry     
        sta  xr + 1
        lda  #<entry
        sta  xr
        push_psp        ; push the entry address to the stack (not the value)
        jmp  next

l089:   .word l088
        .byt $00,$00,$04,"HER"
HERE:   .word  *+2
        lda  cp + 1
        sta  xr + 1
        lda  cp 
        sta  xr
        push_psp
        jmp  next


_find:
        lda  #0      ;todo... does this matter?
        sta  w + 1
        ldy  #0
        lda  (cp),y
        sta  w
        lda  entry + 1  ; latest defined dictionary word (word compared to token)
        sta  up + 1              
        lda  entry
        sta  up
    find_loop
        lda  up + 1  ; move the link address to w
        sta  w + 1
        lda  up 
        sta  w 
        clc             ; store the length address in w
        lda  w
        adc  #4
        sta  w
        lda  w + 1
        adc  #0
        sta  w + 1
        lda  #0              ; clear xr
        sta  xr + 1
        sta  xr
        ldy  #0
        lda  (cp),y
        cmp  #3              ; set length to 3 if longer
        bcs  longer_length   ; if (cp),0 greater than 3, then jump
        lda  (cp),y          ; only runs if word length eq or shorter
        jmp  save_length
    longer_length:  
        lda  #3              ; to compare length also
    save_length:                ; add 1 length
        sta  xr
        clc
        lda  xr
        adc  #1
        sta  xr
        lda  xr + 1
        adc  #0
        sta  xr + 1
    inner_find_loop
        lda  (w),y
        cmp  (cp),y          ; compare chars
        bne  find_continue   ; jmp to next iteration of the loop if chars not equal
        iny                      ; increase y counter (cp),1 - (cp),3
        cpy  xr               ; xr is temp holder could be anything
        bne  inner_find_loop  ; y isnt 0 yet, do another iteration
    match_found:

;;;;;;;;;;;;;;;;;;;;;

        ; ; word address is 4 + word length (xr) 
        ; clc                 ; add 4 to length
        ; lda     xr
        ; adc     #4  
        ; sta     xr
        ; lda     xr + 1
        ; adc     #0
        ; sta     xr + 1

        ; clc                 ; add up to length + 4 to up 
        ; lda     up
        ; adc     xr
        ; sta     up
        ; lda     up + 1
        ; adc     #0
        ; sta     up + 1

        ; lda     up + 1       ; move up to xr (so we can push to the psp) 
        ; sta     xr + 1
        ; lda     up + 0         
        ; sta     xr + 0

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

        lda  up + 1       ; move link address to xr
        sta  xr + 1
        lda  up + 0         
        sta  xr + 0

        push_psp            ; push dictionary link address to stack to stack if match

        lda  #0
        sta  xr + 1
        lda  #1
        sta  xr
        push_psp            ; push success flag
        rts

        ; not related to above code ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
        back_to_top:                ; added here for branch out of range
        jmp  find_loop
        ; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

        find_continue:               ; continue here if word comparison fails

        ldy  #1
        lda  (up),y          ; get the addr in up 
        sta  xr + 1          ; store it to a temp loc

        ldy  #0
        lda  (up),y          ; get the addr in up 
        sta  xr              ; store it to a temp loc

        lda  xr + 1          ; transfer xr to up... to prevent infinite loop
        sta  up + 1
        lda  xr
        sta  up

        lda  up + 1          ; compare UP with 0
        cmp  #$00
        bne  back_to_top     ; is this right?
        lda  up + 0
        cmp  #$00
        bne  back_to_top     ; next iteration if not $0000

        ; // push_psp                ; push 0 to stack to indicate word not found

        lda  #0
        sta  xr + 1
        sta  xr
        push_psp        ; push fail flag if word not found
        rts


_cfa:
        lda  #0              ; clear xr
        sta  xr + 1
        sta  xr
        ldy  #0
        lda  (cp),y
        cmp  #3              ; set length to 3 if longer
        bcs  cfa_longer_length   ; if (cp),0 greater than 3, then jump
        lda  (cp),y          ; only runs if word length eq or shorter
        jmp  cfa_save_length
    cfa_longer_length:  
        lda  #3              ; to compare length also
    cfa_save_length:                ; add 1 length
        sta  xr
        clc
        lda  xr
        adc  #1
        sta  xr
        lda  xr + 1
        adc  #0
        sta  xr + 1
        ; word address is 4 + word length (xr) 
        clc                 ; add 4 to length
        lda  xr
        adc  #4  
        sta  xr
        lda  xr + 1
        adc  #0
        sta  xr + 1
        pop_psp             ; pop link address from stack
        clc                 ; add w to length + 4 to w 
        lda  w
        adc  xr
        sta  w
        lda  w + 1
        adc  #0
        sta  w + 1
        lda  w + 1       ; move w to xr (so we can push to the psp) 
        sta  xr + 1
        lda  w + 0         
        sta  xr + 0
        push_psp
        rts

; dup ... the value at the label dup
; .word dup ... the address of the label dup (little endian)
; the label is an alias for an address
cold_start:

        ; jsr   _temp_input_setup

        lda  #<interpret_address
        sta  ip
        lda  #>interpret_address
        sta  ip + 1
        jmp  next
        interpret_address: .word interpret

; // temp_input: .byt "two one three two + D.R q $" ; // 30 / 2 == 15
; // temp_input: .byt "three SPACES q $" ; // 30 / 2 == 15
; // temp_input: .byt "two 0->1 q $" ; // 30 / 2 == 15

; // temp_input: .byt "tnum 0 <# # # # #> TYPE q $" ; // 30 / 2 == 15
; // temp_input: .byt "300 q $" ; // 30 / 2 == 15
; // temp_input: .byt "one three U* q $" ; // 30 / 2 == 15

; // temp_input: .byt "two zero zero zero D+ q $" ; // 30 / 2 == 15
; // temp_input: .byt "999999 q $" ; // 30 / 2 == 15
; // temp_input: .byt "10 10 + . q $" ; // 30 / 2 == 15

temp_input: .byt "311 0 <# # # # #> TYPE q $" ; // 30 / 2 == 15

_temp_input_setup:
        ldy  #0
        input_loop:
        lda  temp_input,y
        sta  bufaddr,y       ; move temp string to input buffer
        iny
        cmp  #$24            ; checks if matches dollar sign
        bne  input_loop
        sty  bufoffset       ; update the buffer offset
        rts

quit:   jmp blank           ; skip characher output

default:
        lda     getc
done:
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
        lda  getc
        ldy  #0
        sta  (cursor),y
        jsr  inc_cursor

        lda  #$5f
        sta  (cursor),y
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
blank:
        pla             ; pop ac from stack
        rti             ; return from inturrupt
    

inc_cursor:
        clc
        lda  cursor
        adc  #1
        sta  cursor
        lda  cursor + 1
        adc  #0
        sta  cursor + 1
        rts


newline_cursor:
        sec              ; subtract offset from cursor
        lda  cursor
        sbc  bufoffset
        sta  cursor
        lda  cursor + 1
        sbc  #0
        sta  cursor + 1
        clc
        lda  cursor
        adc  #$2b
        sta  cursor
        lda  cursor + 1
        adc  #0
        sta  cursor + 1
        rts


dec_cursor:
        sec
        lda  cursor
        sbc  #1
        sta  cursor
        lda  cursor + 1
        sbc  #0
        sta  cursor + 1
        rts


; flags, bits, smudge shows/hides entry  
; precedence determines immediate vs not
; precedence,smudge,length,name
l093:   .word  l092
        .byt $00,$00,$04,"swa"
swap:   .word *+2
        pop_psp         ; pop item from stack
        lda  w + 1      ; store item in xr
        sta  xr + 1
        lda  w
        sta  xr
        pop_psp         ; pop item from stack
        push_psp        ; push item to stack
        lda  w + 1      ; store item in xr
        sta  xr + 1
        lda  w
        sta  xr
        push_psp        ; push item to stack
        jmp  next


l094:   .word l093
        .byt $00,$00,$03,"DUP"
dup:    .word *+2
        pop_psp         ; store the top psp value into w 
        lda  w + 1      ; load w (2nd half)
        sta  xr + 1     ; store w in xr (2nd half)
        lda  w          ; load w (1st half)
        sta  xr         ; store w in xr (1st half)
        push_psp        ; push the value in xr to the p stack
        push_psp        ; push the value in xr to the p stack again
        jmp  next




#print entry
#print double
#print l097
#print l096
#print l095
#print l094
#print l093
#print l092
#print l091

#print l060
#print l059
#print l058
#print l057
#print l056
#print l055
#print l054
#print l053
#print l052
#print l051
#print l050
#print l049
#print l048
#print l047
#print l046
#print l045
#print l044
#print l043
#print l042

#print interpret
#print up
#print cs_jump
#print cold_start
#print double
#print enter
#print qqq
#print quit
#print exit
#print next
#print key
#print lit
#print colon
#print semicolon
#print immediate
#print plus
#print tick
#print dup
#print sub

#print star
#print slash
#print branch
#print zbranch
#print if
#print then
#print else
#print begin
#print while
#print repeat
#print until
#print tor
#print fromr
#print char
#print docon
#print zero
#print interpret_address


