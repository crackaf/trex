[org 0x0100]
;obstacle start row is 15
;path start row is 18
jmp start 

oldtimeisr: dw 0,0
oldkbisr: dw 0,0
character_position: dw 2410
obstacle_position_fix: dw 2712
obstacle1_position: dw 0, 0
obstacle2_position: dw 0, 0
path_position: dw 2880
coloum_store: times 25 dw 0
score: dd 0
game_end: dw 0
jmp_height_fixed: dw 6
jmp_height: dw 6
is_jumping: dw 0
timeisrInUse: dw 0
temp: dw 0

;location as parameter
deletecharacter:
    push bp
    mov bp,sp
    push ax
    push es
    push di
    mov ax,0xb800
    mov es,ax
    mov di,[bp+4]
    mov ax,0720h
    mov word[es:di],ax 
    add di,158
    mov word[es:di],ax 
    add di,2
    mov word[es:di],ax 
    add di,2
    mov word[es:di],ax 
    add di,158
    mov word[es:di],ax 
    pop di
    pop es
    pop ax
    pop bp
    ret 2

;location as parameter
printcharacter:
    push bp
    mov bp,sp
    push ax
    push es
    push di

    mov ax,0xb800
    mov es,ax
    mov di,[bp+4]
    mov ax,0601h
    cmp word[es:di], 0x07db ; check whether it touches an object
    jne character_skip1
    mov word [game_end], 1 ;if touches, than game end
    or ah, 10000000 ; add blinking attribute
    character_skip1: ;smily face
    mov word[es:di],ax ;smily face

    add di,158
    mov ah,06h
    mov al,'/'
    cmp word[es:di], 0x07db ; check whether it touches an object
    jne character_skip2
    mov word [game_end], 1 ;if touches, than game end
    or ah, 10000000 ; add blinking attribute
    character_skip2: ;left shoulder
    mov word[es:di],ax ;left shoulder /

    add di,2
    mov ax,06b3h
    cmp word[es:di], 0x07db ; check whether it touches an object
    jne character_skip3
    mov word [game_end], 1 ;if touches, than game end
    or ah, 10000000 ; add blinking attribute
    character_skip3: ;body |
    mov word[es:di],ax ;body

    add di,2
    mov ah,06h
    mov al,'\'
    cmp word[es:di], 0x07db ; check whether it touches an object
    jne character_skip41
    mov word [game_end], 1 ;if touches, than game end
    or ah, 10000000 ; add blinking attribute
    character_skip41: ;right shoulder \
    mov word[es:di], ax ;right shoulder
    mov word[es:di], ax

    add di,158
    mov ax,0613h
    cmp word[es:di], 0x07db ; check whether it touches an object
    jne character_skip5
    mov word [game_end], 1 ;if touches, than game end
    or ah, 10000000 ; add blinking attribute
    character_skip5: ;feet !!
    mov word[es:di],ax ;feet

    pop di
    pop es
    pop ax
    pop bp
    ret 2

path:
    push ax
    push es
    push di

    push word 0xb800
    pop es
    mov di,[path_position]
    mov cx,80
    mov ah,08h
    mov al,'_'
    loop1:            ;for first line
        mov word[es:di],ax
        add di,2
    loop loop1
    mov cx,20
    mov al,'-'
    loop2:             ;for second line
        mov word[es:di],ax
        add di,2
        mov word[es:di],ax
        add di,2
        mov word[es:di],ax
        add di,4
    loop loop2
    add di,4
    mov cx,20
    mov al,'-'
    loop3:               ;for third line
        mov word[es:di],ax
        add di,2
        mov word[es:di],ax
        add di,2
        cmp di,3360
        je path_exit
        mov word[es:di],ax
        add di,4
    loop loop3
    path_exit:
    sub di,160
    mov word[es:di],ax

    pop di
    pop es
    pop ax
    ret 

;two parameters, location and character to print
obstacle1:
    push bp
    mov bp,sp
    push ax
    push es
    push di

    push word 0xb800
    pop es
    mov di,[bp+4]
    mov ax, [bp+6]; character , usually DB
    mov ah, 07h
    mov word[es:di],ax
    add di,160
    mov word[es:di],ax

    pop di
    pop es
    pop ax
    pop bp
    ret 4

obstacle2:
    push bp
    mov bp,sp
    push ax
    push es
    push di

    push word 0xb800
    pop es
    mov di,[bp+4]
    mov ax, [bp+6]; character , usually DB
    mov ah, 07h
    mov word[es:di],ax
    add di,160
    mov word[es:di],ax
    mov di,[bp+4]
    sub di,160
    add di,4
    mov word[es:di],ax
    add di,160
    mov word[es:di],ax
    add di,160
    mov word[es:di],ax

    pop di
    pop es
    pop ax
    pop bp
    ret 4

obstacle3:
    push bp
    mov bp,sp
    push ax
    push es
    push di

    push word 0xb800
    pop es
    mov di,[bp+4]
    mov ax, [bp+6]; character , usually DB
    mov ah, 07h
    mov word[es:di],ax
    add di,160
    mov word[es:di],ax
    mov di,[bp+4]
    sub di,160
    add di,4
    mov word[es:di],ax
    add di,160
    mov word[es:di],ax
    add di,160
    mov word[es:di],ax
    add di,2
    mov word[es:di],ax

    pop di
    pop es
    pop ax
    pop bp
    ret 4

clrscr: 
	 pusha
	 mov ax, 0xb800
	 mov es, ax ; point es to video base
	 xor di, di ; point di to top left column
	 mov ax, 0x1620 ; space char in normal attribute
	 mov cx, 2000 ; number of screen locations
	 cld ; auto increment mode
	 rep stosw ; clear the whole screen
	 popa
	 ret 

delay:
	pusha
	pushf
	mov cx,500
	mydelay:
        mov bx,250      ;; increase this number if you want to add more delay, and decrease this number if you want to reduce delay.
        mydelay1:
            dec bx
        jnz mydelay1
	loop mydelay
    add word [temp], 1;-----------------
	popf
	popa
    ret	

pathScroll:
    pusha
    push es
    push ds
    mov ax, 0xb800
    mov es, ax
    mov ds, ax
    mov di, [cs:path_position]
    mov si, [cs:path_position]
    add si, 2
    mov cx, 3
    pathScroll_loop:
        push cx
        mov dx, [di];stores left 2 bytes
        mov cx, 79
        cld
        rep movsw
        mov [es:di], dx;restore 2 bytes
        pop cx
        add si, 2
        add di, 2
    loop pathScroll_loop
    
    pop ds
    pop es
    popa
    ret


; a function to generate random number between 0 and n
; input: n, can be accessed using bp+4
; output: random number can be accessed in function using bp+6
RANDNUM:
   push bp
   mov bp,sp
   push ax
   push cx
   push dx
   
   mov AH, 00h  ; interrupts to get system time        
   int 1AH      ; CX:DX now hold number of clock ticks since midnight      
   mov  ax, dx
   xor  dx, dx
   mov  cx, [bp+4] 
   inc cx   
   div  cx       ; here dx contains the remainder of the division - from 0 to 9
   mov [bp+6], dx
   pop dx
   pop cx
   pop ax
   pop bp   
   ret 2

printObstacle:
    push bp
    mov bp, sp
    pusha

    mov ax, [bp+4];randnum
    mov bx, [bp+6];character
    mov dx, [bp+8];position

    c1:
        cmp ax, 0
        jg c2
        jmp cend
    c2:
        cmp ax, 3
        jg c3
        ;push word [obstacle_position_fix]
        push bx;push 0x07db
        push dx;push 2712
        call obstacle1
        jmp cend
    c3:
        cmp ax, 6
        jg c4
        ;push word [obstacle_position_fix]
        push bx;push 0x07db
        push dx;push 2712
        call obstacle2
        jmp cend
    c4:
        ;push word [obstacle_position_fix]
        push bx;push 0x07db
        push dx;push 2712
        call obstacle3
    cend:
    ; cmp word [obstacle1_position], 0
    ; jne c5
    ; mov word [obstacle1_position], dx
    ; jmp printObstacle_exit
    ; c5: 
    ; mov word [obstacle2_position], dx
    ; printObstacle_exit:

    popa
    pop bp
    ret 6


printRandObstacle:
    push ax
    sub sp, 2
    push word 10
    call RANDNUM
    pop ax

    cmp word [obstacle1_position], 0
    je skipp
    cmp word [obstacle2_position], 0
    je skipp
    jmp printRandObstacle_exit
    skipp:
    mov dx, word [obstacle_position_fix]

    push word [obstacle_position_fix]
    push word 0x07bd
    push ax
    call printObstacle

    cmp word [obstacle1_position], 0
    jne c5
    mov word [obstacle1_position], dx
    mov word [obstacle1_position+2], ax
    jmp printRandObstacle_exit
    c5: 
    cmp word [obstacle2_position], 0
    jne printRandObstacle_exit
    mov word [obstacle2_position], dx
    mov word [obstacle2_position+2], ax
    printRandObstacle_exit:

    pop ax
    ret

movObstacles:
    pusha
 
    
    
    ; mov di, 15*160
    ; mov si, 15*160
    ; add si, 2
    ; mov cx, 3
    ; movObstacles_loop:
    ;     push cx
    ;     mov ax, 0x07db
    ;     mov cx, 0xffff
    ;     cld
    ;     repne scasw

    ;     push word 0x0720
    ;     call prin


    ;     sub di, 2
    ;     mov si, di
    ;     add si, 2
    ;     movsw
    ;     pop cx
    ;     ;add si, 2
    ;     ;add di, 2
    ; loop movObstacles_loop
    movObstacles_c1:
    cmp word [obstacle1_position], 16*160
    jne movObstacles_c2

    push word [obstacle1_position]
    push word 0x0720
    push word [obstacle1_position+2]
    call printObstacle

    mov word [obstacle1_position], 0
    mov word [obstacle1_position+2], 0

    movObstacles_c2:

    cmp word [obstacle2_position], 16*160
    jne movObstacles_c3

    push word [obstacle2_position]
    push word 0x0720
    push word [obstacle2_position+2]
    call printObstacle

    mov word [obstacle2_position], 0
    mov word [obstacle2_position+2], 0

    movObstacles_c3:

    cmp word [obstacle1_position], 0
    je movObstacles_c4

    push word [obstacle1_position]
    push word 0x0720
    push word [obstacle1_position+2]
    call printObstacle

    sub word [obstacle1_position], 2

    push word [obstacle1_position]
    push word 0x07db
    push word [obstacle1_position+2]
    call printObstacle

    movObstacles_c4:

    cmp word [obstacle2_position], 0
    je movObstacles_c5

    push word [obstacle2_position]
    push word 0x0720
    push word [obstacle2_position+2]
    call printObstacle

    sub word [obstacle2_position], 2

    push word [obstacle2_position]
    push word 0x07db
    push word [obstacle2_position+2]
    call printObstacle

    movObstacles_c5:

    movObstacles_exit:
    
    popa
    ret


; takes the number to be printed as its parameter
printnum: 
    push bp
    mov bp, sp
    push es
    push ax
    push bx
    push cx
    push dx
    push di
    mov ax, 0xb800
    mov es, ax ; point es to video base
    mov ax, [bp+4] ; load number in ax
    mov bx, 10 ; use base 10 for division
    mov cx, 0 ; initialize count of digits
    nextdigit: 
    mov dx, 0 ; zero upper half of dividend
    div bx ; divide by 10
    add dl, 0x30 ; convert digit into ascii value
    push dx ; save ascii value on stack
    inc cx ; increment count of values
    cmp ax, 0 ; is the quotient zero
    jnz nextdigit ; if no divide it again
    mov di, 11*160 ; point di to top left column
    add di, 140
    nextpos: 
    pop dx ; remove a digit from the stack
    mov dh, 0x07 ; use normal attribute
    mov [es:di], dx ; print char on screen
    add di, 2 ; move to next screen location
    loop nextpos ; repeat for all digits on stack
    pop di
    pop dx
    pop cx
    pop bx
    pop ax
    pop es
    pop bp
    ret 2


jmpcharacter:
    push ax
;    int 16h

    cmp word [jmp_height], 0
    jle jmp_movedown
    jmp_movup:  ;moves character from bottom to top
                push word [character_position]
                call deletecharacter
                sub word [character_position], 160
                push word [character_position]
                call printcharacter
                dec word [jmp_height]
                jmp jmpcharacter_exit
    jmp_movedown:   ;moves character from top to bottom
                push word [character_position]
                call deletecharacter
                add word [character_position], 160
                push word [character_position]
                call printcharacter
                dec word [jmp_height]
                xor ax, ax
                sub ax, [jmp_height]
                cmp word ax, [jmp_height_fixed]
                jne jmpcharacter_exit
                mov ax, [jmp_height_fixed]
                mov [jmp_height], ax
                mov word [is_jumping], 0 ;stop jumping
    jmpcharacter_exit:
    pop ax
    ret

print_score:
    push word [score]
    call printnum
    inc dword [score]
    ret

kbisr:
    pusha
    in al, 0x60 ; read a char from keyboard port
	cmp al, 57
    jne kbisr_skip
    mov word [is_jumping], 1
    kbisr_skip:
    cmp al, 1
    jne kbisr_end
    mov word [game_end], 1
    kbisr_end:
    mov al, 0x20
	out 0x20, al ; send EOI to PIC
    popa
    jmp far [cs:oldkbisr]

timeisr:
    pusha

    cmp word [game_end], 1
    jne timeisr_gamend
    jmp timeisr_exit2
    timeisr_gamend:
    cmp word [is_jumping], 1
    jne timeisr_skip ;jumping is zero
    ;in al, 0x60

   
    ;cmp al, 57
    ;jne timeisr_skip
    ;mov word [is_jumping], 1
    timeisr_skip:
    cmp word [is_jumping], 1
    jne timeisr_skip2 ;means it's 0
    call jmpcharacter
    
    timeisr_skip2:
    ;cmp word [timeisrInUse], 1
   ; je timeskip
    ;mov word [timeisrInUse], 1
    cmp word [obstacle1_position], 16*160+80
    jg skip_obs
    call printRandObstacle
    skip_obs:
    call pathScroll
    call movObstacles
    call print_score
    cmp word [is_jumping], 1
    je skip_2
    push word [character_position]
    call printcharacter
    skip_2:
    ;jmp timeexit
    ;timeisr_skip:
     
    ;call pathScroll

   ; mov word [timeisrInUse], 0

    ;jmp far [cs:oldtimeisr]
    timeisr_exit:

    mov al, 0x20
    out 0x20, al
    popa
    iret

    timeisr_exit2:
    mov al, 0x20
    out 0x20, al
    popa
    jmp terminate

hookingisr:
    push ax
    push es
    xor ax, ax
    mov es, ax
    mov ax, [es:8*4]
    mov word [oldtimeisr], ax
    mov ax, [es:8*4+2]
    mov word [oldtimeisr+2], ax
    mov ax, [es:9*4]
    mov word [oldkbisr], ax
    mov ax, [es:9*4+2]
    mov word [oldkbisr+2], ax
    cli
    mov ax, timeisr
    mov word [es:8*4], ax
    mov word [es:8*4+2], cs
    mov ax, kbisr
    mov word [es:9*4], ax
    mov word [es:9*4+2], cs
    sti
    pop es
    pop ax
    ret

unhookingisr:
    push ax
    push es
    xor ax, ax
    mov es, ax
    cli
    mov ax, word [oldtimeisr]
    mov [es:8*4], ax
    mov ax, word [oldtimeisr+2]
    mov [es:8*4+2], ax
    mov ax, word [oldkbisr]
    mov [es:9*4], ax
    mov ax, word [oldkbisr+2]
    mov [es:9*4+2], ax
    sti
    pop es
    pop ax
    ret

startgame:
    call path
    push word [character_position]
    call printcharacter
    call hookingisr
    ret

start: 
    call startgame
    ;call printRandObstacle
    ;call printRandObstacle
    ;call printRandObstacle

    inf:
    ; cmp word [is_jumping], 1
    ; jne timeisr_skip3 ;jumping is zero
    ; timeisr_skip3:
    ; cmp word [is_jumping], 1
    ; jne timeisr_skip4 ;means it's 0
    ; ;call jmpcharacter
    
    ; timeisr_skip4:
    ; ;call pathScroll
    ; cmp word [obstacle1_position], 16*160+30
    ; jle print_it
    ; ; skip_obstacle_temp: 
    ; ; cmp word [obstacle2_position], 16*160+20
    ; ; jle print_it
    ; jmp skip_obstacle
    ; print_it:call printRandObstacle
    ; skip_obstacle:
    ; call movObstacles
    ; call print_score
    ; call delay
    jmp inf

terminate:
call unhookingisr
mov ax ,0x4c00
int 0x21