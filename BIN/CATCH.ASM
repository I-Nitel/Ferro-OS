bits 16
cpu 8086
org 0x0000

%define PADDLE_Y 22

start:
    push cs
    pop ds
    call clear_screen   ; Only clear ONCE at the very start

init_game:
    mov byte [paddle_x], 40
    mov byte [old_paddle_x], 40
    mov byte [ball_x], 20
    mov byte [ball_y], 0
    mov byte [old_ball_x], 20
    mov byte [old_ball_y], 0
    mov word [score], 0
    mov byte [lives], 3
    mov byte [ball_speed], 3
    
    xor ax, ax
    mov es, ax
    mov al, [es:0x046C]
    mov [last_tick], al

game_loop:
    ; 1. SMART DRAW
    call update_display
    
    ; 2. INPUT
    mov ah, 01h
    int 16h        
    jz .check_timer
    
    xor ah, ah
    int 16h        
    cmp ah, 4Bh    ; Left
    je .left
    cmp ah, 4Dh    ; Right
    je .right
    cmp al, 27     ; ESC
    je .exit
    jmp .check_timer

.left:
    mov al, [paddle_x]
    mov [old_paddle_x], al
    cmp al, 0
    je .check_timer
    dec byte [paddle_x]
    jmp .check_timer
.right:
    mov al, [paddle_x]
    mov [old_paddle_x], al
    cmp al, 75
    je .check_timer
    inc byte [paddle_x]

.check_timer:
    xor ax, ax
    mov es, ax
    mov al, [es:0x046C]
    cmp al, [last_tick]
    je game_loop
    
    mov [last_tick], al
    inc byte [tick_counter]
    mov al, [tick_counter]
    cmp al, [ball_speed]
    jl game_loop

    ; 3. MOVE BALL
    mov byte [tick_counter], 0
    
    mov al, [ball_x]
    mov [old_ball_x], al
    mov al, [ball_y]
    mov [old_ball_y], al
    
    inc byte [ball_y]
    
    cmp byte [ball_y], PADDLE_Y
    jne game_loop

    ; --- COLLISION ---
    mov al, [ball_x]
    mov bl, [paddle_x]
    cmp al, bl
    jl .miss
    add bl, 5
    cmp al, bl
    jg .miss

    ; --- CAUGHT ---
    inc word [score]
    call play_beep
    call erase_ball
    mov byte [ball_y], 0
    call randomize_ball
    jmp game_loop

.miss:
    dec byte [lives]
    call erase_ball
    cmp byte [lives], 0
    je .game_over
    mov byte [ball_y], 0
    call randomize_ball
    jmp game_loop

.game_over:
    call clear_screen
    mov dh, 12
    mov dl, 20
    call move_cursor
    mov si, msg_over
    call print_string
    xor ah, ah
    int 16h
    call clear_screen
    jmp init_game

.exit:
    retf

; --- Smart Rendering Functions ---

update_display:
    mov al, [paddle_x]
    cmp al, [old_paddle_x]
    je .draw_ball_part
    
    mov dh, PADDLE_Y
    mov dl, [old_paddle_x]
    call move_cursor
    mov si, empty_paddle
    call print_string
    
    mov dh, PADDLE_Y
    mov dl, [paddle_x]
    call move_cursor
    mov si, paddle_str
    call print_string
    mov al, [paddle_x]
    mov [old_paddle_x], al

.draw_ball_part:
    mov dh, [old_ball_y]
    mov dl, [old_ball_x]
    call move_cursor
    mov al, ' '
    mov ah, 0x0E
    int 10h
    
    mov dh, [ball_y]
    mov dl, [ball_x]
    call move_cursor
    mov al, '*'
    mov ah, 0x0E
    int 10h
    
    mov dh, 0
    mov dl, 0
    call move_cursor
    mov si, score_msg
    call print_string
    mov ax, [score]
    call print_int
    
    mov dl, 20
    call move_cursor
    mov si, lives_msg
    call print_string
    mov al, [lives]
    add al, '0'
    mov ah, 0x0E
    int 10h
    ret

erase_ball:
    mov dh, [ball_y]
    mov dl, [ball_x]
    call move_cursor
    mov al, ' '
    mov ah, 0x0E
    int 10h
    ret

; --- Standard Helpers ---

randomize_ball:
    in al, 0x40
    and al, 63
    add al, 5
    mov [ball_x], al
    ret

play_beep:
    mov al, 0xB6
    out 0x43, al
    mov ax, 1200
    out 0x42, al
    mov al, ah
    out 0x42, al
    in al, 0x61
    or al, 3
    out 0x61, al
    
    ; 8086 safe beep delay
    xor dx, dx
    mov es, dx
    mov si, 0x046C
    mov al, [es:si]
    add al, 1
.b_wait:
    cmp al, [es:si]
    jne .wait_done ; use a different label to be safe
    jmp .b_wait
.wait_done:
    in al, 0x61
    and al, 0xFC
    out 0x61, al
    ret

print_int:
    mov bx, 10
    xor cx, cx
.divl:
    xor dx, dx
    div bx
    push dx
    inc cx
    test ax, ax
    jnz .divl
.prtl:
    pop ax
    add al, '0'
    mov ah, 0x0E
    int 10h
    loop .prtl
    ret

move_cursor:
    mov ah, 0x02
    xor bh, bh
    int 10h
    ret

clear_screen:
    mov ax, 0x0003
    int 10h
    ret

print_string:
    mov ah, 0x0E
.l:
    lodsb
    or al, al
    jz .d
    int 10h
    jmp .l
.d:
    ret

; --- Data ---
paddle_x     db 40
old_paddle_x db 40
ball_x       db 20
ball_y       db 0
old_ball_x   db 20
old_ball_y   db 0
lives        db 3
ball_speed   db 3
tick_counter db 0
last_tick    db 0
score        dw 0
high_score   dw 0
paddle_str   db "[===]", 0
empty_paddle db "     ", 0 
score_msg    db "Score: ", 0
lives_msg    db " Lives: ", 0
hi_msg       db "Hi: ", 0
msg_over     db "GAME OVER! Press any key...", 0
