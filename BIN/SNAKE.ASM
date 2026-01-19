bits 16
cpu 8086
org 0x0000

%define MAX_SNAKE_LEN 100

start:
    push cs
    pop ds
    call clear_screen

init_game:
    mov word [score], 0
    mov word [snake_len], 3
    mov byte [cur_dir], 1
    mov byte [move_tick], 0
    
    mov word [body], 0x0C28
    mov word [body+2], 0x0C27
    mov word [body+4], 0x0C26
    
    call spawn_food

game_loop:
    call draw_ui
    
    mov ah, 01h
    int 16h
    jz .check_timer
    
    xor ah, ah
    int 16h
    
    cmp ah, 0x48 ; Up
    je .set_up
    cmp ah, 0x50 ; Down
    je .set_down
    cmp ah, 0x4B ; Left
    je .set_left
    cmp ah, 0x4D ; Right
    je .set_right
    cmp al, 27   ; ESC
    je .exit
    jmp .check_timer

.set_up:
    cmp byte [cur_dir], 2
    je .check_timer
    mov byte [cur_dir], 0
    jmp .check_timer
.set_down:
    cmp byte [cur_dir], 0
    je .check_timer
    mov byte [cur_dir], 2
    jmp .check_timer
.set_left:
    cmp byte [cur_dir], 1
    je .check_timer
    mov byte [cur_dir], 3
    jmp .check_timer
.set_right:
    cmp byte [cur_dir], 3
    je .check_timer
    mov byte [cur_dir], 1

.check_timer:
    xor ax, ax
    mov es, ax
    mov al, [es:0x046C]
    cmp al, [last_tick]
    je game_loop
    
    mov [last_tick], al
    inc byte [move_tick]
    cmp byte [move_tick], 2
    jl game_loop
    mov byte [move_tick], 0

    call move_snake
    
    call check_collision
    jc .game_over
    
    jmp game_loop

.game_over:
    mov dh, 12
    mov dl, 35
    call move_cursor
    mov si, msg_over
    call print_string
    xor ah, ah
    int 16h
    call clear_screen
    jmp init_game

.exit:
    retf

; --- Functions ---

move_snake:
    mov ax, [snake_len]
    dec ax
    shl ax, 1
    mov si, body
    add si, ax
    mov dx, [si]
    call move_cursor
    mov al, ' '
    call print_char

    mov cx, [snake_len]
    dec cx
.shift:
    mov si, body
    mov ax, cx
    shl ax, 1
    add si, ax
    mov dx, [si-2]
    mov [si], dx
    loop .shift

    mov ax, [body]
    cmp byte [cur_dir], 0
    je .go_up
    cmp byte [cur_dir], 1
    je .go_right
    cmp byte [cur_dir], 2
    je .go_down
    cmp byte [cur_dir], 3
    je .go_left
.go_up:
    dec ah
    jmp .head_done
.go_down:
    inc ah
    jmp .head_done
.go_left:
    dec al
    jmp .head_done
.go_right:
    inc al
.head_done:
    mov [body], ax
    
    mov dx, ax
    call move_cursor
    mov al, '#'
    call print_char
    ret

check_collision:
    mov ax, [body] ; AX = Head (Y,X)
    
    ; 1. Wall Boundaries
    cmp al, 0
    jl .hit
    cmp al, 79
    jg .hit
    cmp ah, 1
    jl .hit
    cmp ah, 24
    jg .hit

    ; 2. Self-Collision (Check head against all body segments)
    mov cx, [snake_len]
    dec cx              ; Don't check head against itself
    jz .check_food      ; If length is 1, skip (shouldn't happen)
    mov si, body
    add si, 2           ; Start at the second segment
.self_loop:
    cmp ax, [si]
    je .hit             ; Collision detected!
    add si, 2
    loop .self_loop

.check_food:
    ; 3. Food Collision
    cmp ax, [food_pos]
    jne .no_food
    inc word [score]
    add word [snake_len], 1
    call spawn_food
.no_food:
    clc
    ret
.hit:
    stc
    ret

spawn_food:
    in al, 0x40
    and al, 63
    add al, 5
    mov [food_pos_x], al
    in al, 0x40
    and al, 15
    add al, 4
    mov [food_pos_y], al
    
    mov dx, [food_pos]
    call move_cursor
    mov al, '@'
    call print_char
    ret

draw_ui:
    mov dh, 0
    mov dl, 0
    call move_cursor
    mov si, score_msg
    call print_string
    mov ax, [score]
    call print_int
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
    int 0x10
    loop .prtl
    ret

move_cursor:
    mov ah, 02h
    xor bh, bh
    int 10h
    ret

clear_screen:
    mov ax, 0003h
    int 10h
    ret

print_string:
    mov ah, 0Eh
.l:
    lodsb
    or al, al
    jz .d
    int 10h
    jmp .l
.d:
    ret

print_char:
    mov ah, 0Eh
    int 10h
    ret

; --- Data ---
score       dw 0
snake_len   dw 3
cur_dir     db 1
last_tick   db 0
move_tick   db 0
food_pos:
    food_pos_x db 0
    food_pos_y db 0
body        times MAX_SNAKE_LEN dw 0
score_msg   db "Snake Score: ", 0
msg_over    db "GAME OVER!", 0
