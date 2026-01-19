bits 16
cpu 8086
org 0x0000

%define K_SEG 0x1000

start:
    push cs
    pop ds
    push cs
    pop es

    mov si, msg_header
    call local_print

editor_loop:
    xor ah, ah
    int 16h
    
    cmp al, 27            ; ESC
    je prompt_save
    cmp al, 8             ; Backspace
    je handle_backspace
    cmp al, 13            ; Enter
    je handle_enter

    mov ah, 0x0E
    int 10h
    
    mov di, [buffer_ptr]
    stosb
    mov [buffer_ptr], di
    jmp editor_loop

handle_backspace:
    cmp word [buffer_ptr], text_buffer
    je editor_loop
    dec word [buffer_ptr]
    mov ah, 0x0E
    mov al, 8
    int 10h
    mov al, ' '
    int 10h
    mov al, 8
    int 10h
    jmp editor_loop

handle_enter:
    mov al, 13
    call store_char
    mov al, 10
    call store_char
    mov ah, 0x0E
    mov al, 13
    int 10h
    mov al, 10
    int 10h
    jmp editor_loop

prompt_save:
    mov si, msg_ask_file
    call local_print
    
    ; Local filename input (simple)
    mov di, filename_bin
    mov bx, 0
.in:
    xor ah, ah
    int 16h
    cmp al, 13
    je .done
    stosb
    inc bx
    mov ah, 0x0E
    int 10h
    jmp .in
.done:
    mov al, 0
    stosb

    ; Call Kernel
    mov si, filename_bin    ; DS:SI
    mov bx, text_buffer     ; ES:BX (passed to kernel)
    call far [cs:k_save_file_ptr]

    mov si, msg_done
    call local_print
    xor ah, ah
    int 16h
    retf

store_char:
    mov di, [buffer_ptr]
    stosb
    mov [buffer_ptr], di
    ret

local_print:
    lodsb
    or al, al
    jz .d
    mov ah, 0x0E
    int 10h
    jmp local_print
.d: ret

k_save_file_ptr dw 0x0015, K_SEG
msg_header   db "NED v1.5", 13, 10, 0
msg_ask_file db 13, 10, "Filename: ", 0
msg_done     db 13, 10, "Saved!", 0
buffer_ptr   dw text_buffer
filename_bin times 12 db 0
text_buffer  equ 0x2000
