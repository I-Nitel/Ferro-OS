bits 16
cpu 8086
org 0x0000

start:
    push cs
    pop ds

.retry:
    mov si, msg_prompt
    call print_string

    ; Get user input
    mov di, buffer
    call get_input

    ; Compare input with "secret"
    mov si, buffer
    mov di, password
    call strcmp
    jc .success

    mov si, msg_fail
    call print_string
    jmp .retry

.success:
    mov si, msg_ok
    call print_string
    retf

; --- Local Functions ---
print_string:
    mov ah, 0x0E
.l: lodsb
    or al, al
    jz .d
    int 0x10
    jmp .l
.d: ret

get_input:
    xor cx, cx
.l: mov ah, 0x00
    int 0x16
    cmp al, 13      ; Enter key?
    je .done
    mov ah, 0x0E
    int 0x10
    stosb
    inc cx
    jmp .l
.done:
    mov byte [di], 0
    mov al, 13
    int 0x10
    mov al, 10
    int 0x10
    ret

strcmp:
.l: mov al, [si]
    mov bl, [di]
    cmp al, bl
    jne .fail
    or al, al
    jz .match
    inc si
    inc di
    jmp .l
.match:
    stc
    ret
.fail:
    clc
    ret

; --- Data ---
password   db "gemini", 0   ; Change this to whatever you want
msg_prompt db "Enter Password: ", 0
msg_fail   db "Access Denied!", 13, 10, 0
msg_ok     db "Access Granted. Returning to shell...", 13, 10, 0
buffer     times 32 db 0
