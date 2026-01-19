bits 16
cpu 8086
org 0x0000

start:
    ; Standard setup: Ensure DS points to our app's segment
    push cs
    pop ds
    
    ; 1. Print Welcome Message
    mov si, sys_msg
    call print_string
    
    ; 2. Show the current Code Segment (CS)
    mov si, cs_msg
    call print_string
    mov ax, cs
    call print_hex_word
    call print_newline

    ; 3. Show the current Stack Segment (SS)
    mov si, ss_msg
    call print_string
    mov ax, ss
    call print_hex_word
    call print_newline

    ; 4. Wait for a keypress before returning
    mov si, wait_msg
    call print_string
    xor ah, ah
    int 0x16

    ; 5. Return to Kernel
    retf

; --- Local Functions ---

print_string:
    mov ah, 0x0E
.loop:
    lodsb
    or al, al
    jz .done
    int 0x10
    jmp .loop
.done:
    ret

print_newline:
    mov ah, 0x0E
    mov al, 13
    int 0x10
    mov al, 10
    int 0x10
    ret

print_hex_word:
    push ax
    mov al, ah          ; Print high byte (AH)
    call print_hex_byte
    pop ax              ; Restore AX to print low byte (AL)
    call print_hex_byte
    ret

print_hex_byte:
    push ax
    ; Logic for 8086: Shift right 4 times manually
    shr al, 1
    shr al, 1
    shr al, 1
    shr al, 1
    call print_nibble
    pop ax
    and al, 0x0F        ; Mask out high nibble
    call print_nibble
    ret

print_nibble:
    cmp al, 10
    jl .digit
    add al, 7           ; Convert 10-15 to A-F
.digit:
    add al, '0'         ; Convert 0-9 to '0'-'9'
    mov ah, 0x0E
    int 0x10
    ret

; --- Data ---
sys_msg  db "ColorOS System Monitor", 13, 10, "--------------------", 13, 10, 0
cs_msg   db "Code Segment (CS): 0x", 0
ss_msg   db "Stack Segment (SS): 0x", 0
wait_msg db 13, 10, "Press any key to return...", 0
