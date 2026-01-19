bits 16
cpu 8086
org 0x0000

start:
    push cs
    pop ds

    mov si, msg_start
    call print_string

.loop:
    ; BIOS Get Keystroke
    mov ah, 0x00
    int 0x16        ; Returns: AH = Scancode, AL = ASCII

    push ax         ; Save result
    
    mov si, msg_code
    call print_string

    pop ax
    mov al, ah      ; We want to print the Scancode in AH
    call print_hex_byte
    call print_newline

    cmp al, 0x01    ; If the scancode is 1 (ESC key), exit
    je .exit
    jmp .loop

.exit:
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

print_newline:
    mov ah, 0x0E
    mov al, 13
    int 0x10
    mov al, 10
    int 0x10
    ret

print_hex_byte:
    push ax
    mov cl, 4
    shr al, cl      ; This works if your assembler/CPU level allows it
    ; If not, use: shr al, 1 (four times)
    call .nibble
    pop ax
.nibble:
    and al, 0x0F
    add al, '0'
    cmp al, '9'
    jle .p
    add al, 7
.p: mov ah, 0x0E
    int 0x10
    ret

msg_start db "Scancode Debugger. Press ESC to exit.", 13, 10, 0
msg_code  db "Hex Scancode: ", 0
