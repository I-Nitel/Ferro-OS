bits 16
cpu 8086
org 0x0000

start:
    push cs
    pop ds

    mov si, msg_calc
    call print_string

    ; Perform Math: 123 + 456
    mov ax, 123
    add ax, 456

    ; Result is in AX, now print it
    call print_int

    mov si, msg_done
    call print_string

    ; Wait for key
    xor ah, ah
    int 0x16
    retf

; --- Local Functions ---
print_int:
    mov bx, 10
    xor cx, cx
.div_loop:
    xor dx, dx
    div bx          ; Divide AX by 10. Remainder in DX.
    push dx         ; Save remainder (the digit)
    inc cx
    test ax, ax
    jnz .div_loop
.print_loop:
    pop ax
    add al, '0'     ; Convert to ASCII
    mov ah, 0x0E
    int 0x10
    loop .print_loop
    ret

print_string:
    mov ah, 0x0E
.l: lodsb
    or al, al
    jz .d
    int 0x10
    jmp .l
.d: ret

; --- Data ---
msg_calc db "Calculating 123 + 456... Result: ", 0
msg_done db 13, 10, "Press any key...", 0
