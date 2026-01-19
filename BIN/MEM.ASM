bits 16
cpu 8086
org 0x0000

start:
    push cs
    pop ds

    mov si, msg_mem
    call print_string

    ; BIOS Get Memory Size
    int 0x12        ; Returns AX = number of contiguous 1KB blocks
    
    call print_int  ; Custom function to print the number in AX
    
    mov si, msg_kb
    call print_string

    retf

print_int:          ; Simple routine to print AX as decimal
    mov bx, 10
    xor cx, cx
.l1:
    xor dx, dx
    div bx          ; AX / 10
    push dx         ; Push remainder
    inc cx
    test ax, ax
    jnz .l1
.l2:
    pop ax
    add al, '0'
    mov ah, 0x0E
    int 0x10
    loop .l2
    ret

print_string:
    mov ah, 0x0E
.l: lodsb
    or al, al
    jz .d
    int 0x10
    jmp .l
.d: ret

msg_mem db "Conventional RAM detected: ", 0
msg_kb  db " KB", 13, 10, 0
