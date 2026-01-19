bits 16
cpu 8086
org 0x0000

start:
    push cs
    pop ds

    mov cx, 15      ; Loop through 15 colors
.loop:
    push cx
    ; BIOS Set Background Color
    mov ah, 0x0B
    mov bh, 0x00    ; Set background/border
    mov bl, cl      ; Color index
    int 0x10

    ; Wait for a moment
    mov cx, 0xFFFF
.delay:
    nop
    loop .delay

    pop cx
    loop .loop

    ; Reset to Black
    mov ah, 0x0B
    xor bx, bx
    int 0x10

    retf
