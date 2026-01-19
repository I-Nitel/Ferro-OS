bits 16
org 0x0000          ; Apps load at 0x2000:0000

; --- API Addresses ---
%define K_SEG  0x1000
%define K_PRT  0x0003
%define K_WAIT 0x000F

start:
    ; Setup local segments
    push cs
    pop ds
    push cs
    pop es

    ; Print Header
    mov si, msg_start
    call far_print

    ; Reset Disk System
    xor ax, ax
    mov dl, 0       ; Drive A:
    int 0x13

    ; Read Boot Sector (Sector 1, Head 0, Track 0)
    mov ax, 0x0201  ; Read 1 sector
    mov cx, 0x0001  ; Track 0, Sector 1
    mov dh, 0       ; Head 0
    mov dl, 0       ; Drive A:
    mov bx, buffer  ; Target buffer
    int 0x13
    jc .error

    ; Display first 8 bytes in Hex
    mov si, buffer
    mov cx, 8
.loop:
    lodsb
    call print_hex
    loop .loop

    jmp .exit

.error:
    mov si, msg_err
    call far_print

.exit:
    mov si, msg_end
    call far_print
    
    ; Wait for key using Kernel API
    call K_SEG:K_WAIT
    retf            ; Back to Kernel

; --- Helpers ---
far_print:
    call K_SEG:K_PRT
    ret

print_hex:
    push ax
    shr al, 4
    call .nibble
    pop ax
    and al, 0x0F
.nibble:
    add al, '0'
    cmp al, '9'
    jbe .out
    add al, 7
.out:
    mov ah, 0x0E
    int 0x10
    ret

section .data
    msg_start db "FDISK-Lite: Reading Boot Sector...", 13, 10, 0
    msg_end   db 13, 10, "Success. Press any key...", 0
    msg_err   db "Disk Error!", 13, 10, 0
    buffer    times 512 db 0
