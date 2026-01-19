bits 16
cpu 8086
org 0x0000

start:
    push cs
    pop ds
    
    mov si, msg_start
    call print_string

    ; 1. Set up the PIT (Timer)
    ; Send control byte 0xB6 to port 43h
    mov al, 0xB6
    out 0x43, al

    ; 2. Set the Frequency (approx 440Hz - A4 note)
    ; We send the divisor (1193180 / frequency)
    ; For ~440Hz, divisor is 2711 (0x0A97)
    mov ax, 0x0A97
    out 0x42, al        ; Send low byte
    mov al, ah
    out 0x42, al        ; Send high byte

    ; 3. Turn the speaker ON
    in al, 0x61         ; Read current port 61h state
    or al, 00000011b    ; Set bits 0 and 1 to enable speaker
    out 0x61, al

    ; 4. Wait for a short duration (Simple loop)
    mov cx, 0xFFFF
.delay:
    nop
    loop .delay

    ; 5. Turn the speaker OFF
    in al, 0x61
    and al, 11111100b   ; Clear bits 0 and 1
    out 0x61, al

    mov si, msg_end
    call print_string
    
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

; --- Data ---
msg_start db "Playing beep...", 13, 10, 0
msg_end   db "Done!", 13, 10, 0
