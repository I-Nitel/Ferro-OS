bits 16
cpu 8086
org 0x0000

start:
    ; Fix Data Segment so the app can find its own strings
    push cs
    pop ds
    
    mov si, hello_text
    call print_string
    
    ; Far return back to the address the Kernel's 'call' or 'retf' set up
    retf

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

hello_text db "SUCCESS: HELLO.BIN is running!", 13, 10, 0
