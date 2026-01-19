bits 16
cpu 8086
org 0x0000

start:
    ; Jump to FFFF:0000 (The x86 Reset Vector)
    jmp 0xFFFF:0x0000
