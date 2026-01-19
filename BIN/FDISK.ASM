bits 16
cpu 8086
org 0x0000

start:
    push cs
    pop ds
    push cs
    pop es
    mov byte [current_drive], 0x80

main_loop:
    call clear_screen
    mov dh, 0
    mov dl, 0
    call move_cursor
    
    mov si, msg_header
    call print_string

    mov si, msg_drive
    call print_string
    mov al, [current_drive]
    call print_hex_byte
    call print_newline
    call print_newline

    call read_mbr
    jc .error

    mov si, msg_table_head
    call print_string

    mov bx, mbr_buffer
    add bx, 0x1BE
    xor cx, cx

.part_loop:
    push bx
    push cx
    
    ; Print Boot Indicator (*)
    mov al, [bx]
    cmp al, 0x80
    jne .not_active
    mov al, '*'
    jmp .print_idx
.not_active:
    mov al, ' '
.print_idx:
    call print_char
    
    mov al, cl
    add al, '1'
    call print_char
    mov si, msg_spacer
    call print_string

    mov al, [bx+4]          ; Partition ID
    call get_fs_name        
    call print_string
    
    mov ax, [bx+10]         ; LBA High
    call print_hex_word
    mov ax, [bx+8]          ; LBA Low
    call print_hex_word
    mov si, msg_spacer
    call print_string

    mov ax, [bx+14]         ; Size High
    call print_hex_word
    mov ax, [bx+12]         ; Size Low
    call print_hex_word

    call print_newline
    pop cx
    pop bx
    add bx, 16
    inc cx
    cmp cx, 4
    jne .part_loop
    jmp .input

.error:
    mov si, msg_err
    call print_string

.input:
    mov si, msg_menu
    call print_string

    xor ah, ah
    int 16h

    cmp al, 'q'
    je .exit
    cmp al, 'd'
    je .manual_drive
    cmp al, 'c'
    je .confirm_create
    cmp al, 'f'
    je .select_fs
    cmp al, 'h'
    je .hex_view
    cmp al, 'e'
    je .edit_entry
    cmp al, 'a'
    je .toggle_active
    jmp main_loop

.manual_drive:
    mov si, msg_sel_drive
    call print_string
    call input_hex_byte
    mov [current_drive], al
    jmp main_loop

.confirm_create:
    mov si, msg_confirm
    call print_string
    xor ah, ah
    int 16h
    cmp al, 'y'
    jne main_loop
    call create_simple_mbr
    jmp main_loop

.select_fs:
    mov si, msg_fs_menu
    call print_string
    xor ah, ah
    int 16h
    cmp al, '1'
    je .do_fat12
    cmp al, '2'
    je .do_fat16
    jmp main_loop

.do_fat12:
    mov byte [temp_fs_type], 0x01
    call format_partition
    jmp main_loop

.do_fat16:
    mov byte [temp_fs_type], 0x06
    call format_partition
    jmp main_loop

.hex_view:
    call show_hex_dump
    jmp main_loop

.toggle_active:
    mov si, msg_edit_idx
    call print_string
    xor ah, ah
    int 16h
    sub al, '1'
    cmp al, 3
    ja main_loop
    
    xor ah, ah
    mov bl, 16
    mul bl
    mov bx, mbr_buffer + 0x1BE
    add bx, ax
    
    ; Toggle logic: if 0x80 make 0, else make 0x80
    cmp byte [bx], 0x80
    je .deactivate
    mov byte [bx], 0x80
    jmp .save_active
.deactivate:
    mov byte [bx], 0x00
.save_active:
    call write_mbr
    jmp main_loop

.edit_entry:
    mov si, msg_edit_idx
    call print_string
    xor ah, ah
    int 16h
    sub al, '1'
    cmp al, 3
    ja main_loop
    
    xor ah, ah
    mov bl, 16
    mul bl
    mov bx, mbr_buffer + 0x1BE
    add bx, ax
    push bx

    mov si, msg_edit_id
    call print_string
    call input_hex_byte
    pop bx
    mov [bx+4], al
    
    call write_mbr
    jmp main_loop

.exit:
    retf

; --- New Input Helpers ---

input_hex_byte:
    xor ah, ah
    int 16h
    call print_char
    call hex_to_nibble
    mov cl, 4
    shl al, cl
    mov dl, al
    
    xor ah, ah
    int 16h
    call print_char
    call hex_to_nibble
    or al, dl
    ret

; --- Logic Functions ---

hex_to_nibble:
    cmp al, '9'
    jbe .num
    and al, 0xDF
    sub al, 7
.num:
    sub al, '0'
    and al, 0x0F
    ret

get_fs_name:
    cmp al, 0x00
    je .f_empty
    cmp al, 0x01
    je .f_fat12
    cmp al, 0x06
    je .f_fat16
    cmp al, 0x07
    je .f_ntfs
    cmp al, 0x0B
    je .f_fat32
    cmp al, 0x83
    je .f_linux
    mov si, type_unk
    ret
.f_empty:
    mov si, type_emp
    ret
.f_fat12:
    mov si, type_f12
    ret
.f_fat16:
    mov si, type_f16
    ret
.f_ntfs:
    mov si, type_ntf
    ret
.f_fat32:
    mov si, type_f32
    ret
.f_linux:
    mov si, type_lin
    ret

; --- Disk Functions ---

read_mbr:
    mov ax, 0201h
    mov cx, 0001h
    mov dx, 0000h
    mov dl, [current_drive]
    mov bx, mbr_buffer
    int 13h
    ret

write_mbr:
    mov ax, 0301h
    mov cx, 0001h
    mov dx, 0000h
    mov dl, [current_drive]
    mov bx, mbr_buffer
    int 13h
    ret

create_simple_mbr:
    mov di, mbr_buffer
    mov cx, 256
    xor ax, ax
    rep stosw
    mov word [mbr_buffer + 510], 0xAA55
    mov bx, mbr_buffer + 0x1BE
    mov byte [bx + 0], 0x80
    mov byte [bx + 4], 0x01
    mov word [bx + 8], 0x0001
    mov word [bx + 12], 0x0100
    call write_mbr
    ret

format_partition:
    call read_mbr
    mov al, [temp_fs_type]
    mov [mbr_buffer + 0x1BE + 4], al
    call write_mbr
    mov di, mbr_buffer
    mov cx, 256
    xor ax, ax
    rep stosw
    mov si, msg_fat_name
    mov di, mbr_buffer + 3
    mov cx, 8
    rep movsb
    mov word [mbr_buffer + 510], 0xAA55
    mov ax, 0301h
    mov cx, 0002h
    mov dx, 0000h
    mov dl, [current_drive]
    mov bx, mbr_buffer
    int 13h
    ret

show_hex_dump:
    call clear_screen
    mov si, mbr_buffer
    mov cx, 16
.line:
    push cx
    mov cx, 16
.byte:
    lodsb
    call print_hex_byte
    mov al, ' '
    call print_char
    loop .byte
    call print_newline
    pop cx
    loop .line
    mov si, msg_any_key
    call print_string
    xor ah, ah
    int 16h
    ret

; --- Standard Helpers ---

print_hex_word:
    push ax
    mov al, ah
    call print_hex_byte
    pop ax
    call print_hex_byte
    ret

print_hex_byte:
    push ax
    push bx
    mov bl, al
    mov cl, 4
    shr al, cl
    call .hex_digit
    mov al, bl
    and al, 0x0F
    call .hex_digit
    pop bx
    pop ax
    ret
.hex_digit:
    add al, '0'
    cmp al, '9'
    jbe .out
    add al, 7
.out:
    mov ah, 0Eh
    int 10h
    ret

clear_screen:
    mov ax, 0003h
    int 10h
    ret

move_cursor:
    mov ah, 02h
    xor bh, bh
    int 10h
    ret

print_string:
    push ax
    push si
.l:
    mov al, [si]
    inc si
    or al, al
    jz .d
    mov ah, 0Eh
    int 10h
    jmp .l
.d:
    pop si
    pop ax
    ret

print_char:
    mov ah, 0Eh
    int 10h
    ret

print_newline:
    mov al, 13
    call print_char
    mov al, 10
    call print_char
    ret

; --- Data ---
current_drive  db 0x80
temp_fs_type   db 0

msg_header     db "ColorOS Disk Utility v5", 13, 10, "-----------------------", 13, 10, 0
msg_drive      db "Drive: 0x", 0
msg_table_head db "B #|Type     |LBA Start|Size", 13, 10, "-- -|---------|---------|---------", 13, 10, 0
msg_spacer     db "|", 0
msg_err        db "Read Error!", 13, 10, 0
msg_menu       db 13, 10, "[D]rive [C]reate [F]ormat [E]dit [A]ctive [H]ex [Q]uit", 0
msg_confirm    db 13, 10, "CONFIRM WIPE? (Y/N): ", 0
msg_fs_menu    db 13, 10, "FS: [1] FAT12 [2] FAT16: ", 0
msg_sel_drive  db 13, 10, "Enter Hex Drive ID: ", 0
msg_edit_idx   db 13, 10, "Partition (1-4): ", 0
msg_edit_id    db 13, 10, "New Hex Type ID: ", 0
msg_any_key    db 13, 10, "Press any key...", 0
msg_fat_name   db "COLOROS "

type_emp       db "Empty    ", 0
type_f12       db "FAT12    ", 0
type_f16       db "FAT16    ", 0
type_ntf       db "NTFS     ", 0
type_f32       db "FAT32    ", 0
type_lin       db "Linux    ", 0
type_unk       db "Unknown  ", 0

mbr_buffer:    times 512 db 0
