bits 16
cpu 8086
org 0x1000

; =============================================================================
; SYSTEM CALL JUMP TABLE
; =============================================================================
api_entry:
    jmp start           ; 0x1000:0000
    jmp print           ; 0x1000:0003
    jmp cls             ; 0x1000:0006
    jmp newline         ; 0x1000:0009
    jmp get_input       ; 0x1000:000C
    jmp wait_key        ; 0x1000:000F
    jmp reboot          ; 0x1000:0012
    jmp api_save_file   ; 0x1000:0015  <-- NEW: Save File API for Apps

; --- FAT12 Constants ---
%define ROOT_DIR_SECTOR 19
%define DATA_START      33

; =============================================================================
; KERNEL STARTUP
; =============================================================================
start:
    xor ax, ax
    mov ds, ax
    mov es, ax
    mov [current_drive], dl
    mov word [current_dir_lba], ROOT_DIR_SECTOR

    call cls
    mov si, welcome_msg
    call print

; =============================================================================
; MAIN SHELL LOOP
; =============================================================================
shell:
    xor ax, ax
    mov ds, ax
    mov es, ax
    mov ss, ax
    
    mov si, prompt_pre
    call print
    mov al, [current_drive]
    call print_hex_byte
    mov si, prompt_post
    call print

    mov di, input_buffer
    call get_input
    
    mov si, input_buffer
    cmp byte [si], 0
    je shell

    ; --- Command Comparison ---
    mov di, cmd_ls
    call strcmp
    jc .do_ls

    mov di, cmd_cd
    call strcmp_partial
    jc .do_cd

    mov di, cmd_type
    call strcmp_partial
    jc .do_type

    mov di, cmd_touch
    call strcmp_partial
    jc .do_touch

    mov di, cmd_mkdir
    call strcmp_partial
    jc .do_mkdir

    mov di, cmd_del
    call strcmp_partial
    jc .do_del

    mov di, cmd_ren
    call strcmp_partial
    jc .do_ren

    mov di, cmd_copy
    call strcmp_partial
    jc .do_copy

    mov di, cmd_setdr
    call strcmp_partial
    jc .do_setdrive

    mov di, cmd_run
    call strcmp_partial
    jc .do_run

    mov di, cmd_help
    call strcmp
    jc .do_help

    mov di, cmd_cls
    call strcmp
    jc .do_cls

    mov di, cmd_reboot
    call strcmp
    jc .do_reboot

    mov si, err_unk
    call print
    jmp shell

; --- Command Handlers ---

.do_help:
    mov si, help_text
    call print
    jmp shell

.do_ls:
    call list_files
    jmp shell

.do_type:
    call format_filename
    mov si, file_to_find
    call find_file
    jnc .not_found
    sub ax, 2
    add ax, DATA_START
    mov cx, 1
    mov bx, disk_buffer
    call read_sectors
    jc .disk_error
    mov si, disk_buffer
    call print
    call newline
    jmp shell

.do_cls:
    call cls
    jmp shell

.do_reboot:
    jmp 0xFFFF:0x0000

.do_setdrive:
.sd_skip:
    lodsb
    cmp al, ' '
    je .sd_skip
    dec si
    
    call hex_to_byte
    push ax
    
    mov dl, al
    xor ax, ax
    int 0x13
    pop ax
    jc .disk_error
    
    mov [current_drive], al
    mov word [current_dir_lba], ROOT_DIR_SECTOR
    mov si, msg_drive_changed
    call print
    jmp shell

.disk_error:
    mov si, err_disk
    call print
    jmp shell

.do_del:
    call format_filename
    mov si, file_to_find
    call delete_file
    jmp shell

.do_ren:
    call format_filename 
    mov si, file_to_find
    mov di, rename_buffer
    mov cx, 11
    rep movsb
    call .parse_second_arg
    call format_filename
    call rename_file
    jmp shell

.do_copy:
    call format_filename
    mov si, file_to_find
    mov di, rename_buffer
    mov cx, 11
    rep movsb
    
    call .parse_second_arg
    call format_filename
    
    push si
    mov si, rename_buffer
    call find_file
    pop si
    jnc .not_found
    
    push ax 
    mov si, file_to_find
    call create_file_entry
    
    pop ax
    sub ax, 2
    add ax, DATA_START
    mov cx, 1
    mov bx, disk_buffer
    call read_sectors
    jc .disk_error
    
    call write_current_sector 
    jmp shell

.parse_second_arg:
    lodsb
    or al, al
    jz .arg_err
    cmp al, ' '
    jne .parse_second_arg
.skip_sp:
    lodsb
    cmp al, ' '
    je .skip_sp
    dec si
    ret
.arg_err:
    pop ax
    jmp shell

.do_cd:
.cd_skip:
    cmp byte [si], ' '
    jne .cd_check_special
    inc si
    jmp .cd_skip

.cd_check_special:
    cmp byte [si], '/'
    je .go_root
    cmp byte [si], '.'
    jne .regular_cd
    cmp byte [si+1], '.'
    jne shell 

    mov ax, [current_dir_lba]
    cmp ax, ROOT_DIR_SECTOR
    je shell

    mov bx, disk_buffer
    mov cx, 1
    call read_sectors
    jc .disk_error
    
    mov di, disk_buffer
    mov cx, 16
.dot_loop:
    cmp byte [di], '.'
    jne .dot_next
    cmp byte [di+1], '.'
    je .dot_found
.dot_next:
    add di, 32
    loop .dot_loop
    jmp .not_found

.dot_found:
    mov ax, [di + 26]
    or ax, ax
    jz .go_root
    sub ax, 2
    add ax, DATA_START
    mov [current_dir_lba], ax
    jmp shell

.go_root:
    mov word [current_dir_lba], ROOT_DIR_SECTOR
    jmp shell

.regular_cd:
    call format_filename
    mov si, file_to_find
    call find_file
    jnc .not_found
    sub ax, 2
    add ax, DATA_START
    mov [current_dir_lba], ax
    jmp shell

.do_mkdir:
    call format_filename
    mov si, file_to_find
    call create_directory_entry
    jmp shell

.do_touch:
    call format_filename
    mov si, file_to_find
    xor ax, ax
    call create_file_entry
    jmp shell

.do_run:
    call format_filename
    mov si, file_to_find
    call find_file
    jnc .not_found
    
    mov bx, 0x2000
    mov es, bx
    xor bx, bx
    sub ax, 2
    add ax, DATA_START
    mov cx, 8           
    call read_sectors
    jc .disk_error

    push ds
    mov ax, 0x2000
    mov ds, ax
    mov es, ax
    call 0x2000:0000
    pop ds
    
    mov si, msg_back
    call print
    jmp shell

.not_found:
    mov si, err_nf
    call print
    jmp shell

; =============================================================================
; FILE SYSTEM & DISK LOGIC
; =============================================================================

; NEW API FUNCTION: Save file from App
; Input: DS:SI = Filename, ES:BX = Data Buffer
; --- Update this in your KERNEL code ---

api_save_file:
    ; --- 1. Save EVERY register manually (8086 compatible) ---
    push ds         ; [BP+16] - App Data Segment (0x2000)
    push es         ; [BP+14]
    push ax         ; [BP+12]
    push bx         ; [BP+10] - App Buffer Offset (text_buffer)
    push cx         ; [BP+8]
    push dx         ; [BP+6]
    push si         ; [BP+4]  - App Filename Offset (filename_bin)
    push di         ; [BP+2]
    push bp         ; [BP+0]  <- Current SP
    
    mov bp, sp      ; Set up the stack frame

    ; --- 2. Switch to Kernel's "World" ---
    mov ax, 0x1000
    mov ds, ax      ; Now DS is Kernel
    mov es, ax      ; Now ES is Kernel

    ; --- 3. Format Filename ---
    ; We need the Filename offset from the App (pushed as SI)
    mov si, [bp+4]  
    ; We need to point DS back to App for a moment to read the string
    mov ax, [bp+16]
    push ds         ; Save Kernel DS
    mov ds, ax      ; DS = App Segment
    call format_filename
    pop ds          ; Restore Kernel DS

    ; --- 4. Create File Entry ---
    mov si, file_to_find
    xor ax, ax      ; Attribute 0
    call create_file_entry

    ; --- 5. Copy Data (App -> Kernel Disk Buffer) ---
    ; Destination: Kernel disk_buffer
    mov di, disk_buffer
    ; Source: App text_buffer (stored in BX)
    mov si, [bp+10]
    ; Segments: ES = Kernel, DS = App
    mov ax, [bp+16]
    mov ds, ax      
    
    mov cx, 256     ; 512 bytes
    rep movsw

    ; --- 6. Finalize and Write ---
    mov ax, 0x1000
    mov ds, ax      ; Back to Kernel DS for the disk write
    call write_current_sector

    ; --- 7. Restore and Exit ---
    pop bp
    pop di
    pop si
    pop dx
    pop cx
    pop bx
    pop ax
    pop es
    pop ds
    retf            ; Far return to App
    
list_files:
    mov ax, [current_dir_lba]
    mov cx, 1
    mov bx, disk_buffer
    call read_sectors
    jnc .continue
    mov si, err_disk
    call print
    ret
.continue:
    mov si, disk_buffer
    mov cx, 16
.loop:
    push cx
    cmp byte [si], 0
    je .done
    cmp byte [si], 0xE5
    je .skip
    
    mov bx, 0
.pn: mov al, [si+bx]
    call print_char_bios
    inc bx
    cmp bx, 8
    jne .pn
    mov al, '.'
    call print_char_bios
.pe: mov al, [si+bx]
    call print_char_bios
    inc bx
    cmp bx, 11
    jne .pe
    test byte [si+11], 0x10
    jz .f
    mov si, msg_dir_tag
    call print
    pop cx
    push cx
    mov ax, 16
    sub ax, cx
    mov dx, 32
    mul dx
    mov si, disk_buffer
    add si, ax
    jmp .n
.f: call newline
.n: add si, 32
    pop cx
    loop .loop
.done:
    pop cx
    ret
.skip:
    add si, 32
    pop cx
    loop .loop
    ret

delete_file:
    mov ax, [current_dir_lba]
    mov cx, 1
    mov bx, disk_buffer
    call read_sectors
    jc .fail
    mov di, disk_buffer
    mov dx, 16
.search:
    push di
    push si
    mov cx, 11
    rep cmpsb
    pop si
    pop di
    je .found
    add di, 32
    dec dx
    jnz .search
    mov si, err_nf
    call print
    ret
.found:
    mov byte [di], 0xE5
    call write_current_sector
    ret
.fail:
    mov si, err_disk
    call print
    ret

rename_file:
    mov ax, [current_dir_lba]
    mov cx, 1
    mov bx, disk_buffer
    call read_sectors
    jc .fail
    mov di, disk_buffer
    mov dx, 16
.search:
    push di
    mov si, rename_buffer
    mov cx, 11
    rep cmpsb
    pop di
    je .found
    add di, 32
    dec dx
    jnz .search
    mov si, err_nf
    call print
    ret
.found:
    mov si, file_to_find
    mov cx, 11
    rep movsb
    call write_current_sector
    ret
.fail:
    mov si, err_disk
    call print
    ret

create_file_entry:
    push ax
    mov al, 0x00 
    jmp create_entry_common

create_directory_entry:
    push ax
    mov al, 0x10 
create_entry_common:
    mov ah, al
    mov ax, [current_dir_lba]
    mov cx, 1
    mov bx, disk_buffer
    call read_sectors
    jc .fail
    mov di, disk_buffer
    mov cx, 16
.search:
    cmp byte [di], 0x00
    je .found_slot
    cmp byte [di], 0xE5
    je .found_slot
    add di, 32
    loop .search
    pop ax
    mov si, err_full
    call print
    ret
.found_slot:
    push di
    mov si, file_to_find
    mov cx, 11
    rep movsb
    pop di
    pop ax
    mov [di+11], al
    call write_current_sector
    ret
.fail:
    pop ax
    mov si, err_disk
    call print
    ret

write_current_sector:
    push ax
    push bx
    push cx
    push dx

    ; --- 1. Reset Disk System (Crucial for some BIOS) ---
    push ax
    xor ax, ax
    mov dl, [current_drive]
    int 0x13
    pop ax

    ; --- 2. Calculate CHS ---
    mov ax, [current_dir_lba]
    mov bx, 18          ; Sectors per track
    xor dx, dx
    div bx              ; AX = total tracks, DX = sector-1
    inc dl              ; DL = sector number (1-based)
    mov cl, dl          ; CL = Sector
    
    xor dx, dx
    mov bx, 2           ; Number of heads
    div bx              ; AX = cylinder, DX = head
    
    mov ch, al          ; CH = Cylinder (Lower 8 bits)
    mov dh, dl          ; DH = Head
    mov dl, [current_drive]

    ; --- 3. Execute Write ---
    mov ax, 0x0301      ; AH=03 (Write), AL=01 (1 sector)
    mov bx, disk_buffer ; Source buffer in Kernel
    int 0x13
    
    ; If it fails, the carry flag is set. 
    ; For now, we just stop the loop if it fails.
    jnc .success
    ; Optional: print a '.' to show a retry happened
    
.success:
    pop dx
    pop cx
    pop bx
    pop ax
    ret

find_file:
    push si
    mov ax, [current_dir_lba]
    mov cx, 1
    mov bx, disk_buffer
    call read_sectors
    jc .fail
    mov di, disk_buffer
    mov dx, 16
.search:
    push di
    push si
    mov cx, 11
    rep cmpsb
    pop si
    pop di
    je .found
    add di, 32
    dec dx
    jnz .search
    pop si
    clc
    ret
.found:
    mov ax, [di + 26]
    pop si
    stc
    ret
.fail:
    pop si
    clc
    ret

; =============================================================================
; HELPERS
; =============================================================================

hex_to_byte:
    lodsb
    call .nibble
    shl al, 1
    shl al, 1
    shl al, 1
    shl al, 1
    mov bl, al
    lodsb
    call .nibble
    or al, bl
    ret
.nibble:
    cmp al, 'a'
    jb .not_lower
    sub al, 32
.not_lower:
    cmp al, '9'
    jbe .num
    sub al, 7
.num: 
    sub al, '0'
    and al, 0x0F
    ret

print_hex_byte:
    push ax
    push bx
    mov bl, al
    mov cl, 4
    shr al, cl
    call .h_dig
    mov al, bl
    and al, 0x0F
    call .h_dig
    pop bx
    pop ax
    ret
.h_dig:
    add al, '0'
    cmp al, '9'
    jbe .h_out
    add al, 7
.h_out:
    mov ah, 0x0E
    int 0x10
    ret

print:
    push ax
    push si
.l: lodsb
    or al, al
    jz .d
    mov ah, 0x0E
    int 0x10
    jmp .l
.d: pop si
    pop ax
    ret

print_char_bios:
    mov ah, 0x0E
    int 0x10
    ret

cls:
    mov ax, 0x0003
    int 0x10
    ret

newline:
    mov al, 13
    call print_char_bios
    mov al, 10
    call print_char_bios
    ret

get_input:
    xor cx, cx
.l: mov ah, 0
    int 0x16
    cmp al, 13
    je .e
    cmp al, 8
    je .b
    call print_char_bios
    stosb
    inc cx
    jmp .l
.b: or cx, cx
    jz .l
    dec di
    dec cx
    mov al, 8
    call print_char_bios
    mov al, ' '
    call print_char_bios
    mov al, 8
    call print_char_bios
    jmp .l
.e: mov al, 0
    stosb
    call newline
    ret

wait_key:
    xor ah, ah
    int 0x16
    ret

reboot:
    jmp 0xFFFF:0x0000

strcmp:
    push si
    push di
.l: mov al, [si]
    mov bl, [di]
    cmp al, bl
    jne .d
    or al, al
    jz .s
    inc si
    inc di
    jmp .l
.d: pop di
    pop si
    clc
    ret
.s: pop di
    pop si
    stc
    ret

strcmp_partial:
    push si
    push di
.l: mov bl, [di]
    or bl, bl
    jz .m
    mov al, [si]
    cmp al, bl
    jne .n
    inc si
    inc di
    jmp .l
.m: mov [temp_ptr], si
    pop di
    pop si
    mov si, [temp_ptr]
    stc
    ret
.n: pop di
    pop si
    clc
    ret

format_filename:
    push ax
    push cx
    push si
    push di
    mov di, file_to_find
    mov cx, 11
    mov al, ' '
    rep stosb
    mov di, file_to_find
    mov cx, 8
.n: lodsb
    or al, al
    jz .d
    cmp al, ' '
    jbe .d
    cmp al, '.'
    je .ext
    stosb
    loop .n
.ext:
    cmp al, '.'
    jne .d
    mov di, file_to_find + 8
    mov cx, 3
.e: lodsb
    or al, al
    jz .d
    stosb
    loop .e
.d: pop di
    pop si
    pop cx
    pop ax
    ret

read_sectors:
    push ax
    push bx
    push cx
    push dx
.loop:
    push ax
    push cx
    push bx
    mov bx, 18
    xor dx, dx
    div bx
    inc dl
    mov cl, dl
    xor dx, dx
    mov bx, 2
    div bx
    mov ch, al
    mov dh, dl
    mov dl, [current_drive]
    mov ax, 0x0201
    pop bx
    int 0x13
    jc .error
    pop cx
    pop ax
    add bx, 512
    inc ax
    loop .loop
    clc
    pop dx
    pop cx
    pop bx
    pop ax
    ret
.error:
    pop cx
    pop ax
    stc
    pop dx
    pop cx
    pop bx
    pop ax
    ret

; =============================================================================
; DATA SECTION
; =============================================================================
welcome_msg        db "FerroOS v1.0 - The Iron Release", 13, 10, 0
prompt_pre         db "Drive 0x", 0
prompt_post        db "> ", 0

cmd_ls             db "ls", 0
cmd_run            db "run ", 0
cmd_cls            db "cls", 0
cmd_help           db "help", 0
cmd_reboot         db "reboot", 0
cmd_cd             db "cd ", 0
cmd_mkdir          db "mkdir ", 0
cmd_touch          db "touch ", 0
cmd_type           db "type ", 0
cmd_del            db "del ", 0
cmd_ren            db "ren ", 0
cmd_copy           db "copy ", 0
cmd_setdr          db "setdrive ", 0

help_text          db "ls, cd, type, touch, mkdir, del, ren, copy, run, cls, reboot", 13, 10, 0
err_unk            db "Unknown command.", 13, 10, 0
err_nf             db "File/Dir not found.", 13, 10, 0
err_full           db "Directory full!", 13, 10, 0
err_disk           db "Disk error! (Drive may not exist)", 13, 10, 0
msg_back           db "App finished.", 13, 10, 0
msg_dir_tag        db "  <DIR>", 13, 10, 0
msg_drive_changed  db "Drive changed. Root loaded.", 13, 10, 0

current_drive      db 0
current_dir_lba    dw 19
temp_ptr           dw 0
file_to_find       times 12 db 0
rename_buffer      times 12 db 0
input_buffer       times 64 db 0
disk_buffer        equ 0x8000
