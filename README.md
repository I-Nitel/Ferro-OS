
# FerroOS: The Iron Release

A High-Integrity 16-Bit Bare-Metal Operating System
Current Status: v1.0 (Stable Core)

Target Hardware: IBM PC/XT (Intel 8088/8086) & Emulators (QEMU/Bochs)

## Project Overview

FerroOS is a monolithic, single-tasking operating system built entirely from the ground up in x86 Assembly. Unlike modern operating systems that rely on the safety nets of Protected Mode or 64-bit Long Mode, FerroOS operates in Real Mode.

In this environment, there are no guardrails. The kernel has direct, unmediated access to every byte of memory and every hardware port. This project serves as a "Digital Time Machine," recreating the era of computing where performance was measured in bytes and system stability was a matter of artisanal code craft.

### The "Why" Behind FerroOS
Modern software is built on layers of abstraction that hide how a computer actually works. FerroOS strips those layers away. It was created to:

Master BIOS Interrupts: Utilizing `Int 10h` for video, `Int 13h` for disk operations, and `Int 16h` for keyboard input.

Implement FAT12 Logic: Manually parsing File Allocation Tables to handle data persistence.

Explore Segmented Memory: Managing the `CS`, `DS`, `ES`, and `SS` registers to navigate the 1MB address space.


## Technical Architecture

#### The Boot Sequence
The transition from "Silicon" to "Software" is handled through a carefully choreographed sequence:

MBR Loading: The BIOS identifies the bootable signature `0xAA55` and loads the first 512 bytes of the floppy image to memory address `0x0000:0x7C00`.

FAT12 Traversal: The bootloader initializes, resets the disk controller, and searches the Root Directory (Sector 19) for a file named `KERNEL.BIN`.

Kernel Relocation: The bootloader reads the kernel from the data sectors (Sector 33+) and places it at the absolute segment `0x1000`.

Execution Handoff: The bootloader performs a `jmp 0x1000:0000`, handing total control to the FerroOS kernel.


| Memory Offset | Segment:Offset     | Description               |
| :-------- | :------- | :------------------------- |
| `0x00000` | `0000:0000` | **IVT**: Interrupt Vector Table (Handles hardware signals) |
| `0x00400` | `0000:0400` | **BDA**: BIOS Data Area (Hardware status flags) |
| `0x10000` | `1000:0000` | **FERRO_CORE**: The main Kernel image resides here. |
| `0x20000` | `2000:0000` | **APP_SPACE**: External binaries are loaded into this segment. |
| `0x80000` | `8000:0000` | **FORGE**: A 512-byte scratchpad for disk sector I/O. |
| `0xB8000` | `B800:0000` | **VGA_TEXT**: Direct memory-mapped access to the monitor. |



## Filesystem: The FAT12 Protocol

The filesystem is the heart of FerroOS. We use the FAT12 format to maintain compatibility with 1.44MB floppy disk standards.

1. **Sector 0**: Boot Sector (BPB - BIOS Parameter Block).

2. **Sectors 1-9**: FAT Table #1 (Primary cluster map).

3. **Sectors 10-18**: FAT Table #2 (Backup cluster map).

4. **Sectors 19-32**: Root Directory (Stores filenames, sizes, and starting clusters).

5. **Sectors 33-2879**: Data Region (Where your files actually live).


## The Syscall API
FerroOS provides a standardized **Jump Table** at the very beginning of the kernel. This allows programmers to write software for FerroOS without knowing the internal memory addresses of kernel functions.


| Entry Point | Name     | Function              |
| :-------- | :------- | :------------------------- |
| `0x1000:0003` | `print_str` | Prints a null-terminated string at `DS:SI`. |
| `0x1000:0006` | `clear_scr` | Wipes the screen and resets the cursor. |
| `0x1000:0012` | `load_file` | Searches for and loads a file into memory. |
| `0x1000:0015` | `save_file` | Commits a memory buffer back to the disk. |

## Deployment Guide

#### Building from Source
You will need the NASM (Netwide Assembler). To compile the kernel:

```bash
nasm -f bin kernel.asm -o KERNEL.BIN
nasm -f bin bootloader.asm -o BOOT.BIN
```

#### Creating the Floppy Image
You can use dd (Linux/Mac) or a dedicated image tool to stitch them together:

```bash
dd if=/dev/zero of=ferro_os.img bs=1024 count=1440
dd if=BOOT.BIN of=ferro_os.img conv=notrunc
# Use a FAT12 injector to add KERNEL.BIN to the image
```

#### Emulation
Run FerroOS instantly using QEMU:

```bash
qemu-system-i386 -drive format=raw,file=ferro_os.img
```


