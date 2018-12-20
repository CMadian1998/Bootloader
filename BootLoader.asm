;=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=	;1)
;Teremity System One Bootloader			;2)
;						;3)
;=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=	;4)
org 0x7C00					;5) Memory Address where the boot loader will be loaded too
jmp short Boot					;6)
nop						;7)
;=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=	;8)
bpbOEM			db "AAAAAAAA"		;9) OEM label for the disk (8 bytes)		
bpbBytesPerSector:  	DW 0x0200		;10) The size of the sectors in bytes
bpbSectorsPerCluster: 	DB 0x01			;11) How many sectors make up a cluster
bpbReservedSectors: 	DW 0x0001		;12) How many sectors exist before the first FAT. 1 for the boot loader.
bpbNumberOfFATs: 	DB 0x02			;13) How many FAT's are on the disk: 2, one used as a backup
bpbRootEntries: 	DW 0x00E0		;14) How many file entries in the root directory.
bpbTotalSectors: 	DW 0x0B40		;15) How many sectors exist on this disk. If the values doesn't fit in this field, it is set to zero and the value is stored in bpbTotalSectorsBig
bpbMedia: 		DB 0xf0			;16) The type of media
bpbSectorsPerFAT: 	DW 0x0009		;17) how many sectors the FAT table takes up on disk
bpbSectorsPerTrack: 	DW 0x0012		;18) how many sectors fit on one track
bpbHeadsPerCylinder: 	DW 0x0002		;19) how many physical heads 
bpbHiddenSectors: 	DD 0x00000000		;20) How many sectors exist before the start of the volume after both sector
bpbTotalSectorsBig: 	DD 0x00000000		;21)
bsDriveNumber: 		DB 0x00			;22)
bsUnused: 		DB 0x00			;23)
bsExtBootSignature: 	DB 0x29			;24)
bsSerialNumber:		DD 0xa0a1a2a3		;25)
bsVolumeLabel: 		DB "AAAAAAAASYS"	;26)
bsFileSystem: 		DB "FAT12   "		;27)
;=-=-=-=-=-=-=-=-=-=-=-=-=-=-=			;28)
Boot:						;29)
	cli					;30) Stops interrupts
	xor ax, ax				;31) Puts the value 0 in the AX register
	mov es, ax				;32) AX is used to modify the Segment registers
	mov ds, ax				;33)
	mov ss, ax				;34)
	mov sp, 0x7C00				;35)
	sti					;36) Starts Interrupts
LoadFAT:					;38)
	pusha					;39)
	mov si, READING				;40)
	call BPrint				;41)
	popa					;42)
	mov ax, 0x0001
	call LBAToCHS
	mov al, 0x09				;46)
	mov bx, 0x7E00				;47)
	call ReadDisk				;48)
FindRootDir:					;49)
	mov ax, 0x0013				;53) AX now equals the LBA of the Root Directory
	call LBAToCHS				;54) 
	mov al, 14				;55) The number of sectors loaded into memory
	mov bx, 0x9000				;56) The memory address where the sectors are loaded into
	call ReadDisk				;57) 
	xor cx, cx				;59) Acts as a counter for the current Entry in the root dir
	mov bx, 0x90C0
FindKernel:					;61)
	inc cl
	mov si, bx				;62)
	mov di, TMP				;63)
.SaveName:					;64)
	lodsb
	stosb					;66)
	cmp ch, 0x0A				;67)
	je .Stored				;68)
	inc ch					;69)
	jmp .SaveName				;70)
.Stored:					;71)
	mov si, TMP				;71)
	call BPrint
	mov di, KER					
	xor ch, ch				;73)
	cld					;)
.CMPLoop:					;)
	mov ah, [si]
	mov dh, [di]
	cmp ah, dh
	jne .KerNF
	inc si
	inc di					;) compare this byte to the same byte from SI and then increment both. Set Zero flag if they are equal
	cmp ch, 0x0A				;) Check for the end of the kernel
	je .KerF				;) If we are at the end, jump to the kernel found label
	inc ch					;) if not increment CH
	jmp .CMPLoop 				;) And Repeat
.KerNF:						;)
	add bx, 0x20				;)
	cmp cl, 0xE1				;)
	jne FindKernel				;)
	pusha					;)
	mov si, KERNF				;)
	call BPrint				;)
	cli					;)
	hlt					;)
.KerF:						;)
	pusha 					;)
	mov si, KERF				;)
	call BPrint				;)
	popa					;)
	add bx, 0x001A				;)
	mov al, [bx]				;) These two need to be reversed because of the little endian format
	mov ah, [bx + 1]			;)
	push ax					;)
	add ax, 0x1F				;)
	call LBAToCHS				;)
	mov al, 1				;)
	mov bx, 0xAD00				;)
	call ReadDisk				;)
	pop ax					;)
.CheckIfEven:					;)
	test ax, 1				;)
	jz .Even				;)
.Odd:						;)
	push bx					;)Move the memory of the start of the kernel to the stack
	mov bx, 0x0003				;)
	mul bx					;)
	dec bx					;)
	div bx					;)AX should now equal where the high four bits of the next cluster is stored
	mov bx, 0x7E00				;)This is where the FAT is in memory
	add bx, ax				;)
	mov cl, [bx]				;)This is the last four bits which come from the highest nibble of the byte
	mov ch, [bx + 1]			;)this is the first eight bits
	and cl, 0xF0				;)this removes the the lower nibble leaving only the top nibble
	shr cx, 4				;)CX should now equal the next cluster of the kernel
	cmp cx, 0x0FFF				;)no more sectors remain to be loaded
	je .StartExecuting			;)start executing the kernel code
	add cx, 0x1F				;)convert to LBA 
	mov ax, cx				;)
	call LBAToCHS				;)convert to CHS
	mov ax, 0x01				;)
	pop bx					;)Get the memory location of the previous sector back
	add bx, 0x0200				;)Jump a whole sector in memory
	call ReadDisk				;)
	mov ax, cx				;)AX now equals the next FAT index
	jmp .CheckIfEven			;)Repeat Process
.Even:						;)
	push bx					;)Move the memory of the start of the kernel to the stack
	mov bx, 0x0003				;)
	mul bx					;)
	dec bx					;)
	div bx					;)AX should now equal where the high four bits of the next cluster is stored
	mov bx, 0x7E00				;)This is where the FAT is in memory
	add bx, ax				;)
	mov cl, [bx]				;)This is the last eight bits 
	mov ch, [bx + 1]			;)this is the first four bits which come from the lowest nibble of the byte
	and cl, 0x0F				;)this removes the the lower nibble leaving only the top nibble
	cmp cx, 0x0FFF				;)no more sectors remain to be loaded
	je .StartExecuting			;)start executing the kernel code
	add cx, 0x1F				;)convert to LBA 
	mov ax, cx				;)
	call LBAToCHS				;)convert to CHS
	mov ax, 0x01				;)
	pop bx					;)Get the memory location of the previous sector back
	add bx, 0x0200				;)Jump a whole sector in memory
	call ReadDisk				;)
	mov ax, cx				;)AX now equals the next FAT index
	jmp .CheckIfEven			;)Repeat Process				
.StartExecuting:				;)
	jmp 0xAD00:0x0000			;)
;=-=-=-=-=-=-=-=-=-=-=-=-=-=-=			;)
BPrint:						;)
	mov ah, 0x0E				;)
.Loop:						;)
	lodsb					;)
	cmp al, 0				;)
	je .Done				;)
	int 0x10				;)
	jmp .Loop				;)
.Done:						;)
	ret					;)
WriteHex:					;)
	mov ah, 0x0E				;)
	lea bx, [HEX]				;)
	mov ch, al				;)
	shr al, 4				;) AL now equals the Upper nibble
	and al, 0x0F				;)
	xlat					;)
	int 0x10				;)
	shl ch, 4				;)
	shr ch, 4				;)
	mov al, ch				;)
	and al, 0x0F				;)
	xlat					;)
	int 0x10				;)
	ret					;)
;=-=-=-=-=-=-=-=-=-=-=-=-=-=-=			;)
ReadDisk:					;)
	mov ah, 0x02				;)
	int 0x13				;)
	jc DiskError				;)
	ret					;)
DiskError:					;)
	pusha					;)
	mov si, DISKERR				;)
	call BPrint				;)
	popa					;)
	mov al, ah				;)
	call WriteHex				;)
	cli					;)
	hlt					;)
;=-=-=-=-=-=-=-=-=-=-=-=-=-=-=			;)
LBAToCHS:					;)
	xor dx, dx				;) Upper 16-bit of 32-bit value set to 0 for DIV
	div word [bpbSectorsPerTrack]		;) 32-bit by 16-bit DIV : LBA / SPT
	mov cl, dl				;) CL = S = LBA mod SPT
	inc cl					;) CL = S = (LBA mod SPT) + 1
	xor dx, dx				;) Upper 16-bit of 32-bit value set to 0 for DIV
	div word [bpbHeadsPerCylinder]		;) 32-bit by 16-bit DIV : (LBA / SPT) / HEADS
	mov dh, dl				;) DH = H = (LBA / SPT) mod HEADS
   	mov dl, [bsDriveNumber]			;) boot drive
   	mov ch, al				;) CH = C(lower 8 bits) = (LBA / SPT) / HEADS
   	shl ah, 6				;) Store upper 2 bits of 10-bit Cylinder into
    	or cl, ah				;) upper 2 bits of Sector (CL):: DH = H, CX = C/S
	ret					;)
;=-=-=-=-=-=-=-=-=-=-=-=-=-=-=			;)
READING db 'Read',0x0A,0
TMP times 11 db 0, 
KER db 'KERNEL  SYS'
KERF db 'Loading', 0x0A,0
KERNF db 'No Kernel',0
DISKERR db 'ERR AH:0x',0
HEX db '0123456789ABCDEF'
times 510 - ($-$$) db 0
dw 0xAA55