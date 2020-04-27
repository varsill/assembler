

.MODEL SMALL
INCLUDE fonts.asm
zoom    db 4 dup('$')
tekst   db 240 dup('$')
zoom_int db 0

err_zla_liczba_arg db 'Zla liczba argumentow!$'
err_zoom_za_duzy db 'Za duzy zoom!$'
err_zle_znaki_w_zoomie db 'W zoomie sa znaki innie niz cyfry!$'
err_brak_miejsca db 'Brak miejsca na tekst$'
dane1 ends
.CODE
.386
code1 segment
start:
	;inicjowanie stosu
    mov sp, offset wstosu
    mov ax, seg wstosu
    mov ss, ax

    call wczytanie_danych
    mov ax, seg tekst
    mov ds, ax
    mov si, offset tekst
	
	
zakonczenie_wypisywania:
	call zoom_string_to_int
	
	;call tekst_length
	;cmp cx, 8
	;je blad_zla_liczba_arg
	
	;.exit
	;initialize graphics mode
	mov ax,13h;MCGA 320x200 256 color graphics mode
	mov bl, 9
	Int 10h



	
	;print letter
			
	mov ax, seg tekst
	mov ds, ax
	mov di, offset tekst
	mov dl, byte ptr ds:[di]
	mov bx, 0
	mov ax, 0
	call print_string
	;call print_letter
	call wait_for_key
	;go back to text mode
	mov ax,3
	int 10h
	jmp koniec_programu


koniec_programu:
	mov ah,4ch  ; zakoncz program i wroc do systemu
	int 021h


wait_for_key:
	mov ax, 1
	int 16h
	jnz wait_for_key

	ret






;clear_screen:
; mov ax,0A000h
; mov es,ax ;ES points to the video memory.
; mov dx,03C4h ;dx = indexregister
; mov ax,0F02h ;INDEX = MASK MAP, 
; out dx,ax ;write all the bitplanes.
; mov di,0 ;DI pointer in the video memory.
; mov cx,8000 ;(320 * 200)/8 = 8000
; mov ax,00h ;write to every pixel.
; rep stosb ;fill the screen
;ret
print_string:
	
	mov ax, seg zoom_int
	mov ds, ax
	mov di, offset zoom_int
	movzx cx, byte ptr ds:[di];ch holds value of zoom
	
	mov al, 8
	mul cl
	mov cx, ax;cx (in fact cl) holds zoom*8
	push cx
	;sprawdzanie, czy na ekranie jest wystarczajaco miejsca
	mov ax, 320
	div cl ;320/cl and quotient is in al
	mov bl, al;save quotient in bl
	mov ax, 200
	div cl;200/cl quotient is in al
	mul bl ;ax=al*bl
	call tekst_length;tekst length is in cx
	cmp ax, cx
	jl blad_brak_miejsca
	pop cx
	mov ax, seg tekst
	mov ds, ax
	mov di, offset tekst
	
	mov ax,0
	mov bx,0
	loop4:
		mov dl, byte ptr ds:[di]
		cmp dl, '$'
		je koniec_funkcji
		call print_letter
		add bx, cx
		inc di
		;if bx<320-letter size+1 
		push ax
		mov ax, 320
		sub ax, cx
		inc ax
		cmp bx, ax
		pop ax
		jle loop4
	mov bx, 0
	add ax, cx
	jmp loop4
	koniec_funkcji:
	ret

czy_wystarczajaco_miejsca:
	
	
	ret


tekst_length: ;w cl zwraca dlugosc tekst
	push ax
	mov ax, seg tekst
	mov ds, ax
	mov di, offset tekst
	mov cx, 0
	loop5:
		mov dl, byte ptr ds:[di]
		cmp dl, '$'
		je koniec_funkcji2
		inc cx
		inc di
		jmp loop5
	
	koniec_funkcji2:	
	dec cx
	pop ax
	ret

print_letter: ; prints letter with ascii code stored in dl in position given by (bx, ax) where ax is y-coord of left upper corner and bx is x-coord x-coord of left upper corner
;save all registers
	pusha
	;save segment of data array in ds - use ax as a buffer
	push ax
	mov ax, seg A_font 
	mov ds, ax
	pop ax

	mov ch, 0;initialize ch which is iterating through rows 
	process_row: ;while ch<8
		push cx;save counter values
		push ax
		movzx cx, ch
		mov al, 8
		mul dl ; computes 8*dl to get byte, at which char with ascii stored in dl starts and saves result in ax
		mov di, OFFSET A_font
		add di, ax ; di now holds address of byte, where given letter starts
		pop ax
		add di, cx ;di now holds address of byte, which should be printed in this row
		pop cx
	
		
		;get segment of fonts table
		push ax
		mov ax, seg A_font 
		mov ds, ax
		pop ax
		mov dh,  byte ptr ds:[di];byte describing row is now in dh
		push ax;save ax describing y-coord of position
		mov cl,0;initialize iterator looping through repeated rows
		loop2:;while cl<zoom
			
			push dx
			push cx ;save byte describing row of the letter
			mov cl, 0;initialize columns counter
			process_col: ;while cl<8
				shr dh,1 ;divide by 2 - in carry flag there is now the rest of this division
				jnc further
				;if rest of the division was 1
					;save register values which are affected by following command
					push di
					push ax 
					push bx
					;get zoom
					push ax
					mov ax, seg zoom_int
					mov ds, ax
					pop ax
					mov di, OFFSET zoom_int
				;calculate (x,y)
					;calculate bx
					push dx
					push ax
					mov al, cl
					mul byte ptr ds:[di]
					mov dx,ax
					add bx, dx	;in bx is now x-coord of pixel 
					pop ax
					pop dx
			
				;calculate ax
					push dx
					push ax
					mov al, ch
					mul byte ptr ds:[di]
					movzx dx,al
					pop ax
					add ax, dx		;in ax is now y-coord of pixel 
					pop dx
				
					;loop in order to extend letter in its width
					push cx;save main counter (counting through 8x8 square) value
					mov cl, 0;initialize counter
					loop1: ;while cl<dl
						call print_pixel
						inc bx
						inc cl
						cmp cl, byte ptr ds:[di]
						jl loop1
					pop cx
			
					;retrieve saved register valus
					pop bx 
					pop ax
					pop di
				further:	
				;common both for the case where we have printed pixel and for the case in which we haven't
				inc cl
				cmp cl,8
				jl process_col
			
			pop cx;retrieve information about cl which is an iterator looping through 0 to zoom 
			inc cl
			
			;read zooom
			push ax
			mov ax, seg zoom_int
			mov ds, ax
			pop ax	
			inc ax
			mov di, OFFSET zoom_int
			cmp cl, byte ptr ds:[di]	
			pop dx;retreive dh in which byte describing current letter is stored
			jl loop2
		
		pop ax;retrieve information about initial position of y-coord
		inc ch
		cmp ch, 8
		jl process_row
		
	;retrieve al register values
	popa
	ret

print_pixel: ;prints white pixel at position described by al and bl
	pusha
	mov cx,320 ;320 is width of graphics mode
	mul cx ; multiplies 320 and the value stored in ax and stores result in ax
	add ax,bx ; stores adress of given pixel in ax
	xchg si,ax
	;put location of video memory in es - use bx as a buffer
	mov bx, 0a000h;location of video mem for this mode
	mov es, bx 
	mov word ptr es:[si], 0fh;  0fh corresponds to white color 
	popa
		ret




;----------------------------WCZYTYWANIE-----------------------DANYCH-------------------------------------------
zoom_string_to_int:
	push ax
	push bx
	push cx
	push ds
	push di
	mov ax, seg zoom
    mov ds, ax
	mov di, offset zoom
	mov ax, 0
	loop3:
		mov bx, 0;clear bx
		mov bl, byte ptr ds:[di]
		cmp bl, '$'
		je wyjdz_z_funkcji 
		;check whether chars in zoom are ciphers
		cmp bl, '0'
		jl blad_zle_znaki_w_zoomie
		cmp bl, '9'
		jg blad_zle_znaki_w_zoomie
		sub bl, '0'
		;al*=10
		mov cl, 10
		mul cl
		add ax, bx
		cmp ax, 25
		jg blad_zoom_za_duzy
		inc di
		jmp loop3
		
	wyjdz_z_funkcji:
	mov bx, seg zoom_int
	mov ds, bx
	mov di, offset zoom_int
	mov byte ptr ds:[di], al
	
	pop di
	pop ds
	pop cx
	pop bx
	pop ax
	ret

 ; ----------------------------- Wczytanie Danych -------------------------
 
 wczytanie_danych: 
        mov di, 082h ; wskaznik na buffor z argumentami
        xor cx, cx
        mov cl, byte ptr ds:[080h] ; wpisanie do bl liczby znakow
		cmp cl, 0
		je blad_zla_liczba_arg
        mov ax, seg dane1
        mov es, ax ; extra segment
    ; -------------------------- zoom -------------------------------- 
        mov si, offset zoom
        call pomin_biale_znaki
		cmp cl, 0
		jz blad_zla_liczba_arg
		mov bx, 0
    parametr1:
        mov al, byte ptr ds:[di]
       
        mov byte ptr es:[si], al ; zapis znaku do zoom
		inc di
		inc bx
		dec cl
		inc si
		cmp cl, 0
		je blad_zla_liczba_arg
		cmp byte ptr ds:[di], ' '
		je drugi_parametr
		cmp byte ptr ds:[di], '	'
		je drugi_parametr
		cmp bx, 3 ;4 is length of zoom buffer - 3 for digits and 1 for null terminator
		jge blad_zoom_za_duzy
		jmp parametr1
 
    ; -------------------------- tekst --------------------------------
	
        inc di
        dec cl
        dec cl 
    drugi_parametr:
		;sprawdz czy jest bialy znak
		mov al, byte ptr ds:[di]
		cmp al, ' '
		je pomin
		cmp al, '	'
		je pomin
		jmp blad_zla_liczba_arg
	pomin:	
        call pomin_biale_znaki
        mov si, offset tekst
       
        cmp cl, 0
        jz blad_zla_liczba_arg
        mov al, byte ptr ds:[di]
 
    parametr2: 
        mov al, byte ptr ds:[di]
        mov byte ptr es:[si],al
       
        inc di  ; di - iteruje po wejscu
        inc si ; iteruje po tekst
        loop parametr2
 
        ret
   
 
; -------------------------- DODATKOWE PROCEDURY -------------------------
return:
    ret
   
pomin_biale_znaki:
    mov al, byte ptr ds:[di]
    cmp al, ' '
    jz znaleziony_bialy_znak
    cmp al, '	'
    jz znaleziony_bialy_znak
	
    ret
 
znaleziony_bialy_znak:
    inc di
    dec cl
    cmp cl, 0
    jz return
    jmp pomin_biale_znaki
	ret 
 
blad_zla_liczba_arg:

    mov dx, offset err_zla_liczba_arg
    call print
    jmp koniec_programu
   
blad_zoom_za_duzy:
    mov dx, offset err_zoom_za_duzy
    call print
    jmp koniec_programu
blad_zle_znaki_w_zoomie:
	mov dx, offset err_zle_znaki_w_zoomie
    call print
    jmp koniec_programu  
  
blad_brak_miejsca:
	mov ax,3
	int 10h
    mov dx, offset err_brak_miejsca
    call print
	;call wait_for_key
    jmp koniec_programu  
print: ; funkcja wypisuje string zakonczony $, ktorego offset znajduje sie w dx
    push ax
    push ds
    mov ax, seg dane1
    mov ds, ax
    mov ah, 9
    int 21h
   
    pop ds
    pop ax
    ret
code1 ends



stos1 segment stack
	dw 200 dup(?)
wstosu  dw ?
stos1 ends
END start