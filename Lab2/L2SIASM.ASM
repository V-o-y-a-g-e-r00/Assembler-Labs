 .MODEL Large,C
	.data
	Extrn   ASSS:word,BSSS:word,XSSS:dword, TEST1:word,TESTLZ:dword, TESTUDIV:dword, TDIVID:dword
	.code
	Public	UDIV
 UDIV proc far   ; dx:ax= dword A / word B *** delimoe A v dx:ax     delitel B v bx
    ;	mov TESTUDIV, 789     ;proverka
    ;	mov bx, BSSS                    ;proverka
    ;	mov ax,word ptr TDIVID       ;proverka
    ;	mov dx, word ptr TDIVID[2]	;proverka
	push ax 	; stack1= ml chast A
	mov ax, dx
	xor dx, dx	; dx:ax=000..0:st chast A
	div bx		; ax=(st chast A)/B   dx=ostatok
	mov cx, ax      ; cx=(st chast A)/B
	pop ax		; dx:ax=ostatok:ml chast A
	div bx
	mov dx, cx	; dx:ax=A/B
	mov word ptr TESTUDIV, ax    ; proverka
	mov word ptr TESTUDIV[2], dx  ;        proverka
	ret
 UDIV endp
	Public  LAB2
 LAB2  proc    far
	mov ax, ASSS
	mov bx, BSSS
	cmp ax, bx
	JG GREATER
	JE EQUALHANDLE
	JL LESSHANDLE
 GREATER: mov TEST1, 1  ;a>b
;********* XSSS=(ASSS+86)*6 *********************
	mov ax, ASSS
	mov dx, 86
	neg dx          ; dx= -86
	cmp ax, dx	; ASSS >= -86 ?  <=> ASSS+86>= 0 ?
	jge GEZ
	jl LZ
GEZ:
       ;******** ASSS+86 >= 0
       cwd	; dx:ax=ASSS znak ne izvesten
       add ax, 86
       adc dx, 0	; dx:ax = ASSS+86 bolshe nula
       ;******* (ASSS+86)*6
       mov cx, dx	; cx= starsh chast ASSS+86
       mov bx, 6
       mul bx		; dx:ax= (ml chast (ASSS+86))*6
       mov word ptr XSSS[0], ax	; XSSS[0]=ml chast((ASSS+86)*6)
       mov ax, cx	; ax= starsh chast (ASSS+86)
       mov cx, dx	; cx= st chast(ml chast (ASSS+86))*6)
       mul bx		; dx:ax= (st chast (ASSS+86))*6
       add ax, cx	;
       adc dx, 0	; dx:ax  ax vtoroe i dx tretie slova XSSS
       mov word ptr XSSS[2], ax
       mov TEST1, dx ; pri dannih usloviah zadachi eto slovo dolzno sodergat tolko nuli
       ;*******
	jmp EXIT
LESSHANDLE: jmp LESS  ; k tomu chto vishe i nize otnoshenia ne imeet Eto nuzno tak kak ne hvataet smeshenia
EQUALHANDLE: jmp EQUAL
LZ:
       ;******** ASSS+86 < 0
       ;******** abs(ASSS+86)
       cwd	;dx:ax=ASSS
       add ax, 86
       adc dx, 0	; dx:ax=ASSS+86
       sub ax, 1        ; ASSS+86 iz dop koda k pramomu
       sbb dx, 0        ; ASSS+86 iz dop koda k pramomu
       xor ax, 65535	; ASSS+86 iz dop koda k pramomu invertiruem ax
       xor dx, 65535	; invertiruem dx dx:ax=ASSS+86 v pramom kode
       mov word ptr TESTLZ[0], ax  ; proveraem
       mov word ptr TESTLZ[2], dx ;TESTLZ= abs(ASSS+86) proveraem
       ;******** abs(ASSS+86)*6
       mov cx, dx	; cx= starsh chast ASSS+86
       mov bx, 6
       mul bx		; dx:ax= (ml chast (ASSS+86))*6
		push ax	; stack1=ml chast((ASSS+86)*6)
       mov ax, cx	; ax= starsh chast (ASSS+86)
       mov cx, dx	; cx= st chast(ml chast (ASSS+86))*6)
       mul bx		; dx:ax= (st chast (ASSS+86))*6
       add ax, cx	;
		mov dx, ax	; dx=st chast abs(ASSS+86)*6 v pramom kode
		pop ax		; ax= ml chast abs(ASSS+86)*6 v pramom kode
		xor ax, 65535
		xor dx, 65535
		add ax, 1
		adc dx, 0	; dx:ax=(ASSS+86)*6  v dop kode
       mov word ptr XSSS[0], ax
       mov word ptr XSSS[2], dx
       ;********
;******************************
	jmp EXIT
 EQUAL: mov TEST1, 10  ;a==b
;********* XSSS=-16 *********************rabotaet
	mov ax, 16
	neg ax
	cwd
	mov word ptr [XSSS], ax
	mov word ptr [XSSS+2], dx
;******************************
	jmp EXIT
LESS: 	mov TEST1, -1  ;a<b
;********** XSSS=(128-ASSS)/b ********************
	    cmp ASSS, 128 ; a<=128 ? <=> 128-a>=0 ?
	    jle CHISLGE
	    jg CHISLL
CHISLGE:    ;*********** 128-ASSS>=0
	cmp BSSS, 0
	jg DGEG   ; tut eshe mojet ponadobitsa proverka na nol no v etoy lab mi delaem eto v ci
	jl DGEL
	DGEG:	;*********** 128-ASSS>=0 && BSSS>0
		mov TEST1, 101
		mov bx, 128
		xor cx, cx	; cx:bx=128  so znakom
		mov ax, ASSS
		cwd		; dx:ax=ASSS so znakom
		sub bx, ax
		sbb cx, dx      ; cx:bx=128-ASSS so znakom
		mov ax, bx
		mov dx, cx      ; dx:ax=128-ASSS so znakom (tut >=0 )
		mov bx, BSSS
		call UDIV
		mov word ptr XSSS, ax
		mov word ptr XSSS[2], dx
		;***********
		jmp EXIT
	DGEL:	;*********** 128-ASSS>=0 && BSSS<0
		mov TEST1, 102
		mov bx, 128
		xor cx, cx	; cx:bx=128  so znakom
		mov ax, ASSS
		cwd		; dx:ax=ASSS so znakom
		sub bx, ax
		sbb cx, dx      ; cx:bx=128-ASSS so znakom
		mov ax, bx
		mov dx, cx      ; dx:ax=128-ASSS so znakom (tut >=0 )
		mov bx, BSSS
		neg bx		; bx iz dop koda v pramoi
		call UDIV
		xor ax, 65535
		xor dx, 65535
		add ax, 1
		adc dx, 0	; perevodim rezultat v dop kod
		mov word ptr XSSS, ax
		mov word ptr XSSS[2],dx
		;***********
		jmp EXIT
CHISLL:	;*********** 128-ASSS<0 && BSSS>0 drugih variantov tut net eto ne oshibka
	mov TEST1, 103
	mov bx, 128
	xor cx, cx	; cx:bx=128  so znakom
	mov ax, ASSS
	cwd		; dx:ax=ASSS so znakom
	sub bx, ax
	sbb cx, dx      ; cx:bx=128-ASSS so znakom
	mov ax, bx
	mov dx, cx      ; dx:ax=128-ASSS so znakom (tut <0 )
	sub ax, 1
	sbb dx, 0
	xor ax, 65535
	xor dx, 65535	; dx:ax=128-AAAS v pramom kode
	mov bx, BSSS
	call UDIV
	xor ax, 65535
	xor dx, 65535
	add ax, 1
	adc dx, 0	; perevodim rezultat v dop kod
	mov word ptr XSSS, ax
	mov word ptr XSSS[2],dx



	;***********
	jmp EXIT
;******************************
	jmp EXIT

EXIT:	ret
LAB2  endp
	end