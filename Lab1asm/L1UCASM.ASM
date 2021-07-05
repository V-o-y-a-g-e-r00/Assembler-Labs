 .MODEL Large,C
	.data
	Extrn   AAAS:byte,BBBS:byte,CCCS:byte,ZNAM:byte, REZ:byte, CHISL:word
	.code
	Public  LAB1UC
 LAB1UC  proc    far
;************* cx=bbbS*2   tut vse rabotaet
	mov al,BBBS
	mov bl, 2
	mul bl
    ;	mov ICCCS, ax        ;prosto proveraem chto rabotaet
	mov cx, ax
;************* dl=c/25   tut vse rabotaet
	xor ax, ax       ;ochishaem ax
	mov al, CCCS     ; zanosim tuda CCCS
	mov bl, 25
	div bl
   ;	mov CCCS, al     ;proveraem
	mov dl, al
;************* dx=b*2+c/25  shislitel rabotaet
	mov ax, cx    ; b*2   vnutr ax
	xor dh, dh    ; c/25  v word
	add dx, ax
	mov CHISL, dx
;************* cl=a/b rabotaet
	xor ax, ax       ;ochishaem ax
	mov al, AAAS     ; zanosim tuda AAAS
	mov bl, BBBS     ; zanosim BBBS
	div bl
    ;	mov CCCS, al    ;proveraem
	mov cl, al
;************* cl=a+a/b-1  znamenatel vse rabotaet
	mov al, AAAS
	add cl, al
	dec cl
	mov ZNAM, cl
;*************chislitel na znamenatel
	mov ax, dx     ; chislitel v ax
	div cl
	mov REZ, al
	ret
LAB1UC  endp
	end
