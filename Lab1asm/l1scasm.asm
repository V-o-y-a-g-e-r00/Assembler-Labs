 .MODEL Large,C
	.data
	Extrn   AAAS:byte,BBBS:byte,CCCS:byte,ZNAM:byte, REZ:byte, CHISL:word, ICCCS:word
	.code
	Public  LAB1UC
 LAB1UC  proc    far
;************* cx=bbbS*2   tut vse rabotaet *
	mov al,BBBS
	mov bl, 2
	imul bl
	mov ICCCS, ax        ;prosto proveraem chto rabotaet
	mov cx, ax
;************* dl=c/25   tut vse rabotaet *
	xor ax, ax       ;ochishaem ax
	mov al, CCCS     ; zanosim tuda CCCS
	cbw
	mov bl, 25
	idiv bl
   ;	mov CCCS, al     ;proveraem
	mov dl, al
;************* dx=b*2+c/25  shislitel vrode bi rabotaet *
	mov al, dl    ; c/25 v vord
	cbw           ; c/25 v vord
	add ax, cx    ; c/25 + b*2
	mov dx, ax
	mov CHISL, dx
;************* cl=a/b rabotaet *
	mov al, AAAS     ; zanosim v al AAAS
	cbw
	mov bl, BBBS     ; zanosim BBBS
	idiv bl         ; al=ax/bl
;	mov CCCS, al    ;proveraem
	mov cl, al
;************* cl=a+a/b-1  znamenatel vse rabotaet
	mov al, AAAS
	add cl, al   ; a/b + a
	dec cl
	mov ZNAM, cl
;*************chislitel na znamenatel
	mov ax, dx     ; chislitel v ax
	idiv cl
	mov REZ, al
	ret
LAB1UC  endp
	end
