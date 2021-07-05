 .MODEL Large,C
	.data
	Extrn   ASSS:word,BSSS:word,CSSS:word, ZNAM:word, REZ:word, CHISL:dword
	.code
	Public  LAB1
 LAB1  proc    far
; stack=BSSS*2 
	mov ax,BSSS
	mov bx, 2
	mul bx       ; dx:ax=b*2
	push dx      ; rabotaem so stekom
	push ax     
; dx:ax=c/25 
	mov ax, CSSS     ; zanosim v ax CSSS
	cwd 		; dx:ax=CSSS
	mov bx, 25
	div bx           ; ax=c/25
	cwd              ; dx:ax=c/25
; dx:ax=b*2+c/25
	pop cx           ; cx=(ml chast (b*2))
	add ax, cx	; ax=ml chast c/25 + ml chast b*2
	pop cx          ; cx=(st chast (b*2))
	adc dx, cx
	mov word ptr [CHISL], ax    ; 
	mov word ptr [CHISL+2], dx  ;
; bx=a/b
	mov ax, ASSS     ; zanosim v ax ASSS
	cwd              ; dx:ax=ASSS
	mov bx, BSSS     ; bx=BSSS
	div bx         ; ax= dx:ax / bx
; bx=a+a/b-1 
	mov bx, ASSS	; bx=ASSS
	add bx, ax   ; bx=a + a/b
	dec bx
	mov ZNAM, bx
; rez
	mov ax, word ptr [CHISL]     ; chislitel v dx:ax
	mov dx, word ptr [CHISL+2]
	div bx		; ax=ax:dx / bx
	mov REZ, ax
	ret
LAB1  endp
	end
