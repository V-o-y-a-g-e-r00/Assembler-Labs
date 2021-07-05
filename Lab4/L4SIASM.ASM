 .MODEL Large,C
	.data
	Extrn   CSSS:word, DSSS:word, SUMS:word, NSSS:word ,ASSS
	Extrn TEST1:word, TEST2:word
	.code
	Public  LAB4
LAB4  proc    far
	mov SUMS, 0 	; obnulenie summi
	mov TEST2, 0	; proverka

	mov cx, NSSS    ; podgotovka cikla
	JCXZ ENDLOOPN
	LOOPN:
	    ;	mov cx, 2; proverka cx nomer elementa schitaya s odnogo
		INC TEST2    	; proverka raboti cikla

		mov ax, cx      ; cx nelza ispolzovat dla adresa
		mov bx, 2
		imul bx		; dx:ax=ax*2=cx*2
		mov bx, ax      ; bx=cx*2
		lea bx, [ASSS+bx-2] ; bx=(cx-1)*2 adres tekushego elementa  *2 tak kak u elementov massiva tip word

		mov ax, word ptr [bx]  ; proverka
		mov TEST1, ax          ; proverka

		mov ax, 0
		cmp [bx], ax
		JLE NEXT
		mov ax, DSSS
		cmp [bx], ax
		JL NEXT
		mov ax, CSSS
		cmp [bx], ax
		JG NEXT

		inc SUMS
	NEXT:
	LOOP LOOPN
	ENDLOOPN:






; GREATER:  ;a>b
;********* XSSS=(ASSS+86)*6 *********************
  ;	mov ax, ASSS[4]
;	mov TEST1, ax
 ;	neg dx          ; dx= -86
     ;	cmp

ret
LAB4  endp
	end