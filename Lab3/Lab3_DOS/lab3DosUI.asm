;==========lab3DosUI.asm===========
; .386
assume cs:code, ds:data ; , ss:stk ; без этого работает
code segment
begin:	   
; main proc near ; main здесь не нужен, вместо нее здесь begin. Но запихнуть всю программу в функцию main нам ничего не мешает.
;************Тут пытаемся поместить адреса сегментов в соответствующие сегменты, но все работает и без этого. Видимо, они устанавливаются по умолчанию
    mov ax, data    ; работает и без этого
    mov ds, ax      ; работает и без этого
;    mov ax, stk    ; работает и без этого
;    mov ss, ax     ; работает и без этого
;++++++++++++++++++++++Ввод ASSS
    mov word ptr [msg], 'aM'
    mov word ptr [msg+2], ':i'
    mov word ptr [msg+4], 'SA'
    mov word ptr [msg+6], 'SS'
    mov byte ptr [msg+8], ' '
  ;  mov byte ptr [msg+4], 'A'
  ;  mov byte ptr [msg+4], 'A'
    call msgOut     ;Вывод msg
    call bufferIn   ;Ввод buffer. Buffer можно не использовать. Нужен только field. Это вызвано тем, что не работает 0ah
    call bufferOut  ;Вывод buffer
    call SnDtCheck  ;Определение знака числа . Запись minusFlag
    
    call ASCIToInt
    
    mov ax, binval
    mov ASSS, ax
;++++++++++++++++++++++Ввод BSSS    
    mov word ptr [msg], 'aM'
    mov word ptr [msg+2], ':i'
    mov word ptr [msg+4], 'SB'
    mov word ptr [msg+6], 'SS'
    mov byte ptr [msg+8], ' '
    
    call msgOut     ;Вывод msg
    call bufferIn   ;Ввод buffer. Buffer можно не использовать. Нужен только field. Это вызвано тем, что не работает 0ah
    call bufferOut  ;Вывод buffer
    call SnDtCheck  ;Определение знака числа . Запись minusFlag
    
    call ASCIToInt
    
    mov ax, binval
    mov BSSS, ax
;++++++++++++++++++++++Вызов LAB2 вывод и выход   
    
 ;   mov word ptr [XSSS], 0001010101111011b  ; тут проверка inttoasci
 ;   mov word ptr [XSSS+2], 1010b
 ;   mov ax, word ptr [XSSS]
 ;   mov dx, word ptr [XSSS+2]
 ;   xor ax, 1111111111111111b
 ;   xor dx, 1111111111111111b
 ;   mov word ptr [XSSS], ax
 ;   mov word ptr [XSSS+2], dx
      
    call LAB2
    call IntToASCI 
    

    call ExitFn     ;завершаем работу программы. Без этого программа зависает
; ret
; main endp


;=======================================
;Тут пошли пользовательские функции    |
;=======================================
SnDtCheck proc near     ;запись минуса в minusFlag
    mov byte ptr [msg], 'S'
    mov byte ptr [msg+1], 'n'
    cmp byte ptr [buffer], '-'
    jne FirstNotMinus
;**********************первый символ минус     
        mov byte ptr [msg+4], '-'
        call msgOut
        mov byte ptr [minusFlag], 1
        ret
;**********************первый символ не минус      
    FirstNotMinus:
        mov byte ptr[minusFlag], 0 
        mov byte ptr [msg+4], '+'
        call msgOut
        ;******************проверяем, что 1 символ - цифра
      ;  cmp byte [buffer], '0'
    ;    jb NOTDIGIT
     ;   cmp byte [buffer], '9'
     ;   ja NOTDIGIT
ret
SnDtCheck endp

ASCIToInt proc near
    mov byte ptr [msg], 'A'
    mov byte ptr [msg+1], 't'
    mov byte ptr [msg+2], 'I'
    mov byte ptr [msg+4], 'm'
    mov byte ptr [msg+5], ' '
    mov byte ptr [msg+6], ' '
    mov byte ptr [msg+7], ' '
    mov byte ptr [msg+8], ' '
    call msgOut
    mov word ptr[binval], 0
    cmp byte ptr [minusFlag], 0
    je POSITIVEH
;**************число отрицательное
    mov byte ptr [msg+4], '-'
    call msgOut
    xor cx, cx
    mov cl, byte ptr [StrLen]
   ; dec cl        ; 
    dec cl         ; номер последнего элемента с 0
    
    add cl, 00110000b       ; проверка должно выводиться число элементов с 0
    mov byte ptr [msg+4], cl
    call msgOut
    
    sub cl, 00110000b       ; преобразуем обратно после проверки
    mov si, cx
    lea bx, [buffer+si]      ; bx адрес младшей введенной цифры
    
    mov al, [bx]      ; проверка
    mov byte ptr [msg+4], al
    call msgOut
  
    mov word ptr [mult10], 1               ; инициализируем перед циклом
    mov word ptr [binval], 0
    LOOP2:                  ; тут будем преобразовывать в двоичный формат . в rcx число элементов 1 - старшая цифра перед минусом
    mov si, cx
    lea bx, [buffer+si]      ; rdx адрес младшей введенной цифры
    
   ; push rcx
   ; mov al, byte [rdx]      ; проверка
   ; mov byte [msg+4], al
   ; call msgOut
   ; pop rcx
    mov al, [bx]      ; текущий символ в al
    and ax, 000Fh          ; очистить старшие 4 бита
    mul word ptr [mult10]      ; edx:eax=символ * 10^n
    
    push cx    ;**********складываем с bin val 
    mov bx, word ptr [binval]
    add ax, bx
    mov word ptr [binval], ax
    
    jmp CONT10
    POSITIVEH:
    jmp POSITIVE
    CONT10:
    ;***********************   10^n * 10   ***************
    mov ax, 10
    mul word ptr [mult10]  ;edx:eax=mult10*10
    mov word ptr [mult10], ax ; старшая часть не нужна?
    
    pop cx
    loop LOOP2 

    mov ax, word ptr [binval] ; проверка тут все работает
    mov byte ptr [msg+4], ah
    mov byte ptr [msg+5], al
    call msgOut
    
    neg word ptr[binval]  ; в доп код
    mov byte ptr [msg+4], 'N'
    call msgOut
    
    mov ax, word ptr [binval]
    mov byte ptr [msg+4], ah
    mov byte ptr [msg+5], al
    call msgOut
ret
;**************************число положительное***********************
POSITIVE:
    mov byte ptr [msg+4], '+'
    call msgOut
    xor cx, cx
    mov cl, byte ptr [StrLen]
   ; dec cl        ; 
    dec cl         ; номер последнего элемента с 0
    
    add cl, 00110000b       ; проверка должно выводиться число элементов с 0
    mov byte ptr [msg+4], cl
    call msgOut
    
    sub cl, 00110000b       ; преобразуем обратно после проверки
    mov si, cx
    lea bx, [buffer+si]      ; bx адрес младшей введенной цифры
    
    mov al, [bx]      ; проверка
    mov byte ptr [msg+4], al
    call msgOut
  
    mov word ptr [mult10], 1               ; инициализируем перед циклом
    mov word ptr [binval], 0
    cmp cx, 0
    je LOOPEND
    LOOP3:                  ; тут будем преобразовывать в двоичный формат . в rcx число элементов 1 - старшая цифра перед минусом
    mov si, cx
    lea bx, [buffer+si]      ; bx адрес младшей введенной цифры
    
   ; push rcx
   ; mov al, byte [rdx]      ; проверка
   ; mov byte [msg+4], al
   ; call msgOut
   ; pop rcx
    mov al, [bx]      ; текущий символ в al
    and ax, 000Fh          ; очистить старшие 4 бита
    mul word ptr [mult10]      ; edx:eax=символ * 10^n
    
    push cx    ;**********складываем с bin val 
    mov bx, word ptr [binval]
    add ax, bx
    mov word ptr [binval], ax
    
    ;***********************   10^n * 10   ***************
    mov ax, 10
    mul word ptr [mult10]  ;edx:eax=mult10*10
    mov word ptr [mult10], ax ; старшая часть не нужна?
    
    pop cx
    loop LOOP3 
    LOOPEND:
    mov al, byte ptr [buffer]      ; текущий символ в al
    and ax, 0000Fh          ; очистить старшие 4 бита
    mul word ptr [mult10]      ; dx:ax=символ * 10^n
    
    ;**********складываем с bin val 
    mov bx, word ptr [binval]
    add ax, bx
    mov word ptr [binval], ax   

    ;***********************   10^n * 10   ***************
    mov ax, 10
    mul word ptr [mult10]  ;edx:eax=mult10*10
    mov word ptr [mult10], ax ; старшая часть не нужна
    
    ;************ посто ещё разок при cx=0
    mov ax, word ptr [binval] ; проверка тут все работает
    mov byte ptr [msg+4], ah
    mov byte ptr [msg+5], al
    call msgOut
ret
ASCIToInt endp


IntToASCI proc near
    mov byte ptr [msg], 'I'
    mov byte ptr [msg+1], 't'
    mov byte ptr [msg+2], 'A'
    mov byte ptr [msg+4], 'm'
    mov byte ptr [msg+5], ' '
    mov byte ptr [msg+6], ' '
    mov byte ptr [msg+7], ' '
    mov byte ptr [msg+8], ' '
    
    mov word ptr [mult10], 10               ; инициализируем перед циклом
    cmp word ptr [XSSS+2], 0
    jl XLZERO
    ;************XSSS Больше или равен нулю
    mov byte ptr [msg+4], '+'
    call msgOut
    
    mov cx, 11 ;всего будет цифр
    mov ax, word ptr [XSSS+2] ; ax = st chast XSSS
 ;   mov mult10, 10
    LOOPIAG:
   
    mov bx, 10
    
    mov dx, 0
  ;  div bx
    mov ax, word ptr[XSSS+2]        
    div bx                         ; ax= (st ch XSSS)/10 ; dx=(st ch XSSS)%10
    mov word ptr [XSSS+2], ax           ; st cahst XSSS= st chast (XSSS/10)
    mov ax, word ptr [XSSS]         ; dx:ax= (st ch XSSS)%10 : ml ch XSSS
    
    div bx           ; dx:ax=  XSSS%10 : ml ch (XSSS/10)
    mov word ptr [XSSS], ax         ; XSSS=XSSS/10
    
    mov bx, dx
    or bl, 00110000b
    mov si, cx
    lea di, [buf+si]
    mov [di], bl   ;цифру в буффер
    dec cx
    push cx
    push ax
  ;  call msgOut
 ;   call msgOut2
    pop ax
    pop  cx      ; 
    cmp ax, 0
    
    jne LOOPIAG
    call msgOut2
  ;  mov byte ptr [msg+4], 'X'
  ;  call msgOut
  ;  call BufOutFn
    
ret
    ;************XSSS меньше нуля
    XLZERO:
    mov byte ptr [msg+4], '-'
    call msgOut    
    
    mov ax, word ptr [XSSS] ; XSSS в прямой код
    mov dx, word ptr [XSSS+2]
    sub ax, 1
    sbb dx, 0
    xor ax, 1111111111111111b
    xor dx, 1111111111111111b
    mov word ptr[XSSS], ax
    mov word ptr [XSSS+2], dx 
 
 mov cx, 11 ;всего будет цифр
    mov ax, word ptr [XSSS+2] ; ax = st chast XSSS
 ;   mov mult10, 10
    LOOPIAG1:
   
    mov bx, 10    
    mov dx, 0
  ;  div bx
    mov ax, word ptr[XSSS+2]        
    div bx                         ; ax= (st ch XSSS)/10 ; dx=(st ch XSSS)%10
    mov word ptr [XSSS+2], ax           ; st cahst XSSS= st chast (XSSS/10)
    mov ax, word ptr [XSSS]         ; dx:ax= (st ch XSSS)%10 : ml ch XSSS
    
    div bx           ; dx:ax=  XSSS%10 : ml ch (XSSS/10)
    mov word ptr [XSSS], ax         ; XSSS=XSSS/10
    
    mov bx, dx
    or bl, 00110000b
    mov si, cx
    lea di, [buf+si]
    mov [di], bl   ;цифру в буффер
    dec cx
    push cx
    push ax
  ;  call msgOut
 ;   call msgOut2
    pop ax
    pop  cx      ; 
    cmp ax, 0
    
    jne LOOPIAG1
    
    dec si
    mov byte ptr [buf+si], '-'
    call msgOut2
    
 ;   mov dword [msg+4], 'XSSS'
;    call msgOut
;    call BufOutFn
ret
IntToASCI endp
;*************************************короткие функции ввода вывода и данные
msgOut proc near
    xor al, al
    mov ah, 09h
    lea dx, msg
    int 21h 
ret
msgOut endp

bufferIn proc near
    xor ax, ax
    mov     ah,     3Fh ;Функция для чтения файла через описатель. Функция 0ah у меня работает некорректно. Причина не известна.
    mov     bx,     0   ; описатель файла (stdin)
    mov     dx,     offset buffer   ;адрес буфера для чтения байт
    mov     cx,     20  ; число считываемых байт (перевод строки и возврат каретки туда входят). возвращает в ax число действительно считанных байт
    int     21h
        
    and ax, 0000000000001111b   ; ставим символ $ после введенных символов перевода
    mov bx, ax
    lea di, [buffer]     ;si di bx пригодны для индексации, остальные как повезет
    lea si, [di+bx]
    mov byte ptr [si], '$'
    
    sub al, 2           ; помещаем число символов в StrLen без символов перевода строки
    mov StrLen, al
    
    or al, 00110000b        ;проверяем длину строки  
    mov byte ptr [msg+4], al 
    mov ah, 09h
    lea dx, msg
    int 21h
ret
bufferIn endp

bufferOut proc near
  ;  xor di, di
  ;  xor si, si
    mov ah, 09h
    mov dx, offset buffer
    int 21h  
ret
bufferOut endp

BufOutFn proc near
    xor al, al
    mov ah, 09h
   ; lea dx, BufOut
    int 21h 
ret
BufOutFn endp
msgOut2 proc near
    xor al, al
    mov ah, 09h
    lea dx, buf
    int 21h 
ret
msgOut2 endp
 
ExitFn proc near
    mov ax,4c00h
    int 21h
ret
ExitFn endp	  
;===================================================================
;Пошел модуль Lab2                                                  |
;===================================================================

	Public	UDIV
 UDIV proc near   ; dx:ax= dword A / word B *** delimoe A v dx:ax     delitel B v bx
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
	;mov word ptr TESTUDIV, ax    ; proverka
	;mov word ptr TESTUDIV[2], dx  ;        proverka
	ret
 UDIV endp
	Public  LAB2
 LAB2  proc    near
	mov ax, ASSS
	mov bx, BSSS
	cmp ax, bx
	JA GREATER
	JE EQUAL
	JB LESS
 GREATER:; mov TEST1, 1  ;a>b
;********* XSSS=(ASSS+86)*6 *********************
       ;******** ASSS+86 >= 0
       xor dx, dx	; dx:ax=ASSS
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
      ; mov TEST1, dx ; pri dannih usloviah zadachi eto slovo dolzno sodergat tolko nuli
       ;*******
       jmp EXIT

 EQUAL: ;mov TEST1, 10  ;a==b
;********* XSSS=-16 *********************rabotaet
	mov ax, 16
	neg ax
	cwd
	mov word ptr [XSSS], ax
	mov word ptr [XSSS+2], dx
;******************************
	jmp EXIT

LESS: 	;mov TEST1, -1  ;a<b
;********** XSSS=(128-ASSS)/b ********************
	    cmp ASSS, 128 ; a<=128 ? <=> 128-a>=0 ?
	    jbe CHISLGE
	    ja CHISLL
CHISLGE:    ;*********** 128-ASSS>=0
	    ;*********** 128-ASSS>=0 && BSSS>0
		;mov TEST1, 101
		mov bx, 128
		xor cx, cx	; cx:bx=128  so znakom
		mov ax, ASSS
		xor dx, dx	; dx:ax=ASSS so znakom
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
CHISLL:	;*********** 128-ASSS<0 && BSSS>0 drugih variantov tut net eto ne oshibka
	;mov TEST1, 103
	mov bx, 128
	xor cx, cx	; cx:bx=128  so znakom
	mov ax, ASSS
	xor dx, dx	; dx:ax=ASSS so znakom
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
;===================================================================
;Конец модуля Lab2                                                  |
;===================================================================
code ends

data segment	
    NewLine db 0dh, 0ah,'$'
    msg db 'FuN:mesgE',0dh, 0ah, '$' ; 0dh возврат каретки, 0ah переход на новую строку
    buf db 12 dup ('_'),0dh, 0ah, '$' ; буфер для вывода
  
  ;  bufferl label byte    ; ИмяМетки ДирективаМетки ТипМетки
     ;   maxlen db 10
	;actlen db ?
        memHl db 20 dup('z')
	buffer db 20 dup ('3'), '$'
        
    StrLen DB 100   ; число успешно считанных символов
    
    minusFlag db 3   ; Запоминает, был ли изначально минус в введенном числе 0 - не было 1 - был
    InpCheckFlag db 3 ; Функции ввода его устанавливают. Если 0 то все успешно если 1 то было что то не так
    
    mult10 dw 1 ; для умножения на 10
    binval dw 0 ; для промежуточного результата преобразования из asci в bin 
     db 10 dup(?)
    desat  dw 10     ; специально для inttoasci
        
    	            
    
    ASSS dw 0   ;Для модуля Lab2
    BSSS dw 0   ;
    XSSS dd 0   ;
data ends
stk segment stack       ; тип stack указывает компилятору, что сегмент нужно использовать как стек
	   db 256 dup (?)
stk ends
end begin ; должен стоять в конце файла. Иначе весь последующий текст игнорируется