; Assemble:	  nasm -f elf64 -l lab3.lst  lab3.asm
; Link:		  gcc -m64 -o lab2  lab3.o
; Run:		  ./lab3

;Assemble:         nasm -g -f elf64 $SOURCE$ -l $LSTOUTPUT$ -o $PROGRAM.OBJ$
; Link:		  gcc  $PROGRAM.OBJ$ $MACRO.OBJ$ -g -o $PROGRAM$ -m64 -fno-pie -no-pie

;Assemble:	nasm -g -f elf64 lab3_LinuxSI.asm -l lab3_LinuxSI.lst -o lab3_Linux.obj
;Link:		gcc lab3_Linux.obj -g -o lab3_LinuxSI.exe -m64 -fno-pie -no-pie
;Run:		./lab3_LinuxSI.exe

global main     ;точка входа в программу Необходима для запуска программы
global SnDtCheck  ;
; global SIRgCheck    ; проверка ОДЗ для sign int
; global UIRgCheck    ; проверка ОДЗ для unsign int
global ASCIToInt    ; в прямом и обратном коде
global IntToASCI
global msgOut   ; вывод msg Сообщения для тестирования программы
global bufferIn ; Ввод buffer с клавиатуры
global bufferOut ; вывод buffer на экран
global BufOutFn


section .text   
main:
    mov rbp, rsp; for correct debugging
    mov ebp, esp; for correct debugging
;+++++++++++++++++++++++++++++++++++++++++++Ввод ASSS+++++++++++++++++++++ 
   call msgOut
 
   mov dword [msg], 'Mai:'
   mov dword [msg+4], 'ASSS'
   call msgOut
      
   call bufferIn
   call bufferOut
    
   call SnDtCheck   ;проверка buffer на наличие посторонних символов     
   ;****************************проверка и выдача ошибки
   mov dword [msg], 'Mai:'
   cmp byte [InpCheckFlag], 0
   je POSM
   mov dword [msg+4], '1'
   call msgOut
   mov dword [msg+4], 'ERR!'
   call msgOut
   call ExitFn
   jmp CONT
   POSM: 
   mov dword [msg+4], '0'
   call msgOut
   CONT:
;****************************/проверка и выдача ошибки   
   call ASCIToInt
   call SIRgCheck
;****************************проверка и выдача ошибки      
   mov dword [msg], 'Mai:'
   cmp byte [InpCheckFlag], 0
   je POSM1
   mov dword [msg+4], '1'
   call msgOut
   mov dword [msg+4], 'ERR!'
   call msgOut
   call ExitFn
   jmp CONT1
   POSM1: 
   mov dword [msg+4], '0'
   call msgOut
   CONT1:   
;****************************/проверка и выдача ошибки        
   mov ax, word [binval]  
   mov word [ASSS], ax
   
   
;+++++++++++++++++++++++++++Ввод BSSS++++++++++++++++++++++++
   call msgOut
   
   mov dword [msg], 'Mai:'
   mov dword [msg+4], 'BSSS'
   call msgOut
   
   call bufferIn
   call bufferOut

   call SnDtCheck   ;проверка buffer на наличие посторонних символов     
   ;****************************проверка и выдача ошибки
   mov dword [msg], 'Mai:'
   cmp byte [InpCheckFlag], 0
   je BPOSM
   mov dword [msg+4], '1'
   call msgOut
   mov dword [msg+4], 'ERR!'
   call msgOut
   call ExitFn
   jmp BCONT
   BPOSM: 
   mov dword [msg+4], '0'
   call msgOut
   BCONT:
;****************************/проверка и выдача ошибки   
   call ASCIToInt
   call SIRgCheck
;****************************проверка и выдача ошибки      
   mov dword [msg], 'Mai:'
   cmp byte [InpCheckFlag], 0
   je BPOSM1
   mov dword [msg+4], '1'
   call msgOut
   mov dword [msg+4], 'ERR!'
   call msgOut
   call ExitFn
   jmp BCONT1
   BPOSM1: 
   mov dword [msg+4], '0'
   call msgOut
   BCONT1:   
;****************************/проверка и выдача ошибки        
   mov ax, word [binval]  
   mov word [BSSS], ax
   
   


;***********ввод завершен. Модуль с Lab2********************
    call LAB2
    
    
;***********преобразование в asci и вывод*******************
  ;  mov ax, word [ASSS]     ; просто проверка
 ;   cwde                    ; eax=ASSS со знаком
  ;  mov dword [XSSS], eax   ;
    call IntToASCI          ;
    
    


;**********************выход из программы завершение текущего процесса
    call ExitFn
ret

;===================================================================
;Пошел модуль Lab2                                                  |
;===================================================================
	; .data
	;Extrn   ASSS:word,BSSS:word,XSSS:dword, TEST1:word,TESTLZ:dword, TESTUDIV:dword, TDIVID:dword
;	.code
global	UDIV
UDIV:   ; dx:ax= dword A / word B *** delimoe A v dx:ax     delitel B v bx
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
global  LAB2
 LAB2:
	mov ax, word [ASSS]
	mov bx, word [BSSS]
	cmp ax, bx
	JG GREATER
	JE EQUALHANDLE
	JL LESSHANDLE
 GREATER: ; mov TEST1, 1  ;a>b
;********* XSSS=(ASSS+86)*6 *********************
	mov ax, word [ASSS]
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
       mov word [XSSS], ax	; XSSS[0]=ml chast((ASSS+86)*6)
       mov ax, cx	; ax= starsh chast (ASSS+86)
       mov cx, dx	; cx= st chast(ml chast (ASSS+86))*6)
       mul bx		; dx:ax= (st chast (ASSS+86))*6
       add ax, cx	;
       adc dx, 0	; dx:ax  ax vtoroe i dx tretie slova XSSS
       mov word [XSSS+2], ax
     ;  mov TEST1, dx ; pri dannih usloviah zadachi eto slovo dolzno sodergat tolko nuli
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
   ;    mov word ptr TESTLZ[0], ax  ; proveraem
   ;    mov word ptr TESTLZ[2], dx ;TESTLZ= abs(ASSS+86) proveraem
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
       mov word [XSSS], ax
       mov word [XSSS+2], dx
       ;********
;******************************
	jmp EXIT
 EQUAL: ; mov TEST1, 10  ;a==b
;********* XSSS=-16 *********************rabotaet
	mov ax, 16
	neg ax
	cwd
	mov word [XSSS], ax
	mov word [XSSS+2], dx
;******************************
	jmp EXIT
LESS: ;	mov TEST1, -1  ;a<b
;********** XSSS=(128-ASSS)/b ********************
	    cmp word [ASSS], 128 ; a<=128 ? <=> 128-a>=0 ?
	    jle CHISLGE
	    jg CHISLL
CHISLGE:    ;*********** 128-ASSS>=0
	cmp word [BSSS], 0
	jg DGEG   ; tut eshe mojet ponadobitsa proverka na nol no v etoy lab mi delaem eto v ci
	jl DGEL
	DGEG:	;*********** 128-ASSS>=0 && BSSS>0
	;	mov TEST1, 101
		mov bx, 128
		xor cx, cx	; cx:bx=128  so znakom
		mov ax, word [ASSS]
		cwd		; dx:ax=ASSS so znakom
		sub bx, ax
		sbb cx, dx      ; cx:bx=128-ASSS so znakom
		mov ax, bx
		mov dx, cx      ; dx:ax=128-ASSS so znakom (tut >=0 )
		mov bx, word [BSSS]
		call UDIV
		mov word  [XSSS], ax
		mov word [XSSS+2], dx
		;***********
		jmp EXIT
	DGEL:	;*********** 128-ASSS>=0 && BSSS<0
		;mov TEST1, 102
		mov bx, 128
		xor cx, cx	; cx:bx=128  so znakom
		mov ax, word [ASSS]
		cwd		; dx:ax=ASSS so znakom
		sub bx, ax
		sbb cx, dx      ; cx:bx=128-ASSS so znakom
		mov ax, bx
		mov dx, cx      ; dx:ax=128-ASSS so znakom (tut >=0 )
		mov bx, word [BSSS]
		neg bx		; bx iz dop koda v pramoi
		call UDIV
		xor ax, 65535
		xor dx, 65535
		add ax, 1
		adc dx, 0	; perevodim rezultat v dop kod
		mov word [XSSS], ax
		mov word [XSSS+2],dx
		;***********
		jmp EXIT
CHISLL:	;*********** 128-ASSS<0 && BSSS>0 drugih variantov tut net eto ne oshibka
	; mov TEST1, 103
	mov bx, 128
	xor cx, cx	; cx:bx=128  so znakom
	mov ax, word [ASSS]
	cwd		; dx:ax=ASSS so znakom
	sub bx, ax
	sbb cx, dx      ; cx:bx=128-ASSS so znakom
	mov ax, bx
	mov dx, cx      ; dx:ax=128-ASSS so znakom (tut <0 )
	sub ax, 1
	sbb dx, 0
	xor ax, 65535
	xor dx, 65535	; dx:ax=128-AAAS v pramom kode
	mov bx, word [BSSS]
	call UDIV
	xor ax, 65535
	xor dx, 65535
	add ax, 1
	adc dx, 0	; perevodim rezultat v dop kod
	mov word [XSSS], ax
	mov word [XSSS+2],dx



	;***********
	jmp EXIT
;******************************
	jmp EXIT

EXIT:	ret





;===================================================================
;Конец модуля Lab2                                                  |
;===================================================================

;=======================================
;Тут пошли пользовательские функции    |
;=======================================
SnDtCheck:                 ;sign and digit check запись минуса в minusFlag и проверка, что 1-й символ цифра или минус, а остальные - цифры
    mov dword [msg], 'SnD:'
    cmp byte [buffer], '-'
    jne FirstNotMinus
;**********************первый символ минус     
        mov qword [msg+4], 'Fst-'
        call msgOut
        
        mov byte [minusFlag], 1

        ;******************проверяем, что 2 символ - цифра
        cmp byte [buffer+1], '0'
        jb NOTDIGIT
        cmp byte [buffer+1], '9'
        ja NOTDIGIT
        ;*****************
        jmp DIGCYCLE        ;Прыгаем в цикл, где проверяются на цифры оставшиеся символы
;**********************первый символ не минус      
    FirstNotMinus:
        mov byte [minusFlag], 0 
        mov qword [msg+4], 'FtN-'
        call msgOut
        ;******************проверяем, что 1 символ - цифра
        cmp byte [buffer], '0'
        jb NOTDIGIT
        cmp byte [buffer], '9'
        ja NOTDIGIT
        
;**********************Цикл с проверкой оставшихся символов на цифры и enter
    DIGCYCLE:
        mov rcx, 8          ; нам достаточно 8 прохождений цикла, т.е. проверки 8 символов
        mov rdx, buffer
        lea rdx, [rdx+1]    ;В rdx - адрес buffer+1 т.е. адрес 2 по счету элемента 
        SHORT_DIGCYCLE:
           ; mov dword [msg], '1'
           ; push rcx            ; syscall изменяет значение rcx поэтому используем стек
            ;**************проверям введен ли enter
            cmp byte [rdx], 0x0A  ;проверяем с buffer+1 по buffer+8 включительно. rcx достигает значения 1 и происходит выход из цикла
            
            je DIGCYCLE_SUCCESS
            ;**************проверяем, что символ - цифра
            cmp byte [rdx], '0'
            jb NOTDIGIT
            cmp byte [rdx], '9'
            ja NOTDIGIT
            
            lea rdx, [rdx+1]    ;переходим к следующему символу
        LOOP SHORT_DIGCYCLE
        mov dword [msg+4], 'NoEn'
        call msgOut
        
        mov byte [InpCheckFlag], 1 ; т.е. в ходе ввода возникла ошибка
ret ;тут неуспешно
        
    DIGCYCLE_SUCCESS:       ;если среди символов встретился символ enter 0x0A     
        mov dword [msg+4], 'Succ'
        call msgOut
        
        mov byte [InpCheckFlag], 0 ; т.е. функция успешно выполнила работу
ret
    
    
;*********************Случай, когда где-то оказалась не цифра
    NOTDIGIT: 
        mov dword [msg+4], 'NotD'
        call msgOut
        
        mov byte [InpCheckFlag], 1 ; т.е. в ходе ввода возникла ошибка
ret ; выход из функции
 
ASCIToInt:
    mov dword [msg], 'AtI:'
    mov dword [msg+4], 'mesg'
    call msgOut
    mov dword [binval], 0
    cmp byte [minusFlag], 0
    je POSITIVE
;**************число отрицательное
    mov dword [msg+4], 'mnus'
    call msgOut
    xor rcx, rcx
    mov cl, byte [StrLen]
    dec cl        ; 
    dec cl         ; номер последнего элемента с 0
    
;    add cl, 00110000b       ; проверка должно выводиться число элементов с 0
 ;   mov byte [msg+4], cl
 ;   call msgOut
    
    lea rdx, [buffer+rcx]      ; rdx адрес младшей введенной цифры
    
 ;   mov al, byte [rdx]      ; проверка
;    mov byte [msg+4], al
 ;   call msgOut
  
    mov dword [mult10], 1               ; инициализируем перед циклом
    mov dword [binval], 0
    LOOP2:                  ; тут будем преобразовывать в двоичный формат . в rcx число элементов 1 - старшая цифра перед минусом
    lea rdx, [buffer+rcx]      ; rdx адрес младшей введенной цифры
    
   ; push rcx
   ; mov al, byte [rdx]      ; проверка
   ; mov byte [msg+4], al
   ; call msgOut
   ; pop rcx
    mov al, byte [rdx]      ; текущий символ в al
    and eax, 0000Fh          ; очистить старшие 4 бита
    mul dword [mult10]      ; edx:eax=символ * 10^n
    
    push rcx    ;**********складываем с bin val 
    mov ebx, dword [binval]
    add eax, ebx
    mov dword [binval], eax
    ;***********************   10^n * 10   ***************
    mov eax, 10
    mul dword [mult10]  ;edx:eax=mult10*10
    mov dword [mult10], eax ; старшая часть не нужна
    
    pop rcx
    loop LOOP2 

    mov eax, dword [binval] ; проверка
    mov dword [msg+4], eax
    call msgOut
    
    neg dword [binval]  ; в доп код
    mov dword [msg], 'AtI:' ; проверка
    mov dword [msg+4], 'BNeg'
    call msgOut
    mov eax, dword [binval]
    mov dword [msg+4], eax
    call msgOut
ret
;**************************число положительное***********************
POSITIVE:
    mov dword [msg+4], 'plus'
    call msgOut
    xor rcx, rcx
    mov cl, byte [StrLen]
    dec cl        ; 
    dec cl         ; номер последнего элемента с 0
    
    push rcx
    add cl, 00110000b       ; проверка должно выводиться число элементов с 0
    mov byte [msg+4], cl
    call msgOut
    pop rcx
    lea rdx, [buffer+rcx]      ; rdx адрес младшей введенной цифры
    
 ;   mov al, byte [rdx]      ; проверка
;    mov byte [msg+4], al
 ;   call msgOut
  
    mov dword [mult10], 1               ; инициализируем перед циклом
    mov dword [binval], 0
    
    cmp rcx, 0
    je LOOPEND
    LOOP3:                  ; тут будем преобразовывать в двоичный формат . в rcx число элементов 1 - старшая цифра перед минусом
    lea rdx, [buffer+rcx]      ; rdx адрес младшей введенной цифры
    
   ; push rcx
   ; mov al, byte [rdx]      ; проверка
   ; mov byte [msg+4], al
   ; call msgOut
   ; pop rcx
    mov al, byte [rdx]      ; текущий символ в al
    and eax, 0000Fh          ; очистить старшие 4 бита
    mul dword [mult10]      ; edx:eax=символ * 10^n
    
    push rcx    ;**********складываем с bin val 
    mov ebx, dword [binval]
    add eax, ebx
    mov dword [binval], eax
    ;***********************   10^n * 10   ***************
    mov eax, 10
    mul dword [mult10]  ;edx:eax=mult10*10
    mov dword [mult10], eax ; старшая часть не нужна
    
    pop rcx

    loop LOOP3
    LOOPEND: 
        lea rdx, [buffer+rcx]      ; rdx адрес младшей введенной цифры
    
   ; push rcx
   ; mov al, byte [rdx]      ; проверка
   ; mov byte [msg+4], al
   ; call msgOut
   ; pop rcx
    mov al, byte [rdx]      ; текущий символ в al
    and eax, 0000Fh          ; очистить старшие 4 бита
    mul dword [mult10]      ; edx:eax=символ * 10^n
    
    push rcx    ;**********складываем с bin val 
    mov ebx, dword [binval]
    add eax, ebx
    mov dword [binval], eax
    ;***********************   10^n * 10   ***************
    mov eax, 10
    mul dword [mult10]  ;edx:eax=mult10*10
    mov dword [mult10], eax ; старшая часть не нужна
    
    pop rcx ;************ посто ещё разок при rcx=0

    mov eax, dword [binval] ; проверка
    mov dword [msg+4], eax
    call msgOut
    
    
ret

SIRgCheck:
    cmp dword [binval], 32767
    jg ERRSI
    mov eax, 32768
    neg eax
    cmp dword [binval], eax
    jl ERRSI            
    mov byte [InpCheckFlag], 0
    ret
    ERRSI: mov byte [InpCheckFlag], 1
ret

;UIRgCheck:
 ;   cmp dword [binval], 11111111b
 ;   jg ERRUI
 ;   cmp dword [binval], 0b    
 ;   jl ERRUI            
 ;   mov byte [InpCheckFlag], 0
;    ret
;    ERRUI: mov byte [InpCheckFlag], 1
;ret

IntToASCI:
    mov dword [msg], 'ItA:'
    mov dword [msg+4], 'mesg'
    
    mov dword [mult10], 10               ; инициализируем перед циклом
    mov byte [BufOut+14], 0x0A
    cmp dword [XSSS], 0
    jl XLZERO
    ;************XSSS Больше или равен нулю
    mov dword [msg+4], 'Posi'
    call msgOut
    
    mov rcx, 11 ;всего будет цифр
    mov eax, dword [XSSS] 
    LOOPIAG:
    lea rdx, [BufOut+rcx]      ; rdx адрес текущей цифры
    xor edx, edx                ; edx:eax=XSSS
    div dword [mult10]          ; eax= XSSS/10 ; edx=XSSS%10
    or dl, 00110000b
    mov byte [BufOut+rcx], dl   ;цифру в буффер
    dec rcx
    cmp eax, 0
    jne LOOPIAG
    
    mov dword [msg+4], 'XSSS'
    call msgOut
    call BufOutFn
    
ret
    ;************XSSS меньше нуля
    XLZERO:
    mov dword [msg+4], 'Negv'
    call msgOut    
    
    neg dword [XSSS]
    mov rcx, 11 ;всего будет цифр
    mov eax, dword [XSSS] 
    LOOPIAL:
    lea rdx, [BufOut+rcx]      ; rdx адрес текущей цифры
    xor edx, edx                ; edx:eax=XSSS
    div dword [mult10]          ; eax= XSSS/10 ; edx=XSSS%10
    or dl, 00110000b
    mov byte [BufOut+rcx], dl   ;цифру в буффер
    dec rcx
    cmp eax, 0
    jne LOOPIAL
    mov byte [BufOut+rcx], '-'
    
    mov dword [msg+4], 'XSSS'
    call msgOut
    call BufOutFn
ret


;*********************************короткие функции ввода вывода и данные
msgOut:             ;
    mov byte [msg+8], 0x0A ; символ конца ввода
    mov rax, 1      ; Какую ф-ию вызывать?'write' системный вызов
    mov rdi, 0      ; куда выводить? в данном случае 1 это stdout
    mov rsi, msg    ; Указатель на данные
    mov rdx, msglen   ; Количество данных
    syscall       ; Вызов ядра прерывания
    ret
    
bufferIn:

    mov rax, 0   ; 
    mov rdx, 0   ; Дескриптор stdin
    mov rsi, buffer   ; Адрес буфера для хранения введенных данных
    mov rdx, 64   ; Максимальная длина ввода
    syscall      ; Прерывание - системный вызов
    mov byte [StrLen], al ; В rax возвращается число успешно прочитанных символов
    ret
    
bufferOut:
   mov rax, 1      ; Какую ф-ию вызывать?'write' системный вызов
   mov rdi, 0      ; куда выводить? в данном случае 1 это stdout
   mov rsi, buffer    ; Указатель на данные
   mov rdx, 64    ; Количество данных 
   syscall        ; Вызов ядра прерывания
   ret
   
BufOutFn:
   mov rax, 1      ; Какую ф-ию вызывать?'write' системный вызов
   mov rdi, 0      ; куда выводить? в данном случае 1 это stdout
   mov rsi, BufOut    ; Указатель на данные
   mov rdx, 15    ; Количество данных 
   syscall        ; Вызов ядра прерывания
   ret  

ExitFn:
        mov eax, 60     ; '_exit' системный вызов Чтобы корректно завершить текущий процесс
        mov edi, 0      ; Возвращаем 0 (все хорошо)
        syscall        ; Вызов ядра    

section .data       
  msg: db "FuN:mesgE"
  msglen: equ $- msg    ;длина msg
  buff: dd 5            ; с длиной строки не все ясно
  StrLen: db 3        ;Сколько удалось считать в буфер, устанавливает ее syscall. 100 за пределами ее возможного значения
  
  
  minusFlag: db 3   ; Запоминает, был ли изначально минус в введенном числе 0 - не было 1 - был
  InpCheckFlag: db 3 ; Функции ввода его устанавливают. Если 0 то все успешно если 1 то было что то не так
  
  mult10: dd 1 ; для умножения на 10
  binval: dd 0 ; для промежуточного результата преобразования из asci в bin 
  
  ASSS: dw 0    ;для модуля Lab2
  BSSS: dw 0    ;
  XSSS: dd 0    ;
  BufOut: times 15 db 32 ;буффер для вывода
section .bss
  buffer:  resb    64  ;nasm не умеет делать dup(?), только так


    