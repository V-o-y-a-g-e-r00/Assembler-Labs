.387 	;dla assemblirovania instrukcii soprocessora 80387
.MODEL Large,C
	.data
	Extrn   ASSS:dword, BSSS:dword, CSSS:word, CHISL:dword, ZNAM:dword, XSSS:dword
	Extrn TEST1:dword, TEST2:dword, TESTINT:word
	two DD 2
	twfv DD 25
	one DD 1
	.code
	Public  LAB5
LAB5  proc    far
	FINIT	;inicializacia
     ;	mov ax, 33
;********* CHISL=b*2+c/25 *************
	fld BSSS	;st(0)=BSSS
	fimul two	;st(0)=st(0)*two=BSSS*two
	fst TEST1	;TEST1=st(0)
   ;	fst st(1)	;st(1)=st(0)
	fild CSSS	;st(0)=CSSS	   st(1)=BSSS*two
	fidiv twfv	;st(0)=st(0)/twfv=CSSS/twfv  st(1)=BSSS*two
	faddp st(1), st	;st(0)=st(0)+st(1)
	fst CHISL	;CHISL=b*2+c/25 st(0)=CHISL
;********* ZNAM=a+a/b-1 ***************
	fld ASSS        ;st(0)=ASSS st(1)=CHISL
	fld BSSS	;st(0)=BSSS st(1)=ASSS       st(2)=CHISL
	fdivr st, st(1) ;st(0)=ASSS/BSSS st(1)=ASSS  st(2)=CHISL
	faddp st(1), st	;st(0)=st(0)+st(1)           st(1)=CHISL
	fisub one	;st(0)=st(0)-one=ASSS+ASSS/BSSS-1   st(1)=CHISL
	fst ZNAM	;st(0)=ZNAM   st(1)=CHISL    ZNAM=ASSS+ASSS/BSSS-1
;********* XSSS=CHISL/ZNAM ************
	fdivp st(1), st ;st(0)=CHISL/ZNAM st(7)=pop
	fstp XSSS	;st(0)=pop XSSS=CHISL/ZNAM
ret
LAB5  endp
	end