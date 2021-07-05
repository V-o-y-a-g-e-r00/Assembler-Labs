#include<iostream.h>
#include<stdio.h>  // dla sscanf fgets
#include<stdlib.h> //dla strtol
float  ASSS, BSSS, CHISL=0, ZNAM=0, XSSS=0,	TEST1, TEST2;
signed int CSSS, TESTINT;
extern "C" {void LAB5(void);}

void F_Cpp(void)
{
	CHISL=BSSS*2+(float)CSSS/25;
	ZNAM=ASSS+ASSS/BSSS-1;
	XSSS=CHISL/ZNAM;
	cout<<"F_Cpp:\n";
    //	printf("TEST1=%f\n", float(sizeof(int)));
	printf("CHISL=%f\n", CHISL);
	printf("ZNAM=%f\n", ZNAM);
	printf("XSSS=%f\n", XSSS);

}

void F_ASM(void)
{
	LAB5();
	cout<<"ASM:\n";
//	printf("TEST1=%f\n", TEST1);
//	printf("TEST2=%f\n", TEST2);
//	printf("TESTINT=%d\n", TESTINT);
//	printf("ASSS=%f\n", ASSS);
	printf("CHISL=%f\n", CHISL);
	printf("ZNAM=%f\n", ZNAM);
	printf("XSSS=%f\n", XSSS);
}
void main(void)
{

  printf("ASSS:");
  scanf("%f", &ASSS);
  printf("BSSS:");
  scanf("%f", &BSSS);
  printf("CSSS:");
  scanf("%d", &CSSS);
  //	printf("ASSS=%f\n", ASSS);

  F_ASM();
  CHISL=0;
  ZNAM=0;
  XSSS=0;
  F_Cpp();
}