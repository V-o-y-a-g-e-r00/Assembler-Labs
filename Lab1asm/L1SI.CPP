#include<iostream.h>
#include<stdio.h>
signed int  ASSS, BSSS, CSSS, ZNAM, REZ;
signed int CHISL;
extern "C" {void	LAB1(void);}


void F_Cpp(void)
{
  CHISL=BSSS*2+CSSS/25;
  ZNAM=ASSS+ASSS/BSSS-1;
  REZ=CHISL/ZNAM;
  cout<<"F_Cpp:\n";
  printf("CHISL=%d\n",CHISL);
  printf("ZNAM=%d\n",ZNAM);
  printf("REZ=%d\n",REZ);
}


void F_ASM(void)
{
	LAB1();
	cout<<"ASM:\n";
	printf("CHISL=%d\n",CHISL);
	printf("ZNAM=%d\n", ZNAM);
	printf("REZ=%d\n", REZ);
}

void main(void)
{
  cout<<"Input ASSS"; scanf("%d", &ASSS);
  cout<<"Input BSSS"; scanf("%d", &BSSS);
  cout<<"Input CSSS"; scanf("%d", &CSSS);
  F_ASM();
  F_Cpp();
}

