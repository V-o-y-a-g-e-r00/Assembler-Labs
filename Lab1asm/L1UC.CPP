#include<iostream.h>
#include<stdio.h>
unsigned char  AAAS, BBBS, CCCS, ZNAM, REZ;
 int CHISL;
extern "C" {void	LAB1UC(void);}


void F_Cpp(void)
{
  CHISL=BBBS*2+CCCS/25;
  ZNAM=AAAS+AAAS/BBBS-1;
  REZ=CHISL/ZNAM;
  cout<<"F_Cpp:\n";
  printf("CHISL=%d\n",CHISL);
  printf("ZNAM=%d\n",ZNAM);
  printf("REZ=%d\n",REZ);
}


void F_ASM(void)
{
	LAB1UC();
	cout<<"ASM:\n";
	printf("CHISL=%d\n",CHISL);
	printf("ZNAM=%d\n", ZNAM);
	printf("REZ=%d\n", REZ);

}

void main(void)
{
  cout<<"Input AAAS"; scanf("%d", &AAAS);
  cout<<"Input BBBS"; scanf("%d", &BBBS);
  cout<<"Input CCCS"; scanf("%d", &CCCS);
  F_ASM();
  F_Cpp();
}

