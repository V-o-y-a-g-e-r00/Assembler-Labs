#include<iostream.h>
#include<stdio.h>  // dla sscanf fgets
#include<stdlib.h> //dla strtol
#include<errno.h>  //dla funcsii InpInt
unsigned int  ASSS, BSSS, TEST1, TEST2;
signed long int XSSS, TESTLZ, TESTUDIV, TDIVID;
extern "C" {void	LAB2(void); void UDIV(void);}

int InpInt(unsigned int *a)  //vozvrashaet 0 pri uspehe i 1 pri vihode za predeli
{
    signed long int b;
    char buf[128], *enda;
    fgets(buf, 128, stdin);
    b=strtol(buf, &enda, 10);
    if ((errno==ERANGE) || (b>65535) || (b<0)) return 1;
    *a=(int)b;
    return 0;
}

void InpIntCycle(unsigned int *a, char name[])
{
    printf("Input %s [0;65535]:", name);
    int i=InpInt(a);
    while(i!=0)
    {
	printf("Out of range! Input %s again:", name);
	i=InpInt(a);
    }
}

void F_Cpp(void)
{
  if(ASSS>BSSS)
  {
	XSSS=((long)ASSS+86)*6;
  }
  if(ASSS==BSSS)
  {
	XSSS=-16;
  }
  if(ASSS<BSSS)
  {
	XSSS=(128-(long)ASSS)/BSSS;
  }
  cout<<"F_Cpp:\n";
//  printf("TEST1=%d\n", TEST1);
//  printf("TEST2=%d\n",TEST2);
  printf("XSSS=%ld\n",XSSS);
}


void F_ASM(void)
{
	LAB2();
   //	UDIV();
	cout<<"ASM:\n";
//	printf("TEST1=%d\n", TEST1);
//	printf("TEST2=%d\n", TEST2);
//	printf("TESTLZ=%d\n", TESTLZ);
//	printf("TESTUDIV=%ld\n", TESTUDIV);
	printf("XSSS=%ld\n", XSSS);
}

void main(void)
{
  InpIntCycle(&ASSS, "ASSS");
  InpIntCycle(&BSSS, "BSSS");
//  cout<<"Input TDIVID"; scanf("%ld", &TDIVID);
  F_ASM();
  F_Cpp();
}