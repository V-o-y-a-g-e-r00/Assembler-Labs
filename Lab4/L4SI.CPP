#include<iostream.h>
#include<stdio.h>  // dla sscanf fgets
#include<stdlib.h> //dla strtol
#include<errno.h>  //dla funcsii InpInt
#define LEN 128
signed int  CSSS, DSSS, TEST1, TEST2;
signed int NSSS, ISSS, SUMS;
signed int ASSS[LEN];
extern "C" {void LAB4(void);}

int InpInt(int *a)  //vozvrashaet 0 pri uspehe i 1 pri vihode za predeli
{
    signed long int b;
    char buf[128], *enda;
    fgets(buf, 128, stdin);
    b=strtol(buf, &enda, 10);
    if ((errno==ERANGE) || (b>32767) || (b<-32768)) return 1;
    *a=(int)b;
    return 0;
}
void InpIntCycle(int *a, char name[])
{
    printf("Input %s [-32768;32767]:", name);
    int i=InpInt(a);
    while(i!=0)
    {
	printf("Out of range! Input %s again:", name);
	i=InpInt(a);
    }
}
void InpArr(int A[], int NSSS)
{
	int i;
	for(i=0; i<NSSS; i++)
	{
		printf("A[%d] ", i);
		InpIntCycle(&A[i], "A");
	}
}
void F_Cpp(void)
{
  int i;
  SUMS=0;
  for(i=0; i<NSSS; i++)
  {
	if((ASSS[i]>0) && (ASSS[i]>=DSSS) && (ASSS[i]<=CSSS))
	{
		SUMS++;
	}
  }
  cout<<"F_Cpp:\n";
  printf("SUMS=%d\n", SUMS);
}
void F_ASM(void)
{
	LAB4();
	cout<<"ASM:\n";
	printf("TEST1=%d\n", TEST1);
	printf("TEST2=%d\n", TEST2);
	printf("SUMS=%d\n", SUMS);
}
void main(void)
{
  InpIntCycle(&CSSS, "CSSS");
  InpIntCycle(&DSSS, "DSSS");
  InpIntCycle(&NSSS, "NSSS"); //Vvod ishodnih dannih

  InpArr(ASSS, NSSS);

  F_ASM();
  F_Cpp();
}