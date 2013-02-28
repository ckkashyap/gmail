// haskell_from_cpp.cpp : main project file.

using namespace std;
#include <stdio.h>
#include <windows.h>

//#include "Glue_stub.h"


void (__stdcall *HsStart)();
void (*HsEnd)();
int (__stdcall *test)(char *, char *, int);
int (__stdcall *initializeConnection)(char *, char *);
int (__stdcall *selectMailBox)(char *, char *);


struct FunctionRecord {
  int **funPtr;
  char *namePtr;
};


int main()
{
	HINSTANCE hDLL = LoadLibrary("Glue.dll");    

	if(hDLL == NULL) {
		printf("Error loading the DLL\n");
		exit(0);
	}
	
	FunctionRecord table [] = {
	  {(int **)&HsStart			, "HsStart"},
	  {(int **)&HsEnd			, "HsEnd"},
	  {(int **)&test			, "test@12"},
	  {(int **)&initializeConnection	, "initializeConnection@8"},
	  {(int **)&selectMailBox		, "selectMailBox@8"},
	};

	for(int i=0;i<(sizeof(table)/sizeof(FunctionRecord));i++) {
	  printf("Hooking up %s\n", table[i].namePtr);
	  int *p = (int *)GetProcAddress(hDLL, table[i].namePtr);
	  if(p == NULL){
	    printf("Error getting the function %s\n",table[i].namePtr);
	    exit(0);
	  }
	  *(table[i].funPtr) = p;
	}

	HsStart(); // Initialize the Haskell subsystem

	//test("kashyapnrishi@gmail.com", "ya29.AHES6ZRdjOg9hBUHZOxjRReiCf0mfoV8VwPdpS2edLeh-zBR", 1);
	initializeConnection("kashyapnrishi@gmail.com", "ya29.AHES6ZRdjOg9hBUHZOxjRReiCf0mfoV8VwPdpS2edLeh-zBR");
	printf("press enter to continue..\n");
	getchar();
	selectMailBox("out.txt", "INBOX");
    return 0;

}
