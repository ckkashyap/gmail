// haskell_from_cpp.cpp : main project file.

using namespace std;
#include <stdio.h>
#include <windows.h>

//#include "Glue_stub.h"


void (__stdcall *HsStart)();
void (*HsEnd)();
int (__stdcall *test)(char *, char *, int);
int (__stdcall *authenticate)(char *, char *, char *);
int (__stdcall *selectMailBox)(char *, char *);
int (__stdcall *fetch)(char *, char *);
int (__stdcall *search)(char *, char *);


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
	  {(int **)&HsStart		, "HsStart"},
	  {(int **)&HsEnd		, "HsEnd"},
	  {(int **)&authenticate	, "authenticate@12"},
	  {(int **)&selectMailBox	, "selectMailBox@8"},
	  {(int **)&fetch		, "fetch@8"},
	  {(int **)&search		, "search@8"},
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

	getchar();
	//	authenticate("auth.txt","kashyapnrishi@gmail.com", "ya29.AHES6ZT3cO7SMxGVKGddQCOohqOF5or3lELMF1VTG6Oj-FFd");
	selectMailBox("sel.txt", "INBOX");
	search("search.txt","ALL");
	
	while(1);

    return 0;

}
