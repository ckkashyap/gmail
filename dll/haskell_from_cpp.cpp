// haskell_from_cpp.cpp : main project file.

using namespace std;
#include <stdio.h>
#include <windows.h>

#include "Glue_stub.h"


void (__stdcall *HsStart)();
void (*HsEnd)();
int (__stdcall *adder)(int, int);


int main()
{
	HINSTANCE hDLL = LoadLibrary("Glue.dll");    

	if(hDLL == NULL) {
		printf("Error loading the DLL\n");
		exit(0);
	}

	HsStart = (void (__stdcall *)())GetProcAddress(hDLL, "HsStart");

	if(HsStart == NULL) {
		printf("Error getting the function handle HsStart \n");
		exit(0);
	}

	HsEnd = (void (*)())GetProcAddress(hDLL, "HsEnd");

	if(HsEnd == NULL) {
		printf("Error getting the function handle HsEnd\n");
		exit(0);
	}

	adder = (int (_stdcall *)(int,int))GetProcAddress(hDLL, "adder@8");

	if(adder == NULL) {
		printf("Error getting the function handle adder\n");
		exit(0);
	}

	HsStart();

    // can now safely call functions from the DLL
    printf("12 + 5 =%i\n", adder(12,5))    ;
    return 0;

}
