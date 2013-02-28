using namespace std;
#include <stdio.h>
#include <windows.h>

void (__stdcall *HsStart)();
void (*HsEnd)();
int (__stdcall *test)(char *, char *, int);
int (__stdcall *authenticate)(char *, char *, char *);
int (__stdcall *selectMailBox)(char *, char *);
int (__stdcall *fetch)(char *, char *);
int (__stdcall *search)(char *, char *);
int (__stdcall *setup)(char *);

struct FunctionRecord {
  int **funPtr;
  char *namePtr;
};

void loadFunctionTable();

int main()
{
  loadFunctionTable();

  HsStart(); // Initialize the Haskell subsystem

  setup("");
  printf("Press enter to continue\n");
  getchar();
  authenticate("auth.txt", "kashyapnrishi@gmail.com", "ya29.AHES6ZQtGETNSbt1NMPyS4cOc3C6eY7esAs30waDqQ82TSK7");
  selectMailBox("sel.txt", "INBOX");
  search("search.txt","ALL");
  fetch("fetch.txt","1 RFC822");
  printf("Press enter to continue\n");
  getchar();

  return 0;

}

void loadFunctionTable(){
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
    {(int **)&setup		, "setup@4"},
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

}
