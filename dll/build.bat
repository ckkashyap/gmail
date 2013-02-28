ghc -c Glue.hs
ghc -c StartEnd.c
ghc -shared -o Glue.dll Glue.hs StartEnd.o
runhaskell filter.hs Glue_stub.h stdcall
cl /I"C:\Program Files (x86)\Haskell Platform\2012.4.0.0\lib\include" main.cpp
