copy C:\Users\kck\AppData\Roaming\cabal\hs-gmail-0.1.0.0\ghc-7.4.2\libHShs-gmail*.a
ghc -c Glue.hs
ghc -c StartEnd.c
ghc -shared -o Glue.dll Glue.o StartEnd.o libHShs-gmail-0.1.0.0.a
runhaskell filter.hs Glue_stub.h stdcall
cl /I"C:\Program Files (x86)\Haskell Platform\2012.4.0.0\lib\include" haskell_from_cpp.cpp
