{-# LANGUAGE ForeignFunctionInterface #-}
module Glue where
import qualified Network.HSGmail as G
import Foreign.C.String

adder :: Int -> Int -> IO Int  -- gratuitous use of IO
adder x y = do
      G.dummy
      return (x+y)

test :: CString -> CString -> Int -> IO Int
test u a n = do
     user <- peekCString u
     accessToken <- peekCString a
     putStrLn $ "user = " ++ user
     putStrLn $ "accessToken = " ++ accessToken
     putStrLn $ "Mail # = " ++ (show n)
     G.download user accessToken n
     return 100;


initializeConnection :: CString -> CString -> IO Int
initializeConnection u a = do
     user <- peekCString u
     accessToken <- peekCString a
     putStrLn $ "Initialize called"
     putStrLn $ "user = " ++ user
     putStrLn $ "accessToken = " ++ accessToken
     G.initializeConnection user accessToken
     return 1234;


selectMailBox :: CString -> CString -> IO Int
selectMailBox o f = do
              outFile <- peekCString o
              folder <- peekCString f
              G.selectMailBox outFile folder
              return 5678



foreign export stdcall adder :: Int -> Int -> IO Int
foreign export stdcall test :: CString -> CString -> Int -> IO Int
foreign export stdcall initializeConnection :: CString -> CString -> IO Int
foreign export stdcall selectMailBox :: CString -> CString -> IO Int

