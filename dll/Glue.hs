{-# LANGUAGE ForeignFunctionInterface #-}
module Glue where
import Network.HSGmail
import Foreign.C.String

adder :: Int -> Int -> IO Int  -- gratuitous use of IO
adder x y = do
      dummy
      return (x+y)

test :: CString -> CString -> IO Int
test u a = do
     user <- peekCString u
     accessToken <- peekCString a
     putStrLn $ "user = " ++ user
     putStrLn $ "accessToken = " ++ accessToken
     download user accessToken
     return 100;


foreign export stdcall adder :: Int -> Int -> IO Int
foreign export stdcall test :: CString -> CString -> IO Int
