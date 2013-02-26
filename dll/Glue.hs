{-# LANGUAGE ForeignFunctionInterface #-}
module Glue where
import Network.HSGmail

adder :: Int -> Int -> IO Int  -- gratuitous use of IO
adder x y = do
      dummy
      return (x+y)


foreign export stdcall adder :: Int -> Int -> IO Int
