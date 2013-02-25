{-# LANGUAGE ForeignFunctionInterface #-}
module Glue where
import Network.HSGmail



adder :: Int -> Int -> IO Int  -- gratuitous use of IO
adder x y = return (bingo x y)


foreign export stdcall adder :: Int -> Int -> IO Int
