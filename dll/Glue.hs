{-# LANGUAGE ForeignFunctionInterface #-}
module Glue where
import qualified Network.HSGmail as G
import Foreign.C.String


authenticate :: CString -> CString -> CString -> IO Int
authenticate o u a = do
     user <- peekCString u
     accessToken <- peekCString a
     outFile <- peekCString o
     G.authenticate outFile user accessToken
     return 1234;


generic2argFunc :: (String -> String -> IO ()) -> CString -> CString ->  IO Int
generic2argFunc f a1 a2 = do
              arg1 <- peekCString a1
              arg2 <- peekCString a2
              f arg1 arg2
              return 1234



fetch = generic2argFunc G.fetch
search = generic2argFunc G.search
selectMailBox  = generic2argFunc G.selectMailBox



foreign export stdcall authenticate :: CString -> CString -> CString -> IO Int
foreign export stdcall selectMailBox :: CString -> CString -> IO Int
foreign export stdcall fetch :: CString -> CString -> IO Int
foreign export stdcall search :: CString -> CString -> IO Int


