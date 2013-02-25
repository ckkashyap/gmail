{-# LANGUAGE OverloadedStrings #-}
module Network.HSGmail (dingo,bingo, getConnection) where
import qualified Data.ByteString as B
import qualified Network.Connection as NC
import Network (withSocketsDo)
import Data.Default

import Data.ByteString (ByteString)
import qualified Data.ByteString.Char8 as BS

import Control.Applicative ((<$>))
import Control.Monad


import qualified Control.Exception as E
import qualified System.IO.Error as E

import Data.Char

--type Connection = Types.Connection


dingo :: IO ()
dingo = putStrLn "hello from HSGmail"


bingo :: Int -> Int -> Int
bingo x y = 1234


{-
sendCommand' :: IMAPConnection -> String -> IO (ByteString, Int)
sendCommand' c cmdstr = do
  (_, num) <- withNextCommandNum c $ \num -> bsPutCrLf (stream c) $
              BS.pack $ show6 num ++ " " ++ cmdstr
  resp <- getResponse (stream c)
  return (resp, num)

show6 :: (Ord a, Num a, Show a) => a -> String
show6 n | n > 100000 = show n
        | n > 10000  = '0' : show n
        | n > 1000   = "00" ++ show n
        | n > 100    = "000" ++ show n
        | n > 10     = "0000" ++ show n
        | otherwise  = "00000" ++ show n


sendCommand :: Connection -> String
            -> (RespDerivs -> Result RespDerivs (ServerResponse, MboxUpdate, v))
            -> IO v
sendCommand imapc cmdstr pFunc =
    do (buf, num) <- sendCommand' imapc cmdstr
       let (resp, mboxUp, value) = eval pFunc (show6 num) buf
       case resp of
         OK _ _        -> do mboxUpdate imapc mboxUp
                             return value
         NO _ msg      -> fail ("NO: " ++ msg)
         BAD _ msg     -> fail ("BAD: " ++ msg)
         PREAUTH _ msg -> fail ("preauth: " ++ msg)
-}

getResponse :: NC.Connection -> IO ByteString
getResponse s = unlinesCRLF <$> getLS where 
            unlinesCRLF = BS.concat . concatMap (:[crlfStr])

            getLS = do 
                  l <- strip <$> connectionGetLine s
                  case () of
                    _ | isLiteral l -> do l' <- getLiteral l (getLitLen l)
                                          ls <- getLS
                                          return (l' : ls)
                      | isTagged l -> (l:) <$> getLS
                      | otherwise -> return [l]
            getLiteral l len = 
              do lit <- NC.connectionGet s len
                 l2 <- strip <$> connectionGetLine s
                 let l' = BS.concat [l, crlfStr, lit, l2]
                 if isLiteral l2
                   then getLiteral l' (getLitLen l2)
                   else return l'


            isLiteral l = BS.last l == '}' &&
                        BS.last (fst (BS.spanEnd isDigit (BS.init l))) == '{'
            getLitLen = read . BS.unpack . snd . BS.spanEnd isDigit . BS.init
            isTagged l = BS.head l == '*' && BS.head (BS.tail l) == ' '



crlfStr :: ByteString
crlfStr = BS.pack "\r\n"

strip :: ByteString -> ByteString
strip = fst . BS.spanEnd isSpace . BS.dropWhile isSpace


{-

          getLs =
              do l <- strip <$> bsGetLine s
                 case () of
                   _ | isLiteral l ->  do l' <- getLiteral l (getLitLen l)
                                          ls <- getLs
                                          return (l' : ls)
                     | isTagged l -> (l:) <$> getLs
                     | otherwise -> return [l]
          getLiteral l len = 
              do lit <- bsGet s len
                 l2 <- strip <$> bsGetLine s
                 let l' = BS.concat [l, crlfStr, lit, l2]
                 if isLiteral l2
                   then getLiteral l' (getLitLen l2)
                   else return l'
          crlfStr = BS.pack "\r\n"
          isLiteral l = BS.last l == '}' &&
                        BS.last (fst (BS.spanEnd isDigit (BS.init l))) == '{'
          getLitLen = read . BS.unpack . snd . BS.spanEnd isDigit . BS.init
          isTagged l = BS.head l == '*' && BS.head (BS.tail l) == ' '
-}







getConnection = withSocketsDo $ do
    ctx <- NC.initConnectionContext
    con <- NC.connectTo ctx $ NC.ConnectionParams
                              { NC.connectionHostname  = "imap.gmail.com"
                              , NC.connectionPort      = 993
                              , NC.connectionUseSecure = Just def
                              , NC.connectionUseSocks  = Nothing
                              }
    return con
                                                            
--    connectionPut con "GET / HTTP/1.0\r\n"
--    connectionPut con "\r\n"
--    r <- connectionGet con 2048
--    putStrLn $ show r
--    connectionClose con


connectionGetLine :: NC.Connection -> IO ByteString
connectionGetLine conn = do
                  b <- NC.connectionGet conn 1
                  if b == (BS.pack "\n") then 
                     return b 
                     else do   
                      bs <- connectionGetLine conn
                      return (BS.concat [b,bs])

                  
                  
