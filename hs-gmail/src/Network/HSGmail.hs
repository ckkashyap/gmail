{-# LANGUAGE OverloadedStrings #-}
module Network.HSGmail (dingo,bingo, getConnection, sendCommandAndGetResponse, dummy) where
import qualified Data.ByteString as B
import qualified Network.Connection as NC
import Network (withSocketsDo)
import Data.Default

import Data.ByteString.Base64
import Data.ByteString (ByteString)
import qualified Data.ByteString.Char8 as BS

import Control.Applicative ((<$>))
import Control.Monad

import qualified Control.Exception as E
import qualified System.IO.Error as E

import Data.Char (chr, isDigit, isSpace)

dingo :: IO ()
dingo = putStrLn "hello from HSGmail"


bingo :: Int -> Int -> Int
bingo x y = 1234


sendCommandAndGetResponse :: NC.Connection -> ByteString -> IO ByteString
sendCommandAndGetResponse c bs = do
            putStrLn (show bs)
            NC.connectionPut c (BS.concat [bs,"\r\n"])
            resp <- getResponse c
            return resp

authenticate :: NC.Connection -> ByteString -> IO ByteString
authenticate c authString = do
             r <- sendCommandAndGetResponse c authString
             return r

getAuthString :: ByteString -> ByteString -> ByteString
getAuthString user accessToken = encode $ BS.concat [ "user=", user, controlA, "auth=Bearer ", accessToken, controlA, controlA ]



dummy u at = do
      let accessToken = BS.pack at
          user = BS.pack u
          authString = getAuthString user accessToken
      
      c <- getConnection
      sendCommandAndGetResponse c "C01 CAPABILITY"
      authenticate c (BS.concat [ "A01 AUTHENTICATE XOAUTH2 ", authString ]) 

        


controlA :: ByteString
controlA = BS.pack [(chr 1)]



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




getConnection = withSocketsDo $ do
    ctx <- NC.initConnectionContext
    con <- NC.connectTo ctx $ NC.ConnectionParams
                              { NC.connectionHostname  = "imap.gmail.com"
                              , NC.connectionPort      = 993
                              , NC.connectionUseSecure = Just def
                              , NC.connectionUseSocks  = Nothing
                              }
    return con
                                                            
connectionGetLine :: NC.Connection -> IO ByteString
connectionGetLine conn = do
                  b <- NC.connectionGet conn 1
                  if b == (BS.pack "\n") then 
                     return b 
                     else do   
                      bs <- connectionGetLine conn
                      return (BS.concat [b,bs])

                  
                  
