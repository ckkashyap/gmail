{-# LANGUAGE OverloadedStrings #-}
module Network.HSGmail (dummy, download) where
import qualified Data.ByteString as B
import qualified Network.Connection as NC
import qualified Network as N
import qualified System.IO as SI

import Data.Default

import Data.ByteString.Base64
import Data.ByteString (ByteString)
import qualified Data.ByteString.Char8 as BS

import Control.Applicative ((<$>))
import Control.Monad

import qualified Control.Exception as E
import qualified System.IO.Error as E

import Data.Char (chr, isDigit, isSpace)

type GmailConnection = (NC.Connection, SI.Handle)

sendCommandAndGetResponse :: GmailConnection -> ByteString -> IO ByteString
sendCommandAndGetResponse (c,h) bs = do
            putStrLn ("REQUEST: " ++ (show bs))
            NC.connectionPut c (BS.concat [bs,"\r\n"])
            resp <- getResponse (c,h)
            putStr "RESPONSE: "
            BS.putStrLn (BS.take 1024 resp)
            return resp

authenticate :: GmailConnection -> ByteString -> IO ByteString
authenticate (c,h) authString = do
             r <- sendCommandAndGetResponse (c,h) authString
             return r

getAuthString :: ByteString -> ByteString -> ByteString
getAuthString user accessToken = encode $ BS.concat [ "user=", user, controlA, "auth=Bearer ", accessToken, controlA, controlA ]

doit c = do
     putStr "Enter command: "
     str <- getLine
     sendCommandAndGetResponse c (BS.pack str)


dummy = dummy1 "kashyapnrishi@gmail.com" "ya29.AHES6ZQ9yFCqFbOmwKVSSww7u0uf4AIKT9YPGdhbNaylLp_M"


download :: String -> String -> IO ()
download u a = do
         dummy1 u a
         return ()

dummy1 u at = do
      let accessToken = BS.pack at
          user = BS.pack u
          authString = getAuthString user accessToken
      
      (c,h) <- getConnection
      sendCommandAndGetResponse (c,h) "C01 CAPABILITY"
      authenticate (c,h) (BS.concat [ "A01 AUTHENTICATE XOAUTH2 ", authString ])
      sendCommandAndGetResponse (c,h) "S01 SELECT INBOX"
      l <- sendCommandAndGetResponse (c,h) "FETCH01 FETCH 9 RFC822"
      BS.writeFile "out.txt" l
      return (c,h)

        
controlA :: ByteString
controlA = BS.pack [(chr 1)]


getResponse :: GmailConnection -> IO ByteString
getResponse (c,h) = unlinesCRLF <$> getLS where 
            unlinesCRLF = BS.concat . concatMap (:[crlfStr])

            getLS = do 
                  l <- strip <$> connectionGetLine c
                  case () of
                    _ | isLiteral l -> do l' <- getLiteral l (getLitLen l)
                                          ls <- getLS
                                          return (l' : ls)
                      | isTagged l -> (l:) <$> getLS
                      | otherwise -> return [l]
            getLiteral l len = 
              do
                 SI.hSetBuffering h SI.NoBuffering
                 lit <- connectionGetNBytes c len
                 SI.hSetBuffering h SI.LineBuffering
                 l2 <- strip <$> connectionGetLine c
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

connectionGetLine :: NC.Connection -> IO ByteString
connectionGetLine conn = do
                  b <- NC.connectionGet conn 1
                  if b == (BS.pack "\n") then 
                     return b 
                     else do   
                      bs <- connectionGetLine conn
                      return (BS.concat [b,bs])

                  
connectionGetNBytes :: NC.Connection -> Int -> IO ByteString
connectionGetNBytes _ 0 = return (BS.empty)
connectionGetNBytes c n = do
                    l <- NC.connectionGet c n
                    let ll = BS.length l
                    remaining <- connectionGetNBytes c (n - ll)
                    return (BS.concat [l, crlfStr, remaining])

getConnection :: IO GmailConnection
getConnection = N.withSocketsDo $ do
    handle <- N.connectTo "imap.gmail.com" (N.PortNumber 993)
    ctx <- NC.initConnectionContext
    con <- NC.connectFromHandle ctx handle $ NC.ConnectionParams
                              { NC.connectionHostname  = "imap.gmail.com"
                              , NC.connectionPort      = 993
                              , NC.connectionUseSecure = Just def
                              , NC.connectionUseSocks  = Nothing
                              }
    return (con, handle)

getConnection' = N.withSocketsDo $ do
    ctx <- NC.initConnectionContext
    con <- NC.connectTo ctx $ NC.ConnectionParams
                              { NC.connectionHostname  = "imap.gmail.com"
                              , NC.connectionPort      = 993
                              , NC.connectionUseSecure = Just def
                              , NC.connectionUseSocks  = Nothing
                              }
    return con
