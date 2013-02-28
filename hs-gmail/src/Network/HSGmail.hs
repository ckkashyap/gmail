{-# LANGUAGE OverloadedStrings #-}
module Network.HSGmail where
import qualified Data.ByteString as B
import qualified Network.Connection as NC
import qualified Network as N
import qualified System.IO as SI
import qualified Control.Concurrent as CC

import Data.Default

import Data.ByteString.Base64
import Data.ByteString (ByteString)
import qualified Data.ByteString.Char8 as BS

import Control.Applicative ((<$>))
import Control.Monad

import qualified Control.Exception as E
import qualified System.IO.Error as E

import Data.Char (chr, isDigit, isSpace)

data GmailConnection = GmailConnection (NC.Connection, SI.Handle) | NoGmailConnection


-------------------------------------
------------SERVER-------------------
-------------------------------------

sendCommandAndGetResponse :: GmailConnection -> ByteString -> IO ByteString
sendCommandAndGetResponse con@(GmailConnection (c,h)) bs = do
            putStrLn ("REQUEST: " ++ (show bs))
            NC.connectionPut c (BS.concat [bs,"\r\n"])
            resp <- getResponse con
            putStr "RESPONSE: "
            BS.putStrLn (BS.take 1024 resp)
            return resp

authenticateServer :: GmailConnection -> ByteString -> IO ByteString
authenticateServer con authString = do
             r <- sendCommandAndGetResponse con authString
             return r

getAuthString :: ByteString -> ByteString -> ByteString
getAuthString user accessToken = encode $ BS.concat [ "user=", user, controlA, "auth=Bearer ", accessToken, controlA, controlA ]

controlA :: ByteString
controlA = BS.pack [(chr 1)]

getResponse :: GmailConnection -> IO ByteString
getResponse (GmailConnection (c,h)) = unlinesCRLF <$> getLS where 
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
    return $ GmailConnection (con, handle)

thePort = N.PortNumber 8888

data Command =  Authenticate String String String
              | Select String String
              | Search String String
              | Fetch String String
              | Bad
              deriving (Show,Read)



setup str = do
      CC.forkIO main
      CC.threadDelay 2000000


main = do
     con <- getConnection
     socket <- N.listenOn thePort
     serve socket con 1                  

serve socket con num = do
            putStrLn "Awating command"
            (handle, _, _) <- N.accept socket
            SI.hSetBuffering handle SI.LineBuffering
            line <- SI.hGetLine handle
            let command = parseCommand line
            putStrLn (show command)
            processCommand con num command
            SI.hPutStrLn handle "DONE"
            SI.hClose handle
            serve socket con ((\n->if (n+1) > 10000 then 0 else (n+1)) num)
            

parseCommand :: String -> Command
parseCommand str = let c = reads str :: [(Command, String)]
                   in case c of
                           [] -> Bad
                           ((cmd,_):_) -> cmd

processCommand con num (Authenticate outFile us at)  = do
      let accessToken = BS.pack at
          user = BS.pack us
          authString = getAuthString user accessToken
      sendCommandAndGetResponse con "C01 CAPABILITY"
      res <- authenticateServer con (BS.concat [ "A01 AUTHENTICATE XOAUTH2 ", authString ])
      BS.writeFile outFile res

processCommand con num (Select outFile str)  = do
      res<-sendCommandAndGetResponse con (BS.pack ("SL" ++ (show num) ++ " SELECT "++ str))
      BS.writeFile outFile res

processCommand con num (Search outFile str)  = do
      res<-sendCommandAndGetResponse con (BS.pack ("SR" ++ (show num) ++ " SEARCH "++ str))
      BS.writeFile outFile res

processCommand con num (Fetch outFile str)  = do
      res<-sendCommandAndGetResponse con (BS.pack ("FE" ++ (show num) ++ " FETCH "++ str))
      BS.writeFile outFile res


processCommand _ _ Bad  = do
               putStrLn "BAD command"
               return ()

-------------------------------------
------------CLIENT-------------------
-------------------------------------

pumpCommand :: Command -> IO ()
pumpCommand command = N.withSocketsDo $ do 
              handle <- N.connectTo "localhost" thePort
              SI.hSetBuffering handle SI.LineBuffering
              putStrLn (show command)
              SI.hPutStrLn handle (show command)
              line <- SI.hGetLine handle
              putStrLn $ "Server returned " ++ line
              return ()
            
fetch outFile mailId = do
      let command = Fetch outFile mailId
      pumpCommand command

authenticate outFile user token = do
             let command = Authenticate outFile user token
             pumpCommand command

search outFile query = do
       let command = Search outFile query
       pumpCommand command

selectMailBox outFile folder = do
              let command = Select outFile folder
              pumpCommand command

