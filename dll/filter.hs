import Prelude hiding (words, unwords, lines, unlines, length)
import System.IO hiding (hGetContents, hPutStr)
import System.Environment
import System.Directory
import Text.Regex.Posix
import Data.ByteString.Lazy.Char8 hiding (filter, putStrLn)

fltr :: String -> ByteString -> ByteString
fltr pat = unlines . filter (\l -> matches l) . lines where
	matches str = 0 == length b where
			(a,b,c,d) = str =~ pat ::(ByteString, ByteString, ByteString, [ByteString]) 

tempFileName =  (++) ".temp"

performFiltering :: String -> String -> IO ()
performFiltering file pat = do
	inHandle <- openFile file ReadMode
	contents <- hGetContents inHandle
	outHandle <- openFile (tempFileName file) WriteMode
	let modifiedContents = fltr pat contents
	hPutStr outHandle modifiedContents

	mapM hClose [inHandle, outHandle]

	removeFile file
	renameFile (tempFileName file) file

main = do
	args <- getArgs
	case args of
		[file, pat] -> performFiltering file pat
		(_:_:_)     -> putStrLn "Too many arguments - I just need filename and filter pattern"
		_           -> putStrLn "Wrong usage - please provide the input file and filter pattern"
			
