import Control.Monad
import System.PosixCompat
import System.IO

main :: IO ()
main = replicateM_ 10 mktemp

mktemp :: IO ()
mktemp = do
    (p,h) <- mkstemp "tempfileXXXXXXX"
    putStrLn $ "Created temp file: " ++ p
    hPutStrLn h "This is a temp file"
    hClose h
