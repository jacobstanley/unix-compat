module MkstempSpec(mkstempSpec) where
import Control.Monad.Parallel
import System.Directory
import System.IO
import System.PosixCompat ( mkstemp )
import Test.Hspec

mkstempSpec :: Spec
mkstempSpec = describe "mkstemp" $ do
    it "TODO" $ do
        let n = 10000
        hSetBuffering stdout NoBuffering

        putStr $ "Creating " ++ show n ++ " temp files..."
        xs <- replicateM n createTempFile
        if length xs == n
        then putStrLn "ok"
        else putStrLn "FAIL"

        putStr "Deleting temp files..."
        Control.Monad.Parallel.mapM_ removeFile xs
        putStrLn "ok"

createTempFile :: IO FilePath
createTempFile = do
    (p,h) <- mkstemp "tempfileXXXXXXX"
    hPutStrLn h "this is a temporary file"
    hClose h
    return p
