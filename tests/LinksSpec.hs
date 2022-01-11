module LinksSpec(linksSpec) where
import System.PosixCompat ( createSymbolicLink, removeLink, fileExist )
import Test.Hspec
import Test.HUnit
import System.IO.Error (tryIOError)
import System.Info(os)
import Control.Monad.Extra (whenM)

isWindows :: Bool
isWindows = os == "mingw32"

linksSpec :: Spec
linksSpec = describe "createSymbolicLink" $ do
    it "should error on Windows and succeed on other OSes" $ do
        whenM (fileExist "README2.md") $ removeLink "README2.md"
        result <- tryIOError $ createSymbolicLink "README.md" "README2.md"
        case result of
          Left _  | isWindows -> return ()
          Right _ | isWindows -> do
            removeLink "README2.md"
            assertFailure "Succeeded while expected to fail on Windows"
          Left e              -> assertFailure $ "Expected to succeed, but failed with " ++ show e
          Right _             -> removeLink "README2.md"
