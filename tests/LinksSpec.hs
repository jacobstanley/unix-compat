module LinksSpec(linksSpec) where

import Control.Concurrent ( threadDelay )
import Control.Exception ( finally )
import qualified System.Directory as D
import System.Info ( os )
import System.IO.Error ( tryIOError )
import System.IO.Temp
import System.PosixCompat
import Test.Hspec
import Test.HUnit

isWindows :: Bool
isWindows = os == "mingw32"

linksSpec :: Spec
linksSpec = do
  describe "createSymbolicLink" $ do
    it "should error on Windows and succeed on other OSes" $ do
      runInTempDir $ do
        writeFile "file" ""
        result <- tryIOError $ createSymbolicLink "file" "file_link"
        case result of
          Left _  | isWindows -> return ()
          Right _ | isWindows -> do
            assertFailure "Succeeded while expected to fail on Windows"
          Left e              -> assertFailure $ "Expected to succeed, but failed with " ++ show e
          Right _             -> return ()
  describe "getSymbolicLinkStatus" $ do
    it "should detect symbolic link to a file" $ do
      runFileLinkTest $ do
        stat <- getSymbolicLinkStatus "file_link"
        assert $ isSymbolicLink stat
    it "should detect symbolic link to a directory" $ do
      runDirLinkTest $ do
        stat <- getSymbolicLinkStatus "dir_link"
        assert $ isSymbolicLink stat
    it "should give later time stamp than getFileStatus for link to file" $ do
      runFileLinkTest $ do
        lstat_mtime <- modificationTimeHiRes <$> getSymbolicLinkStatus "file_link"
        stat_mtime <- modificationTimeHiRes <$> getFileStatus "file_link"
        assert $ lstat_mtime > stat_mtime
    it "should give later time stamp than getFileStatus for link to dir" $ do
      runDirLinkTest $ do
        lstat_mtime <- modificationTimeHiRes <$> getSymbolicLinkStatus "dir_link"
        stat_mtime <- modificationTimeHiRes <$> getFileStatus "dir_link"
        assert $ lstat_mtime > stat_mtime
    it "should give a different fileID than getFileStatus for link to file" $ do
      runFileLinkTest $ do
        lstat_id <- fileID <$> getSymbolicLinkStatus "file_link"
        fstat_id <- fileID <$> getFileStatus "file_link"
        assert $ lstat_id /= fstat_id
    it "should give a different fileID than getFileStatus for link to dir" $ do
      runDirLinkTest $ do
        lstat_id <- fileID <$> getSymbolicLinkStatus "dir_link"
        fstat_id <- fileID <$> getFileStatus "dir_link"
        assert $ lstat_id /= fstat_id
  describe "getFileStatus" $ do
    it "should detect that symbolic link target is a file" $ do
      runFileLinkTest $ do
        stat <- getFileStatus "file_link"
        assert $ isRegularFile stat
    it "should detect that symbolic link target is a directory" $ do
      runDirLinkTest $ do
        stat <- getFileStatus "dir_link"
        assert $ isDirectory stat
    it "should be equal for link and link target (except access time)" $ do
      runFileLinkTest $ do
        fstat <- getFileStatus "file"
        flstat <- getFileStatus "file_link"
        assert $ fstat `mostlyEq` flstat
      runDirLinkTest $ do
        fstat <- getFileStatus "dir"
        flstat <- getFileStatus "dir_link"
        assert $ fstat `mostlyEq` flstat

  where

    runFileLinkTest action =
      runInTempDir $ do
        writeFile "file" ""
        threadDelay delay
        D.createFileLink "file" "file_link"
        action

    runDirLinkTest action =
      runInTempDir $ do
        D.createDirectory "dir"
        threadDelay delay
        D.createDirectoryLink "dir" "dir_link"
        action

    runInTempDir action = do
      orig <- D.getCurrentDirectory
      withTempDirectory orig "xxxxxxx" $ \tmp -> do
        D.setCurrentDirectory tmp
        action `finally` D.setCurrentDirectory orig

    -- We need to set the delay this high because otherwise the timestamp test
    -- above fails on Linux and Windows, though not on MacOS. This seems to be
    -- an artefact of the GHC runtime system which gives two subsequently
    -- created files the same timestamp unless the delay is large enough.
    delay = 10000

    -- Test equality for all parts except accessTime
    mostlyEq :: FileStatus -> FileStatus -> Bool
    mostlyEq x y = tuple x == tuple y
      where
        tuple s =
          ( deviceID s
          , fileID s
          , fileMode s
          , linkCount s
          , fileOwner s
          , fileGroup s
          , specialDeviceID s
          , fileSize s
          , modificationTime s
          , statusChangeTime s
          , modificationTimeHiRes s
          , statusChangeTimeHiRes s
          )
