{-# OPTIONS_GHC -fglasgow-exts -XTemplateHaskell #-}

import Test.Framework.TH
import Test.Framework.Providers.HUnit
import Test.HUnit

import Control.Monad

import System.PosixCompat (usingPortableImpl)
import qualified System.Posix.Time as Time
import qualified System.PosixCompat.Time as CompatTime

main = $(defaultMainGenerator)

case_running_portable_build = assertBool msg usingPortableImpl
  where msg = "tests are only valid when run against the portable build\n" ++
              "try rebuilding unix-compat using -fportable"


case_epochTime = do
    time <- Time.epochTime
    compatTime <- CompatTime.epochTime
    assertWithin 1 time compatTime


assertWithin delta expected actual =
    assertBool msg ok
  where
    ok  = abs (expected - actual) <= delta
    msg = "expected: " ++ show expected ++
          " ±" ++ show delta ++
          "\n but got: " ++ show actual

