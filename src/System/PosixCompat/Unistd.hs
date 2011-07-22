{-|
This module makes the operations exported by @System.Posix.Unistd@
available on all platforms. On POSIX systems it re-exports operations from
@System.Posix.Unistd@, on other platforms it emulates the operations as far
as possible.
-}
module System.PosixCompat.Unistd
    (
    -- * System environment
      SystemID(..)
    , getSystemID
    -- * Sleeping
    , sleep
    , usleep
    , nanosleep
    ) where

#ifdef UNIX_IMPL

import System.Posix.Unistd

#else

import Control.Concurrent (threadDelay)

data SystemID = SystemID {
      systemName :: String
    , nodeName :: String
    , release :: String
    , version :: String
    , machine :: String
    } deriving (Eq, Read, Show)

getSystemID :: IO SystemID
getSystemID = undefined

-- | Sleep for the specified duration (in seconds). Returns the time
-- remaining (if the sleep was interrupted by a signal, for example).
--
-- On non-Unix systems, this is implemented in terms of
-- 'Control.Concurrent.threadDelay'.
--
-- GHC Note: the comment for 'usleep' also applies here.
sleep :: Int -> IO Int
sleep secs = threadDelay (secs * 1000000) >> return 0

-- | Sleep for the specified duration (in microseconds).
--
-- On non-Unix systems, this is implemented in terms of
-- 'Control.Concurrent.threadDelay'.
--
-- GHC Note: 'Control.Concurrent.threadDelay' is a better
-- choice. Without the @-threaded@ option, 'usleep' will block all other
-- user threads. Even with the @-threaded@ option, 'usleep' requires a
-- full OS thread to itself. 'Control.Concurrent.threadDelay' has
-- neither of these shortcomings.
usleep :: Int -> IO ()
usleep = threadDelay

-- | Sleep for the specified duration (in nanoseconds).
--
-- On non-Unix systems, this is implemented in terms of
-- 'Control.Concurrent.threadDelay'.
nanosleep :: Integer -> IO ()
nanosleep nsecs = threadDelay (round (fromIntegral nsecs / 1000 :: Double))

#endif
