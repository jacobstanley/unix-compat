module System.PosixCompat.Time (
    epochTime,
  ) where

#ifdef UNIX_IMPL

import System.Posix.Time

#else

import Control.Monad (liftM)
import System.Posix.Types (EpochTime)
import System.Time (ClockTime(..), getClockTime)

epochTime :: IO EpochTime
epochTime = liftM clockTimeToEpochTime getClockTime

clockTimeToEpochTime :: ClockTime -> EpochTime
clockTimeToEpochTime (TOD s _) = fromInteger s

#endif

