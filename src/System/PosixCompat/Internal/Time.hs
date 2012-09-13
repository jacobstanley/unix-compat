{-# LANGUAGE CPP #-}
{-
Compatibility wrapper to help manage the transition from
old-time to time packages. Only used at all on win32.
-}
module System.PosixCompat.Internal.Time (
      ClockTime
    , getClockTime
    , clockTimeToEpochTime
    , ModificationTime
    , modificationTimeToEpochTime
    ) where

import System.Posix.Types (EpochTime)

#ifdef OLD_TIME

import System.Time (ClockTime(TOD), getClockTime)

clockTimeToEpochTime :: ClockTime -> EpochTime
clockTimeToEpochTime (TOD s _) = fromInteger s

type ModificationTime = ClockTime

modificationTimeToEpochTime :: ModificationTime -> EpochTime
modificationTimeToEpochTime = clockTimeToEpochTime

#else

import Data.Time.Clock (UTCTime)
import Data.Time.Clock.POSIX (POSIXTime, getPOSIXTime, utcTimeToPOSIXSeconds)

type ClockTime = POSIXTime

getClockTime :: IO ClockTime
getClockTime = getPOSIXTime

clockTimeToEpochTime :: ClockTime -> EpochTime
clockTimeToEpochTime = fromInteger . floor

type ModificationTime = UTCTime

modificationTimeToEpochTime :: UTCTime -> EpochTime
modificationTimeToEpochTime = clockTimeToEpochTime . utcTimeToPOSIXSeconds

#endif
