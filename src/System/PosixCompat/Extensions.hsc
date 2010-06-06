-- | This module provides some functions not present in the unix package.
module System.PosixCompat.Extensions (
         -- * Device IDs.
         CMajor, CMinor,
         deviceMajor, deviceMinor, makeDeviceID
   ) where


#if UNIX_IMPL
#include "HsUnixCompat.h"
#endif

import Foreign.C.Types
import System.PosixCompat.Types


type CMajor = CUInt
type CMinor = CUInt

-- | Gets the major number from a 'DeviceID' for a device file.
-- 
-- The portable implementation always returns @0@.
deviceMajor :: DeviceID -> CMajor
#if UNIX_IMPL
deviceMajor dev = unix_major dev

foreign import ccall unsafe "unix_major" unix_major :: CDev -> CUInt
#else
deviceMajor _ = 0
#endif

-- | Gets the minor number from a 'DeviceID' for a device file.
-- 
-- The portable implementation always returns @0@.
deviceMinor :: DeviceID -> CMinor
#if UNIX_IMPL
deviceMinor dev = unix_minor dev

foreign import ccall unsafe "unix_minor" unix_minor :: CDev -> CUInt
#else
deviceMinor _ = 0
#endif

-- | Creates a 'DeviceID' for a device file given a major and minor number.
makeDeviceID :: CMajor -> CMinor -> DeviceID
#if UNIX_IMPL
makeDeviceID ma mi = unix_makedev ma mi

foreign import ccall unsafe "unix_makedev" unix_makedev :: CUInt -> CUInt -> CDev
#else
makeDeviceID _ _ = 0
#endif

