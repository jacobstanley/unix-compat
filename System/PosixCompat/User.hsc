module System.PosixCompat.User (
    -- * User environment
    -- ** Querying the user environment
    getRealUserID,
    getRealGroupID,
    getEffectiveUserID,
    getEffectiveGroupID,
    getGroups,
    getLoginName,
    getEffectiveUserName,

    -- *** The group database
    GroupEntry(..),
    getGroupEntryForID,
    getGroupEntryForName,
    getAllGroupEntries,

    -- *** The user database
    UserEntry(..),
    getUserEntryForID,
    getUserEntryForName,
    getAllUserEntries,

    -- ** Modifying the user environment
    setUserID,
    setGroupID
  ) where

#if UNIX_IMPL

#include "HsUnixCompat.h"

import System.Posix.User

#else /* Portable implementation */

import System.IO.Error
import System.PosixCompat.Types

#endif


#if UNIX_IMPL

#if __GLASGOW_HASKELL__<605
getAllGroupEntries :: IO [GroupEntry]
getAllGroupEntries = return []

getAllUserEntries :: IO [UserEntry]
getAllUserEntries = return []
#endif

#else /* Portable implementation */

unsupported :: String -> IO a
unsupported f = ioError $ mkIOError illegalOperationErrorType x Nothing Nothing
    where x = "System.PosixCompat.User." ++ f ++ ": not supported"

-- -----------------------------------------------------------------------------
-- User environment

getRealUserID :: IO UserID
getRealUserID = unsupported "getRealUserID"

getRealGroupID :: IO GroupID
getRealGroupID = unsupported "getRealGroupID"

getEffectiveUserID :: IO UserID
getEffectiveUserID = unsupported "getEffectiveUserID"

getEffectiveGroupID :: IO GroupID
getEffectiveGroupID = unsupported "getEffectiveGroupID"

getGroups :: IO [GroupID]
getGroups = return []

getLoginName :: IO String
getLoginName = unsupported "getLoginName"

setUserID :: UserID -> IO ()
setUserID uid = return ()

setGroupID :: GroupID -> IO ()
setGroupID gid = return ()

-- -----------------------------------------------------------------------------
-- User names

getEffectiveUserName :: IO String
getEffectiveUserName = unsupported "getEffectiveUserName"

-- -----------------------------------------------------------------------------
-- The group database 

data GroupEntry =
 GroupEntry {
  groupName    :: String,
  groupPassword :: String,
  groupID      :: GroupID,
  groupMembers :: [String]
 } deriving (Show, Read, Eq)

getGroupEntryForID :: GroupID -> IO GroupEntry
getGroupEntryForID gid = unsupported "getGroupEntryForID"

getGroupEntryForName :: String -> IO GroupEntry
getGroupEntryForName name = unsupported "getGroupEntryForName"

getAllGroupEntries :: IO [GroupEntry]
getAllGroupEntries = return []

-- -----------------------------------------------------------------------------
-- The user database (pwd.h)

data UserEntry =
 UserEntry {
   userName      :: String,
   userPassword  :: String,
   userID        :: UserID,
   userGroupID   :: GroupID,
   userGecos     :: String,
   homeDirectory :: String,
   userShell     :: String
 } deriving (Show, Read, Eq)

getUserEntryForID :: UserID -> IO UserEntry
getUserEntryForID uid = unsupported "getUserEntryForID"

getUserEntryForName :: String -> IO UserEntry
getUserEntryForName name = unsupported "getUserEntryForName"

getAllUserEntries :: IO [UserEntry]
getAllUserEntries = return []

#endif
