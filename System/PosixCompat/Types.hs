module System.PosixCompat.Types (
  module System.Posix.Types,
#ifndef UNIX_IMPL
  UserID, GroupID, LinkCount
#endif
  ) where

import System.Posix.Types

#ifndef UNIX_IMPL

import Data.Word (Word32)

newtype UserID = UserID Word32
  deriving (Eq, Ord, Enum, Bounded, Integral, Num, Real)
instance Show UserID where show (UserID x) = show x
instance Read UserID where readsPrec i s = [ (UserID x, s')
                                           | (x,s') <- readsPrec i s]

newtype GroupID = GroupID Word32
  deriving (Eq, Ord, Enum, Bounded, Integral, Num, Real)
instance Show GroupID where show (GroupID x) = show x
instance Read GroupID where readsPrec i s = [ (GroupID x, s')
                                            | (x,s') <- readsPrec i s]

newtype LinkCount = LinkCount Word32
  deriving (Eq, Ord, Enum, Bounded, Integral, Num, Real)
instance Show LinkCount where show (LinkCount x) = show x
instance Read LinkCount where readsPrec i s = [ (LinkCount x, s')
                                              | (x,s') <- readsPrec i s]

#endif

