{-# LANGUAGE CPP #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}

{-|
This module re-exports the types from @System.Posix.Types@ on all platforms.
It defines drop-in replacements for types that are missing from that module on a
specific OS. Namely Mac OS and Windows are supported.
-}
module System.PosixCompat.Types (
      module System.Posix.Types
#ifdef darwin_HOST_OS
    , CTimer(..)
#endif
#ifdef mingw32_HOST_OS
    , CBlkCnt(..)
    , CBlkSize(..)
    , CCc(..)
    , CFsBlkCnt(..)
    , CFsFilCnt(..)
    , UserID
    , CUid(..)
    , GroupID
    , CGid(..)
    , LinkCount
    , CNlink(..)
#endif
    ) where

import System.Posix.Types

#ifdef darwin_HOST_OS
import Foreign.C.Types (CUIntPtr)

newtype CTimer = CTimer CUIntPtr
  deriving (Eq, Ord)
instance Show CTimer where show (CTimer x) = show x

#else
#ifdef mingw32_HOST_OS
-- Since CIno (FileID's underlying type) reflects <sys/type.h> ino_t,
-- which mingw defines as short int (int16), it must be overriden to
-- match the size of windows fileIndex (word64).

import Data.Int (Int64)
import Data.Word (Word8, Word32, Word64)

newtype CBlkCnt = CBlkCnt Int64
  deriving (Eq, Ord, Enum, Bounded, Integral, Num, Real)
instance Show CBlkCnt where show (CBlkCnt x) = show x
instance Read CBlkCnt where readsPrec i s = [ (CBlkCnt x, s')
                                          | (x,s') <- readsPrec i s]

newtype CBlkSize = CBlkSize Int64
  deriving (Eq, Ord, Enum, Bounded, Integral, Num, Real)
instance Show CBlkSize where show (CBlkSize x) = show x
instance Read CBlkSize where readsPrec i s = [ (CBlkSize x, s')
                                           | (x,s') <- readsPrec i s]

newtype CCc = Ccs Word8
  deriving (Eq, Ord, Enum, Bounded, Integral, Num, Real)
instance Show CCc where show (CCc x) = show x
instance Read CCc where readsPrec i s = [ (CCc x, s')
                                      | (x,s') <- readsPrec i s]

newtype CFsBlkCnt = CFsBlkCnt Word64
  deriving (Eq, Ord, Enum, Bounded, Integral, Num, Real)
instance Show CFsBlkCnt where show (CFsBlkCnt x) = show x
instance Read CFsBlkCnt where readsPrec i s = [ (CFsBlkCnt x, s')
                                            | (x,s') <- readsPrec i s]

newtype CFsFilCnt = CFsFilCnt Word64
  deriving (Eq, Ord, Enum, Bounded, Integral, Num, Real)
instance Show CFsFilCnt where show (CFsFilCnt x) = show x
instance Read CFsFilCnt where readsPrec i s = [ (CFsFilCnt x, s')
                                            | (x,s') <- readsPrec i s]

type UserID = UserID CUid

newtype CUid = CUid Word32
  deriving (Eq, Ord, Enum, Bounded, Integral, Num, Real)
instance Show CUid where show (CUid x) = show x
instance Read CUid where readsPrec i s = [ (CUid x, s')
                                         | (x,s') <- readsPrec i s]

type GroupID = CGid

newtype CGid = CGid Word32
  deriving (Eq, Ord, Enum, Bounded, Integral, Num, Real)
instance Show CGid where show (CGid x) = show x
instance Read CGid where readsPrec i s = [ (CGid x, s')
                                         | (x,s') <- readsPrec i s]

type LinkCount = CNlink

newtype CNlink = CNlink Word64
  deriving (Eq, Ord, Enum, Bounded, Integral, Num, Real)
instance Show CNlink where show (CNlink x) = show x
instance Read CNlink where readsPrec i s = [ (CNlink x, s')
                                           | (x,s') <- readsPrec i s]

#endif
#endif
