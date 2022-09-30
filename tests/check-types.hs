{-# LANGUAGE CPP #-}

{-# OPTIONS_GHC -Wno-unused-top-binds #-}

module Main (main) where

import Data.Int
import Data.Word
import System.PosixCompat.Types

import qualified Foreign
import qualified Foreign.C.Types

main :: IO ()
main = pure ()

newtypeCBlkCnt :: CBlkCnt -> Int64
newtypeCBlkCnt (CBlkCnt x) = x

#ifdef darwin_HOST_OS
newtypeCBlkSize :: CBlkSize -> Int32
#else
newtypeCBlkSize :: CBlkSize -> Int64
#endif
newtypeCBlkSize (CBlkSize x) = x

newtypeCCc :: CCc -> Word8
newtypeCCc (CCc x) = x

#ifdef darwin_HOST_OS
newtypeCClockId :: CClockId -> Word32
#else
newtypeCClockId :: CClockId -> Int32
#endif
newtypeCClockId (CClockId x) = x

#ifdef darwin_HOST_OS
newtypeCDev :: CDev -> Int32
#else
newtypeCDev :: CDev -> Word64
#endif
newtypeCDev (CDev x) = x

#ifdef darwin_HOST_OS
newtypeCFsBlkCnt :: CFsBlkCnt -> Word32
#else
newtypeCFsBlkCnt :: CFsBlkCnt -> Word64
#endif
newtypeCFsBlkCnt (CFsBlkCnt x) = x

#ifdef darwin_HOST_OS
newtypeCFsFilCnt :: CFsFilCnt -> Word32
#else
newtypeCFsFilCnt :: CFsFilCnt -> Word64
#endif
newtypeCFsFilCnt (CFsFilCnt x) = x

newtypeCGid :: CGid -> Word32
newtypeCGid (CGid x) = x

newtypeCId :: CId -> Word32
newtypeCId (CId x) = x

newtypeCIno :: CIno -> Word64
newtypeCIno (CIno x) = x

newtypeCKey :: CKey -> Int32
newtypeCKey (CKey x) = x

#ifdef darwin_HOST_OS
newtypeCMode :: CMode -> Word16
#else
newtypeCMode :: CMode -> Word32
#endif
newtypeCMode (CMode x) = x

#if defined darwin_HOST_OS && MIN_VERSION_base(4,14,0)
newtypeCNfds :: CNfds -> Word32
#else
newtypeCNfds :: CNfds -> Word64
#endif
newtypeCNfds (CNfds x) = x

#ifdef darwin_HOST_OS
newtypeCNlink :: CNlink -> Word16
#else
newtypeCNlink :: CNlink -> Word64
#endif
newtypeCNlink (CNlink x) = x

newtypeCOff :: COff -> Int64
newtypeCOff (COff x) = x

newtypeCPid :: CPid -> Int32
newtypeCPid (CPid x) = x

newtypeCRLim :: CRLim -> Word64
newtypeCRLim (CRLim x) = x

newtypeCSocklen :: CSocklen -> Word32
newtypeCSocklen (CSocklen x) = x

#ifdef darwin_HOST_OS
newtypeCSpeed :: CSpeed -> Word64
#else
newtypeCSpeed :: CSpeed -> Word32
#endif
newtypeCSpeed (CSpeed x) = x

newtypeCSsize :: CSsize -> Int64
newtypeCSsize (CSsize x) = x

#ifdef darwin_HOST_OS
newtypeCTcflag :: CTcflag -> Word64
#else
newtypeCTcflag :: CTcflag -> Word32
#endif
newtypeCTcflag (CTcflag x) = x

newtypeCTimer :: CTimer -> Foreign.Ptr ()
newtypeCTimer (CTimer x) = x

newtypeCUid :: CUid -> Word32
newtypeCUid (CUid x) = x

newtypeFd :: Fd -> Foreign.C.Types.CInt
newtypeFd (Fd x) = x

typeByteCount :: ByteCount -> Foreign.C.Types.CSize
typeByteCount = id

typeClockTick :: ClockTick -> Foreign.C.Types.CClock
typeClockTick = id

typeDeviceID :: DeviceID -> CDev
typeDeviceID = id

typeEpochTime :: EpochTime -> Foreign.C.Types.CTime
typeEpochTime = id

typeFileID :: FileID -> CIno
typeFileID = id

typeFileMode :: FileMode -> CMode
typeFileMode = id

typeFileOffset :: FileOffset -> COff
typeFileOffset = id

typeGroupID :: GroupID -> CGid
typeGroupID = id

typeLimit :: Limit -> Foreign.C.Types.CLong
typeLimit = id

typeLinkCount :: LinkCount -> CNlink
typeLinkCount = id

typeProcessGroupID :: ProcessGroupID -> CPid
typeProcessGroupID = id

typeProcessID :: ProcessID -> CPid
typeProcessID = id

typeUserID :: UserID -> CUid
typeUserID = id
