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

#if MIN_VERSION_base(4, 13, 0)
newtypeCBlkCnt :: CBlkCnt -> Int64
newtypeCBlkCnt (CBlkCnt x) = x

newtypeCBlkSize :: CBlkSize -> Int64
newtypeCBlkSize (CBlkSize x) = x

newtypeCCc :: CCc -> Word8
newtypeCCc (CCc x) = x

newtypeCClockId :: CClockId -> Int32
newtypeCClockId (CClockId x) = x

newtypeCDev :: CDev -> Word64
newtypeCDev (CDev x) = x

newtypeCFsBlkCnt :: CFsBlkCnt -> Word64
newtypeCFsBlkCnt (CFsBlkCnt x) = x

newtypeCFsFilCnt :: CFsFilCnt -> Word64
newtypeCFsFilCnt (CFsFilCnt x) = x

newtypeCGid :: CGid -> Word32
newtypeCGid (CGid x) = x

newtypeCId :: CId -> Word32
newtypeCId (CId x) = x

newtypeCIno :: CIno -> Word64
newtypeCIno (CIno x) = x

newtypeCKey :: CKey -> Int32
newtypeCKey (CKey x) = x

newtypeCMode :: CMode -> Word32
newtypeCMode (CMode x) = x

newtypeCNfds :: CNfds -> Word64
newtypeCNfds (CNfds x) = x

newtypeCNlink :: CNlink -> Word64
newtypeCNlink (CNlink x) = x

newtypeCOff :: COff -> Int64
newtypeCOff (COff x) = x

newtypeCPid :: CPid -> Int32
newtypeCPid (CPid x) = x

newtypeCRLim :: CRLim -> Word64
newtypeCRLim (CRLim x) = x

newtypeCSocklen :: CSocklen -> Word32
newtypeCSocklen (CSocklen x) = x

newtypeCSpeed :: CSpeed -> Word32
newtypeCSpeed (CSpeed x) = x

newtypeCSsize :: CSsize -> Int64
newtypeCSsize (CSsize x) = x

newtypeCTcflag :: CTcflag -> Word32
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

#endif
