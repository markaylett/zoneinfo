-- | Provides access to the Olson zone-info database, using an adapted version
-- of the Olson zone-info library.

module Data.Time.ZoneInfo (
    Context,
    ZoneInfo (..),
    newContext,
    newOlsonZone,
    utcOlsonZone,
) where

#include "olson.h"

import Data.Fixed
import Data.Time.Calendar
import Data.Time.Clock
import Data.Time.Clock.POSIX
import Data.Time.LocalTime

import Foreign
import Foreign.C

data CContext
data CZoneInfo

-- | Library context.

type Context = ForeignPtr CContext

-- | The 'CContext' object must outlive any 'CZoneInfo' descendants.  The
-- 'OlsonZone' type includes the parent 'CContext' to ensure that it is not
-- garbage collected while in use.

data OlsonZone = OlsonZone Context (Ptr CZoneInfo)

-- Foreign ctime interface.

-- struct tm

data CTm

foreign import ccall unsafe "olson_create"
  c_create :: CString -> IO (Ptr CContext)

foreign import ccall unsafe "&olson_destroy"
  c_destroy :: FunPtr (Ptr CContext -> IO ())

foreign import ccall unsafe "olson_get"
  c_get :: Ptr CContext -> CString -> IO (Ptr CZoneInfo)

foreign import ccall unsafe "olson_gmt"
  c_gmt :: Ptr CContext -> IO (Ptr CZoneInfo)

foreign import ccall unsafe "olson_localtime"
  c_localtime :: Ptr CZoneInfo -> Ptr CTime -> Ptr CTm -> IO (Ptr CTm)

foreign import ccall unsafe "olson_mktime"
  c_mktime :: Ptr CZoneInfo -> Ptr CTm -> IO CTime

foreign import ccall unsafe "olson_gmtoff"
  c_gmtoff :: Ptr CZoneInfo -> CInt -> IO CLong

foreign import ccall unsafe "olson_zone"
  c_zone :: Ptr CZoneInfo -> CInt -> IO (Ptr CChar)

-- Private conversions.

-- | Pico seconds from seconds and fractional pico seconds.

toPico :: Real a => a -> Pico -> Pico
toPico i =
    (+) (realToFrac i)

posixToUTCTime :: POSIXTime -> UTCTime
posixToUTCTime =
    posixSecondsToUTCTime

utcToPOSIXTime :: UTCTime -> POSIXTime
utcToPOSIXTime =
    utcTimeToPOSIXSeconds

cToPOSIXTime :: CTime -> Pico -> POSIXTime
cToPOSIXTime ct =
    realToFrac . toPico ct

-- Day

setCTmDay :: Ptr CTm -> Day -> IO ()
setCTmDay tm day = do

    (#poke struct tm,tm_year) tm (fromIntegral year - 1900 :: CInt)
    (#poke struct tm,tm_mon) tm (fromIntegral mon - 1 :: CInt)
    (#poke struct tm,tm_mday) tm (fromIntegral mday :: CInt)
    return ()

  where
    (year, mon, mday) = toGregorian day

getCTmDay :: Ptr CTm -> IO Day
getCTmDay tm = do

    year <- (#peek struct tm,tm_year) tm :: IO CInt
    mon <- (#peek struct tm,tm_mon) tm :: IO CInt
    mday <- (#peek struct tm,tm_mday) tm :: IO CInt

    return $ fromGregorian (fromIntegral year + 1900)
        (fromIntegral mon + 1) (fromIntegral mday)

-- TimeOfDay

setCTmTimeOfDay :: Ptr CTm -> TimeOfDay -> IO ()
setCTmTimeOfDay tm tod = do

    (#poke struct tm,tm_hour) tm (fromIntegral hour :: CInt)
    (#poke struct tm,tm_min) tm (fromIntegral min' :: CInt)
    (#poke struct tm,tm_sec) tm (truncate sec :: CInt)

    return ()

  where
    hour = todHour tod
    min' = todMin tod
    sec = todSec tod

getCTmTimeOfDay :: Ptr CTm -> Pico -> IO TimeOfDay
getCTmTimeOfDay tm psec = do

    hour <- (#peek struct tm,tm_hour) tm :: IO CInt
    min' <- (#peek struct tm,tm_min) tm :: IO CInt
    sec <- (#peek struct tm,tm_sec) tm :: IO CInt

    return $ TimeOfDay (fromIntegral hour) (fromIntegral min')
               (toPico sec psec)

-- TimeZone

ctmToTimeZone :: Ptr CZoneInfo -> Ptr CTm -> IO TimeZone
ctmToTimeZone zi tm = do

    isdst <- (#peek struct tm,tm_isdst) tm :: IO CInt
    gmtoff <- c_gmtoff zi isdst
    zone <- c_zone zi isdst >>= peekCString

    return $ TimeZone (fromIntegral $ div gmtoff 60) (isdst /= 0) zone

-- ZonedTime

ctmToZonedTime :: Ptr CZoneInfo -> Ptr CTm -> Pico -> IO ZonedTime
ctmToZonedTime zi tm psec = do

    day <- getCTmDay tm
    tod <- getCTmTimeOfDay tm psec
    zone <- ctmToTimeZone zi tm

    return $ ZonedTime (LocalTime day tod) zone

-- CTime

ctimeToZonedTime :: Ptr CZoneInfo -> Ptr CTime -> Pico -> IO ZonedTime
ctimeToZonedTime zi ct psec = do
    allocaBytes (#const sizeof(struct tm)) $ \ tm -> do
                                c_localtime zi ct tm
                                ctmToZonedTime zi tm psec

-- Context finalizer.

newContextFinalizer :: Ptr CContext -> IO Context
newContextFinalizer =
    newForeignPtr c_destroy

throwError :: String -> IO a
throwError = ioError . userError

-- Exports.

-- | Create a 'Context' object.  A path to the zone-info database may be
-- specified.  Otherwise, the TZDIR environment variable, or a reasonable
-- default, will be used.  An 'IOError' will be thrown on failure.

newContext :: Maybe String -> IO Context

newContext Nothing = do
    throwErrnoIfNull "Data.Time.ZoneInfo.newContext" io
    io >>= newContextFinalizer
  where
    io = c_create nullPtr

newContext (Just s) = do
    throwErrnoIfNull "Data.Time.ZoneInfo.newContext" io
    io >>= newContextFinalizer
  where
    io = withCString s c_create

-- | Obtain an 'OlsonZone' object based on the specified Olson identifier or
-- time-zone.  If the zone-info database cannot be found, or the time-zone not
-- recognised, then an 'IOError' will be thrown.  Calls to this function for a
-- given 'Context' must be serialised across threads.

newOlsonZone :: Context -> String -> IO OlsonZone
newOlsonZone ctx s = do
    io' <- throwErrnoIfNull "Data.Time.ZoneInfo.newOlsonZone" io
    return $ OlsonZone ctx io'
  where
    ctx' = unsafeForeignPtrToPtr ctx
    io = withCString s $ c_get ctx'

-- | 'OlsonZone' for the UTC time-zone.

utcOlsonZone :: Context -> OlsonZone
utcOlsonZone ctx =
    OlsonZone ctx (unsafePerformIO $ c_gmt ctx')
  where
    ctx' = unsafeForeignPtrToPtr ctx

class ZoneInfo a where

    -- | Returns the zone-name for either the standard or daylight saving
    -- zone, depending on the 'isdst' boolean argument; an 'IOError' will be
    -- thrown if this information is unavailable.

    zoneInfoName :: a -> Bool -> IO (String)

    -- | Returns the UTC offset for either the standard or daylight saving
    -- zone, depending on the 'isdst' boolean argument; an 'IOError' will be
    -- thrown if this information is unavailable.

    zoneInfoMinutes :: a -> Bool -> IO Int

    -- | Convert from 'POSIXTime' to zoned 'LocalTime'.

    posixToZonedTime :: a -> POSIXTime -> IO ZonedTime
    posixToZonedTime zi =
        utcToZonedTime' zi . posixToUTCTime

    -- | Convert from 'UTCTime' to zoned 'LocalTime'.

    utcToZonedTime' :: a -> UTCTime -> IO ZonedTime
    utcToZonedTime' zi =
        posixToZonedTime zi . utcToPOSIXTime

    -- | Convert 'LocalTime' to a daylight saving adjusted pair.

    localToPOSIXZoned :: a -> LocalTime -> IO (POSIXTime, TimeZone)
    localToPOSIXZoned zi lt = do
        (ut, tz) <- localToUTCZoned zi lt
        return (utcToPOSIXTime ut, tz)

    -- | Convert 'LocalTime' to a daylight saving adjusted pair.

    localToUTCZoned :: a -> LocalTime -> IO (UTCTime, TimeZone)
    localToUTCZoned zi lt = do
        (pt, tz) <- localToPOSIXZoned zi lt
        return (posixToUTCTime pt, tz)

    -- | Convert the 'LocalTime' argument from the source to destination
    -- 'TimeZone'.

    convertTimeZone :: ZoneInfo b => a -> LocalTime -> b -> IO ZonedTime
    convertTimeZone src lt dst = do
        (pt, _) <- localToPOSIXZoned src lt
        posixToZonedTime dst pt

-- | 'OlsonZone' implementation.

instance ZoneInfo OlsonZone where

    zoneInfoName (OlsonZone _ zi) isdst = do
        c_zone zi isdst' >>= peekCString
      where
        isdst' = if isdst then 1 else 0

    zoneInfoMinutes (OlsonZone _ zi) isdst = do
        tz <- c_gmtoff zi isdst'
        return $ fromIntegral $ div tz 60
      where
        isdst' = if isdst then 1 else 0

    posixToZonedTime (OlsonZone _ zi) pt = do

        -- "with" creates a Ptr CTime.

        with (fromInteger sec :: CTime) $ \ ct -> do
            ctimeToZonedTime zi ct (realToFrac psec)
      where
        (sec, psec) = pt `divMod'` 1

    localToPOSIXZoned (OlsonZone _ zi) lt =
        allocaBytes (#const sizeof(struct tm)) $ \ tm -> do
          setCTmDay tm day
          setCTmTimeOfDay tm tod
          (#poke struct tm,tm_isdst) tm (-1 :: CInt)
          ct <- c_mktime zi tm
          tz <- ctmToTimeZone zi tm
          return (cToPOSIXTime ct psec, tz)
      where
        day = localDay lt
        tod = localTimeOfDay lt
        sec = todSec tod
        psec = sec `mod'` 1

-- | 'TimeZone' implementation.

instance ZoneInfo TimeZone where

    zoneInfoName tz True = do
        if timeZoneSummerOnly tz then return $ timeZoneName tz
                                 else throwError "not summer-only"

    zoneInfoName tz False = do
        if timeZoneSummerOnly tz then throwError "summer-only"
                                 else return $ timeZoneName tz

    zoneInfoMinutes tz True = do
        if timeZoneSummerOnly tz then return $ timeZoneMinutes tz
                                 else throwError "not summer-only"

    zoneInfoMinutes tz False = do
        if timeZoneSummerOnly tz then throwError "summer-only"
                                 else return $ timeZoneMinutes tz

    utcToZonedTime' tz ut = do
        return $ ZonedTime (utcToLocalTime tz ut) tz

    localToUTCZoned tz lt = do
        return (localTimeToUTC tz lt, tz)
