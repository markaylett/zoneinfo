import Data.Time.Clock
import Data.Time.LocalTime
import Data.Time.ZoneInfo

import System.IO.Unsafe

import Test.QuickCheck
import Text.Printf

prop_init = do
    -- Use TZDIR environment variable.
    -- Example:
    -- TZDIR=/usr/etc/zoneinfo runhaskell Setup test
    x <- mapM_ fn [1..1000]
    return True
  where
    fn x = do
        ctx <- newContext Nothing
        newOlsonZone ctx "America/Chicago"
        newOlsonZone ctx "America/New_York"
        return ctx

prop_minutes = do
    ctx <- newContext Nothing
    zi <- newOlsonZone ctx "America/New_York"
    est <- zoneInfoMinutes zi False
    edt <- zoneInfoMinutes zi True
    return (est == -300 && edt == -240)

prop_inverse = do
    ctx <- newContext Nothing
    zi <- newOlsonZone ctx "America/Chicago"
    ut <- getCurrentTime
    zt <- utcToZonedTime' zi ut
    (ut', _) <- localToUTCZoned zi (zonedTimeToLocalTime zt)
    return (ut == ut')

prop_timezone = do
    zt <- getZonedTime
    mins <- zoneInfoMinutes (zonedTimeZone zt)
            (timeZoneSummerOnly $ zonedTimeZone zt)
    (_, tz) <- localToUTCZoned (zonedTimeZone zt) (zonedTimeToLocalTime zt)
    return (tz == zonedTimeZone zt)

quickCheckUnsafe =
    quickCheck . unsafePerformIO

tests = [("init", quickCheckUnsafe prop_init),
         ("minutes", quickCheckUnsafe prop_minutes),
         ("inverse", quickCheckUnsafe prop_inverse),
         ("timezone", quickCheckUnsafe prop_timezone)]

main = mapM_ (\(s, a) -> printf "%-25s: " s >> a) tests
