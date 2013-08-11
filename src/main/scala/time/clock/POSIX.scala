package time.clock

/**
 * POSIX time, if you need to deal with timestamps and the like.
 * Most people won't need this module.
 */
object POSIX {

  //module Data.Time.Clock.POSIX
  //(
  //  posixDayLength,POSIXTime,posixSecondsToUTCTime,utcTimeToPOSIXSeconds,getPOSIXTime
  //) where
  //
  import UTC._
  import time.calendar._
  import data.Fixed._
  import Scale._

  //import Control.Monad
  //
  //#ifdef mingw32_HOST_OS
  //import Data.Word  ( Word64)
  //import System.Win32.Time
  //#else
  //import Data.Time.Clock.CTimeval
  //#endif
  //
  /** 86400 nominal seconds in every day */
  val posixDayLength: NominalDiffTime =
    NominalDiffTime(86400)

  /**
   * POSIX time is the nominal time since 1970-01-01 00:00 UTC
   * To convert from a 'Foreign.C.CTime' or 'System.Posix.EpochTime', use 'realToFrac'.
   */
  type POSIXTime = NominalDiffTime

  val unixEpochDay: Date =
    Date.fromModifiedJulianDate(40587)

  def posixMillisecondsToUTCTime(ms: BigDecimal): UTCTime = 
    posixSecondsToUTCTime(NominalDiffTime((ms / 1000) : Pico))
    
  def posixSecondsToUTCTime(i: POSIXTime): UTCTime = {
    val (d, t) = i.n.value /% posixDayLength.n.value
    UTCTime(unixEpochDay addDays d.toInt, DiffTime(t : Pico)) // ?
  }

  
  //utcTimeToPOSIXSeconds :: UTCTime -> POSIXTime
  //utcTimeToPOSIXSeconds (UTCTime d t) =
  // (fromInteger (diffDays d unixEpochDay) * posixDayLength) + min posixDayLength (realToFrac t)
  //
  //-- | Get the current POSIX time from the system clock.
  //getPOSIXTime :: IO POSIXTime
  //
  //#ifdef mingw32_HOST_OS
  //-- On Windows, the equlvalent of POSIX time is "file time", defined as
  //-- the number of 100-nanosecond intervals that have elapsed since
  //-- 12:00 A.M. January 1, 1601 (UTC).  We can convert this into a POSIX
  //-- time by adjusting the offset to be relative to the POSIX epoch.
  //
  //getPOSIXTime = do
  //  FILETIME ft <- System.Win32.Time.getSystemTimeAsFileTime
  //  return (fromIntegral (ft - win32_epoch_adjust) / 10000000)
  //
  //win32_epoch_adjust :: Word64
  //win32_epoch_adjust = 116444736000000000
  //
  //#else
  //
  //-- Use POSIX time
  //ctimevalToPosixSeconds :: CTimeval -> POSIXTime
  //ctimevalToPosixSeconds (MkCTimeval s mus) = (fromIntegral s) + (fromIntegral mus) / 1000000
  //
  //getPOSIXTime = liftM ctimevalToPosixSeconds getCTimeval
  //
  //#endif

}