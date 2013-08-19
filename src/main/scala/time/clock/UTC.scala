// package time.clock

// /**
//  * UTC is time as measured by a clock, corrected to keep pace with the earth by adding or removing occasional seconds,
//  * known as "leap seconds". These corrections are not predictable and are announced with six month's notice. No table
//  * of these corrections is provided, as any program compiled with it would become out of date in six months.
//  *
//  * If you don't care about leap seconds, use UTCTime and NominalDiffTime for your clock calculations, and you'll be
//  * fine.
//  */
// object UTC {

//   //  {-# OPTIONS -fno-warn-unused-imports #-}
//   //#include "HsConfigure.h"
//   //-- #hide
//   //module Data.Time.Clock.UTC
//   //(
//   //  UTCTime(..),NominalDiffTime
//   //) where
//   //
//   //import Control.DeepSeq

//   import time.calendar._
//   import time.clock.Scale._
//   import data.Fixed._

//   //import Data.Typeable
//   //#if LANGUAGE_Rank2Types
//   //import Data.Data
//   //#endif
//   //
//   /**
//    * This is the simplest representation of UTC.
//    * It consists of the day number, and a time offset from midnight.
//    * Note that if a day has a leap second added to it, it will have 86401 seconds.
//    * @param utctDay the day
//    * @param utctDayTime the time from midnight, 0 <= t < 86401s (because of leap-seconds)
//    */
//   case class UTCTime(utctDay: Date, utctDayTime: DiffTime)

//   object UTCTime extends UTCTimeInstances

//   trait UTCTimeInstances {

//     //#if LANGUAGE_DeriveDataTypeable
//     //#if LANGUAGE_Rank2Types
//     //#if HAS_DataPico
//     //    deriving (Data)
//     //#endif
//     //#endif
//     //#endif
//     //
//     //instance NFData UTCTime where
//     //  rnf (UTCTime d t) = d `deepseq` t `deepseq` ()
//     //
//     //instance Typeable UTCTime where
//     //  typeOf _ = mkTyConApp (mkTyCon "Data.Time.Clock.UTC.UTCTime") []
//     //
//     //instance Eq UTCTime where
//     //  (UTCTime da ta) == (UTCTime db tb) = (da == db) && (ta == tb)
//     //
//     //instance Ord UTCTime where
//     //  compare (UTCTime da ta) (UTCTime db tb) = case (compare da db) of
//     //    EQ -> compare ta tb
//     //    cmp -> cmp

//   }

//   /**
//    * This is a length of time, as measured by UTC.
//    * Conversion functions will treat it as seconds.
//    * It has a precision of 10^-12 s.
//    * It ignores leap-seconds, so it's not necessarily a fixed amount of clock time.
//    * For instance, 23:00 UTC + 2 hours of NominalDiffTime = 01:00 UTC (+ 1 day), regardless of whether a leap-second
//    * intervened.
//    */
//   case class NominalDiffTime(n: Pico)

//   object NominalDiffTime extends NominalDiffTimeInstances

//   trait NominalDiffTimeInstances {

//     // deriving (Eq,Ord
//     //#if LANGUAGE_DeriveDataTypeable
//     //#if LANGUAGE_Rank2Types
//     //#if HAS_DataPico
//     //    ,Data
//     //#endif
//     //#endif
//     //#endif
//     //    )
//     //
//     //-- necessary because H98 doesn't have "cunning newtype" derivation
//     //instance NFData NominalDiffTime -- FIXME: Data.Fixed had no NFData instances yet at time of writing
//     //
//     //instance Typeable NominalDiffTime where
//     //  typeOf _ = mkTyConApp (mkTyCon "Data.Time.Clock.UTC.NominalDiffTime") []
//     //
//     //instance Enum NominalDiffTime where
//     //  succ (MkNominalDiffTime a) = MkNominalDiffTime (succ a)
//     //  pred (MkNominalDiffTime a) = MkNominalDiffTime (pred a)
//     //  toEnum = MkNominalDiffTime . toEnum
//     //  fromEnum (MkNominalDiffTime a) = fromEnum a
//     //  enumFrom (MkNominalDiffTime a) = fmap MkNominalDiffTime (enumFrom a)
//     //  enumFromThen (MkNominalDiffTime a) (MkNominalDiffTime b) = fmap MkNominalDiffTime (enumFromThen a b)
//     //  enumFromTo (MkNominalDiffTime a) (MkNominalDiffTime b) = fmap MkNominalDiffTime (enumFromTo a b)
//     //  enumFromThenTo (MkNominalDiffTime a) (MkNominalDiffTime b) (MkNominalDiffTime c) = fmap MkNominalDiffTime (enumFromThenTo a b c)
//     //
//     //instance Show NominalDiffTime where
//     //  show (MkNominalDiffTime t) = (showFixed True t) ++ "s"
//     //
//     //-- necessary because H98 doesn't have "cunning newtype" derivation
//     //instance Num NominalDiffTime where
//     //  (MkNominalDiffTime a) + (MkNominalDiffTime b) = MkNominalDiffTime (a + b)
//     //  (MkNominalDiffTime a) - (MkNominalDiffTime b) = MkNominalDiffTime (a - b)
//     //  (MkNominalDiffTime a) * (MkNominalDiffTime b) = MkNominalDiffTime (a * b)
//     //  negate (MkNominalDiffTime a) = MkNominalDiffTime (negate a)
//     //  abs (MkNominalDiffTime a) = MkNominalDiffTime (abs a)
//     //  signum (MkNominalDiffTime a) = MkNominalDiffTime (signum a)
//     //  fromInteger i = MkNominalDiffTime (fromInteger i)
//     //
//     //-- necessary because H98 doesn't have "cunning newtype" derivation
//     //instance Real NominalDiffTime where
//     //  toRational (MkNominalDiffTime a) = toRational a
//     //
//     //-- necessary because H98 doesn't have "cunning newtype" derivation
//     //instance Fractional NominalDiffTime where
//     //  (MkNominalDiffTime a) / (MkNominalDiffTime b) = MkNominalDiffTime (a / b)
//     //  recip (MkNominalDiffTime a) = MkNominalDiffTime (recip a)
//     //  fromRational r = MkNominalDiffTime (fromRational r)
//     //
//     //-- necessary because H98 doesn't have "cunning newtype" derivation
//     //instance RealFrac NominalDiffTime where
//     //  properFraction (MkNominalDiffTime a) = (i,MkNominalDiffTime f) where
//     //    (i,f) = properFraction a
//     //  truncate (MkNominalDiffTime a) = truncate a
//     //  round (MkNominalDiffTime a) = round a
//     //  ceiling (MkNominalDiffTime a) = ceiling a
//     //  floor (MkNominalDiffTime a) = floor a
//   }

// }