package time.clock

import spire.algebra.Eq
import spire.math.Fractional
import spire.math.Integral
import spire.math.Numeric
import spire.math.Rational
import scalaz.Equal
import scalaz.Order
import scalaz.Enum
import scalaz.syntax.enum._
import scalaz.Show

// http://www.haskell.org/ghc/docs/7.4-latest/html/libraries/time-1.4/src/Data-Time-Clock-Scale.html
object Scale {
  import data.Fixed._

  /**
   * The Modified Julian Date is the day with the fraction of the day, measured from UT midnight.
   * It's used to represent UT1, which is time as measured by the earth's rotation, adjusted for various wobbles.
   */
  case class UniversalTime(getModJulianDate: Rational) extends AnyVal

  object UniversalTime extends UniversalTimeInstances

  trait UniversalTimeInstances {

//[3:33pm] larsrh: tpolecat: *cough* pull request *cough*
//[3:34pm] tpolecat: :-)
//[3:34pm] tpolecat: is scalaz-contrib the right place for this?[3:34pm] tpolecat: i'm not clear on the state or plans for bringing the typelevel typeclasses together
//[3:34pm] larsrh: tpolecat: somewhere in there https://github.com/typelevel/scalaz-contrib/tree/master/spire/main/scala
//[3:34pm] tpolecat: k
//[3:34pm] • larsrh cackles manically and leaves tpolecat alone with that code
//[3:35pm] larsrh: It's pretty convoluted, I have to admint.
//[3:35pm] larsrh: *admit
//[3:35pm] larsrh: here's the gist of how it works: https://github.com/typelevel/scalaz-contrib#spire
//[3:36pm] tpolecat: ok, thanks. i'm way way out on a limb already so i'll add this to the pile

    implicit val eqUniversalTime: Eq[UniversalTime] =
      Eq.by(_.getModJulianDate)

    implicit val equalUniversalTime: Equal[UniversalTime] =
      Equal.equal(eqUniversalTime.eqv)

    //      Equal.equalBy(_.getModJulianDate    
    // Eq, Ord 

  }

  /**
   * This is a length of time, as measured by a clock.
   * Conversion functions will treat it as seconds.
   * It has a precision of 10^-12 s.
   */
  case class DiffTime(p: Pico) extends AnyVal

  object DiffTime extends DiffTimeInstances

  trait DiffTimeInstances {

//    implicit val diffTimeEqual: Equal[DiffTime] =
//      Equal.equalA
//
//    implicit val ordDiffTime: Order[DiffTime] =
//      Order.orderBy(_.p)
//
//    implicit val enumDiffTime: Enum[DiffTime] =
//      new Enum[DiffTime] {
//        def succ(dt: DiffTime) = DiffTime(dt.p.succ)
//        def pred(dt: DiffTime) = DiffTime(dt.p.pred)
//        def order(a: DiffTime, b: DiffTime) = Order[Pico].order(a.p, b.p)
//      }
//
//    implicit val showDiffTime: Show[DiffTime] =
//      Show.shows(dt => f"${dt.p.n}%ds")

    //-- necessary because H98 doesn't have "cunning newtype" derivation
    //instance Num DiffTime where
    //  (MkDiffTime a) + (MkDiffTime b) = MkDiffTime (a + b)
    //  (MkDiffTime a) - (MkDiffTime b) = MkDiffTime (a - b)
    //  (MkDiffTime a) * (MkDiffTime b) = MkDiffTime (a * b)
    //  negate (MkDiffTime a) = MkDiffTime (negate a)
    //  abs (MkDiffTime a) = MkDiffTime (abs a)
    //  signum (MkDiffTime a) = MkDiffTime (signum a)
    //  fromInteger i = MkDiffTime (fromInteger i)
    //
    //-- necessary because H98 doesn't have "cunning newtype" derivation
    //instance Real DiffTime where
    //  toRational (MkDiffTime a) = toRational a
    //
    //-- necessary because H98 doesn't have "cunning newtype" derivation
    //instance Fractional DiffTime where
    //  (MkDiffTime a) / (MkDiffTime b) = MkDiffTime (a / b)
    //  recip (MkDiffTime a) = MkDiffTime (recip a)
    //  fromRational r = MkDiffTime (fromRational r)
    //
    //-- necessary because H98 doesn't have "cunning newtype" derivation
    //instance RealFrac DiffTime where
    //    properFraction (MkDiffTime a) = let (b',a') = properFraction a in (b',MkDiffTime a')
    //    truncate (MkDiffTime a) = truncate a
    //    round (MkDiffTime a) = round a
    //    ceiling (MkDiffTime a) = ceiling a
    //    floor (MkDiffTime a) = floor a
  }

  /** Create a 'DiffTime' which represents an integral number of seconds. */
  def secondsToDiffTime(x: BigInt): DiffTime =
    DiffTime(x)
  //
  /** Create a 'DiffTime' from a number of picoseconds. */
  def picosecondsToDiffTime(x: BigInt): DiffTime =
    DiffTime(x / 1000000000000L)

}

