package tick

import scalaz.Enum
import scalaz.Ordering
import scalaz.Show

/** 
 * Algebraic type representing the 7 days of the Julian/Gregorian week. 
 * @param ord Natural ordinal, in [1, 7]
 */
sealed abstract class Weekday(val ord: Int)

object Weekday extends WeekdayFunctions with WeekdayInstances {

  case object Sun extends Weekday(1)
  case object Mon extends Weekday(2)
  case object Tue extends Weekday(3)
  case object Wed extends Weekday(4)
  case object Thu extends Weekday(5)
  case object Fri extends Weekday(6)
  case object Sat extends Weekday(7)

  /** List of all Weekdays, in order starting with Sun. */
  val weekdays: List[Weekday] =
    List(Sun, Mon, Tue, Wed, Thu, Fri, Sat) // unsafe, can we improve this?

  /** Weekday corresponding with ordinal in 1 .. 7, otherwise None. */
  def weekdayFromOrdinal(n: Int): Option[Weekday] =
    weekdays.find(_.ord == n)

  def fromDay[A : HasDay]: Weekday =
    ???

}

trait WeekdayFunctions extends WeekdayFunctionsUnsafe

trait WeekdayFunctionsUnsafe {

  private[tick] def unsafeWeekdayFromOrdinal(n: Int): Weekday =
    Weekday.weekdayFromOrdinal(n).getOrElse(sys.error(s"$n is out of range [1, 7]"))

}

trait WeekdayInstances { this: Weekday.type =>

  implicit val enum: Enum[Weekday] = 
    new Enum[Weekday] {

      def succ(a: Weekday): Weekday =
        a match { 
          case Sun => Mon
          case Mon => Tue
          case Tue => Wed
          case Wed => Thu 
          case Thu => Fri
          case Fri => Sat 
          case Sat => Sun
        }

      def pred(a: Weekday): Weekday =
        a match { 
          case Sun => Sat
          case Mon => Sun
          case Tue => Mon
          case Wed => Tue 
          case Thu => Wed
          case Fri => Thu 
          case Sat => Fri
        }

      def order(x: Weekday, y: Weekday): Ordering = 
        Ordering.fromInt(x.ord - y.ord)

      override def succn(n: Int, a: Weekday) = super.succn(n % 7, a)
      override def predn(n: Int, a: Weekday) = super.predn(n % 7, a)

      override def min = Some(Sun)
      override def max = Some(Sat)

    }

  implicit def monthShow: Show[Weekday] =
    Show.shows {
      case Sun => "Sat"
      case Mon => "Sun"
      case Tue => "Mon"
      case Wed => "Tue" 
      case Thu => "Wed"
      case Fri => "Thu" 
      case Sat => "Fri"
    }

}

