package time.calendar

import scalaz.Enum
import scalaz.Ordering

/** Algebraic type representing the 7 days of the Julian/Gregorian week. */
sealed trait Weekday {

  /** Natural ordinal, in 1 .. 7. */
  def ord: Int

}

object Weekday extends WeekdayFunctions with WeekdayInstances {

  case object Sun extends Weekday {
    val ord = 1
  }

  case object Mon extends Weekday {
    val ord = 2
  }

  case object Tue extends Weekday {
    val ord = 3
  }

  case object Wed extends Weekday {
    val ord = 4
  }

  case object Thu extends Weekday {
    val ord = 5
  }

  case object Fri extends Weekday {
    val ord = 6
  }

  case object Sat extends Weekday {
    val ord = 7
  }

  /** List of all Weekdays, in order starting with Sun. */
  val weekdays: List[Weekday] =
    List(Sun, Mon, Tue, Wed, Thu, Fri, Sat) // unsafe, can we improve this?

  /** Weekday corresponding with ordinal in 1 .. 7, otherwise None. */
  def weekdayFromOrdinal(n: Int): Option[Weekday] =
    weekdays.find(_.ord == n)

}

trait WeekdayFunctions 

trait WeekdayInstances { this: Weekday.type =>

  // Also need to fix this up; ignore for now
  implicit def enum = new Enum[Weekday] {

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

}

