package time.calendar

import scalaz.Enum
import scalaz.Ordering
import scalaz.Show

/** 
 * Algebraic type representing the 12 months of the Julian/Gregorian calendar. 
 * @param ord Natural ordinal in [1, 12]
 * @param commonDays Length of this month in a non-leap year.
 * @param leapDays Length of this month in a leap year.
 */
sealed abstract class Month(val ord: Int, val commonDays: Int, val leapDays: Int) {
  def this(ord: Int, days: Int) = this(ord, days, days)
}

object Month extends MonthFunctions with MonthInstances {

  case object Jan extends Month(1, 31)
  case object Feb extends Month(2, 28, 29)
  case object Mar extends Month(3, 31)
  case object Apr extends Month(4, 30)
  case object May extends Month(5, 31)
  case object Jun extends Month(6, 30)
  case object Jul extends Month(7, 31)
  case object Aug extends Month(8, 31)
  case object Sep extends Month(9, 30)
  case object Oct extends Month(10, 31)
  case object Nov extends Month(11, 30)
  case object Dec extends Month(12, 31)

  /** All month instances, in calendar order. */
  val months: List[Month] =
    List(Jan, Feb, Mar, Apr, May, Jun, Jul, Aug, Sep, Oct, Nov, Dec)

 }

trait MonthFunctions { this: Month.type =>

  /** Returns the corresponding Month for the natural ordinal in 1 .. 12, otherwise None. */
  def monthFromOrdinal(n: Int): Option[Month] =
    months.find(_.ord == n)

  // Find month and day; used below
  private def monthAndDay(n: Int, f: Month => Int): Option[(Month, Int)] = {
    def find(ms: List[Month], n: Int): Option[(Month, Int)] = ms match {
      case m :: ms if n <= f(m) => Some((m, n))
      case m :: ms => find(ms, n - f(m))
      case _ => None
    }
    if (n < 1) None else find(months, n)
  }

  /** 
   * Returns the month and day of month, given day of a common (non-leap) year, or None if the 
   * given day is out of the range [1, 365].
   */
  def monthAndDayCommon(n: Int): Option[(Month, Int)] =
    monthAndDay(n, _.commonDays)

  /** 
   * Returns the month and day of month, given day of a leap year, or None if the given day is out 
   * of the range [1, 366].
   */
  def monthAndDayLeap(n: Int): Option[(Month, Int)] =
    monthAndDay(n, _.leapDays)

}

trait MonthInstances { this: Month.type =>

  implicit def monthEnum: Enum[Month]= 
    new Enum[Month] { 

      def pred(a: Month): Month = 
        a match {
          case Jan => Dec
          case Feb => Jan
          case Mar => Feb
          case Apr => Mar
          case May => Apr
          case Jun => May
          case Jul => Jun
          case Aug => Jul
          case Sep => Aug
          case Oct => Sep
          case Nov => Oct
          case Dec => Nov
        }
      
      def succ(a: Month): Month = 
        a match {
          case Jan => Feb
          case Feb => Mar
          case Mar => Apr
          case Apr => May
          case May => Jun
          case Jun => Jul
          case Jul => Aug
          case Aug => Sep
          case Sep => Oct
          case Oct => Nov
          case Nov => Dec
          case Dec => Jan
        }
      
      def order(x: Month, y: Month): Ordering = 
        Ordering.fromInt(x.ord - y.ord)

      override def succn(n: Int, a: Month): Month = super.succn(n % 12, a)
      override def predn(n: Int, a: Month): Month = super.predn(n % 12, a)

      override def min: Option[Month] = Some(Jan)
      override def max: Option[Month] = Some(Dec)

    }

  implicit def monthShow: Show[Month] =
    Show.shows {
      case Jan => "Jan"
      case Feb => "Feb"
      case Mar => "Mar"
      case Apr => "Apr"
      case May => "May"
      case Jun => "Jun"
      case Jul => "Jul"
      case Aug => "Aug"
      case Sep => "Sep"
      case Oct => "Oct"
      case Nov => "Nov"
      case Dec => "Dec"
    }

}
