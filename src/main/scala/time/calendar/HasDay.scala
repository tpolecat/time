package time
package calendar

import scalaz.syntax.enum._
import scalaz.syntax.std.boolean._

/** 
 * Typeclass for calendar dates with day-of-year precision. 
 *
 * The fundamental operation here is conversion to/from modified Julain date; all other operations
 * can be defined in these terms.
 */
trait HasDay[A] extends HasMonth[A] {

  def toModifiedJulianDate(a: A): Int

  def fromModifiedJulianDate(n: Int): A

  /** Construct a new day-precision date on the first day of the given month. */
  def fromYearAndMonth(year: Int, month: Month): A = 
    unsafeFromYearMonthDay(year, month, 1)

  def fromYearMonthDay(year: Int, month: Month, day: Int): Option[A] =
    ???

  private def unsafeFromYearMonthDay(year: Int, month: Month, day: Int): A =
    fromYearMonthDay(year, month, day).getOrElse(sys.error(s"day $day is out of range for $month $year"))

  def dayOfWeek(a: A): Weekday =
    Weekday.weekdayFromOrdinal(mondayStartWeek(a)._2).map(_.succ).get // ***

  def addDays(a:A, n: Int): A =
    fromModifiedJulianDate(toModifiedJulianDate(a) + n)

  def diffDays(a: A, b: A): Int =
    toModifiedJulianDate(a) - toModifiedJulianDate(b)

  def isLeapDay(a:A): Boolean =
    dayOfYear(a) == HasDay.leapDayOfYear

  /**
   * Get the number of the Monday-starting week in the year, and the 1-indexed day of that week. 
   * The first Monday is the first day of week 1, any earlier days in the year are week 0. Monday 
   * is 1, Sunday is 7.
   */
  def mondayStartWeek(a:A): (Int, Int) = {
    val d = toModifiedJulianDate(a) + 2
    val k = d - dayOfYear(a)
    ((d / 7) - (k / 7), (d % 7) + 1)
  }

  // TODO: generalize this eh?
  def sundayStartWeek(a:A): (Int, Int) = {
    val d = toModifiedJulianDate(a) + 3
    val k = d - dayOfYear(a)
    ((d / 7) - (k / 7), d % 7)
  }

  ////// Conversion

  def to[B](a: A)(implicit B: HasDay[B]) =
    B.fromModifiedJulianDate(toModifiedJulianDate(a))

  ////// HasYear implementation (free)

  def year(a: A): Int = 
    toOrdinalDate(a)._1

  def dayOfYear(a: A): Int = 
    toOrdinalDate(a)._2

  ////// HasMonth implementation (free)

  def month(a: A): Month =
    monthAndDay(a)._1

  def dayOfMonth(a: A): Int =
    monthAndDay(a)._2

  // TODO: this is suspiciously complex
  def addMonths(a: A, n: Int, mode: AddMode): A = {

    def addMonths(n: Int): (Int, Month, Int) = {
      def rolloverMonths(y: Int, m: Int): (Int, Int) =
        (y + ((m - 1) / 12), ((m - 1) % 12).toInt + 1)
      val (y, m) = rolloverMonths(year(a), month(a).ord + n)
      (y, Month.unsafeMonthFromOrdinal(m), dayOfMonth(a))
    }

    val (y, m, d) = addMonths(n)
    mode.fold(
      unsafeFromYearMonthDay(y, m, d),
      addDays(unsafeFromYearMonthDay(y, m, 1), (d - 1))) 
    
  }

  ////// Some private helpers. Perhaps these should be exposed?

  private def toOrdinalDate(a: A): (Int /* Year */, Int /* Day of year */) = {
    val x = toModifiedJulianDate(a) + 678575
    val quadcent = x / 146097
    val b = x % 146097
    val cent = (b / 36524) min 3
    val c = b - (cent * 36524)
    val quad = c / 1461
    val d = c % 1461
    val y = (d / 365) min 3
    val yd = (d - (y * 365) + 1)
    val year = quadcent * 400 + cent * 100 + quad * 4 + y + 1
    (year, yd)
  }

  private def monthAndDay(a:A): (Month, Int) = {
    val isLeap = isLeapYear(a)
    val dy = toOrdinalDate(a)._2
    (isLeap ? Month.unsafeMonthAndDayLeap(dy) | Month.unsafeMonthAndDayCommon(dy))
  }

}

object HasDay extends HasDayInstances with HasDayFunctions {
  def apply[A](implicit ev: HasDay[A]): HasDay[A] = ev

  /** Minimal implementation is possible with conversions to and from ModifiedJulianDate. */
  def byModifiedJulianDate[A](f: A => Int, g: Int => A): HasDay[A] =
    new HasDay[A] {
      def toModifiedJulianDate(a: A): Int = f(a)
      def fromModifiedJulianDate(n: Int): A = g(n)
    }

}

trait HasDayInstances

trait HasDayFunctions {

  /** In leap years, an extra day falls here. */
  val leapDayOfYear: Int =
    Month.Jan.leapDays + Month.Feb.leapDays 

}


