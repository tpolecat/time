package time
package calendar

import scalaz.syntax.enum._

/** Typeclass for calendar dates with day-of-year precision. */
trait HasDay[A] extends HasMonth[A] {

  def toModifiedJulianDate(a: A): Int

  def fromModifiedJulianDate(n: Int): A

  def fromYearAndMonth(y: Int, m: Month): A =
    ???

  /** Convert to any other representation with day-of-year precision. */
  def to[B](a: A)(implicit B: HasDay[B]) =
    B.fromModifiedJulianDate(toModifiedJulianDate(a))

  def dayOfYear(a: A): Int =
    ???

  def dayOfMonth(a: A): Int =
    ???

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

}

object HasDay extends HasDayInstances with HasDayFunctions {

  def apply[A](implicit ev: HasDay[A]): HasDay[A] =
    ev

  /** Minimal implementation is possible with conversions to and from ModifiedJulianDate. */
  def byModifiedJulianDate[A](f: A => Int, g: Int => A): HasDay[A] =
    new HasDay[A] {

      def toModifiedJulianDate(a: A): Int = f(a)
      def fromModifiedJulianDate(n: Int): A = g(n)

      def yearAndMonth(a: A): (Int, Month) =
        ???

      // A wee helper
      private def addMonths(a: A, n: Int): (Int, Int, Int) = {
        def rolloverMonths(y: Int, m: Int): (Int, Int) =
          (y + ((m - 1) / 12), ((m - 1) % 12).toInt + 1)
        val y = year(a)
        val m = month(a)
        val d = dayOfMonth(a)
        // val DateYMD(y, m, d) = toDateYMD(a)
        val (y0, m0) = rolloverMonths(y, m.ord + n)
        (y0, m0, d)
      }

      def addMonths(a: A, n: Int, mode: AddMode): A = 
        ???

    }

}

trait HasDayInstances

trait HasDayFunctions {

  /** In leap years, an extra day falls here. */
  val leapDayOfYear: Int =
    Month.Jan.leapDays + Month.Feb.leapDays 


}
