package time
package calendar

import scalaz.syntax.enum._

/** Typeclass for calendar dates with day-of-year precision. */
trait HasDay[A] extends HasMonth[A] {

  def dayOfYear(a: A): Int =
    toDateYD(a).dayOfYear

  def dayOfMonth(a: A): Int = {
    val DateYMD(y, m, d) = toDateYMD(a)
    d
  }

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
    val k = d - toDateYD(a).dayOfYear
    ((d / 7) - (k / 7), (d % 7) + 1)
  }

  // TODO: generalize this eh?
  def sundayStartWeek(a:A): (Int, Int) = {
    val d = toModifiedJulianDate(a) + 3
    val k = d - toDateYD(a).dayOfYear
    ((d / 7) - (k / 7), d % 7)
  }

  def toModifiedJulianDate(a: A): Int

  def fromModifiedJulianDate(n: Int): A

  def toDateYD(a:A): DateYD = {
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
    DateYD(year, yd).getOrElse(sys.error("Problem with the implementation."))
  }

  def toDateYMD(a: A): DateYMD =
    DateYMD.fromModifiedJulianDate(toModifiedJulianDate(a))

}

object HasDay extends HasDayInstances with HasDayFunctions {

  def apply[A](implicit ev: HasDay[A]): HasDay[A] =
    ev

  /** Minimal implementation is possible with conversions to and from ModifiedJulianDate. */
  def byModifiedJulianDate[A](f: A => Int, g: Int => A): HasDay[A] =
    new HasDay[A] {
  
      def month(a:A): Month = {
        val DateYMD(y, m, d) = toDateYMD(a)
        m
      }

      // A wee helper
      private def addMonths(a: A, n: Int): (Int, Int, Int) = {
        def rolloverMonths(y: Int, m: Int): (Int, Int) =
          (y + ((m - 1) / 12), ((m - 1) % 12).toInt + 1)
        val DateYMD(y, m, d) = toDateYMD(a)
        val (y0, m0) = rolloverMonths(y, m.ord + n)
        (y0, m0, d)
      }

      def addMonthsClip(a: A, n: Int): A = 
        ???

      def addMonthsRollOver(a: A, n: Int): A = 
        ???

      def year(a:A): Int =
        toDateYD(a).year

      def addYearsClip(a: A, n: Int): A =
        addMonthsClip(a, n * 12)

      def addYearsRollOver(a: A, n: Int): A =
        addMonthsRollOver(a, n * 12)

      def toModifiedJulianDate(a: A): Int = 
        f(a)

      def fromModifiedJulianDate(n: Int): A = 
        g(n)

    }

}

trait HasDayInstances

trait HasDayFunctions {

  /** In leap years, an extra day falls here. */
  val leapDayOfYear: Int =
    Month.Jan.leapDays + Month.Feb.leapDays 


}
