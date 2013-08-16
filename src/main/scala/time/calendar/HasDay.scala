package time
package calendar

/** 
 * Typeclass for calendar dates with day-of-year precision. 
 *
 * Minimal implementation is to/fromModifiedJulianDate.
 */
trait HasDay[A] extends HasWeek[A] {

  def dayOfYear(a: A): Int =
    toDateYD(a).day

  def dayOfMonth(a: A): Int = {
    val DateYMD(y, m, d) = toDateYMD(a)
    d
  }

  def dayOfWeek(a: A): Weekday =
    ???

  def addDays(a:A, n: Int): A =
    fromModifiedJulianDate(toModifiedJulianDate(a) + n)

  def diffDays(a: A, b: A): Int =
    toModifiedJulianDate(a) - toModifiedJulianDate(b)

  def isLeapDay(a:A): Boolean =
    dayOfYear(a) == HasDay.leapDayOfYear

  /**
   * Get the number of the Monday-starting week in the year, and the day of the week. 
   * The first Monday is the first day of week 1, any earlier days in the year are week 0. Monday 
   * is 1, Sunday is 7.
   */
  def mondayStartWeek(a:A): (Int, Int) = {
    val d = toModifiedJulianDate(a) + 2
    val k = d - toDateYD(a).day
    ((d / 7) - (k / 7), (d % 7) + 1)
  }

  // TODO: sundayStartWeek

  ////// HasMonth implementation

  def month(a:A): Month = {
    val DateYMD(y, m, d) = toDateYMD(a)
    m
  }

  def addMonthsClip(a: A, n: Int): A = 
    ???

  def addMonthsRollOver(a: A, n: Int): A = 
    ???

  ////// HasYear implementation

  def year(a:A): Int =
    toDateYD(a).year

  def addYearsClip(a: A, n: Int): A =
    addMonthsClip(a, n * 12)

  def addYearsRollOver(a: A, n: Int): A =
    addMonthsRollOver(a, n * 12)

  ////// Conversions

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
    DateYD(year, yd.toInt).getOrElse(sys.error("Problem with the implementation."))
  }

  def toDateYMD(a: A): DateYMD =
    DateYMD.fromModifiedJulianDate(toModifiedJulianDate(a))

}

object HasDay extends HasDayInstances with HasDayFunctions {

  def apply[A](implicit ev: HasDay[A]): HasDay[A] =
    ev

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
