package time
package calendar

/** Typeclass for calendar dates with day-of-year precision. */
trait HasDay[A] extends HasWeek[A] {

  def dayOfYear(a: A): Int =
    toDateYD(a).day

  def dayOfMonth(a: A): Int =
    toGregorian(a)._3

  def weekday(a: A): Weekday =
    ???

  def weekAndDayOfWeekStarting(w: Weekday): (Int, Int) =
    ???

  def addDays(a:A, n: Int): A =
    fromModifiedJulianDate(toModifiedJulianDate(a) + n)

  def isLeapDay(a:A): Boolean =
    dayOfYear(a) == HasDay.leapDayOfYear

  ////// HasMonth implementation

  def month(a:A): Month =
    toGregorian(a)._2

  def addMonthsClip(a: A, n: Int): A = 
    ???

  def addMonthsRollOver(a: A, n: Int): A = 
    ???

  ////// HasYear implementation

  def year(a:A): Int =
    toDateYD(a).year

  def addYearsClip(a: A, n: Int): A =
    ???

  def addYearsRollOver(a: A, n: Int): A =
    ???

  ////// Conversions

  def toGregorian(a:A): (Int, Month, Int) =
    ???
 
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

}

object HasDay extends HasDayInstances with HasDayFunctions {

  def apply[A](implicit ev: HasDay[A]): HasDay[A] =
    ev

  // def byModifiedJulianDate[A](f: A => Int): HasDay[A] =
  //   new HasDay[A] {
  //     def toModifiedJulianDate(a: A): Int =
  //       f(a)
  //   }

}

trait HasDayInstances

trait HasDayFunctions {

  /** In leap years, an extra day falls here. */
  val leapDayOfYear: Int =
    Month.Jan.leapDays + Month.Feb.leapDays 

}
