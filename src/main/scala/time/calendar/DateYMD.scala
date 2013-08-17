package time
package calendar

import scalaz.Enum
import scalaz.Ordering
import scalaz.Show
import scalaz.std.tuple._
import scalaz.syntax.bifunctor._

/** ISO-8601 calendar date with extended year. */
final class DateYMD private (val toModifiedJulianDate: Int) extends AnyVal {

  override def toString: String = {
    val DateYMD(y, m, d) = this
    s"DateYMD($y,$m,$d)"
  }

}

object DateYMD extends DateYMDFunctions with DateYMDInstances {

  def fromModifiedJulianDate(n: Int): DateYMD =
    new DateYMD(n)

 def fromOrdinalDate(year: Int, dayOfYear: Int): Option[DateYMD] =
    clipValid(1, daysInYear(year), dayOfYear) map { day0 =>
      val y = year - 1
      val mjd = day0 + (365 * y) + (y / 4) - (y / 100) + (y / 400) - 678576
      fromModifiedJulianDate(mjd)
    }

  def fromOrdinalDateClipped(year: Int, dayOfYear: Int): DateYMD = {
    val day0 = clip(1, daysInYear(year), dayOfYear)
    val y = year - 1
    val mjd = day0 + (365 * y) + (y / 4) - (y / 100) + (y / 400) - 678576
    fromModifiedJulianDate(mjd)
  }

  /**
   * Convert from proleptic Gregorian calendar. First argument is year, second month number (1-12), 
   * third day (1-31). Invalid values will be clipped to the correct range, month first, then day.
   */
  def fromYearMonthDayClipped(year: Int, month: Int, day: Int): DateYMD =
    fromOrdinalDateClipped(year, dayOfYearClipped(isLeapYear(year), month, day))

  /**
   * Convert from proleptic Gregorian calendar. First argument is year, second month number (1-12), 
   * third day (1-31). Invalid values will return None
   */
  def apply(year: Int, month: Int, day: Int): Option[DateYMD] =
    dayOfYear(isLeapYear(year), month, day).flatMap(fromOrdinalDate(year, _))

  def unapply(d: DateYMD): Some[(Int, Month, Int)] =
    Some {
      val DateYD(year, dayOfYear) = DateYMD.hasDay.toDateYD(d)
      val (month, day) = Date.monthAndDay(HasYear.isLeapYear(year), dayOfYear)
      (year, month, day)
    }
}

trait DateYMDFunctions {

    /** Convert month and day (clipped) in the Gregorian or Julian calendars to day of year. */
  def dayOfYearClipped(isLeap: Boolean, month: Int, day: Int): Int = {
    val month0 = clip(1, 12, month)
    val day0 = clip(1, monthLength0(isLeap, month0), day)
    val k = if (month0 <= 2) 0 else if (isLeap) -1 else -2
    ((367 * month0 - 362) / 12) + k + day0
  }

  /** Convert month and day in the Gregorian or Julian calendars to day of year. */
  def dayOfYear(isLeap: Boolean, month: Int, day: Int): Option[Int] =
    for {
      month0 <- clipValid(1, 12, month)
      day0 <- clipValid(1, monthLength0(isLeap, month0), day)
      k = if (month0 <= 2) 0 else if (isLeap) -1 else -2
    } yield ((367 * month0 - 362) / 12) + k + day0

  /** Convert day of year in the Gregorian or Julian calendars to month and day. */
  def monthAndDay(isLeap: Boolean, dayOfYear: Int): (Month, Int) = {
    val (m, d) = findMonthDay(monthLengths(isLeap), clip(1, if (isLeap) 366 else 365, dayOfYear))
    (Month.monthFromOrdinal(m).get, d) // **
  }

  // Helpers

  private def findMonthDay(ns: List[Int], yd: Int): (Int, Int) = ns match {
    case n :: ns if yd > n => findMonthDay(ns, yd - n).bimap(_ + 1, identity)
    case _                 => (1, yd)
  }

  private def monthLength0(isLeap: Boolean, month0: Int): Int =
    monthLengths(isLeap)(month0 - 1) // ***

  private def monthLengths(isLeap: Boolean): List[Int] =
    if (isLeap) Month.months.map(_.leapDays) else Month.months.map(_.commonDays)

}

trait DateYMDInstances {

  implicit val hasDay: HasDay[DateYMD] =
    HasDay.byModifiedJulianDate(_.toModifiedJulianDate, DateYMD.fromModifiedJulianDate)

  implicit val enum: Enum[DateYMD] =
    new Enum[DateYMD] {

      def pred(a: DateYMD): DateYMD =
        DateYMD.fromModifiedJulianDate(a.toModifiedJulianDate - 1)

      def succ(a: DateYMD): DateYMD =
        DateYMD.fromModifiedJulianDate(a.toModifiedJulianDate + 1)

      def order(x: DateYMD, y: DateYMD): Ordering = 
        Ordering.fromInt(x.toModifiedJulianDate - y.toModifiedJulianDate)

    }

  /** Show instance for ISO-8601 YYYY-MM-DD extended format. */
  implicit val show: Show[DateYMD] = 
    Show.shows { a => 
      val DateYMD(y, m, d) = a
      f"${y}%04d-${m.ord}%02d-${d}%02d"
    }

}

