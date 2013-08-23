package time
package calendar

import scalaz.Enum
import scalaz.Ordering
import scalaz.Show
import scalaz.std.tuple._
import scalaz.syntax.bifunctor._
import scalaz.syntax.std.boolean._

/** ISO-8601 calendar date with extended year. */
final class Date private (val toModifiedJulianDate: Int) extends AnyVal {

  override def toString: String = {
    val Date(y, m, d) = this
    s"Date($y,$m,$d)"
  }

}

object Date extends DateFunctions with DateInstances {

  def fromModifiedJulianDate(n: Int): Date =
    new Date(n)

  def fromOrdinalDate(year: Year, dayOfYear: Int): Option[Date] =
    clipValid(1, year.length, dayOfYear) map { day0 =>
      val y = year.toInt - 1
      val mjd = day0 + (365 * y) + (y / 4) - (y / 100) + (y / 400) - 678576
      fromModifiedJulianDate(mjd)
    }

  /**
   * Convert from proleptic Gregorian calendar. First argument is year, second month number (1-12), 
   * third day (1-31). Invalid values will return None
   */
  def apply(year: Year, month: Month, day: Int): Option[Date] =
    dayOfYear(year.fold(_ => false, _ => true), month, day).flatMap(fromOrdinalDate(year, _))

  def unapply(d: Date): Some[(Year, Month, Int)] =
    Some((hasDay.year(d), hasDay.month(d), hasDay.dayOfMonth(d)))

}

trait DateFunctions {

  /** Convert month and day in the Gregorian or Julian calendars to day of year. */
  def dayOfYear(isLeap: Boolean, month: Month, day: Int): Option[Int] =
    for {
      day0 <- clipValid(1, isLeap ? month.leapDays | month.commonDays, day)
      k = if (month.ord <= 2) 0 else if (isLeap) -1 else -2
    } yield ((367 * month.ord - 362) / 12) + k + day0

}

trait DateInstances {

  implicit val hasDay: HasDay[Date] =
    HasDay.byModifiedJulianDate(_.toModifiedJulianDate, Date.fromModifiedJulianDate)

  implicit val enum: Enum[Date] =
    new Enum[Date] {

      def pred(a: Date): Date =
        Date.fromModifiedJulianDate(a.toModifiedJulianDate - 1)

      def succ(a: Date): Date =
        Date.fromModifiedJulianDate(a.toModifiedJulianDate + 1)

      def order(x: Date, y: Date): Ordering = 
        Ordering.fromInt(x.toModifiedJulianDate - y.toModifiedJulianDate)

    }

  /** Show instance for ISO-8601 YYYY-MM-DD extended format. */
  implicit val show: Show[Date] = 
    Show.shows { a => 
      val Date(y, m, d) = a
      f"${y.toInt}%04d-${m.ord}%02d-${d}%02d"
    }

}

