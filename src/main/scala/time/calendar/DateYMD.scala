package time
package calendar

import scalaz.Enum
import scalaz.Ordering
import scalaz.Show

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

  def unapply(d: DateYMD): Some[(Int, Month, Int)] =
    Some {
      val DateYD(year, dayOfYear) = DateYMD.hasDay.toDateYD(d)
      val (month, day) = Date.monthAndDay(HasYear.isLeapYear(year), dayOfYear)
      (year, month, day)
    }
}

trait DateYMDFunctions

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
      f"${y}%04d-${m.ord}%s2d-${d}%02d"
    }

}

