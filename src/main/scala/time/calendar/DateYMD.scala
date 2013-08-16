package time
package calendar

import scalaz.Enum
import scalaz.Ordering
import scalaz.Show

final class DateYMD private (val toModifiedJulianDate: Int) {

  override def toString: String =
    "DateYMD" + DateYMD.hasDay.toGregorian(this)

}

object DateYMD extends DateYMDFunctions with DateYMDInstances {

  def fromModifiedJulianDate(n: Int): DateYMD =
    new DateYMD(n)

  def unapply(d: DateYMD): Some[(Int, Month, Int)] =
    Some(hasDay.toGregorian(d))

}

trait DateYMDFunctions

trait DateYMDInstances {

  implicit val hasDay: HasDay[DateYMD] =
    ???

  implicit val enum: Enum[DateYMD] =
    new Enum[DateYMD] {

      def pred(a: DateYMD): DateYMD =
        DateYMD.fromModifiedJulianDate(a.toModifiedJulianDate - 1)

      def succ(a: DateYMD): DateYMD =
        DateYMD.fromModifiedJulianDate(a.toModifiedJulianDate - 1)

      def order(x: DateYMD, y: DateYMD): Ordering = 
        Ordering.fromInt(x.toModifiedJulianDate - y.toModifiedJulianDate)

    }

  /** Show instance for ISO-8601 YYYY-MM-DD extended format. */
  implicit val show: Show[DateYMD] = 
    Show.shows { a => 
      val (y, m, d) = hasDay.toGregorian(a)
      f"${y}%04d-${m.ord}%s2d-${d}%02d"
    }

}

