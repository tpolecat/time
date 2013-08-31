package tick

import scalaz.Enum
import scalaz.Order
import scalaz.Ordering
import scalaz.Show
import scalaz.std.tuple._
import scalaz.syntax.bifunctor._
import scalaz.syntax.order._
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

  def apply(year: Year, month: Month, day: Int): Option[Date] =
    hasDay.fromYearMonthDay(year, month, day)

  def unapply(d: Date): Some[(Year, Month, Int)] =
    Some((hasDay.year(d), hasDay.month(d), hasDay.dayOfMonth(d)))

}

trait DateFunctions

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

