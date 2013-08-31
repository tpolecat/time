package tick

import scalaz.Enum
import scalaz.Order
import scalaz.Ordering
import scalaz.Show
import scalaz.std.tuple._
import scalaz.syntax.bifunctor._
import scalaz.syntax.order._
import scalaz.syntax.std.boolean._

/** ISO-8601 ordinal date with extended year and day of year. */
final class DateD private (val toModifiedJulianDate: Int) extends AnyVal {

  override def toString: String = {
    val DateD(y, d) = this
    s"DateD($y,$d)"
  }

}

object DateD extends DateDFunctions with DateDInstances {

  def fromModifiedJulianDate(n: Int): DateD =
    new DateD(n)

  def apply(year: Year, dayOfYear: Int): Option[DateD] =
    hasDay.fromOrdinalDate(year, dayOfYear).map(fromModifiedJulianDate)

  def unapply(d: DateD): Some[(Year, Int)] =
    Some((hasDay.year(d), hasDay.dayOfYear(d)))

}

trait DateDFunctions

trait DateDInstances {

  implicit val hasDay: HasDay[DateD] =
    HasDay.byModifiedJulianDate(_.toModifiedJulianDate, DateD.fromModifiedJulianDate)

  implicit val enum: Enum[DateD] =
    new Enum[DateD] {

      def pred(a: DateD): DateD =
        DateD.fromModifiedJulianDate(a.toModifiedJulianDate - 1)

      def succ(a: DateD): DateD =
        DateD.fromModifiedJulianDate(a.toModifiedJulianDate + 1)

      def order(x: DateD, y: DateD): Ordering = 
        Ordering.fromInt(x.toModifiedJulianDate - y.toModifiedJulianDate)

    }

  /** Show instance for ISO-8601 YYYY-DDD extended format. */
  implicit val show: Show[DateD] = 
    Show.shows { a => 
      val DateD(y, d) = a
      f"${y.toInt}%04d-${d}%03d"
    }

}

