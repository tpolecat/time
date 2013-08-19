package time
package calendar

import scalaz._
import Scalaz._

/** ISO-8601 ordinal date with extended year and day fo year. */
final class DateYD private (val year: Int, val dayOfYear: Int) {

  override def toString =
    s"DateYD($year,$dayOfYear)"

}

object DateYD extends DateYDFunctions with DateYDInstances {

  def apply(year: Int, day: Int): Option[DateYD] = 
    (day > 0 && day <= HasYear.lengthOfYear(year)) option new DateYD(year, day)

  def from[A](a:A)(implicit ev: HasDay[A]): DateYD =
    fromModifiedJulianDate(ev.toModifiedJulianDate(a))

  def fromModifiedJulianDate(jd: Int): DateYD = {
    val x = jd + 678575
    val quadcent = x / 146097
    val b = x % 146097
    val cent = (b / 36524) min 3
    val c = b - (cent * 36524)
    val quad = c / 1461
    val d = c % 1461
    val y = (d / 365) min 3
    val yd = (d - (y * 365) + 1)
    val year = quadcent * 400 + cent * 100 + quad * 4 + y + 1
    new DateYD(year, yd)
  }

  def unapply(yd: DateYD): Some[(Int, Int)] =
    Some((yd.year, yd.dayOfYear))

  // Needs to be here because it constructs instances
  implicit val enum: Enum[DateYD] = 
    new Enum[DateYD] {

      def pred(a: DateYD): DateYD =
        a match {
          case DateYD(y, 1) => new DateYD(y.pred, HasYear.lengthOfYear(y.pred))
          case DateYD(y, d) => new DateYD(y, d.pred)
        }

      def succ(a: DateYD): DateYD = 
        a match {
          case DateYD(y, d) if d == HasYear.lengthOfYear(y) => new DateYD(y.succ, 1)
          case DateYD(y, d) => new DateYD(y,  d.succ)
        }

      def order(x: DateYD,y: DateYD): Ordering = 
        Ordering.fromLessThan(x, y)((x, y) => (x.year, x.dayOfYear) < ((y.year, y.dayOfYear)))

    }

}

trait DateYDInstances {

  // TODO: HasDay

  /** Show instance for ISO-8601 YYYY-DDD extended format. */
  implicit val show: Show[DateYD] =
    Show.shows(yd => f"${yd.year}%04d-${yd.dayOfYear}%03d")

}

trait DateYDFunctions

