package time
package calendar

import scalaz._
import Scalaz._

/** ISO-8601 ordinal date with extended year and day fo year. */
final class DateD private (val year: Int, val dayOfYear: Int) {

  override def toString =
    s"DateD($year,$dayOfYear)"

}

object DateD extends DateDFunctions with DateDInstances {

  def apply(year: Int, day: Int): Option[DateD] = 
    (day > 0 && day <= HasYear.lengthOfYear(year)) option new DateD(year, day)

  def unapply(yd: DateD): Some[(Int, Int)] =
    Some((yd.year, yd.dayOfYear))

  // Needs to be here because it constructs instances
  implicit val enum: Enum[DateD] = 
    new Enum[DateD] {

      def pred(a: DateD): DateD =
        a match {
          case DateD(y, 1) => new DateD(y.pred, HasYear.lengthOfYear(y.pred))
          case DateD(y, d) => new DateD(y, d.pred)
        }

      def succ(a: DateD): DateD = 
        a match {
          case DateD(y, d) if d == HasYear.lengthOfYear(y) => new DateD(y.succ, 1)
          case DateD(y, d) => new DateD(y,  d.succ)
        }

      def order(x: DateD,y: DateD): Ordering = 
        Ordering.fromLessThan(x, y)((x, y) => (x.year, x.dayOfYear) < ((y.year, y.dayOfYear)))

    }

}

trait DateDInstances {

  // TODO: HasDay

  /** Show instance for ISO-8601 YYYY-DDD extended format. */
  implicit val show: Show[DateD] =
    Show.shows(yd => f"${yd.year}%04d-${yd.dayOfYear}%03d")

}

trait DateDFunctions

