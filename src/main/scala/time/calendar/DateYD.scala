package time
package calendar

import scalaz._
import Scalaz._

/** ISO-8601 ordinal date with extended year and day fo year. */
final class DateYD private (val year: Int, val day: Int) {

  override def toString =
    s"DateYD($year,$day)"

}

object DateYD extends DateYDFunctions with DateYDInstances {

  def apply(year: Int, day: Int): Option[DateYD] = 
    (day > 0 && day <= HasYear.lengthOfYear(year)) option new DateYD(year, day)

  def unapply(yd: DateYD): Some[(Int, Int)] =
    Some((yd.year, yd.day))

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
        Ordering.fromLessThan(x, y)((x, y) => (x.year, x.day) < ((y.year, y.day)))

    }

}

trait DateYDInstances {

  implicit val hasYear: HasYear[DateYD] =
    new HasYear[DateYD] {

      def year(d: DateYD): Int = 
        d.year

      def addYearsClip(d:DateYD, n: Int): DateYD = 
        ???

      def addYearsRollOver(d:DateYD, n: Int): DateYD = 
        ???

    }

  /** Show instance for ISO-8601 YYYY-DDD extended format. */
  implicit val show: Show[DateYD] =
    Show.shows(yd => f"${yd.year}%04d-${yd.day}%03d")

}

trait DateYDFunctions

