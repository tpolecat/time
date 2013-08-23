package time
package calendar

import scalaz._
import Scalaz._

/** ISO-8601 ordinal date with extended year and day fo year. */
final class DateD private (val year: Year, val dayOfYear: Int) {

  override def toString =
    s"DateD($year,$dayOfYear)"

}

object DateD extends DateDFunctions with DateDInstances {

  def apply(year: Year, day: Int): Option[DateD] = 
    (day > 0 && day <= year.length) option new DateD(year, day)

  def unapply(yd: DateD): Some[(Year, Int)] =
    Some((yd.year, yd.dayOfYear))

  // Needs to be here because it constructs instances
  implicit val enum: Enum[DateD] = 
    new Enum[DateD] {

      def pred(a: DateD): DateD =
        a match {
          case DateD(y, 1) => new DateD(y.pred, y.pred.length)
          case DateD(y, d) => new DateD(y, d.pred)
        }

      def succ(a: DateD): DateD = 
        a match {
          case DateD(y, d) if d == y.length => new DateD(y.succ, 1)
          case DateD(y, d) => new DateD(y,  d.succ)
        }

      def order(x: DateD,y: DateD): Ordering = 
        Ordering.fromLessThan(x, y)((x, y) => (x.year, x.dayOfYear) < ((y.year, y.dayOfYear)))

    }


  // TODO: HasDay
  implicit val hasYear: HasYear[DateD] =
    new HasYear[DateD] {
      
      def year(a: DateD): Year = 
        a.year
      
      def fromYear(a: Year): DateD = 
        new DateD(a, 1)

      def addYears(a: DateD, n: Int, addMode: AddMode): DateD =
        ???
    
    }

}

trait DateDInstances {

  /** Show instance for ISO-8601 YYYY-DDD extended format. */
  implicit val show: Show[DateD] =
    Show.shows(yd => f"${yd.year.toInt}%04d-${yd.dayOfYear}%03d")

}

trait DateDFunctions

