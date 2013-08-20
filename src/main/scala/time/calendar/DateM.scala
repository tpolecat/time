package time
package calendar

import scalaz._
import Scalaz._

/** ISO-8601 reduced-precision date with extended calendar year and month. */
case class DateM(year: Int, month: Month)

object DateM extends YearAndMonthFunctions with YearAndMonthInstances 

trait YearAndMonthFunctions 

trait YearAndMonthInstances {

  implicit val hasMonth: HasMonth[DateM] =
    new HasMonth[DateM] {

      def fromYearAndMonth(y: Int, m: Month): DateM =
        DateM(y, m)

      def year(d: DateM): Int =
        d.year

      def month(d: DateM): Month =
        d.month

      def addMonths(d: DateM, n: Int, mode: AddMode): DateM = 
        enum.succn(n, d)

    }

  implicit val enum: Enum[DateM] = 
    new Enum[DateM] {

      def pred(a: DateM): DateM =
        DateM((a.month === Month.Jan) ? a.year.pred | a.year, a.month.pred)

      def succ(a: DateM): DateM = 
        DateM((a.month === Month.Dec) ? a.year.succ | a.year, a.month.succ)

      def order(x: DateM,y: DateM): Ordering = 
        Ordering.fromLessThan(x, y)((x, y) => (x.year, x.month) < ((y.year, y.month)))

    }

  /** Show instance for ISO-8601 YYYY-MM extended format. */
  implicit val show: Show[DateM] =
    Show.shows(yam => f"${yam.year}%04d-${yam.month.ord}%02d")

}


