package time
package calendar

import scalaz._
import Scalaz._

/** ISO-8601 reduced-precision date with extended calendar year and month. */
case class DateYM(year: Int, month: Month)

object DateYM extends YearAndMonthFunctions with YearAndMonthInstances 

trait YearAndMonthFunctions 

trait YearAndMonthInstances {

  implicit val hasMonth: HasMonth[DateYM] =
    new HasMonth[DateYM] {

      def yearAndMonth(d: DateYM): (Int, Month) = 
        (d.year, d.month)
      
      def fromYearAndMonth(y: Int, m: Month): DateYM =
        DateYM(y, m)

      def addMonths(d: DateYM, n: Int, mode: AddMode): DateYM = 
        enum.succn(n, d)

    }

  implicit val enum: Enum[DateYM] = 
    new Enum[DateYM] {

      def pred(a: DateYM): DateYM =
        DateYM((a.month === Month.Jan) ? a.year.pred | a.year, a.month.pred)

      def succ(a: DateYM): DateYM = 
        DateYM((a.month === Month.Dec) ? a.year.succ | a.year, a.month.succ)

      def order(x: DateYM,y: DateYM): Ordering = 
        Ordering.fromLessThan(x, y)((x, y) => (x.year, x.month) < ((y.year, y.month)))

    }

  /** Show instance for ISO-8601 YYYY-MM extended format. */
  implicit val show: Show[DateYM] =
    Show.shows(yam => f"${yam.year}%04d-${yam.month.ord}%02d")

}


