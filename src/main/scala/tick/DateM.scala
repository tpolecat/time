package tick

import scalaz._
import Scalaz._

/** Reduced-precision date with calendar year and month. */
case class DateM(year: Year, month: Month) 

object DateM extends YearAndMonthFunctions with YearAndMonthInstances 

trait YearAndMonthFunctions 

trait YearAndMonthInstances {

  implicit val hasMonth: HasMonth[DateM] =
    new HasMonth[DateM] {

      def fromYearAndMonth(y: Year, m: Month): DateM =
        DateM(y, m)

      def year(d: DateM): Year =
        d.year

      def month(d: DateM): Month =
        d.month

      def addMonths(d: DateM, n: Int)(implicit mode: AddMode): DateM = 
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

      override val min: Option[DateM] =
        (Enum[Year].min |@| Enum[Month].min)(DateM.apply)

      override val max: Option[DateM] =
        (Enum[Year].max |@| Enum[Month].max)(DateM.apply)

    }

  /** Show instance for ISO-8601 YYYY-MM extended format. */
  implicit val show: Show[DateM] =
    Show.shows(yam => f"${yam.year.toInt}%04d-${yam.month.ord}%02d")

}


