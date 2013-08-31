package tick
package tc

/** Typeclass for calendar dates with month and year precision (or better). */
trait HasMonth[A] extends HasYear[A] {

  ////// Minimal Implementation

  def fromYearAndMonth(y: Year, m: Month): A

  def month(a:A): Month

  def addMonths(a:A, n: Int)(implicit mode: AddMode): A 

  /** Difference in months, `a` - `b`. */
  def diffMonths(a: A, b: A): Int =
    diffYears(a, b) * 12 + month(a).ord - month(b).ord

  ////// Useful Functions

  def lengthOfMonth(a:A): Int =
    year(a).fold(_ => month(a).commonDays, _ => month(a).leapDays)

  ////// Default HasYear Implementation

  def fromYear(y: Year): A =
    fromYearAndMonth(y, Month.Jan)

  def addYears(a: A, n: Int)(implicit mode: AddMode) : A = 
    addMonths(a, n * 12)

}

object HasMonth extends HasMonthFunctions {
  def apply[A](implicit ev: HasMonth[A]): HasMonth[A] = ev
}

trait HasMonthFunctions {

  def lengthOfMonth(year: Year, month: Month): Int =
    year.fold(_ => month.commonDays, _ => month.leapDays)

}

