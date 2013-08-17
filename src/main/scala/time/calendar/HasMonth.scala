package time
package calendar

/** Typeclass for calendar dates with month and year precision. */
trait HasMonth[A] extends HasYear[A] {

  def month(a:A): Month

  /**
   * Add months, with days past the last day of the month clipped to the last day.
   * For instance, 2005-01-30 + 1 month = 2005-02-28.
   */
  def addMonthsClip(a:A, n: Int): A 

  /**
   * Add months, with days past the last day of the month rolling over to the next month.
   * For instance, 2005-01-30 + 1 month = 2005-03-02.
   */
  def addMonthsRollOver(a: A, n: Int): A 

  def lengthOfMonth(a:A): Int =
    HasMonth.lengthOfMonth(year(a), month(a))

  def toDateYM(a:A): DateYM =
    DateYM(year(a), month(a))

}

object HasMonth extends HasMonthFunctions {

  def apply[A](implicit ev: HasMonth[A]): HasMonth[A] =
    ev

}

trait HasMonthFunctions {

  def lengthOfMonth(year: Int, month: Month): Int =
    if (HasYear.isLeapYear(year)) month.leapDays else month.commonDays

}

