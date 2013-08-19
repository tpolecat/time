package time
package calendar

/** Typeclass for calendar dates with month and year precision. */
trait HasMonth[A] extends HasYear[A] {

  def yearAndMonth(a: A): (Int, Month)

  def fromYearAndMonth(y: Int, m: Month): A

  def to[B](a: A)(implicit B: HasMonth[B]): B =
    B.fromYearAndMonth(year(a), month(a))

  def month(a:A): Month =
    yearAndMonth(a)._2

  def addMonths(a:A, n: Int, mode: AddMode): A 


  def lengthOfMonth(a:A): Int =
    HasMonth.lengthOfMonth(year(a), month(a))

  ////// HasYear implementation 

  def year(a: A): Int =
    yearAndMonth(a)._1

  def fromYear(y: Int): A =
    fromYearAndMonth(y, Month.Jan)

  def addYears(a: A, n: Int, mode: AddMode) : A = 
    addMonths(a, n * 12, mode)

}

object HasMonth extends HasMonthFunctions {

  def apply[A](implicit ev: HasMonth[A]): HasMonth[A] =
    ev

}

trait HasMonthFunctions {

  def lengthOfMonth(year: Int, month: Month): Int =
    if (HasYear.isLeapYear(year)) month.leapDays else month.commonDays

}

