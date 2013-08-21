package time
package calendar

/** Typeclass for calendar dates with year precision. */
trait HasYear[A] {
  
  /** Calendar year. */
  def year(a:A): Int

  /** Construct an instance given a year. */
  def fromYear(y: Int): A

  /** Add `n` years given the specified `AddMode`. */
  def addYears(a:A, n: Int, mode: AddMode): A

  /** True if this is a leap year in the proleptic Gregorian calendar. */
  def isLeapYear(a:A): Boolean = 
    HasYear.isLeapYear(year(a))

  /** True if this is a common (non-leap) year in the proleptic Gregorian calendar. */
  def isCommonYear(a:A): Boolean =
    HasYear.isCommonYear(year(a))

  /** True if this year is outside the ISO-8601 range [0000, 9999]. */
  def isExtendedYear(a:A): Boolean = 
    HasYear.isExtendedYear(year(a))

  /** Length of this year in days. */
  def lengthOfYear(a: A): Int =
    HasYear.lengthOfYear(year(a))

  ////// Conversion

  def to[B](a: A)(implicit B: HasYear[B]): B =
    B.fromYear(year(a))

}

object HasYear extends HasYearFunctions{
  def apply[A](implicit ev: HasYear[A]): HasYear[A] = ev
}

trait HasYearFunctions {

  // It's sometimes convenient to do simple year operations on raw ints, so the default typeclass
  // implementations just delegate to these functions.

  val DaysInCommonYear = 365
  val DaysInLeapYear = 366

  def lengthOfYear(year: Int): Int =
    if(isLeapYear(year)) DaysInLeapYear else DaysInCommonYear

  /** True if this is a leap year in the proleptic Gregorian calendar. */
  def isLeapYear(year: Int): Boolean =
    (year % 4 == 0) && ((year % 400 == 0) || !(year % 100 == 0))

  /** True if this is a common (non-leap) year in the proleptic Gregorian calendar. */
  def isCommonYear(year: Int): Boolean =
    !isLeapYear(year)

  /** True if this year is outside the ISO-8601 range [0000, 9999]. */
  def isExtendedYear(year: Int): Boolean =
    year < 0 || year > 9999

}

