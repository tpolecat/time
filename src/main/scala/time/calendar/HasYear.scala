package time
package calendar

/** Typeclass for calendar dates with year precision. */
trait HasYear[A] {
  
  def year(a:A): Int

  /**
   * Add years, matching month and day, with Feb 29th clipped to Feb 28th if necessary.
   * For instance, 2004-02-29 + 2 years = 2006-02-28.
   */
  def addYearsClip(a:A, n: Int): A

  /**
   * Add years, matching month and day, with Feb 29th rolled over to Mar 1st if necessary.
   * For instance, 2004-02-29 + 2 years = 2006-03-01.
   */
  def addYearsRollOver(a: A, n: Int): A 

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

  def toDateY(a:A): DateY =
    DateY(year(a))

}

object HasYear extends HasYearFunctions{

  def apply[A](implicit ev: HasYear[A]): HasYear[A] =
    ev

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

