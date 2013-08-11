package time.calendar

import scalaz.Enum
import scalaz.Equal
import scalaz.Order
import scalaz.Ordering
import scalaz.std.tuple._
import scalaz.std.anyVal._
import scalaz.syntax.enum._
import scalaz.syntax.bifunctor._

/**
 * A calendar date, with operations over the Gregorian calendar.
 * @see numerous smart constructors in the companion object
 */
sealed trait Date {

  /** Convert to Modified Julian Date. */
  def toModifiedJulianDate: Int

  /**
   * Convert to ISO 8601 Ordinal Date as (Gregorian year, day in year). The day will be in the range 1 .. 365 (or 366
   * for leap years).
   */
  def toOrdinalDate: (Int, Int) = {
    val a = toModifiedJulianDate + 678575
    val quadcent = a / 146097
    val b = a % 146097
    val cent = (b / 36524) min 3
    val c = b - (cent * 36524)
    val quad = c / 1461
    val d = c % 1461
    val y = (d / 365) min 3
    val yd = (d - (y * 365) + 1)
    val year = quadcent * 400 + cent * 100 + quad * 4 + y + 1
    (year, yd.toInt)
  }

  /** Gregorian year. By convention there is no year zero; a value of 0 here is typically shown as 1 BC. */
  def year: Int =
    toOrdinalDate._1

  /** Day in Gregorian year, in the range 1 .. 365 (or 366 for leap years). */
  def dayInYear: Int =
    toOrdinalDate._1

  /** Month in Gregorian year. */
  def month: Month =
    toGregorian._2

  /** Day in Gregorian month, in the range 1 .. 28, 29, 30, 31 depending on month and year. */
  def dayInMonth: Int =
    toGregorian._3

  /** Day in week in the Gregorian calendar. */
  def dayInWeek: Weekday =
    Weekday.fromOrdinal(mondayStartWeek._2).map(_.succ).get // ***

  /** Roll forward by 'n' days. */
  def addDays(n: Int): Date =
    Date.fromModifiedJulianDate(toModifiedJulianDate + n)

  /** Difference in days between 'this' and 'that'. */
  def diffDays(that: Date): Int =
    toModifiedJulianDate - that.toModifiedJulianDate

  /**
   * Get the number of the Monday-starting week in the Gregorian year, and the day of the week. The first Monday is the
   * first day of week 1, any earlier days in the year are week 0. Monday is 1, Sunday is 7.
   */
  def mondayStartWeek: (Int, Int) = {
    val d = toModifiedJulianDate + 2
    val k = d - toOrdinalDate._2
    ((d / 7) - (k / 7), (d % 7) + 1)
  }

  //-- | Get the number of the Sunday-starting week in the year and the day of the week.
  //-- The first Sunday is the first day of week 1, any earlier days in the year are week 0 (as \"%U\" in 'Data.Time.Format.formatTime').
  //-- Sunday is 0, Saturday is 6 (as \"%w\" in 'Data.Time.Format.formatTime').
  //sundayStartWeek :: Day -> (Int,Int)
  //sundayStartWeek date =(fromInteger ((div d 7) - (div k 7)),fromInteger (mod d 7)) where
  //  yd = snd (toOrdinalDate date)
  //  d = (toModifiedJulianDay date) + 3
  //  k = d - (toInteger yd)
  //

  // A wee helper
  private def addMonths(n: Int): (Int, Int, Int) = {
    def rolloverMonths(y: Int, m: Int): (Int, Int) =
      (y + ((m - 1) / 12), ((m - 1) % 12).toInt + 1)
    val (y, m, d) = toGregorian
    val (y0, m0) = rolloverMonths(y, m.ord + n)
    (y0, m0, d)
  }

  /**
   * Convert to proleptic Gregorian calendar.
   * First element of result is year, second month number (1-12), third day (1-31).
   */
  def toGregorian: (Int, Month, Int) = {
    val (year, dayOfYear) = toOrdinalDate
    val (month, day) = Date.monthAndDay(isLeapYear(year), dayOfYear)
    (year, month, day)
  }

  /**
   * Add months, with days past the last day of the month clipped to the last day.
   * For instance, 2005-01-30 + 1 month = 2005-02-28.
   */
  def addMonthsClip(n: Int): Date = {
    val (y, m, d) = addMonths(n)
    Date.fromYearMonthDayClipped(y, m, d)
  }

  /**
   * Add months, with days past the last day of the month rolling over to the next month.
   * For instance, 2005-01-30 + 1 month = 2005-03-02.
   */
  def addMonthsRollOver(n: Int): Date = {
    val (y, m, d) = addMonths(n)
    Date.fromYearMonthDayClipped(y, m, 1).addDays(d - 1)
  }

  /**
   * Add years, matching month and day, with Feb 29th clipped to Feb 28th if necessary.
   * For instance, 2004-02-29 + 2 years = 2006-02-28.
   */
  def addYearsClip(n: Int): Date =
    addMonthsClip(n * 12)

  /**
   * Add years, matching month and day, with Feb 29th rolled over to Mar 1st if necessary.
   * For instance, 2004-02-29 + 2 years = 2006-03-01.
   */
  def addYearsRollOver(n: Integer): Date =
    addMonthsRollOver(n * 12)

}

object Date extends DayFunctions with DateInstances {

  def fromModifiedJulianDate(mjd: Int): Date =
    new Date {
      val toModifiedJulianDate = mjd
    }

  def fromOrdinalDate(year: Int, dayOfYear: Int): Option[Date] =
    clipValid(1, daysInYear(year), dayOfYear) map { day0 =>
      val y = year - 1
      val mjd = day0 + (365 * y) + (y / 4) - (y / 100) + (y / 400) - 678576
      Date.fromModifiedJulianDate(mjd)
    }

  def fromOrdinalDateClipped(year: Int, dayOfYear: Int): Date = {
    val day0 = clip(1, daysInYear(year), dayOfYear)
    val y = year - 1
    val mjd = day0 + (365 * y) + (y / 4) - (y / 100) + (y / 400) - 678576
    Date.fromModifiedJulianDate(mjd)
  }

  /**
   * Convert from proleptic Gregorian calendar. First argument is year, second month number (1-12), third day (1-31).
   * Invalid values will be clipped to the correct range, month first, then day.
   */
  def fromYearMonthDayClipped(year: Int, month: Int, day: Int): Date =
    fromOrdinalDateClipped(year, dayOfYearClipped(isLeapYear(year), month, day))

  /**
   * Convert from proleptic Gregorian calendar.
   * First argument is year, second month number (1-12), third day (1-31).
   * Invalid values will return None
   */
  def fromYearMonthDay(year: Int, month: Int, day: Int): Option[Date] =
    dayOfYear(isLeapYear(year), month, day).flatMap(fromOrdinalDate(year, _))

  //-- | The inverse of 'mondayStartWeek'. Get a 'Day' given the year,
  //-- the number of the Monday-starting week, and the day of the week.
  //-- The first Monday is the first day of week 1, any earlier days in the year 
  //-- are week 0 (as \"%W\" in 'Data.Time.Format.formatTime').
  //fromMondayStartWeek :: Integer -- ^ Year.
  //                    -> Int     -- ^ Monday-starting week number.
  //                    -> Int     -- ^ Day of week. 
  //                               -- Monday is 1, Sunday is 7 (as \"%u\" in 'Data.Time.Format.formatTime').
  //                    -> Day
  //fromMondayStartWeek y w d = ModifiedJulianDay (firstDay + yd)
  //    where yd = firstMonday + 7 * toInteger (w-1) + toInteger d - 1
  //          -- first day of the year
  //          firstDay = toModifiedJulianDay (fromOrdinalDate y 1)
  //          -- 0-based year day of first monday of the year
  //          firstMonday = (5 - firstDay) `mod` 7
  //
  //fromMondayStartWeekValid :: Integer -- ^ Year.
  //                    -> Int     -- ^ Monday-starting week number.
  //                    -> Int     -- ^ Day of week. 
  //                               -- Monday is 1, Sunday is 7 (as \"%u\" in 'Data.Time.Format.formatTime').
  //                    -> Maybe Day
  //fromMondayStartWeekValid year w d = do
  //  d' <- clipValid 1 7 d
  //  -- first day of the year
  //  let firstDay = toModifiedJulianDay (fromOrdinalDate year 1)
  //  -- 0-based year day of first monday of the year
  //  let firstMonday = (5 - firstDay) `mod` 7
  //  let yd = firstMonday + 7 * toInteger (w-1) + toInteger d'
  //  yd' <- clipValid 1 (if isLeapYear year then 366 else 365) yd
  //  return (ModifiedJulianDay (firstDay - 1 + yd'))
  //
  //-- | The inverse of 'sundayStartWeek'. Get a 'Day' given the year and
  //-- the number of the day of a Sunday-starting week.
  //-- The first Sunday is the first day of week 1, any earlier days in the 
  //-- year are week 0 (as \"%U\" in 'Data.Time.Format.formatTime').
  //fromSundayStartWeek :: Integer -- ^ Year.
  //                    -> Int     -- ^ Sunday-starting week number.
  //                    -> Int     -- ^ Day of week
  //                               -- Sunday is 0, Saturday is 6 (as \"%w\" in 'Data.Time.Format.formatTime').
  //                    -> Day
  //fromSundayStartWeek y w d = ModifiedJulianDay (firstDay + yd)
  //    where yd = firstSunday + 7 * toInteger (w-1) + toInteger d
  //          -- first day of the year
  //          firstDay = toModifiedJulianDay (fromOrdinalDate y 1)
  //          -- 0-based year day of first sunday of the year
  //          firstSunday = (4 - firstDay) `mod` 7
  //
  //fromSundayStartWeekValid :: Integer -- ^ Year.
  //                    -> Int     -- ^ Monday-starting week number.
  //                    -> Int     -- ^ Day of week. 
  //                               -- Monday is 1, Sunday is 7 (as \"%u\" in 'Data.Time.Format.formatTime').
  //                    -> Maybe Day
  //fromSundayStartWeekValid year w d = do
  //  d' <- clipValid 1 7 d
  //  -- first day of the year
  //  let firstDay = toModifiedJulianDay (fromOrdinalDate year 1)
  //  -- 0-based year day of first sunday of the year
  //  let firstMonday = (4 - firstDay) `mod` 7
  //  let yd = firstMonday + 7 * toInteger (w-1) + toInteger d'
  //  yd' <- clipValid 1 (if isLeapYear year then 366 else 365) yd
  //  return (ModifiedJulianDay (firstDay - 1 + yd'))

}

trait DayFunctions {

  /** Convert month and day (clipped) in the Gregorian or Julian calendars to day of year. */
  def dayOfYearClipped(isLeap: Boolean, month: Int, day: Int): Int = {
    val month0 = clip(1, 12, month)
    val day0 = clip(1, monthLength0(isLeap, month0), day)
    val k = if (month0 <= 2) 0 else if (isLeap) -1 else -2
    ((367 * month0 - 362) / 12) + k + day0
  }

  /** Convert month and day in the Gregorian or Julian calendars to day of year. */
  def dayOfYear(isLeap: Boolean, month: Int, day: Int): Option[Int] =
    for {
      month0 <- clipValid(1, 12, month)
      day0 <- clipValid(1, monthLength0(isLeap, month0), day)
      k = if (month0 <= 2) 0 else if (isLeap) -1 else -2
    } yield ((367 * month0 - 362) / 12) + k + day0

  /** Convert day of year in the Gregorian or Julian calendars to month and day. */
  def monthAndDay(isLeap: Boolean, dayOfYear: Int): (Month, Int) = {
    val (m, d) = findMonthDay(monthLengths(isLeap), clip(1, if (isLeap) 366 else 365, dayOfYear))
    (Month.fromOrdinal(m).get, d) // **
  }

  // Helpers

  private def findMonthDay(ns: List[Int], yd: Int): (Int, Int) = ns match {
    case n :: ns if yd > n => findMonthDay(ns, yd - n).bimap(_ + 1, identity)
    case _                 => (1, yd)
  }

  private def monthLength0(isLeap: Boolean, month0: Int): Int =
    monthLengths(isLeap)(month0 - 1) // ***

  private def monthLengths(isLeap: Boolean): List[Int] =
    Month.values.map(_.length(isLeap))

}

trait DateInstances {

  implicit val equalDay: Equal[Date] =
    Equal.equalBy(_.toModifiedJulianDate)

  implicit val orderDay: Order[Date] =
    Order.orderBy(_.toModifiedJulianDate)

  implicit val enumDay: Enum[Date] =
    new Enum[Date] {

      def pred(d: Date): Date =
        Date.fromModifiedJulianDate(d.toModifiedJulianDate - 1)

      def succ(d: Date): Date =
        Date.fromModifiedJulianDate(d.toModifiedJulianDate + 1)

      def order(a: Date, b: Date): Ordering =
        Ordering.fromLessThan(a, b)(_.toModifiedJulianDate < _.toModifiedJulianDate)

    }

}

