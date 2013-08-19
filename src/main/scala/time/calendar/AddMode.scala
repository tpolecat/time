package time
package calendar

sealed trait AddMode

object AddMode {

  case object Clip extends AddMode
  case object Rollover extends AddMode

  /**
   * Add years, matching month and day, with Feb 29th clipped to Feb 28th if necessary.
   * For instance, 2004-02-29 + 2 years = 2006-02-28.
   */

  /**
   * Add years, matching month and day, with Feb 29th rolled over to Mar 1st if necessary.
   * For instance, 2004-02-29 + 2 years = 2006-03-01.
   */


  /**
   * Add months, with days past the last day of the month clipped to the last day.
   * For instance, 2005-01-30 + 1 month = 2005-02-28.
   */

  /**
   * Add months, with days past the last day of the month rolling over to the next month.
   * For instance, 2005-01-30 + 1 month = 2005-03-02.
   */

}