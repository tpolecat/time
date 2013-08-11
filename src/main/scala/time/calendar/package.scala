package time

import scalaz.Order
import scalaz.syntax.order._

// http://www.haskell.org/ghc/docs/7.4-latest/html/libraries/time-1.4/src/Data-Time-Calendar-Private.html
package object calendar {

  def clip[A: Order](a: A, b: A, x: A): A =
    if (x < a) a
    else if (x > b) b
    else x

  def clipValid[A: Order](a: A, b: A, x: A): Option[A] =
    if (x < a || x > b) None
    else Some(x)

  /** Is this year a leap year according to the proleptic Gregorian calendar? */
  def isLeapYear(year: Int): Boolean =
    (year % 4 == 0) && ((year % 400 == 0) || !(year % 100 == 0))

  def daysInYear(year: Int): Int =
    if (isLeapYear(year)) 366 else 365

}