package time
package calendar

import scalaz.Enum
import scalaz.Equal
import scalaz.Ordering
import scalaz.Show
import scalaz.std.anyVal._

/** Algebraic type for a calendar year, either a `CommonYear` or a `LeapYear`. */
sealed trait Year {

  val toInt: Int  
  val length: Int

  def fold[A](common: Int => A, leap: Int => A): A =
    this match {
      case CommonYear(n) => common(n)
      case LeapYear(n) => leap(n)
    }

  override def toString = 
    fold(n => s"CommonYear($n)", n => s"LeapYear($n)")

}

object Year extends YearFunctions with YearInstances {

  def apply(n: Int): Year =
    if (isLeapYear(n)) new LeapYear(n) else new CommonYear(n)

  class CommonYear private[Year] (val toInt: Int) extends Year {
    val length = 365
  }

  class LeapYear private[Year] (val toInt: Int) extends Year {
    val length = 366
  }

  object CommonYear {
    def unapply(y: CommonYear): Some[Int] =
      Some(y.toInt)
  }

  object LeapYear {
    def unapply(y: LeapYear): Some[Int] =
      Some(y.toInt)
  }

}

trait YearFunctions {

  def isLeapYear(year: Int): Boolean =
    (year % 4 == 0) && ((year % 400 == 0) || !(year % 100 == 0))

  def isExtendedYear(year: Int): Boolean =
    year < 0 || year > 9999

}

trait YearInstances {

  implicit val equal: Equal[Year] = 
    Equal.equalBy(_.toInt)

  implicit val enum: Enum[Year] =
    new Enum[Year] {

      def pred(a: Year): Year = Year(a.toInt - 1)
      def succ(a: Year): Year = Year(a.toInt + 1)
      
      override def predn(n: Int, a: Year): Year = Year(a.toInt - n)
      override def succn(n: Int, a: Year): Year = Year(a.toInt + n)

      override def min: Option[Year] = Some(Year(Int.MinValue))
      override def max: Option[Year] = Some(Year(Int.MaxValue))

      def order(x: Year, y: Year): Ordering =
        Ordering.fromLessThan(x, y)(_.toInt < _.toInt)

    }

  /** Show instance for standard ISO-8601 YYYY format. */
  implicit val show: Show[Year] =
    Show.shows(y => f"${y.toInt}%d")

}

