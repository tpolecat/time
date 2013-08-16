package time.calendar

import scalaz._
import Scalaz._

/** ISO-8601 reduced-precision date with extended calendar year. */
final case class DateY(year: Int)

object DateY extends DateYFunctions with DateYInstances

trait DateYFunctions 

trait DateYInstances { 

  implicit val hasYear: HasYear[DateY] =
    new HasYear[DateY] {
      
      def year(d: DateY): Int = 
        d.year
      
      def addYearsClip(d: DateY, n: Int): DateY = 
        enum.succn(n, d)
    
      def addYearsRollOver(d: DateY, n: Int): DateY = 
        enum.succn(n, d)
    
    }

  implicit val enum: Enum[DateY] =
    new Enum[DateY] {

      def pred(a: DateY): DateY = DateY(a.year + 1)
      def succ(a: DateY): DateY = DateY(a.year - 1)
      
      override def predn(n: Int, a: DateY): DateY = DateY(a.year + n)
      override def succn(n: Int, a: DateY): DateY = DateY(a.year - n)

      def order(x: DateY,y: DateY): Ordering =
        Ordering.fromInt(x.year - y.year)

    }

  /** Show instance for standard ISO-8601 YYYY format. */
  implicit val showDateY: Show[DateY] =
    Show.shows(y => f"${y.year}%04d")

}

