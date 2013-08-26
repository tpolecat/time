package tick
package syntax

import scala.language.implicitConversions
import scalaz.syntax.Ops

trait HasDayOps[A] extends Ops[A] {
  implicit def A: HasDay[A]

  def dayOfYear: Int =
    A.dayOfYear(self)

  def dayOfMonth: Int = 
    A.dayOfMonth(self)

  def dayOfWeek: Weekday =
    A.dayOfWeek(self)

  def addDays(n: Int): A =
    A.addDays(self, n)

  def diffDays(b: A): Int =
    A.diffDays(self, b)

  def isLeapDay: Boolean =
    A.isLeapDay(self)

  def mondayStartWeek: (Int, Int) = 
    A.mondayStartWeek(self)

  def sundayStartWeek: (Int, Int) = 
    A.sundayStartWeek(self)

  def toModifiedJulianDate: Int =
    A.toModifiedJulianDate(self)

}

trait ToHasDayOps {

  implicit def ToHasDayOps[A](a: A)(implicit A0: HasDay[A]) =
    new HasDayOps[A] { 
      val self: A = a
      implicit val A: HasDay[A] = A0
    }

}

