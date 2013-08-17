package time
package calendar
package syntax

import scala.language.implicitConversions
import scalaz.syntax.Ops

trait HasYearOps[A] extends Ops[A] {
  implicit def A: HasYear[A]

  def year: Int =
    A.year(self)

  def addYearsClip(n: Int): A =
    A.addYearsClip(self, n)

  def addYearsRollOver(n: Int): A =
    A.addYearsRollOver(self, n)

  def isLeapYear: Boolean = 
    A.isLeapYear(self)

  def isCommonYear: Boolean =
    A.isCommonYear(self)

  def isExtendedYear: Boolean = 
    A.isExtendedYear(self)

  def lengthOfYear: Int =
    A.lengthOfYear(self)

  def toDateY: DateY =
    A.toDateY(self)

}

trait ToHasYearOps {

  implicit def ToHasYearOps[A](a: A)(implicit A0: HasYear[A]) =
    new HasYearOps[A] { 
      val self: A = a
      implicit val A: HasYear[A] = A0
    }

}

