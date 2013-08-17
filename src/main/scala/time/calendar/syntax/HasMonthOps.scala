package time
package calendar
package syntax

import scala.language.implicitConversions
import scalaz.syntax.Ops

trait HasMonthOps[A] extends Ops[A] {
  implicit def A: HasMonth[A]

  def month: Month =
    A.month(self)

  def addMonthsClip(n: Int): A =
    A.addMonthsClip(self, n)

  def addMonthsRollOver(n: Int): A =
    A.addMonthsRollOver(self, n)

  def lengthOfMonth: Int =
    A.lengthOfMonth(self)

  def toDateYM: DateYM =
    A.toDateYM(self)

}

trait ToHasMonthOps {

  implicit def ToHasMonthOps[A](a: A)(implicit A0: HasMonth[A]) =
    new HasMonthOps[A] { 
      val self: A = a
      implicit val A: HasMonth[A] = A0
    }

}

