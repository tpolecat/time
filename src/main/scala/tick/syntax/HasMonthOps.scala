package tick
package syntax

import scala.language.implicitConversions
import scalaz.syntax.Ops

trait HasMonthOps[A] extends Ops[A] {
  implicit def A: HasMonth[A]

  def month: Month =
    A.month(self)

  def addMonths(n: Int, mode: AddMode): A =
    A.addMonths(self, n, mode)

  def lengthOfMonth: Int =
    A.lengthOfMonth(self)

}

trait ToHasMonthOps {

  implicit def ToHasMonthOps[A](a: A)(implicit A0: HasMonth[A]) =
    new HasMonthOps[A] { 
      val self: A = a
      implicit val A: HasMonth[A] = A0
    }

}

