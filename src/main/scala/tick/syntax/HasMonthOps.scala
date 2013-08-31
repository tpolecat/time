package tick
package syntax

import tick.Tags._
import scalaz.Tag
import scalaz.@@
import scala.language.implicitConversions
import scalaz.syntax.Ops

trait HasMonthOps[A] extends Ops[A] {
  implicit def A: HasMonth[A]

  def month: Month =
    A.month(self)

  def addMonths(n: Int)(implicit mode: AddMode): A =
    A.addMonths(self, n)

  def +(n: Int @@ Months)(implicit mode: AddMode): A =
    addMonths(n)

  def -(n: Int @@ Months)(implicit mode: AddMode): A =
    addMonths(-n)

  def lengthOfMonth: Int =
    A.lengthOfMonth(self)

  def diffMonths(b: A): Int =
    A.diffMonths(self, b)

}

trait ToHasMonthOps {

  implicit def ToHasMonthOps[A](a: A)(implicit A0: HasMonth[A]) =
    new HasMonthOps[A] { 
      val self: A = a
      implicit val A: HasMonth[A] = A0
    }

}

