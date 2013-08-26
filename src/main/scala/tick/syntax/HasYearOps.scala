package tick
package syntax

import scala.language.implicitConversions
import scalaz.syntax.Ops

trait HasYearOps[A] extends Ops[A] {
  implicit def A: HasYear[A]

  def year: Year =
    A.year(self)

  def addYears(n: Int, mode: AddMode): A =
    A.addYears(self, n, mode)

}

trait ToHasYearOps {

  implicit def ToHasYearOps[A](a: A)(implicit A0: HasYear[A]) =
    new HasYearOps[A] { 
      val self: A = a
      implicit val A: HasYear[A] = A0
    }

}

