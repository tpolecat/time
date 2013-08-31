package tick
package syntax

import tick.Tags._
import scalaz.Tag
import scalaz.@@
import scala.language.implicitConversions
import scalaz.syntax.Ops

trait IntOps extends Ops[Int] {

  def years: Int @@ Years =
    Tag(self)

  def months: Int @@ Months =
    Tag(self)

  def days: Int @@ Days =
    Tag(self)

}

trait ToIntOps {

  implicit def ToIntOps(a: Int) =
    new IntOps { 
      val self: Int = a
    }

}

