package tick
package tc

import scalaz.@@
import scalaz.Tag
import tick.Tags.Years

/** Typeclass for calendar dates with year precision (or better). */
trait HasYear[A] {
  
  /** Calendar year. */
  def year(a:A): Year

  /** Construct an instance given a year. */
  def fromYear(y: Year): A

  /** Add `n` years given the specified `AddMode`. */
  def addYears(a:A, n: Int)(implicit mode: AddMode): A

  /** Difference in years, `a` - `b`. */
  def diffYears(a: A, b: A): Int @@ Years =
    Tag(year(a).toInt - year(b).toInt)

}

object HasYear extends HasYearFunctions {
  def apply[A](implicit ev: HasYear[A]): HasYear[A] = ev
}

trait HasYearFunctions
