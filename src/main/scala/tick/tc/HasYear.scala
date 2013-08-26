package tick
package tc

/** Typeclass for calendar dates with year precision (or better). */
trait HasYear[A] {
  
  /** Calendar year. */
  def year(a:A): Year

  /** Construct an instance given a year. */
  def fromYear(y: Year): A

  /** Add `n` years given the specified `AddMode`. */
  def addYears(a:A, n: Int, mode: AddMode): A

}

object HasYear extends HasYearFunctions {
  def apply[A](implicit ev: HasYear[A]): HasYear[A] = ev
}

trait HasYearFunctions
