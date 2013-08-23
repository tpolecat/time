package time
package calendar

/** Typeclass for calendar dates with year precision. */
trait HasYear[A] {
  
  /** Calendar year. */
  def year(a:A): Year

  /** Construct an instance given a year. */
  def fromYear(y: Year): A

  /** Add `n` years given the specified `AddMode`. */
  def addYears(a:A, n: Int, mode: AddMode): A

  ////// Conversion

  def to[B](a: A)(implicit B: HasYear[B]): B =
    B.fromYear(year(a))

}

object HasYear extends HasYearFunctions {
  def apply[A](implicit ev: HasYear[A]): HasYear[A] = ev
}

trait HasYearFunctions
