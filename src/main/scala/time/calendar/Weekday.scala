package time.calendar

import scalaz.Enum
import scalaz.Ordering

sealed abstract class Weekday private (val ord: Int)

object Weekday {

  case object Sun extends Weekday(1)
  case object Mon extends Weekday(2)
  case object Tue extends Weekday(3)
  case object Wed extends Weekday(4)
  case object Thu extends Weekday(5)
  case object Fri extends Weekday(6)
  case object Sat extends Weekday(7)

  val values: List[Weekday] =
    List(Sun, Mon, Tue, Wed, Thu, Fri, Sat)

  def fromOrdinal(n: Int): Option[Weekday] = 
    values.find(_.ord == n)

  implicit def enum = new Enum[Weekday] {
    def succ(a: Weekday): Weekday = fromOrdinal((a.ord % 7) + 1).get
    def pred(a: Weekday): Weekday = fromOrdinal(((a.ord + 5) % 7) + 1).get
    def order(x: Weekday, y: Weekday): Ordering = Ordering.fromInt(x.ord - y.ord)
    override def succn(n: Int, a: Weekday) = super.succn(n % 7, a)
    override def predn(n: Int, a: Weekday) = super.predn(n % 7, a)
    override def min = Some(Sun)
    override def max = Some(Sat)
  }  
  
}

