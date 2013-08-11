package time.calendar

import scalaz.{ Enum, Ordering }
import java.util.Calendar._

sealed abstract class Month (val ord: Int, lens: (Int, Int)) {
  def length(isLeap: Boolean): Int =
    if (isLeap) lens._1 else lens._2
}


object Month {

  case object Jan extends Month(1, (31, 31))
  case object Feb extends Month(2, (29, 28))
  case object Mar extends Month(3, (31, 31))
  case object Apr extends Month(4, (30, 30))
  case object May extends Month(5, (31, 31))
  case object Jun extends Month(6, (30, 30))
  case object Jul extends Month(7, (31, 31))
  case object Aug extends Month(8, (31, 31))
  case object Sep extends Month(9, (30, 30))
  case object Oct extends Month(10, (31, 31))
  case object Nov extends Month(11, (30, 30))
  case object Dec extends Month(12, (31, 31))

  val values: List[Month] =
    List(Jan, Feb, Mar, Apr, May, Jun, Jul, Aug, Sep, Oct, Nov, Dec)

  def fromOrdinal(n: Int): Option[Month] = 
    values.find(_.ord == n)

//  implicit def enum = new Enum[Month] { // TODO: pred and succ are wrong
//    def pred(a: Month): Month = forOrdinal((a.ord - 1) % 12).get
//    def succ(a: Month): Month = forOrdinal((a.ord + 1) % 12).get
//    def order(x: Month, y: Month): Ordering = Ordering.fromInt(x.ord - y.ord)
//    override def succn(n: Int, a: Month) = super.succn(n % 12, a)
//    override def predn(n: Int, a: Month) = super.predn(n % 12, a)
//    override def min = Some(Jan)
//    override def max = Some(Dec)
//  }

}

