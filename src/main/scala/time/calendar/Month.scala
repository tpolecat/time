package time.calendar

import scalaz.Enum
import scalaz.Ordering
import scalaz.Show

/** Algebraic type representing the 12 months of the Julian/Gregorian calendar. */
sealed trait Month {

  /* Natural ordinal in 1 .. 12 **/
  def ord: Int

  /* Length of this month in a non-leap year. **/
  def commonDays: Int

  /* Length of this month in a leap year. **/
  def leapDays: Int =
    commonDays

  /** Length of this month, depending on 'isLeap'. */
  def length(isLeap: Boolean): Int =
    if (isLeap) leapDays else commonDays

}

object Month extends MonthFunctions with MonthInstances {

  case object Jan extends Month {
    val ord = 1
    val commonDays = 31
  }

  case object Feb extends Month {
    val ord = 2
    val commonDays = 28
    override val leapDays = 29
  }

  case object Mar extends Month {
    val ord = 3
    val commonDays = 31
  }

  case object Apr extends Month {
    val ord = 4
    val commonDays = 30
  }

  case object May extends Month {
    val ord = 5
    val commonDays = 31
  }

  case object Jun extends Month {
    val ord = 6
    val commonDays = 30
  }

  case object Jul extends Month {
    val ord = 7
    val commonDays = 31
  }

  case object Aug extends Month {
    val ord = 8
    val commonDays = 31
  }

  case object Sep extends Month {
    val ord = 9
    val commonDays = 30
  }

  case object Oct extends Month {
    val ord = 10
    val commonDays = 31
  }

  case object Nov extends Month {
    val ord = 11
    val commonDays = 30
  }

  case object Dec extends Month {
    val ord = 12
    val commonDays = 31
  }

 }

trait MonthFunctions { this: Month.type =>

  /** All month instances, in calendar order. */
  val months: List[Month] =
    List(Jan, Feb, Mar, Apr, May, Jun, Jul, Aug, Sep, Oct, Nov, Dec)

  /** Returns the corresponding Month for the natural ordinal in 1 .. 12, otherwise None. */
  def monthFromOrdinal(n: Int): Option[Month] =
    months.find(_.ord == n)

}

trait MonthInstances { this: Month.type =>

  implicit def monthEnum: Enum[Month]= 
    new Enum[Month] { 

      def pred(a: Month): Month = 
        a match {
          case Jan => Dec
          case Feb => Jan
          case Mar => Feb
          case Apr => Mar
          case May => Apr
          case Jun => May
          case Jul => Jun
          case Aug => Jul
          case Sep => Aug
          case Oct => Sep
          case Nov => Oct
          case Dec => Nov
        }
      
      def succ(a: Month): Month = 
        a match {
          case Jan => Feb
          case Feb => Mar
          case Mar => Apr
          case Apr => May
          case May => Jun
          case Jun => Jul
          case Jul => Aug
          case Aug => Sep
          case Sep => Oct
          case Oct => Nov
          case Nov => Dec
          case Dec => Jan
        }
      
      def order(x: Month, y: Month): Ordering = 
        Ordering.fromInt(x.ord - y.ord)

      override def succn(n: Int, a: Month): Month = super.succn(n % 12, a)
      override def predn(n: Int, a: Month): Month = super.predn(n % 12, a)

      override def min: Option[Month] = Some(Jan)
      override def max: Option[Month] = Some(Dec)

    }

  implicit def monthShow: Show[Month] =
    Show.shows {
      case Jan => "Jan"
      case Feb => "Feb"
      case Mar => "Mar"
      case Apr => "Apr"
      case May => "May"
      case Jun => "Jun"
      case Jul => "Jul"
      case Aug => "Aug"
      case Sep => "Sep"
      case Oct => "Oct"
      case Nov => "Nov"
      case Dec => "Dec"
    }

}
