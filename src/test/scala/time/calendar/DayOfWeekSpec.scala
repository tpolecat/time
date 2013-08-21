package time
package calendar

import org.scalacheck._
import org.scalacheck.Arbitrary._
import org.specs2.scalaz._
import scalaz.scalacheck.ScalazProperties._

class WeekdaySpec extends Spec {

  implicit def arb: Arbitrary[Weekday] = Arbitrary { 
    Gen.choose(1, 7).map(Weekday.unsafeWeekdayFromOrdinal)
  }
  
  checkAll(equal.laws[Weekday])
  checkAll(enum.laws[Weekday])

}