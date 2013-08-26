package tick

import org.scalacheck._
import org.scalacheck.Arbitrary._
import org.specs2.scalaz._
import scalaz.scalacheck.ScalazProperties._

class WeekdaySpec extends Spec {
  import WeekdaySpec._

  checkAll(equal.laws[Weekday])
  checkAll(enum.laws[Weekday])

}

object WeekdaySpec {

  implicit def arb: Arbitrary[Weekday] = 
    Arbitrary(Gen.choose(1, 7).map(Weekday.unsafeWeekdayFromOrdinal))

}