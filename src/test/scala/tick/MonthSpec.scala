package tick

import org.scalacheck._
import org.scalacheck.Arbitrary._
import org.specs2.scalaz._
import scalaz.scalacheck.ScalazProperties._

class MonthSpec extends Spec {
  import MonthSpec._
  
  checkAll(equal.laws[Month])
  checkAll(enum.laws[Month])

}

object MonthSpec {

  implicit def arbMonth: Arbitrary[Month] = 
    Arbitrary(Gen.choose(1, 12).map(Month.unsafeMonthFromOrdinal))

}