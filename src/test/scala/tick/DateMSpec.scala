package tick

import org.scalacheck.Arbitrary
import org.scalacheck.Arbitrary._
import org.scalacheck.Prop._
import org.specs2.scalaz._
import scalaz.scalacheck.ScalaCheckBinding._
import scalaz.scalacheck.ScalazProperties._
import scalaz._
import Scalaz._

class DateMSpec extends Spec {
  import YearSpec._
  import MonthSpec._

  implicit def arb: Arbitrary[DateM] = 
    Arbitrary((arbitrary[Year] |@| arbitrary[Month])(DateM(_, _)))
  
  checkAll(enum.laws[DateM])

}

