package tick

import org.scalacheck.Arbitrary
import org.scalacheck.Arbitrary._
import org.scalacheck.Prop._
import org.specs2.scalaz._
import scalaz.scalacheck.ScalazProperties._
import scalaz.Equal

class YearSpec extends Spec {
  import YearSpec._

  checkAll(equal.laws[Year])
  checkAll(enum.laws[Year])

  "catamorphism must satisfy" >> {

    "identity" ! forAll { (y: Year) => 
      Equal[Year].equal(y.fold(Year.apply, Year.apply), y) 
    }

    "partition" ! forAll { (y: Year) =>
      y.fold(!Year.isLeapYear(_), Year.isLeapYear)
    }

  }

}

object YearSpec {

  implicit def arbYear: Arbitrary[Year] = 
    Arbitrary(arbitrary[Int].map(Year.apply))

}