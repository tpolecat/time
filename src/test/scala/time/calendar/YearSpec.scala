package time
package calendar

import org.scalacheck.Arbitrary
import org.scalacheck.Arbitrary._
import org.scalacheck.Prop._
import org.specs2.scalaz._
import scalaz.scalacheck.ScalazProperties._
import scalaz.Equal

class YearSpec extends Spec {

  implicit def arb: Arbitrary[Year] = Arbitrary { 
    arbitrary[Int].map(Year.apply) 
  }
  
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