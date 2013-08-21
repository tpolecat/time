package time
package calendar

import org.scalacheck.Arbitrary
import org.scalacheck.Arbitrary._
import org.specs2.scalaz._
import scalaz.scalacheck.ScalazProperties._

class YearSpec extends Spec {

  implicit def arb: Arbitrary[Year] = Arbitrary { 
    arbitrary[Int].map(Year.apply) 
  }
  
  checkAll(equal.laws[Year])
  checkAll(enum.laws[Year])

}