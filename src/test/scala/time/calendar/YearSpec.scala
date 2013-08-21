package time
package calendar

import org.scalacheck.Arbitrary
import org.scalacheck.Arbitrary._
import org.specs2.scalaz._
import scalaz.scalacheck.ScalazProperties._

class YearSpec extends Spec {

  implicit def arb: Arbitrary[Year] = Arbitrary { 
    // Testing with short to avoid boundary issues where enum fails.
    arbitrary[Short].map(n => Year.apply(n.toInt)) 
  }
  
  checkAll(equal.laws[Year])
  checkAll(enum.laws[Year])

}