package data

import shapeless._
import shapeless.Nat._
import scala.math.BigInt
import language._
import shapeless.Nat._

object Fixed {

  sealed trait Fixed[P <: Nat] {
    def num: BigInt
    def exp: Int // exponent
    def den: BigInt =
      BigInt(10).pow(-exp)

    def value: BigDecimal =
      BigDecimal(num) / den
      
      
  }

  private abstract class FixedImpl[P <: Nat: ToInt] extends Fixed[P] {
    def exp = -toInt[P]
    override def toString =
      s"%1.${toInt[P]}f".format(value)
  }

  object Fixed extends FixedInstances {

    def apply[P <: Nat: ToInt](d: BigDecimal): Fixed[P] =
      new FixedImpl[P] {
        val num = (d * den).toBigInt
      }

    def apply[P <: Nat: ToInt](d: BigInt): Fixed[P] =
      new FixedImpl[P] {
        val num = d * den.toBigInt
      }

  }

  trait FixedInstances {

  }
  
  // format: OFF
  type Uni   = Fixed[_0]
  type Deci  = Fixed[_1]
  type Centi = Fixed[_2]
  type Milli = Fixed[_3]
  type Micro = Fixed[_6]
  type Nano  = Fixed[_9]
  type Pico  = Fixed[_12]
  // format: ON

  implicit def bd2Fixed[N <% BigDecimal, P <: Nat: ToInt](d: N) =
    Fixed[P](d)

  implicit def bi2bd(n: BigInt): BigDecimal =
    BigDecimal(n)

}