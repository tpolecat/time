package util

import shapeless._
import shapeless.TypeOperators._
import shapeless.Nat._
import shapeless.LTEq._
import scala.annotation.implicitNotFound

@implicitNotFound("Cannot prove that ${A} is evenly divisble by ${B}")
trait Div[A <: Nat, B <: Nat]

object Div {
  implicit def mod[A <: Nat, B <: Nat, C <: Nat](implicit div: DivAux[A, B, C], ev: C =:= _0): Div[A, B] =
    new Div[A, B] {}
}

trait DivAux[A <: Nat, B <: Nat, C <: Nat]

object DivAux {
  implicit def divAux[A <: Nat, B <: Nat](implicit m: Mod[A, B]): DivAux[A, B, m.Out] =
    new DivAux[A, B, m.Out] {}
}

@implicitNotFound("Cannot prove that ${A} is NOT evenly divisble by ${B}")
trait NDiv[A <: Nat, B <: Nat]

object NDiv {
  implicit def mod[A <: Nat, B <: Nat, C <: Nat](implicit div: DivAux[A, B, C], ev: C =:!= _0): NDiv[A, B] =
    new NDiv[A, B] {}
}

object Foo {


  // H is centuries, L is low years 0 - 99
  trait LeapYear[H <: Nat, L <: Nat]
  object LeapYear {

    // if low is divisible by 4 and is not zero, it's a leap year always
    implicit def leap1[H <: Nat, L <: Nat](implicit div: Div[L, _4], nz: L =:!= _0): LeapYear[H, L] =
      new LeapYear[H, L] {}

    // If low is zero and hi is divisible by 4, it's a leap year
    implicit def leap2[H <: Nat, L <: Nat](implicit div: Div[H, _4], z: L =:= _0): LeapYear[H, L] =
      new LeapYear[H, L] {}

  }

  def ly[H <: Nat, L <: Nat](implicit ev: LeapYear[H, L]) = "\\o/"

  implicitly[Div[_20, _4]]
  implicitly[_0 =:= _0]

  
 ly[_19, _12] // ok; 1913 won't compile
  
}






