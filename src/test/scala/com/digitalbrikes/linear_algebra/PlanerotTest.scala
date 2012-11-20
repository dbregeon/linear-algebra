package com.digitalbrikes.linear_algebra

import org.scalacheck._
import org.scalacheck.Prop._
import Nat._
import com.digitalbrikes.linear_algebra.Matrix._
import scala.math.BigDecimal

object PlanerotSpecification extends Properties("Planerot") {
	import Arbitrary.arbitrary
	
	val limitedBigDecimalGen = for {
		  n <- arbitrary[Double]
	} yield (BigDecimal(n, java.math.MathContext.DECIMAL128))
	
	property("(c -s; s c) * (a; b) = (r; 0)") = forAll(limitedBigDecimalGen, limitedBigDecimalGen)  {(a : BigDecimal, b : BigDecimal) => {
			val (c, s, r) = planerot(a, b)
			val J = Matrix(Nat(2), Nat(2), Array(c, -s, s, c))
			val x = Vector(Nat(2), Array(a, b))
			val result = Vector(Nat(2), Array(r, 0))
			
			(J * x - result).norm <= a.abs.max(b.abs).max(r) * BigDecimal(10).pow(-30)
		}	  	
	}
}