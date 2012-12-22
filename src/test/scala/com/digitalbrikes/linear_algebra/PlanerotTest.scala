package com.digitalbrikes.linear_algebra

import scala.collection.immutable.{Vector => _Vector}
import scala.math.BigDecimal
import scala.math.BigDecimal.int2bigDecimal

import org.scalacheck.Arbitrary.arbitrary
import org.scalacheck.Prop.forAll
import org.scalacheck.Prop.propBoolean
import org.scalacheck.Properties

object PlanerotSpecification extends Properties("Planerot") {
	val limitedBigDecimalGen = for {
		  n <- arbitrary[Double]
	} yield (BigDecimal(n, java.math.MathContext.DECIMAL128))
	
	property("(c s; -s c) * (a; b) = (r; 0)") = forAll(limitedBigDecimalGen, limitedBigDecimalGen)  {(a : BigDecimal, b : BigDecimal) => {
			val (c, s, r) = planerot(a, b)
			val J = Matrix(Nat(2), Nat(2), _Vector(c, s, -s, c))
			val x = Vector(Nat(2), _Vector(a, b))
			val result = Vector(Nat(2), _Vector(r, 0))
			
			(J * x - result).norm <= a.abs.max(b.abs).max(r) * BigDecimal(10).pow(-30)
		}	  	
	}
}