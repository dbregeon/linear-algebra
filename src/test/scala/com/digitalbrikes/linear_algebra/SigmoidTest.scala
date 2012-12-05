package com.digitalbrikes.linear_algebra

import scala.math.BigDecimal.double2bigDecimal

import org.scalacheck.Arbitrary.arbitrary
import org.scalacheck.Gen
import org.scalacheck.Prop.forAll
import org.scalacheck.Prop.propBoolean
import org.scalacheck.Properties

import Nat.Nat

object SigmoidSpecification extends Properties("Sigmoid") {
	val unlimitedBigDecimalGen = for {
		  n <- arbitrary[Double]
	} yield (BigDecimal(n, java.math.MathContext.UNLIMITED))
	
	val matrixGenerator = for {
		  n <- Gen.choose(1, 100)
		  m <- Gen.choose(1, 100)
		  values1 <- Gen.listOfN[BigDecimal](n * m, unlimitedBigDecimalGen)
	} yield (Matrix(Nat(n), Nat(m), values1))

	property("sigmoid calculate the sigmoid of the BigDecimal") = forAll(unlimitedBigDecimalGen) {x : BigDecimal  => 
		sigmoid(x) == 1 / (1 + scala.math.exp(-1. * x.doubleValue))
	}
	
	property("sigmoid calculate the sigmoid all the elements in the matrix") = forAll(matrixGenerator) {X : Matrix[Nat, Nat]  => 
		sigmoid(X) == X.map( x => 1 / (1 + scala.math.exp(-1 * x.doubleValue)))
	}
}