package com.digitalbrikes.linear_algebra

import org.scalacheck.Arbitrary.arbitrary
import org.scalacheck.Gen
import org.scalacheck.Prop.forAll
import org.scalacheck.Prop.propBoolean
import org.scalacheck.Properties

import Nat.Nat

object SqrtSpecification extends Properties("Sqrt") {
	val limitedBigDecimalGen = for {
		  n <- arbitrary[Double]
	} yield (BigDecimal(scala.math.abs(n), java.math.MathContext.DECIMAL128))
	
	val matrixGenerator = for {
		  n <- Gen.choose(1, 100)
		  m <- Gen.choose(1, 100)
		  values1 <- Gen.listOfN[BigDecimal](n * m, limitedBigDecimalGen)
	} yield (Matrix(Nat(n), Nat(m), values1))

	property("sqrt calculate the sqrt of the BigDecimal") = forAll(limitedBigDecimalGen) {x : BigDecimal  => {
			val root = sqrt(x) 
			root * root - x <= x * BigDecimal(10).pow(-30)
		}
	}
	
	property("sqrt calculate the sqrt all the elements in the matrix") = forAll(matrixGenerator) {X : Matrix[Nat, Nat]  => 
		sqrt(X) == X.map( x => sqrt(x))
	}
}