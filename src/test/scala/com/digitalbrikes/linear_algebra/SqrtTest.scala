package com.digitalbrikes.linear_algebra

import org.scalacheck._
import org.scalacheck.Prop._
import Nat._

object SqrtSpecification extends Properties("Sqrt") {
	import Arbitrary.arbitrary
	
	val unlimitedBigDecimalGen = for {
		  n <- arbitrary[Double]
	} yield (BigDecimal(scala.math.abs(n) + 0.00000001, java.math.MathContext.UNLIMITED))
	
	val matrixGenerator = for {
		  n <- Gen.choose(1, 100)
		  m <- Gen.choose(1, 100)
		  values1 <- Gen.containerOfN[Array, BigDecimal](n * m, unlimitedBigDecimalGen)
	} yield (Matrix(Nat(n), Nat(m), values1))

	property("sqrt calculate the sqrt of the BigDecimal") = forAll(unlimitedBigDecimalGen) {x : BigDecimal  => 
		sqrt(x) == scala.math.sqrt(x.doubleValue)
	}
	
	property("sqrt calculate the sqrt all the elements in the matrix") = forAll(matrixGenerator) {X : Matrix[Nat, Nat]  => 
		sqrt(X) == X.map( x => scala.math.sqrt(x.doubleValue))
	}
}