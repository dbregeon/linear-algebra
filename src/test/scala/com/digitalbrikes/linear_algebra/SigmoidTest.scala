package com.digitalbrikes.linear_algebra

import org.scalacheck._
import org.scalacheck.Prop._
import Nat._

object SigmoidSpecification extends Properties("Sigmoid") {
	import Arbitrary.arbitrary
	
	val unlimitedBigDecimalGen = for {
		  n <- arbitrary[Double]
	} yield (BigDecimal(n, java.math.MathContext.UNLIMITED))
	
	val matrixGenerator = for {
		  n <- Gen.choose(1, 100)
		  m <- Gen.choose(1, 100)
		  values1 <- Gen.containerOfN[Array, BigDecimal](n * m, unlimitedBigDecimalGen)
	} yield (Matrix(Nat(n), Nat(m), values1))

	property("sigmoid calculate the sigmoid of the BigDecimal") = forAll(unlimitedBigDecimalGen) {x : BigDecimal  => 
		sigmoid(x) == 1 / (1 + scala.math.exp(x.doubleValue))
	}
	
	property("sigmoid calculate the sigmoid all the elements in the matrix") = forAll(matrixGenerator) {X : Matrix[Nat, Nat]  => 
		sigmoid(X) == X.map( x => 1 / (1 + scala.math.exp(x.doubleValue)))
	}
}