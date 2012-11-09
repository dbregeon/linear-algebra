package com.digitalbrikes.linear_algebra

import org.scalacheck._
import org.scalacheck.Prop._
import Nat._

object MeanSpecification extends Properties("Mean") {
	import Arbitrary.arbitrary
	
	val unlimitedBigDecimalGen = for {
		  n <- arbitrary[Double]
	} yield (BigDecimal(n, java.math.MathContext.UNLIMITED))

	val sameDimensionMatrixGenerator = for {
		  n <- Gen.choose(1, 100)
		  m <- Gen.choose(1, 100)
		  values1 <- Gen.containerOfN[Array, BigDecimal](n * m, unlimitedBigDecimalGen)
		  values2 <- Gen.containerOfN[Array, BigDecimal](n * m, unlimitedBigDecimalGen)
	} yield (Matrix(Nat(n), Nat(m), values1), Matrix(Nat(n), Nat(m), values2))
	
	val matrixGenerator = for {
		  n <- Gen.choose(1, 100)
		  m <- Gen.choose(1, 100)
		  values1 <- Gen.containerOfN[Array, BigDecimal](n * m, unlimitedBigDecimalGen)
	} yield (Matrix(Nat(n), Nat(m), values1))
	
	property("mean of mean is the same as mean") = forAll(matrixGenerator) {(m : Matrix[_ <: Nat, _ <: Nat]) => {
			mean(mean(m)) == mean(m)
		}
	}
		
}