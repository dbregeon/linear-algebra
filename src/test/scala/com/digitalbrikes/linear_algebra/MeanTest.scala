package com.digitalbrikes.linear_algebra

import org.scalacheck.Arbitrary.arbitrary
import org.scalacheck.Gen
import org.scalacheck.Prop.forAll
import org.scalacheck.Prop.propBoolean
import org.scalacheck.Properties

import Nat.Nat

object MeanSpecification extends Properties("Mean") {
	val unlimitedBigDecimalGen = for {
		  n <- arbitrary[Double]
	} yield (BigDecimal(n, java.math.MathContext.UNLIMITED))

	val sameDimensionMatrixGenerator = for {
		  n <- Gen.choose(1, 100)
		  m <- Gen.choose(1, 100)
		  values1 <- Gen.listOfN[BigDecimal](n * m, unlimitedBigDecimalGen)
		  values2 <- Gen.listOfN[BigDecimal](n * m, unlimitedBigDecimalGen)
	} yield (Matrix(Nat(n), Nat(m), values1), Matrix(Nat(n), Nat(m), values2))
	
	val matrixGenerator = for {
		  n <- Gen.choose(1, 100)
		  m <- Gen.choose(1, 100)
		  values1 <- Gen.listOfN[BigDecimal](n * m, unlimitedBigDecimalGen)
	} yield (Matrix(Nat(n), Nat(m), values1))
	
	property("mean of mean is the same as mean") = forAll(matrixGenerator) {(m : Matrix[_ <: Nat, _ <: Nat]) => {
			mean(mean(m)) == mean(m)
		}
	}
		
}