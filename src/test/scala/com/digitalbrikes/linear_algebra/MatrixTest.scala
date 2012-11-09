package com.digitalbrikes.linear_algebra

import org.scalacheck._
import org.scalacheck.Prop._
import Nat._

object MatrixSpecification extends Properties("Matrix") {
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

	property("addition is commutative") = forAll(sameDimensionMatrixGenerator) {tuple  => 
		val (a, b) =  tuple
		(a + b) == (b + a)
	}
	
	property("subtraction is the inverse of the addition") = forAll(sameDimensionMatrixGenerator)  {tuple =>
	  	val (a, b) =  tuple
		b == (a + b - a)
	}
	
	property("scalar multiplication is associative") = forAll(matrixGenerator, unlimitedBigDecimalGen, unlimitedBigDecimalGen) {(m : Matrix[_ <: Nat, _ <: Nat], s1 : BigDecimal, s2 : BigDecimal) => 
		((s1 * s2) *: m) == (s1 *: (s2 *: m))
	}
	
	property("scalar multiplication is associative") = forAll(matrixGenerator, unlimitedBigDecimalGen, unlimitedBigDecimalGen) {(m : Matrix[_ <: Nat, _ <: Nat], s1 : BigDecimal, s2 : BigDecimal) => 
		((s1 * s2) *: m) == (s1 *: (s2 *: m))
	}
	
	property("one is the neutral element of the scalar multiplication") = forAll(matrixGenerator) {(m : Matrix[_ <: Nat, _ <: Nat]) => 
		(1 *: m) == m
	}
	
	property("transpose of transpose is the same matrix") = forAll(matrixGenerator) {(m : Matrix[_ <: Nat, _ <: Nat]) => 
		(m.transpose).transpose == m
	}
	
	property("map of identity is the same matrix") = forAll(matrixGenerator) {(m : Matrix[_ <: Nat, _ <: Nat]) => 
		(m.map(value => value)) == m
	}
	
	property("map by row of identity is the same matrix") = forAll(matrixGenerator) {(m : Matrix[_ <: Nat, _ <: Nat]) => 
		(m.mapByRow((columnIndex, value) => value)) == m
	}
	
	property("map by row and map are identical when map by row is not using the columnIndex") = forAll(matrixGenerator) {(m : Matrix[_ <: Nat, _ <: Nat]) => 
		(m.mapByRow((columnIndex, value) => value + 1)) == (m.map(value => value + 1))
	}
	
	property("map by column and map are identical when map by row is not using the rowIndex") = forAll(matrixGenerator) {(m : Matrix[_ <: Nat, _ <: Nat]) => 
		(m.mapByRow((rowIndex, value) => value + 1)) == (m.map(value => value + 1))
	}
}