package com.digitalbrikes.linear_algebra

import org.scalacheck._
import org.scalacheck.Prop._
import Nat._

object AbsSpecification extends Properties("Abs") {
	import Arbitrary.arbitrary
	
	val unlimitedBigDecimalGen = for {
		  n <- arbitrary[Double]
	} yield (BigDecimal(scala.math.abs(n) + 0.00000001, java.math.MathContext.UNLIMITED))
	
	val matrixGenerator = for {
		  n <- Gen.choose(1, 100)
		  m <- Gen.choose(1, 100)
		  values1 <- Gen.containerOfN[Array, BigDecimal](n * m, unlimitedBigDecimalGen)
	} yield (Matrix(Nat(n), Nat(m), values1))

	property("abs calculate the abs of the BigDecimal") = forAll(unlimitedBigDecimalGen) {x : BigDecimal  => 
		abs(x) == scala.math.abs(x.doubleValue)
	}
	
	property("abs calculate the abs all the elements in the matrix") = forAll(matrixGenerator) {X : Matrix[Nat, Nat]  => 
		abs(X) == X.map( x => scala.math.abs(x.doubleValue))
	}
	
	property("abs of the opposite of a matrix is the same the abs of the matrix") = forAll(matrixGenerator) {X : Matrix[Nat, Nat]  => 
		abs(X) == abs(-1 *: X)
	}
}