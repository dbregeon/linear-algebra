package com.digitalbrikes.linear_algebra

import scala.math.BigDecimal.double2bigDecimal
import scala.math.BigDecimal.int2bigDecimal

import org.scalacheck.Arbitrary.arbitrary
import org.scalacheck.Gen
import org.scalacheck.Prop.forAll
import org.scalacheck.Prop.propBoolean
import org.scalacheck.Properties

import Nat.Nat

object AbsSpecification extends Properties("Abs") {
	val unlimitedBigDecimalGen = for {
		  n <- arbitrary[Double]
	} yield (BigDecimal(scala.math.abs(n) + 0.00000001, java.math.MathContext.UNLIMITED))
	
	val matrixGenerator = for {
		  n <- Gen.choose(1, 100)
		  m <- Gen.choose(1, 100)
		  values1 <- Gen.listOfN[BigDecimal](n * m, unlimitedBigDecimalGen)
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