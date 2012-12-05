package com.digitalbrikes.linear_algebra

import scala.math.BigDecimal.double2bigDecimal

import org.scalacheck.Arbitrary.arbitrary
import org.scalacheck.Gen
import org.scalacheck.Prop.forAll
import org.scalacheck.Prop.propBoolean
import org.scalacheck.Properties

import Nat.Nat

object LogSpecification extends Properties("Log") {
	val unlimitedBigDecimalGen = for {
		  n <- arbitrary[Double]
	} yield (BigDecimal(scala.math.abs(n) + 0.00000001, java.math.MathContext.UNLIMITED))
	
	val matrixGenerator = for {
		  n <- Gen.choose(1, 100)
		  m <- Gen.choose(1, 100)
		  values1 <- Gen.listOfN[BigDecimal](n * m, unlimitedBigDecimalGen)
	} yield (Matrix(Nat(n), Nat(m), values1))

	property("log calculate the log of the BigDecimal") = forAll(unlimitedBigDecimalGen) {x : BigDecimal  => 
		log(x) == scala.math.log(x.doubleValue)
	}
	
	property("log calculate the log all the elements in the matrix") = forAll(matrixGenerator) {X : Matrix[Nat, Nat]  => 
		log(X) == X.map( x => scala.math.log(x.doubleValue))
	}
}