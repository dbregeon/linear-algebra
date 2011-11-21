package com.digitalbrikes.linear_algebra

import org.scalacheck._
import org.scalacheck.Prop._
import Nat._

object LogSpecification extends Properties("Log") {
	import Arbitrary.arbitrary
	
	val unlimitedBigDecimalGen = for {
		  n <- arbitrary[Double] suchThat (_ > 0)
	} yield (BigDecimal(n, java.math.MathContext.UNLIMITED))
	
	val matrixGenerator = for {
		  n <- Gen.choose(1, 100)
		  m <- Gen.choose(1, 100)
		  values1 <- Gen.containerOfN[Array, BigDecimal](n * m, unlimitedBigDecimalGen)
	} yield (Matrix(Nat(n), Nat(m), values1))

	property("log calculate the log of the BigDecimal") = forAll(unlimitedBigDecimalGen) {x : BigDecimal  => 
		log(x) == scala.math.log(x.doubleValue)
	}
	
	property("log calculate the log all the elements in the matrix") = forAll(matrixGenerator) {X : Matrix[Nat, Nat]  => 
		log(X) == X.map( x => scala.math.log(x.doubleValue))
	}
}