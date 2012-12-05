package com.digitalbrikes.linear_algebra

import org.scalacheck.Arbitrary.arbitrary
import org.scalacheck.Gen
import org.scalacheck.Prop.forAll
import org.scalacheck.Prop.propBoolean
import org.scalacheck.Properties

import Matrix.identity

object HouseholderSpecification extends Properties("Householder") {	
	val limitedBigDecimalGen = for {
		  n <- arbitrary[Double]
	} yield (BigDecimal(n, java.math.MathContext.DECIMAL128))

	val householderParamsGenerator = for {
		  n <- Gen.choose(1, 10)
		  columnIndex <- Gen.choose(0 , n-1)
		  values <- Gen.listOfN[BigDecimal](n, limitedBigDecimalGen)
	} yield (Vector(Nat(n), values), columnIndex)
	
	property("given a householder transform H, H is orthogonal ") = forAll(householderParamsGenerator)  {tuple =>
	  	val (v, columnIndex) =  tuple
	  	(columnIndex < v.rowCount) ==> {
	  		val I = identity(v.rows)
	  		val H = householder(v, columnIndex)
	  		val HTranspose = H.transpose
	  		((HTranspose * H - I).norm <= BigDecimal(10).pow(-30) 
				&& (H * HTranspose - I).norm <= BigDecimal(10).pow(-30))
	  	}
	}
}