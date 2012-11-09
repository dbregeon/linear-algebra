package com.digitalbrikes.linear_algebra

import org.scalacheck._
import org.scalacheck.Prop._
import Nat._
import java.math.RoundingMode
import com.digitalbrikes.linear_algebra.Matrix._

object HouseholderSpecification extends Properties("Householder") {
	import Arbitrary.arbitrary
	
	val limitedBigDecimalGen = for {
		  n <- arbitrary[Double]
	} yield (BigDecimal(n, java.math.MathContext.DECIMAL128))

	val householderParamsGenerator = for {
		  n <- Gen.choose(1, 100)
		  columnIndex <- Gen.choose(0, 100)
		  values <- Gen.containerOfN[Array, BigDecimal](n, limitedBigDecimalGen)
	} yield (Vector(Nat(n), values), columnIndex)
	
	property("given a householder transform H, H is orthogonal ") = forAll(householderParamsGenerator)  {tuple =>
	  	val (v, columnIndex) =  tuple
	  	(columnIndex < v.rowCount) ==> {
		  	val H = householder(v, columnIndex)
		  	val I = identity(v.rows)
		  	val HTranspose = H.transpose
			val result = ((HTranspose * H).map((value) => value.setScale(14, scala.math.BigDecimal.RoundingMode.HALF_EVEN)) == I) 
				&& ((H * HTranspose).map((value) => value.setScale(14, scala.math.BigDecimal.RoundingMode.HALF_EVEN)) == I)
			if (!result) {
				println(((HTranspose * H).map((value) => value.setScale(14, scala.math.BigDecimal.RoundingMode.HALF_EVEN)) - I)) 
			}
			result
	  	}
	}
}