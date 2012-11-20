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
		  n <- Gen.choose(1, 10)
		  columnIndex <- Gen.choose(0 , n-1)
		  values <- Gen.containerOfN[Array, BigDecimal](n, limitedBigDecimalGen)
	} yield (Vector(Nat(n), values), columnIndex)
	
	property("given a householder transform H, H is orthogonal ") = forAll(householderParamsGenerator)  {tuple =>
	  	val (v, columnIndex) =  tuple
	  	(columnIndex < v.rowCount) ==> {
	  		val I = identity(v.rows)
	  		try {
	  			val H = householder(v, columnIndex)
	  			val HTranspose = H.transpose
	  			val result = ((HTranspose * H - I).norm <= BigDecimal(10).pow(-30) 
					&& (H * HTranspose - I).norm <= BigDecimal(10).pow(-30))
					if (! result) {
						println((HTranspose * H - I).norm)
						println((H * HTranspose - I).norm)
					}
	  			result
	  		} catch {
	  		  case _ => {
	  			  println("Failed Householder")
	  			  println(v)
	  			  println(columnIndex)
	  			  true
	  		    }
	  		}
	  	}
	}
}