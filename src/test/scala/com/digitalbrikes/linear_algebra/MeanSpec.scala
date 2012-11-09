package com.digitalbrikes.linear_algebra

import org.specs2.mutable._
import com.digitalbrikes.linear_algebra.Nat._

class MeanSpec extends Specification {
	"mean " should {
		"average the values by column" in {
			val m = Matrix(Nat(2), Nat(3), Array[BigDecimal](1, 2, 3, 4, 5, 6))
			val expectedMean = Matrix(one, Nat(3), Array[BigDecimal](2.5, 3.5, 4.5))
			
			mean(m) must be_==(expectedMean)
		}
	}
}