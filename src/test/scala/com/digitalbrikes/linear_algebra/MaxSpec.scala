package com.digitalbrikes.linear_algebra

import org.specs2.mutable._
import com.digitalbrikes.linear_algebra.Nat._

class MaxSpec extends Specification {
	"max " should {
		"provide the max of the values by column" in {
			val m = Matrix(Nat(2), Nat(3), Array[BigDecimal](1, 2, 3, 4, 5, 6))
			val expectedMax = Matrix(one, Nat(3), Array[BigDecimal](4, 5, 6))
			
			max(m) must be_==(expectedMax)
		}
	}
}