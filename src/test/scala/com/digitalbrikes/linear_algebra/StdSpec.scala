package com.digitalbrikes.linear_algebra

import org.specs2.mutable._
import com.digitalbrikes.linear_algebra.Nat._

class StdSpec extends Specification {
	"std " should {
		"compute the square root of the variance per column" in {
			val m = Matrix(Nat(2), Nat(3), Array(1, 2, 3, 4, 5, 6))
			val expectedStd = Matrix(one, Nat(3), Array(2.1213203435596424, 2.1213203435596424, 2.1213203435596424))
			
			std(m) must be_==(expectedStd)
		}
	}
}