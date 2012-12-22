package com.digitalbrikes.linear_algebra

import scala.collection.immutable.{Vector => _Vector}
import org.scalatest.Ignore
import org.specs2.mutable._

import com.digitalbrikes.linear_algebra.Nat._

@Ignore
class DiagSpec extends Specification {
	"diag " should {
		"creates a diagonal matrix from a one row matrix" in {
			val m = Vector(Nat(3), _Vector(1, 2, 3))
			val expectedStd = Matrix(Nat(3), Nat(3), _Vector(1, 0, 0, 0, 2, 0, 0, 0, 3))
			
			diag(m) must be_==(expectedStd)
		}
	}
}