package com.digitalbrikes.linear_algebra

import scala.collection.immutable.{Vector => _Vector}
import scala.math.BigDecimal.int2bigDecimal

import org.specs2.mutable.Specification

import com.digitalbrikes.linear_algebra.Nat.one

class StdSpec extends Specification {
	"std " should {
		"compute the square root of the variance per column" in {
			val m = Matrix(Nat(2), Nat(3), _Vector(1, 2, 3, 4, 5, 6))
			val expectedStd = Matrix(one, Nat(3), _Vector(BigDecimal("2.121320343559642573202533086314554"), BigDecimal("2.121320343559642573202533086314554"), BigDecimal("2.121320343559642573202533086314554")))
			
			std(m) must be_==(expectedStd)
		}
	}
}