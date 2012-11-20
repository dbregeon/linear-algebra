package com.digitalbrikes.linear_algebra

import org.specs2.mutable._
import com.digitalbrikes.linear_algebra.Nat._
import java.math.MathContext

class PlanerotSpec extends Specification {
	"planerot " should {
		"compute the givens plane rotation c value" in {
			planerot(1, 0)._1 must be_==(1)
		}
		
		"compute the givens plane rotation s value" in {
			planerot(1, 0)._2 must be_==(0)
		}
		
		"compute the givens plane r value" in {
			planerot(1, 0)._3 must be_==(1)
		}
		
		"compute the givens plane rotation for 4.795628519951503E+307, 2.807032599346196E+307" in {
		    val x = Vector(Nat(2), Array(BigDecimal("4.795628519951503E+307"), BigDecimal("2.807032599346196E+307")))
			val (c, s, r) = planerot(BigDecimal("4.795628519951503E+307"), BigDecimal("2.807032599346196E+307"))
			val J = Matrix(Nat(2), Nat(2), Array(c, -s, s, c))
			val result = Vector(Nat(2), Array(r, 0))
			((J * x) - result).norm must be_<=(abs(x(0, 0)).max(abs(x(0, 1))).max(r) * BigDecimal(10).pow(-30))
		}
		
		"compute the givens plane rotation for -1.0, -8.988465674311579E+307" in {
		    val x = Vector(Nat(2), Array(BigDecimal("-1.0"), BigDecimal("-8.988465674311579E+307")))
			val (c, s, r) = planerot(BigDecimal("-1.0"), BigDecimal("-8.988465674311579E+307"))
			val J = Matrix(Nat(2), Nat(2), Array(c, -s, s, c))
			val result = Vector(Nat(2), Array(r, 0))
			((J * x) - result).norm must be_<=(abs(x(0, 0)).max(abs(x(0, 1))).max(r) * BigDecimal(10).pow(-30))
		}
	}
}