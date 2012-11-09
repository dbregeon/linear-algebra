package com.digitalbrikes.linear_algebra

import org.specs2.mutable._
import com.digitalbrikes.linear_algebra.Nat._

class UpperBidiagonalSpec extends Specification {
	"upperbidiagonal " should {
		"creates an upper bidiagonal matrix from a square matrix" in {
			val m = Matrix(Nat(3), Nat(3), Array(1, 2, 3, 3, 4, 5, 6, 7, 8))
			val expectedUpperBidiagonal = Matrix(Nat(3), Nat(3), Array(BigDecimal("-6.782329983125266643744080017901629"), BigDecimal("12.76203609215203434982716680775638"), BigDecimal("-9.362539119420733090000000000000000E-16"), BigDecimal("1.017753741669721826244263145928678E-15"), BigDecimal("1.974112267839988492545003842128287"), BigDecimal("-0.4830274697906361644088457732697324"), BigDecimal("-5.752284764637077407562629624930904E-16"), BigDecimal("3.94878370305670379000000000000000E-17"), BigDecimal("2.551767611827435206700000000000000E-16")))
			
			upperbidiagonal(m)._1 must be_==(expectedUpperBidiagonal)
		}
		
		"creates a U matrix from the Householder reflectors" in {
			val m = Matrix(Nat(3), Nat(3), Array(1, 2, 3, 3, 4, 5, 6, 7, 8))
			val expectedU = Matrix(Nat(3), Nat(3), Array(BigDecimal("-0.1474419561548968955041413443336050"), BigDecimal("-0.8610567718228913093192391792307479"), BigDecimal("0.4866642633922880572848930937625330"), BigDecimal("-0.4423258684646913165493292449045350"), BigDecimal("-0.3826918985879524110095231964686656"), BigDecimal("-0.8111071056538125268530798878791652"), BigDecimal("-0.8846517369293826330986584898090698"), BigDecimal("0.3348554112644582600169250730597619"), BigDecimal("0.3244428422615248246743116843612037")))
			val (b, u, v) = upperbidiagonal(m)
			u must be_==(expectedU)
		}
		
		"creates a V matrix from the Householder reflectors" in {
			val m = Matrix(Nat(3), Nat(3), Array(1, 2, 3, 3, 4, 5, 6, 7, 8))
			val expectedV = Matrix(Nat(3), Nat(3), Array(BigDecimal(1), BigDecimal(0), BigDecimal(0), BigDecimal(0), BigDecimal("-0.6469774481950956606774814791835820"), BigDecimal("-0.7625091353727914330877783454252078"), BigDecimal(0), BigDecimal("-0.7625091353727914330877783454252078"), BigDecimal("0.6469774481950958665956853227402174")))
			val (b, u, v) = upperbidiagonal(m)
			v must be_==(expectedV)
		}
		
		"creates a U and V matrices such that we can find the original matrix" in {
			val m = Matrix(Nat(3), Nat(3), Array(1, 2, 3, 3, 4, 5, 6, 7, 8))
			val (b, u, v) = upperbidiagonal(m)
			(u * b * v.transpose - m).norm must be_<(BigDecimal("0.0000000000001"))
		}
		
		"creates a U and V matrices such that we can find the upperbigiagonal" in {
			val m = Matrix(Nat(3), Nat(3), Array(1, 2, 3, 3, 4, 5, 6, 7, 8))
			val (b, u, v) = upperbidiagonal(m)
			(u.transpose * m * v - b).norm must be_<(BigDecimal("0.000000000000001"))
		}
	}
}