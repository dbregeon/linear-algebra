package com.digitalbrikes.linear_algebra

import scala.collection.immutable.{Vector => _Vector}
import scala.math.BigDecimal.int2bigDecimal

import org.specs2.mutable.Specification

class UpperBidiagonalSpec extends Specification {
	"upperbidiagonal " should {
		"creates an upper bidiagonal matrix from a square matrix" in {
			val m = Matrix(Nat(3), Nat(3), _Vector(1, 2, 3, 3, 4, 5, 6, 7, 8))
			val expectedUpperBidiagonal = Matrix(Nat(3), Nat(3), _Vector(BigDecimal("-6.782329983125268139064556326625912"), BigDecimal("12.76203609215203968457022331428589"), BigDecimal("-3.500000000000000000000000000000000E-32"), BigDecimal("4.671976928565961099136987910154715E-32"), BigDecimal("1.974112267839990861838358470933165"), BigDecimal("-0.4830274697906360619391728173560150"), BigDecimal("-2.640574100256868084290202340131487E-32"), BigDecimal("-6.60000000000000000000000000000000E-33"), BigDecimal("1.369000000000000000000000000000000E-32")))
			
			upperbidiagonal(m)._1 must be_==(expectedUpperBidiagonal)
		}
		
		"creates an upper bidiagonal matrix from a 3 x 2 matrix" in {
			val m = Matrix(Nat(3), Nat(2), _Vector(BigDecimal("2.5180e-01"), BigDecimal("7.0645e-01"), BigDecimal("6.9026e-03"), BigDecimal("3.5878e-01"), BigDecimal("5.6886e-04"), BigDecimal("8.9756e-01")))
			val expectedUpperBidiagonal = Matrix(Nat(3), Nat(2), _Vector(BigDecimal("-0.2518952351444139564195102959991754"), BigDecimal("0.7180414139469720359690743175819534"), BigDecimal("-2.858138070665547739767021860435834E-36"), BigDecimal("0.9580319265854521366979203697911579"), BigDecimal("-5.288960262529131579095981281020501E-36"), BigDecimal("-1.000000000000000000000000000000000E-33")))
			
			upperbidiagonal(m)._1 must be_==(expectedUpperBidiagonal)
		}
		
		"creates a U matrix from the Householder reflectors" in {
			val m = Matrix(Nat(3), Nat(3), _Vector(1, 2, 3, 3, 4, 5, 6, 7, 8))
			val expectedU = Matrix(Nat(3), Nat(3), _Vector(BigDecimal("-0.1474419561548971334579251375353390"), BigDecimal("-0.8610567718228917666163703283036789"), BigDecimal("0.4866642633922876144934362406411468"), BigDecimal("-0.4423258684646914003737754126060382"), BigDecimal("-0.3826918985879518962739423681349966"), BigDecimal("-0.8111071056538126908223937344018627"), BigDecimal("-0.8846517369293828007475508252120764"), BigDecimal("0.3348554112644579092396995721181192"), BigDecimal("0.3244428422615250763289574937607359")))
			val (b, u, v) = upperbidiagonal(m)
			u must be_==(expectedU)
		}
		
		"creates a V matrix from the Householder reflectors" in {
			val m = Matrix(Nat(3), Nat(3), _Vector(1, 2, 3, 3, 4, 5, 6, 7, 8))
			val expectedV = Matrix(Nat(3), Nat(3), _Vector(BigDecimal(1), BigDecimal(0), BigDecimal(0), BigDecimal(0), BigDecimal("-0.6469774481950958289529477172252380"), BigDecimal("-0.7625091353727915126945455238726050"), BigDecimal(0), BigDecimal("-0.7625091353727915126945455238726050"), BigDecimal("0.6469774481950958289529477172252446")))
			val (b, u, v) = upperbidiagonal(m)
			v must be_==(expectedV)
		}
		
		"creates a U and V matrices such that we can find the original matrix" in {
			val m = Matrix(Nat(3), Nat(3), _Vector(1, 2, 3, 3, 4, 5, 6, 7, 8))
			val (b, u, v) = upperbidiagonal(m)
			(u * b * v.transpose - m).norm must be_<=(BigDecimal(10).pow(-20))
		}
		
		"creates a U and V matrices such that we can find the upperbigiagonal" in {
			val m = Matrix(Nat(3), Nat(3), _Vector(1, 2, 3, 3, 4, 5, 6, 7, 8))
			val (b, u, v) = upperbidiagonal(m)
			(u.transpose * m * v - b).norm must be_<=(BigDecimal(10).pow(-20))
		}
	}
}