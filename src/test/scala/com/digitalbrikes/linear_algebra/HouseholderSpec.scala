package com.digitalbrikes.linear_algebra

import org.specs2.mutable._
import com.digitalbrikes.linear_algebra.Nat._
import com.digitalbrikes.linear_algebra.Matrix._
import java.math.MathContext

class HouseholderSpec extends Specification {
	"householder " should {
		"compute the householder decomposition for the vector (1, 2, 3, 4) for index 0" in {
			val v = Vector(Nat(4), Array(1, 2, 3, 4))
			val expectedStd = Matrix(Nat(4), Nat(4), Array(BigDecimal("-0.182574185835055371152323260933601"), BigDecimal("-0.3651483716701107423046465218672014"),	BigDecimal("-0.5477225575051661134569697828008020"), BigDecimal("-0.7302967433402214846092930437344026"), BigDecimal("-0.3651483716701107423046465218672014"), BigDecimal("0.8872516118393179822279066566804967"), BigDecimal("-0.1691225822410230266581400149792549"), BigDecimal("-0.2254967763213640355441866866390066"), BigDecimal("-0.5477225575051661134569697828008020"), BigDecimal("-0.1691225822410230266581400149792549"), BigDecimal("0.7463161266384654600127899775311176"), BigDecimal("-0.3382451644820460533162800299585098"), BigDecimal("-0.7302967433402214846092930437344026"), BigDecimal("-0.2254967763213640355441866866390066"), BigDecimal("-0.3382451644820460533162800299585098"), BigDecimal("0.5490064473572719289116266267219870")))
			
			householder(v, 0) must be_==(expectedStd)
		}
		
		"compute the householder decomposition for the vector (1, 3, 6) for index 0" in {
			val v = Vector(Nat(3), Array(1, 3, 6))
			val expectedStd = Matrix(Nat(3), Nat(3), Array(BigDecimal("-0.147441956154897133457925137535339"), BigDecimal("-0.4423258684646914003737754126060382"), BigDecimal("-0.8846517369293828007475508252120764"), BigDecimal("-0.4423258684646914003737754126060382"), BigDecimal("0.8294883912309794266915850275070708"), BigDecimal("-0.3410232175380411466168299449858584"), BigDecimal("-0.8846517369293828007475508252120764"), BigDecimal("-0.3410232175380411466168299449858584"), BigDecimal("0.3179535649239177067663401100282832")))
			
			householder(v, 0) must be_==(expectedStd)
		}
		
		"compute the householder decomposition for the vector (-6.7823, -8.2567, -9.7312) for index 1" in {
			val v = Vector(Nat(3), Array(-6.7823, -8.2567, -9.7312))
			val expectedStd = Matrix(Nat(3), Nat(3), Array(1, 0, 0, 0, BigDecimal("-0.646973996778954889836543363895297"), BigDecimal("-0.7625120638336582198671831098063320"), 0, BigDecimal("-0.7625120638336582198671831098063320"), BigDecimal("0.6469739967789548898365433638953030")))
			
			householder(v, 1) must be_==(expectedStd)
		}
		
		"compute the householder decomposition for the vector (12.76204, -0.10021, 1.97157) for index 1" in {
			val v = Vector(Nat(3), Array(12.76204, -0.10021, 1.97157))
			val expectedStd = Matrix(Nat(3), Nat(3), Array(1, 0, 0, 0, BigDecimal("-0.050761985220916399430455403892579"), BigDecimal("0.9987107793833164916186304825116618"), 0, BigDecimal("0.9987107793833164916186304825116618"), BigDecimal("0.0507619852209163994304554038925830")))
			
			householder(v, 1) must be_==(expectedStd)
		}
		
		"compute the householder decomposition for the vector (-8.8818e-16, -4.8303e-01, 4.8572e-16) for index 2" in {
			val v = Vector(Nat(3), Array(-8.8818e-16, -4.8303e-01, 4.8572e-16))
			val expectedStd = Matrix(Nat(3), Nat(3), Array(1, 0, 0, 0, 1, 0, 0, 0, -1))
			
			householder(v, 2) must be_==(expectedStd)
		}
		
		"compute the householder decomposition for the vector (0, 0, 0) for index 0" in {
			val v = Vector(Nat(3), Array(0, 0, 0))
			val expectedStd = Matrix(Nat(3), Nat(3), Array(1, 0, 0, 0, 1, 0, 0, 0, 1))
			val I = identity(v.rows)
			val H = householder(v, 0) 
			(H * H.transpose - I).norm.compare(BigDecimal(0)) == 0 && (H.transpose * H - I).norm.compare(BigDecimal(0)) == 0 must beTrue 
		}
		
		"compute the householder decomposition for the vector (4.518227016649075E+307, 1.0, -1.0, 1.0) for index 0" in {
			val v = Vector(Nat(3), Array(BigDecimal("4.518227016649075E+307"), 1.0, -1.0, 1.0))
			val expectedStd = Matrix(Nat(3), Nat(3), Array(1, 0, 0, 0, 1, 0, 0, 0, 1))
			val I = identity(v.rows)
			val H = householder(v, 0)
			(H * H.transpose - I).norm <= BigDecimal(10).pow(-30) && (H.transpose * H - I).norm <= BigDecimal(10).pow(-30) must beTrue  
		}
	}
}