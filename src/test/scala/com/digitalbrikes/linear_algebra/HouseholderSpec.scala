package com.digitalbrikes.linear_algebra

import org.specs2.mutable._
import com.digitalbrikes.linear_algebra.Nat._
import java.math.MathContext

class HouseholderSpec extends Specification {
	"householder " should {
		"compute the householder decomposition for the vector (1, 2, 3, 4) for index 0" in {
			val v = Vector(Nat(4), Array(1, 2, 3, 4))
			val expectedStd = Matrix(Nat(4), Nat(4), Array(BigDecimal("-0.182574185835055414810145992166213"), BigDecimal("-0.3651483716701107520964742776842722"),	BigDecimal("-0.5477225575051661281447114165264082"), BigDecimal("-0.7302967433402215041929485553685442"), BigDecimal("-0.3651483716701107520964742776842722"), BigDecimal("0.8872516118393179803433815211206907"), BigDecimal("-0.1691225822410230294849277183189640"), BigDecimal("-0.2254967763213640393132369577586186"), BigDecimal("-0.5477225575051661281447114165264082"), BigDecimal("-0.1691225822410230294849277183189640"), BigDecimal("0.7463161266384654557726084225215540"), BigDecimal("-0.3382451644820460589698554366379280"), BigDecimal("-0.7302967433402215041929485553685442"), BigDecimal("-0.2254967763213640393132369577586186"), BigDecimal("-0.3382451644820460589698554366379280"), BigDecimal("0.5490064473572719213735260844827626")))
			
			householder(v, 0) must be_==(expectedStd)
		}
		
		"compute the householder decomposition for the vector (1, 3, 6) for index 0" in {
			val v = Vector(Nat(3), Array(1, 3, 6))
			val expectedStd = Matrix(Nat(3), Nat(3), Array(BigDecimal("-0.147441956154896895504141344333605"), BigDecimal("-0.4423258684646913165493292449045350"), BigDecimal("-0.8846517369293826330986584898090698"), BigDecimal("-0.4423258684646913165493292449045350"), BigDecimal("0.8294883912309794559580409314542225"), BigDecimal("-0.3410232175380410880839181370915550"), BigDecimal("-0.8846517369293826330986584898090698"), BigDecimal("-0.3410232175380410880839181370915550"), BigDecimal("0.3179535649239178238321637258168902")))
			
			householder(v, 0) must be_==(expectedStd)
		}
		
		"compute the householder decomposition for the vector (-6.7823, -8.2567, -9.7312) for index 1" in {
			val v = Vector(Nat(3), Array(-6.7823, -8.2567, -9.7312))
			val expectedStd = Matrix(Nat(3), Nat(3), Array(1, 0, 0, 0, BigDecimal("-0.646973996778954925752383725584012"), BigDecimal("-0.7625120638336581856457998599287626"), 0, BigDecimal("-0.7625120638336581856457998599287626"), BigDecimal("0.6469739967789549292225086798562624")))
			
			householder(v, 1) must be_==(expectedStd)
		}
		
		"compute the householder decomposition for the vector (12.76204, -0.10021, 1.97157) for index 1" in {
			val v = Vector(Nat(3), Array(12.76204, -0.10021, 1.97157))
			val expectedStd = Matrix(Nat(3), Nat(3), Array(1, 0, 0, 0, BigDecimal("-0.050761985220916231888153789303774"), BigDecimal("0.9987107793833163644520637504806428"), 0, BigDecimal("0.9987107793833163644520637504806428"), BigDecimal("0.0507619852209164898103145008862056")))
			
			householder(v, 1) must be_==(expectedStd)
		}
		
		"compute the householder decomposition for the vector (-8.8818e-16, -4.8303e-01, 4.8572e-16) for index 2" in {
			val v = Vector(Nat(3), Array(-8.8818e-16, -4.8303e-01, 4.8572e-16))
			val expectedStd = Matrix(Nat(3), Nat(3), Array(1, 0, 0, 0, 1, 0, 0, 0, -1))
			
			householder(v, 2) must be_==(expectedStd)
		}
	}
}