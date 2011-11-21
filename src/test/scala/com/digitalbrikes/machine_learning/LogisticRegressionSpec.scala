package com.digitalbrikes.machine_learning

import org.specs2.mutable._
import com.digitalbrikes.linear_algebra.Nat
import com.digitalbrikes.linear_algebra.Vector
import com.digitalbrikes.linear_algebra.Matrix

class LogisticRegressionSpec extends Specification {
	"biasless" should {
		"replace the first row with zeros" in {
			val m = Nat(4)
	     	val n = Nat(3)
			val input = Matrix(m, n, Array.tabulate(12) (index => 1))
			val expected = Matrix(m, n, Array[BigDecimal](0, 0, 0, 1, 1, 1, 1, 1, 1, 1, 1, 1))
			
			LogisticRegression.biasless(input) must be_==(expected)
		}
	}
	
	"cost" should {
	     "be 0.175929351253886" in {
	     	val m = Nat(4)
	     	val n = Nat(2)
	     	val X = Matrix(m, n, Array[BigDecimal](0.647467353462031, 0.619943027403621, -0.728400772644785, -0.742388619327580, 1.052134449375800, 1.075380899803712, -0.971201030193046, -0.952935307879753))
	     	val y = Vector(m, Array[BigDecimal](1, 0, 1, 0))
	     	val theta = Vector(n, Array[BigDecimal](1, 1))
	     	
	     	LogisticRegression.cost(X, y, theta, 0) must be_==(0.175929351253886)
	     }
	}
	
	"gradient" should {
	     "be [-0.128515946041982620765951783957735; -0.127694362729414210946876358869805]" in {
	     	val m = Nat(4)
	     	val n = Nat(2)
	     	val X = Matrix(m, n, Array[BigDecimal](0.647467353462031, 0.619943027403621, -0.728400772644785, -0.742388619327580, 1.052134449375800, 1.075380899803712, -0.971201030193046, -0.952935307879753))
	     	val y = Vector(m, Array[BigDecimal](1, 0, 1, 0))
	     	val theta = Vector(n, Array[BigDecimal](1, 1))
	     	val expected = Vector(n, Array[BigDecimal](BigDecimal("-0.128515946041982620765951783957735"), BigDecimal("-0.127694362729414210946876358869805")))
	     	
	     	LogisticRegression.gradient(X, y, theta, 0) must be_==(expected)
	     }
	}
}
