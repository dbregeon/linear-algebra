package com.digitalbrikes.machine_learning

import org.specs2.mutable._
import com.digitalbrikes.linear_algebra.Nat
import com.digitalbrikes.linear_algebra.Nat._
import com.digitalbrikes.linear_algebra.Vector
import com.digitalbrikes.linear_algebra.Matrix
import com.digitalbrikes.linear_algebra.Matrix._

class LogisticRegressionSpec extends Specification {
	val m = Nat(4)
	val n = Nat(2)
	val X = Matrix(m, n, Array[BigDecimal](0.647467353462031, 0.619943027403621, -0.728400772644785, -0.742388619327580, 1.052134449375800, 1.075380899803712, -0.971201030193046, -0.952935307879753))
	val y = Vector(m, Array[BigDecimal](1, 0, 1, 0))
	val minimize = GradientDescent(1., 1)
	     	
	val result = new LogisticRegression(minimize).train(X, y, 0.)
	     	
	"cost" should {
	     "be 0.175929351253886" in {	
	     	val (v, cost) = result.trainingCurve()(0)
	     	cost must be_==(0.175929351253886)
	     }
	}
	
	"gradient" should {
	     "be [-0.128515946041982620765951783957735; -0.127694362729414210946876358869805]" in {
	     	val expectedGradient = Vector(n, Array[BigDecimal](BigDecimal("-0.128515946041982620765951783957735"), BigDecimal("-0.127694362729414210946876358869805")))
	     	result.minimizationResult.minimum must be_==(zeros(n, one) - expectedGradient)
	     }
	}
	
}
