package com.digitalbrikes.machine_learning

import org.specs2.mutable._
import com.digitalbrikes.linear_algebra.Nat
import com.digitalbrikes.linear_algebra.Nat._
import com.digitalbrikes.linear_algebra.Vector
import com.digitalbrikes.linear_algebra.Matrix
import com.digitalbrikes.linear_algebra.Matrix._
import scala.util.Random

final class LogisticCostFunctionSpec extends Specification {
    "the logistic cost function" should {
    	val m = Nat(4)
    	val n = Nat(2)
    	val X = Matrix(m, n, Array[BigDecimal](0.647467353462031, 0.619943027403621, -0.728400772644785, -0.742388619327580, 1.052134449375800, 1.075380899803712, -0.971201030193046, -0.952935307879753))
    	val y = Vector(m, Array[BigDecimal](1, 0, 1, 0))
	    val theta = Vector(n, Array[BigDecimal](1, 1))
		
      "compute || X*theta - y || when lambda is 0" in {
        LogisticCostFunction(X, y, 0)(theta) must be_==(0.175929351253886)
      }
      
      "add lambda * Biasless(theta)" in {
        val lambda = 1.5
        
        LogisticCostFunction(X, y, lambda)(theta) must be_==(0.175929351253886 + 0.125 * lambda)
      }
    }
}

final class LogisticGradientFunctionSpec extends Specification {
    
    "the logistic gradient function" should {
	    val m = Nat(4)
    	val n = Nat(2)
    	val X = Matrix(m, n, Array[BigDecimal](0.647467353462031, 0.619943027403621, -0.728400772644785, -0.742388619327580, 1.052134449375800, 1.075380899803712, -0.971201030193046, -0.952935307879753))
    	val y = Vector(m, Array[BigDecimal](1, 0, 1, 0))
	    val theta = Vector(n, Array[BigDecimal](1, 1))
	    
	    "compute X' * (sigmoid(X*theta) -y) when lambda is 0" in {
	      LogisticGradientFunction(X, y, 0)(theta) must be_==(Vector(n, Array[BigDecimal](BigDecimal("-0.128515946041982620765951783957735"), BigDecimal("-0.127694362729414210946876358869805"))))
	    }
	    
	    "add lambda * Biasless(theta)" in {
	      val lambda = 1.5
	      val expectedValue = Vector(n, Array[BigDecimal](BigDecimal("-0.128515946041982620765951783957735"), BigDecimal("-0.127694362729414210946876358869805") + lambda / 4))
	      
	      LogisticGradientFunction(X, y, lambda)(theta) must be_==(expectedValue)
	    }
    }
}