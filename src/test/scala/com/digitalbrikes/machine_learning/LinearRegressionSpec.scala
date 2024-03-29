package com.digitalbrikes.machine_learning

import scala.collection.immutable.{Vector => _Vector}
import scala.math.BigDecimal.double2bigDecimal
import scala.math.BigDecimal.int2bigDecimal

import org.specs2.mutable.Specification

import com.digitalbrikes.linear_algebra.Matrix
import com.digitalbrikes.linear_algebra.Nat
import com.digitalbrikes.linear_algebra.Vector

final class LinearCostFunctionSpec extends Specification {
    "the linear cost function" should {
    	val m = Nat(4)
	    val n = Nat(3)
	    val X = Matrix(m, n, _Vector[BigDecimal](1, 89, 7921, 1, 72, 5184, 1, 94, 8836, 1, 69, 4761))
	    val y = Vector(m, _Vector[BigDecimal](96, 74, 87, 78))
	    val theta = Vector(n, _Vector[BigDecimal](1, 1, 1))
		
      "compute || X*theta - y || when lambda is 0" in {
        LinearCostFunction(X, y, 0)(theta) must be_==(23789757.375)
      }
      
      "add lambda * Biasless(theta)" in {
        val lambda = 1.5
        
        LinearCostFunction(X, y, lambda)(theta) must be_==(23789757.375 + 0.25 * lambda)
      }
    }
}

final class LinearGradientFunctionSpec extends Specification {
    
    "the linear gradient function" should {
	    val m = Nat(4)
	    val n = Nat(3)
	    val X = Matrix(m, n, _Vector[BigDecimal](1, 89, 7921, 1, 72, 5184, 1, 94, 8836, 1, 69, 4761))
	    val y = Vector(m, _Vector[BigDecimal](96, 74, 87, 78))
	    val theta = Vector(n, _Vector[BigDecimal](1, 1, 1))
	    
	    "compute X' * (X*theta -y) when lambda is 0" in {
	      LinearGradientFunction(X, y, 0)(theta) must be_==(Vector(n, Array[BigDecimal]( 6673.75, 559226, 47584501)))
	    }
	    
	    "add lambda * Biasless(theta)" in {
	      val lambda = 1.5
	      val expectedValue = Vector(n, _Vector[BigDecimal](6673.75, 559226.0 + lambda / 4, 47584501.0 + lambda / 4))
	      
	      LinearGradientFunction(X, y, lambda)(theta) must be_==(expectedValue)
	    }
    }
}
