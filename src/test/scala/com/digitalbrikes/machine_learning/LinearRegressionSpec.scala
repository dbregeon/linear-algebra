package com.digitalbrikes.machine_learning

import org.specs2.mutable._
import com.digitalbrikes.linear_algebra.Nat
import com.digitalbrikes.linear_algebra.Vector
import com.digitalbrikes.linear_algebra.Matrix

class LinearRegressionSpec extends Specification {
	"biasless" should {
		"replace the first row with zeros" in {
			val m = Nat(4)
	     	val n = Nat(3)
			val input = Matrix(m, n, Array.tabulate(12) (index => 1))
			val expected = Matrix(m, n, Array[BigDecimal](0, 0, 0, 1, 1, 1, 1, 1, 1, 1, 1, 1))
			
			LinearRegression.biasless(input) must be_==(expected)
		}
	}
	
	"cost" should {
	     "be 23789757.625" in {
	     	val m = Nat(4)
	     	val n = Nat(3)
	     	val X = Matrix(m, n, Array[BigDecimal](1, 89, 7921, 1, 72, 5184, 1, 94, 8836, 1, 69, 4761))
	     	val y = Vector(m, Array[BigDecimal](96, 74, 87, 78))
	     	val theta = Vector(n, Array[BigDecimal](1, 1, 1))
	     	
	     	LinearRegression.cost(X, y, theta, 1) must be_==(23789757.625)
	     }
	}
	
	"gradient" should {
	     "be [6673.75; 559226.25; 47584501.25]" in {
	     	val m = Nat(4)
	     	val n = Nat(3)
	     	val X = Matrix(m, n, Array[BigDecimal](1, 89, 7921, 1, 72, 5184, 1, 94, 8836, 1, 69, 4761))
	     	val y = Vector(m, Array[BigDecimal](96, 74, 87, 78))
	     	val theta = Vector(n, Array[BigDecimal](1, 1, 1))
	     	val expected = Vector(n, Array[BigDecimal](6673.75, 559226.25, 47584501.25))
	     	
	     	LinearRegression.gradient(X, y, theta, 1) must be_==(expected)
	     }
	}
}
