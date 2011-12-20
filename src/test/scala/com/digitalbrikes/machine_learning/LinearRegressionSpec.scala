package com.digitalbrikes.machine_learning

import org.specs2.mutable._
import com.digitalbrikes.linear_algebra.Nat
import com.digitalbrikes.linear_algebra.Nat._
import com.digitalbrikes.linear_algebra.Vector
import com.digitalbrikes.linear_algebra.Matrix
import com.digitalbrikes.linear_algebra.Matrix._

class LinearRegressionSpec extends Specification {
	"the training result's training curve" should {
		 val m = Nat(4)
	     val n = Nat(3)
	     val X = Matrix(m, n, Array[BigDecimal](1, 89, 7921, 1, 72, 5184, 1, 94, 8836, 1, 69, 4761))
	     val y = Vector(m, Array[BigDecimal](96, 74, 87, 78))
		 val minimize = GradientDescent(1., 1)
	     	
	     val result = new LinearRegression(minimize).train(X, y, 1.)

	     "uses zeros as the intial parameters" in {
	     	result.minimizationResult.path(0) must be_==(zeros(n, one))
	     }
	     	
	     "have an error of 23789757.375 for theta0 with a regularization of 1" in {
	     	val (v, error) = result.trainingCurve()(0)
	     	error must be_==(23789757.375)
	     }
	    
	     "have applied a gradient of [6673.75; 559226.25; 47584501.25] from theta0" in {
	     	val expectedGradient = Vector(n, Array[BigDecimal](6673.75, 559226.25, 47584501.25))

	     	result.minimizationResult.minimum must be_==(zeros(n, one) - expectedGradient)
	     }
	}
}
