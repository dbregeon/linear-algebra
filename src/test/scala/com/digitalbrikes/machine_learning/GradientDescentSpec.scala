package com.digitalbrikes.machine_learning

import org.specs2.mutable._
import com.digitalbrikes.linear_algebra.Nat
import com.digitalbrikes.linear_algebra.Nat._
import com.digitalbrikes.linear_algebra.Vector
import com.digitalbrikes.linear_algebra.Vector._
import com.digitalbrikes.linear_algebra.Matrix._

class GradientDescentSpec extends Specification {
	def constantGradient[N <: Nat](n : N) : (Vector[N] => Vector[N]) = (any => Vector(n, Array[BigDecimal](1, 1, 1)))
	
	"gradientDescent" should {
		"return the theta minus alpha times the gradient" in {
	     	val n = Nat(3)
	     	val theta = ones(n, one)
			
			GradientDescent(0.1, 1)(constantGradient(n), theta).minimum must be_==(0.9 *: theta)
		}
		
		"return applies the learning rate for the number of iterations" in {
	     	val n = Nat(3)
	     	val theta = ones(n, one)
	     	val expected = Vector(n, Array[BigDecimal](0.9, 0.9, 0.9))
			
			GradientDescent(0.01, 10)(constantGradient(n), theta).minimum must be_==((1 - 10 * 0.01) *: theta)
		}
	}
}
