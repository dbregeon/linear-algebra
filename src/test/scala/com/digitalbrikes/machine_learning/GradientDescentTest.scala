package com.digitalbrikes.machine_learning

import org.scalacheck._
import org.scalacheck.Prop._
import com.digitalbrikes.linear_algebra.Nat
import com.digitalbrikes.linear_algebra.Nat._
import com.digitalbrikes.linear_algebra.Vector
import com.digitalbrikes.linear_algebra.Vector._

object GradientDescentSpecification extends Properties("GradientDescent") {
	import Arbitrary.arbitrary
	
	val moreThanOneGen = for {
		  n <- Gen.choose(2,200)
	} yield (n)
	
	val unlimitedBigDecimalGen = for {
		  n <- arbitrary[Double]
	} yield (BigDecimal(n, java.math.MathContext.UNLIMITED))
	
	val vectorGen = for {
		  n <- Gen.choose(1, 100)
		  values1 <- Gen.containerOfN[Array, BigDecimal](n, unlimitedBigDecimalGen)
	} yield (Vector(Nat(n), values1))
	
	def constantGradient[N <: Nat](n : N) : (Vector[N] => Vector[N]) = (any => Vector(n, Array.tabulate(n) (index => 1)))
	
	def verifyGradientStep[N <: Nat](theta0 : Vector[N], alpha : BigDecimal, iterations : Int) = {
		val gradient = constantGradient(theta0.rows)
		val previousIteration = GradientDescent(alpha, iterations - 1)(gradient, theta0).minimum
		
		(GradientDescent(alpha, iterations)(gradient, theta0).minimum == (previousIteration - (alpha *: gradient(previousIteration))))
	}
	
	def verifyGradientNoStep[N <: Nat](theta0 : Vector[N], alpha : BigDecimal) = {
		val gradient = constantGradient(theta0.rows)		
		(GradientDescent(alpha, 0)(gradient, theta0).minimum == theta0)
	}
	
	property("next iteration is the application of one step") = forAll(vectorGen, unlimitedBigDecimalGen, moreThanOneGen) {  (theta0 : Vector[Nat], alpha : BigDecimal, iterations : Int) => 
		verifyGradientStep(theta0, alpha, iterations)
	}
	
	property("no iteration no application returns the original theta") = forAll(vectorGen, unlimitedBigDecimalGen) {  (theta0 : Vector[Nat], alpha : BigDecimal) => 
		verifyGradientNoStep(theta0, alpha)
	}
}