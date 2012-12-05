package com.digitalbrikes.machine_learning

import scala.collection.immutable.{Vector => _Vector}
import scala.math.BigDecimal.int2bigDecimal

import org.scalacheck.Arbitrary.arbitrary
import org.scalacheck.Gen
import org.scalacheck.Prop.forAll
import org.scalacheck.Prop.propBoolean
import org.scalacheck.Properties

import com.digitalbrikes.linear_algebra.Nat
import com.digitalbrikes.linear_algebra.Nat.Nat
import com.digitalbrikes.linear_algebra.Nat.natToInt
import com.digitalbrikes.linear_algebra.Vector
import com.digitalbrikes.linear_algebra.Vector.Vector

object GradientDescentSpecification extends Properties("GradientDescent") {
	val moreThanOneGen = for {
		  n <- Gen.choose(2,200)
	} yield (n)
	
	val unlimitedBigDecimalGen = for {
		  n <- arbitrary[Double]
	} yield (BigDecimal(n, java.math.MathContext.UNLIMITED))
	
	val vectorGen = for {
		  n <- Gen.choose(1, 100)
		  values1 <- Gen.listOfN[BigDecimal](n, unlimitedBigDecimalGen)
	} yield (Vector(Nat(n), values1))
	
	def constantGradient[N <: Nat](n : N) : (Vector[N] => Vector[N]) = (any => Vector(n, _Vector.tabulate(n) (index => 1)))
	
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