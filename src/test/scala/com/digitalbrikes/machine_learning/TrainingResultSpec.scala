package com.digitalbrikes.machine_learning

import scala.collection.immutable.{Vector => _Vector}
import scala.math.BigDecimal.double2bigDecimal
import scala.math.BigDecimal.int2bigDecimal
import scala.util.Random

import org.specs2.mutable.Specification

import com.digitalbrikes.linear_algebra.Matrix.ones
import com.digitalbrikes.linear_algebra.Nat
import com.digitalbrikes.linear_algebra.Nat.Nat
import com.digitalbrikes.linear_algebra.Nat.one
import com.digitalbrikes.linear_algebra.Vector
import com.digitalbrikes.linear_algebra.Vector.Vector

class TrainingResultSpec extends Specification {
  def dummyErrorFor[N <: Nat](theta : Vector[N]) : Vector[N] => BigDecimal = errorFor(theta, 0)
  def errorFor[N <: Nat](theta : Vector[N], value : BigDecimal) : Vector[N] => BigDecimal = v1 => if (v1 == theta) value else 0
  
  def emptySequenceFor[N <: Nat](vector : Vector[N]) : Seq[Vector[N]] = Seq.empty
  
  "trainingResult theta" should {
    "return the minimization result's minimum" in {
      val n = Nat(3)
	  val minimum = Vector(n, _Vector[BigDecimal](0.9, 0.9, 0.9))
	  
	  TrainingResult(dummyErrorFor(minimum), MinimizationResult(minimum, emptySequenceFor(minimum))).theta must be_==(minimum) 
    }
  }

  "trainingResult error" should {
    "return the error of the minimization result's minimum" in {
      val n = Nat(3)
	  val minimum = Vector(n, _Vector[BigDecimal](0.9, 0.9, 0.9))
	  val expectedError = Random.nextDouble
	  
	  TrainingResult(errorFor(minimum, expectedError), MinimizationResult(minimum, emptySequenceFor(minimum))).error must be_==(expectedError)
    }
  }
  
  "trainingResult predict" should {
    "return the result of the prediction for the minimization result's minimum" in {
      val n = Nat(3)
	  val minimum = Vector(n, _Vector[BigDecimal](0.9, 0.9, 0.9))
	  val point = ones(n, one)
	  val expectedPrediction = (point.transpose() * minimum)(0, 0)
	  
	  TrainingResult(dummyErrorFor(minimum), MinimizationResult(minimum, emptySequenceFor(minimum)))(point) must be_==(expectedPrediction)
    }
  }
}
