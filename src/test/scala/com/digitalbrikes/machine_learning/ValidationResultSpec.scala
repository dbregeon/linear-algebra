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

final class ValidationPointSpec extends Specification {
	def errorFor[N <: Nat](theta : Vector[N], value : BigDecimal) : Vector[N] => BigDecimal = v1 => if (v1 == theta) value else 0
	def emptySequenceFor[N <: Nat](vector : Vector[N]) : Seq[Vector[N]] = Seq.empty
	
	val n = Nat(3)
	val minimum = Vector(n, _Vector[BigDecimal](0.9, 0.9, 0.9))
	val trainingResult = TrainingResult(errorFor(minimum, Random.nextDouble), MinimizationResult(minimum, emptySequenceFor(minimum)))
  
	"validationPoint trainingError" should {
	  "be the training result's error" in {
	    ValidationPoint(Random.nextDouble, trainingResult, Random.nextDouble).trainingError must be_==(trainingResult.error)
	  }
	}
	
	"validationPoint apply method" should {
	  "be the application of the trainingResult" in {
	    val point = ones(n, one)
	    ValidationPoint(Random.nextDouble, trainingResult, Random.nextDouble)(point) must be_==(trainingResult(point)) 
	  }
	}
	
	"validationPoint comparison" should {
	  "be done on error" in {
	      val error1 = Random.nextDouble
	      val error2 = Random.nextDouble
		  val point1 = ValidationPoint(Random.nextDouble, trainingResult, error1)
		  val point2 = ValidationPoint(Random.nextDouble, trainingResult, error2)
		  
		  point1.compareTo(point2) must be_==(error1.compareTo(error2))
	  }
	}
}

final class ValidationResultSpec extends Specification {
  def errorFor[N <: Nat](theta : Vector[N], value : BigDecimal) : Vector[N] => BigDecimal = v1 => if (v1 == theta) value else 0  
  def emptySequenceFor[N <: Nat](vector : Vector[N]) : Seq[Vector[N]] = Seq.empty
  def emptySequenceFor[N <: Nat](validationPoint : ValidationPoint[N]) : Seq[ValidationPoint[N]] = Seq.empty
  
  val n = Nat(3)
  val minimumTheta = Vector(n, _Vector[BigDecimal](0.9, 0.9, 0.9))
  val trainingResult = TrainingResult(errorFor(minimumTheta, Random.nextDouble), MinimizationResult(minimumTheta, emptySequenceFor(minimumTheta)))
  val minimum = ValidationPoint(Random.nextDouble, trainingResult, Random.nextDouble)
  
  "validationResult lambda" should {
    "return the minimum point's lambda" in {
	  ValidationResult(emptySequenceFor(minimum), minimum).lambda must be_==(minimum.lambda)
    }
  }
  
  "validationResult error" should {
    "return the minimum point's error" in {
	  ValidationResult(emptySequenceFor(minimum), minimum).lambda must be_==(minimum.lambda)
    }
  }
  
  "validationResult apply method" should {
    "be the application of the minimum point" in {
      val point = ones(n, one)

	  ValidationResult(emptySequenceFor(minimum), minimum)(point) must be_==(minimum(point))
    }
  }
}
