package com.digitalbrikes.machine_learning

import com.digitalbrikes.linear_algebra.Nat._
import com.digitalbrikes.linear_algebra.Vector._

final class ValidationResult[N  <: Nat](trainingResults : Seq[ValidationPoint[N]], minimum : ValidationPoint[N]) {	
	def apply(x : Vector[N]) : BigDecimal = minimum(x)
	
	def lambda() : BigDecimal = minimum.lambda
	
	def error() : BigDecimal = minimum.error
	
	def validationCurve() : Seq[(BigDecimal, BigDecimal, BigDecimal)] =
		trainingResults.map ( (p : ValidationPoint[N]) => (p.lambda, p.trainingError, p.error))
}

final class ValidationPoint[N  <: Nat] private (val lambda : BigDecimal, val training : TrainingResult[N], val error : BigDecimal) extends Ordered[ValidationPoint[N]] {
	def apply(x : Vector[N]) : BigDecimal = training(x)
	
	def trainingError() : BigDecimal = training.error
	
	def compare(that: ValidationPoint[N]): Int = this.error.compare(that.error)
}

private object ValidationPoint {
	def apply[N <: Nat](lambda : BigDecimal, training : TrainingResult[N], error : BigDecimal) : ValidationPoint[N] = new ValidationPoint[N](lambda, training, error)
}

private object ValidationResult {
	def apply[N <: Nat](trainingResults : Seq[ValidationPoint[N]], minimum : ValidationPoint[N]) : ValidationResult[N] = new ValidationResult[N](trainingResults, minimum)
}