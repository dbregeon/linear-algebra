package com.digitalbrikes.machine_learning

import com.digitalbrikes.linear_algebra.Nat._
import com.digitalbrikes.linear_algebra.Vector._
import com.digitalbrikes.linear_algebra.Matrix

/**
 * Instances of TrainingResult represent the outcome of a training run for a given error and prediction function.
 */
final class TrainingResult[N  <: Nat] private (error : (Vector[N] => BigDecimal) , minimizationResult : MinimizationResult[N]) {
	/**
	 * Makes a prediction using the theta of the training and the predict funtion of the model.
	 */
	def apply(x : Vector[N]) : BigDecimal = (x.transpose() * theta)(0, 0)
	
	/**
	 * Computes the training error.
	 */
	def error() : BigDecimal = error(theta)
	
	/**
	 * The vector that minimized the error during the training.
	 */
	def theta() : Vector[N] = minimizationResult.minimum
		
	def trainingCurve() : Seq[(Vector[N], BigDecimal)] = minimizationResult.path.map(step => (step, error(step)))
}

private object TrainingResult {
	def apply[N  <: Nat](error : (Vector[N] => BigDecimal) , minimizationResult : MinimizationResult[N]) = new TrainingResult[N](error, minimizationResult)
}