package com.digitalbrikes.machine_learning

import com.digitalbrikes.linear_algebra.Nat._
import com.digitalbrikes.linear_algebra.Vector._
import com.digitalbrikes.linear_algebra.Matrix
import com.digitalbrikes.linear_algebra.Matrix._

import scala.collection.immutable.{Vector => _Vector}

/**
 * This is the base class of a regression algorithm. It enables to iterate through a gradient algorithm using
 * characteristic cost function.
 */
abstract class BaseRegression(minimize : GradientDescent) {
    /**
     * Trains based on the features X and expected outcomes y for the given regularization parameters lambdas.
     * It computes ValidationResults.
     */
	final def train[M  <: Nat, N  <: Nat, Mval <: Nat](X : Matrix[M,N], y : Vector[M], Xval : Matrix[Mval,N], yval : Vector[Mval], lambdas : Seq[BigDecimal]) : ValidationResult[N] = {
		val validationResults = lambdas.foldLeft[(Seq[ValidationPoint[N]], ValidationPoint[N])]((Seq.empty, null))((accu, lambda) => {
		  val minimum = accu._2
		  val trainingResult = train(X, y, lambda)
		  val newPoint = ValidationPoint[N](lambda, trainingResult, cost(Xval, yval, 0.)(trainingResult.theta))
		  (accu._1 :+ newPoint, if (null == minimum || newPoint < minimum) newPoint else minimum)
		})
		validationResults match {
		  case (trainingResults, minimum) => ValidationResult[N](trainingResults, minimum)
		}
	}
	
	/**
	 * Trains based on the features X and expected outcomes y for the given regularization parameter lambda.
	 * It computes a single TrainingResult.
	 */
	final def train[M  <: Nat, N  <: Nat](X : Matrix[M,N], y : Vector[M], lambda : BigDecimal) : TrainingResult[N] =
		TrainingResult[N](cost(X, y, 0.), minimize(gradient(X, y, lambda), zeros(X.columns, one)))
		
	protected
	
	/**
	 * The characteristic cost function of this regression algorithm.
	 */
	def cost[M  <: Nat, N  <: Nat](X : Matrix[M,N], y : Vector[M], lambda : BigDecimal) : Vector[N] => BigDecimal
	
	/**
	 * Computes the gradient one step using the given regularization parameter.
	 */
	def gradient[M  <: Nat, N  <: Nat] (X : Matrix[M,N], y : Vector[M], lambda : BigDecimal) : Vector[N] => Vector[N]
}

/**
 * Removes the first row of the matrix and replaces it with. This is to use for regularization.
 */
object Biasless {
  def apply[M  <: Nat, N  <: Nat](X : Matrix[M,N]) : Matrix[M,N] =
		new Matrix[M, N](X.rows, X.columns, _Vector.tabulate(X.rows * X.columns) (index => if (index < X.columns) 0 else X(index / X.columns, index % X.columns)))
}
