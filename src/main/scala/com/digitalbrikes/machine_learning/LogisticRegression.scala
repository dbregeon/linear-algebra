package com.digitalbrikes.machine_learning

import com.digitalbrikes.linear_algebra.Nat._
import com.digitalbrikes.linear_algebra.Vector._
import com.digitalbrikes.linear_algebra.Matrix
import com.digitalbrikes.linear_algebra.Matrix._
import com.digitalbrikes.linear_algebra.sigmoid
import com.digitalbrikes.linear_algebra.log

final class LogisticRegression(minimize : GradientDescent) extends BaseRegression(minimize) {
	protected
	
	/**
	 * The cost in logistic regression is based on the sigmoid function.
	 */
	def cost[M  <: Nat, N  <: Nat](X : Matrix[M,N], y : Vector[M], lambda : BigDecimal) : Vector[N] => BigDecimal =
		LogisticCostFunction(X,y,lambda)

	/**
	 * This the derived function of the cost. 
	 */
	def gradient[M  <: Nat, N  <: Nat] (X : Matrix[M,N], y : Vector[M], lambda : BigDecimal) :Vector[N] => Vector[N] = 
		LogisticGradientFunction(X, y, lambda)
}

private object LogisticCostFunction {
  def apply[M  <: Nat, N  <: Nat](X : Matrix[M,N], y : Vector[M], lambda : BigDecimal) : Vector[N] => BigDecimal =
		theta => {
		val scale = X.rowCount
		val sigmoidValue = sigmoid(X * theta)
		val regularization = lambda * Biasless(theta).norm()(0,0) / (2 * scale)
		val positive = y.transpose * log(sigmoidValue)
		val negative = (ones(y.size) - y).transpose() * log(ones(sigmoidValue.size) - sigmoidValue)
		(positive + negative)(0, 0) / (-1 * scale) + regularization
	}
}

private object LogisticGradientFunction {
  def apply[M  <: Nat, N  <: Nat] (X : Matrix[M,N], y : Vector[M], lambda : BigDecimal) :Vector[N] => Vector[N] = 
		theta => {
			val scale = X.rowCount
			val sigmoidValue = sigmoid(X * theta)
			val regularization = lambda *: Biasless(theta)
			val difference = sigmoidValue - y
			((X.transpose * difference) + regularization) / scale
		}
}
