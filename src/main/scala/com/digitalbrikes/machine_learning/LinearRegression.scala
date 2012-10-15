package com.digitalbrikes.machine_learning

import com.digitalbrikes.linear_algebra.Nat._
import com.digitalbrikes.linear_algebra.Vector._
import com.digitalbrikes.linear_algebra.Matrix
import com.digitalbrikes.linear_algebra.Matrix._

final class LinearRegression(minimize : GradientDescent) extends BaseRegression(minimize) {
	
	protected
	
	/**
	 * The cost in a linear regression is based off a simple difference.
	 */
	def cost[M  <: Nat, N  <: Nat](X : Matrix[M,N], y : Vector[M], lambda : BigDecimal) : Vector[N] => BigDecimal =
		LinearCostFunction(X, y, lambda)
	
	/**
	 * This is the derived function of the cost.
	 */
	def gradient[M  <: Nat, N  <: Nat](X : Matrix[M,N], y : Vector[M], lambda : BigDecimal) :Vector[N] => Vector[N] =
		LinearGradientFunction(X, y, lambda)
}

private object LinearCostFunction {
  def apply[M  <: Nat, N  <: Nat](X : Matrix[M,N], y : Vector[M], lambda : BigDecimal) : Vector[N] => BigDecimal =
	theta => {
		val scale = 2 * X.rowCount
		val difference = X * theta - y
		(difference.norm()(0, 0) + lambda * Biasless(theta).norm()(0,0)) / scale
	}
}

private object LinearGradientFunction {
  def apply[M  <: Nat, N  <: Nat](X : Matrix[M,N], y : Vector[M], lambda : BigDecimal) :Vector[N] => Vector[N] =
		theta =>{
			val scale = X.rowCount
			val difference = X * theta - y
			(X.transpose() * difference + lambda *: Biasless(theta)) / scale
		}
}

object LinearRegression {
  def apply(minimize : GradientDescent) : LinearRegression = new LinearRegression(minimize)
}