package com.digitalbrikes.machine_learning

import com.digitalbrikes.linear_algebra.Nat._
import com.digitalbrikes.linear_algebra.Vector._
import com.digitalbrikes.linear_algebra.Matrix
import com.digitalbrikes.linear_algebra.Matrix._
import com.digitalbrikes.linear_algebra.sigmoid
import com.digitalbrikes.linear_algebra.log

sealed class LogisticRegression(minimize : GradientDescent) extends BaseRegression(minimize) {
	protected
	
	def cost[M  <: Nat, N  <: Nat](X : Matrix[M,N], y : Vector[M], theta : Vector[N], lambda : BigDecimal) : BigDecimal = {
		val scale = X.rowCount
		val sigmoidValue = sigmoid(X * theta)
		val regularization = lambda * biasless(theta).norm()(0,0) / (2 * scale)
		val positive = y.transpose * log(sigmoidValue)
		val negative = (ones(y.size) - y).transpose() * log(ones(sigmoidValue.size) - sigmoidValue)
		(positive + negative)(0, 0) / (-1 * scale) + regularization
	}

	def gradient[M  <: Nat, N  <: Nat] (X : Matrix[M,N], y : Vector[M], theta : Vector[N], lambda : BigDecimal) :Vector[N] = {
		val scale = X.rowCount
		val sigmoidValue = sigmoid(X * theta)
		val regularization = lambda *: biasless(theta)
		((X.transpose * (sigmoidValue - y)) + regularization) / scale
	}
	
	private
	
	def biasless[M  <: Nat, N  <: Nat](X : Matrix[M,N]) : Matrix[M,N] =
		new Matrix[M, N](X.rows, X.columns, Array.tabulate(X.values.length) (index => {
			if (index < X.columns) {
				0
			} else {
				X.values(index)
			}
		}))
}
