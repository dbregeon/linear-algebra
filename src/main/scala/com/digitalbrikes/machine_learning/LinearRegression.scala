package com.digitalbrikes.machine_learning

import com.digitalbrikes.linear_algebra.Nat._
import com.digitalbrikes.linear_algebra.Vector._
import com.digitalbrikes.linear_algebra.Matrix
import com.digitalbrikes.linear_algebra.Matrix._

class LinearRegression(val minimize : GradientDescent) extends BaseRegression(minimize) {
	
	protected
	
	def cost[M  <: Nat, N  <: Nat](X : Matrix[M,N], y : Vector[M], theta : Vector[N], lambda : BigDecimal) : BigDecimal = {
		val scale = 2 * X.rowCount
		val difference = X * theta - y
		(difference.norm()(0, 0) + lambda * biasless(theta).norm()(0,0)) / scale
	}
	
	def gradient[M  <: Nat, N  <: Nat](X : Matrix[M,N], y : Vector[M], theta : Vector[N], lambda : BigDecimal) :Vector[N] = {
		val scale = X.rowCount
		val difference = X * theta - y
		(X.transpose() * difference + lambda *: biasless(theta)) / scale
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