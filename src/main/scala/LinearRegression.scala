import Nat._
import Vector._

object LinearRegression {
	def apply[M  <: Nat, N  <: Nat](X : Matrix[M,N], y : Vector[M], theta0 : Vector[N], lambda : BigDecimal, minimize : GradientDescent) : (Vector[N] => BigDecimal) = {
		val theta = minimize(gradient(X, y, lambda), theta0)
		(x => (x.transpose() * theta)(0, 0))
	}
	
	def cost[M  <: Nat, N  <: Nat](X : Matrix[M,N], y : Vector[M], theta : Vector[N], lambda : BigDecimal) : BigDecimal = {
		val scale = 2 * X.rows
		val difference = X * theta - y
		(square(difference)(0, 0) + lambda * square(biasless(theta))(0,0)) / scale
	}
	
	def gradient[M  <: Nat, N  <: Nat](X : Matrix[M,N], y : Vector[M], lambda : BigDecimal) : (Vector[N] => Vector[N]) = {
		gradient(X, y, _ : Vector[N], lambda)
	}
	
	def gradient[M  <: Nat, N  <: Nat](X : Matrix[M,N], y : Vector[M], theta : Vector[N], lambda : BigDecimal) :Vector[N] = {
		val scale = X.rows
		val difference = X * theta - y
		(X.transpose() * difference + lambda *: biasless(theta)) / scale
	}
	
	def square[M  <: Nat, N  <: Nat](X : Matrix[M,N]) : Matrix[N,N] = {
		X.transpose() * X
	}
	
	def biasless[M  <: Nat, N  <: Nat](X : Matrix[M,N]) : Matrix[M,N] =
		new Matrix[M, N](X.rows, X.columns, Array.tabulate(X.values.length) (index => {
			if (index < X.columns) {
				0
			} else {
				X.values(index)
			}
		}))
}