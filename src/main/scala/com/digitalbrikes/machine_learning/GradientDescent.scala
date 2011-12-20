package com.digitalbrikes.machine_learning

import com.digitalbrikes.linear_algebra.Nat._
import com.digitalbrikes.linear_algebra.Vector._

sealed class MinimizationResult[N  <: Nat] private (val minimum : Vector[N], val path : scala.collection.Seq[Vector[N]]) {
}

/**
 * alpha is the learning  rate of the gradient descent.
 * iterations is the number of steps in the descent.
 */
sealed class GradientDescent private (val alpha : BigDecimal, val iterations : Int) extends Serializable {
	def apply[N  <: Nat](gradient : (Vector[N] => Vector[N]), theta0 : Vector[N]) : MinimizationResult[N] = {
		var history : scala.collection.Seq[Vector[N]] = scala.collection.Seq.empty
		var result = theta0;
		for (iteration <- 0 until iterations) {
			history = history :+ result
			result = result - alpha *: gradient(result)
		}
		MinimizationResult[N](result, history)
	}
}

object MinimizationResult {
	def apply[N  <: Nat](minimum : Vector[N], path : scala.collection.Seq[Vector[N]]) : MinimizationResult[N] = new MinimizationResult(minimum, path)
}

object GradientDescent {
	def apply(alpha : BigDecimal, iterations : Int) : GradientDescent = new GradientDescent(alpha, iterations)
}
