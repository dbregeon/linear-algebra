package com.digitalbrikes.machine_learning

import com.digitalbrikes.linear_algebra.Nat._
import com.digitalbrikes.linear_algebra.Vector._

/**
 * An instance of the Minimization result captures both the outcome (minimum) and the path.
 */
final class MinimizationResult[N  <: Nat] private (val minimum : Vector[N], val path : scala.collection.Seq[Vector[N]])

/**
 * alpha is the learning  rate of the gradient descent.
 * iterations is the number of steps in the descent.
 */
final class GradientDescent private (alpha : BigDecimal, iterations : Int) extends Serializable {
	def apply[N  <: Nat](gradient : (Vector[N] => Vector[N]), theta0 : Vector[N]) : MinimizationResult[N] =
		(0 to iterations -1).foldLeft((theta0, Seq.empty[Vector[N]]))((pair , _)  => ((pair._1 - alpha *: gradient(pair._1)), pair._2 :+ pair._1)) match {
		  case (result, history) => MinimizationResult[N](result, history)
		}
}

private object MinimizationResult {
	def apply[N  <: Nat](minimum : Vector[N], path : Seq[Vector[N]]) : MinimizationResult[N] = new MinimizationResult(minimum, path)
}

object GradientDescent {
	def apply(alpha : BigDecimal, iterations : Int) : GradientDescent = new GradientDescent(alpha, iterations)
}
