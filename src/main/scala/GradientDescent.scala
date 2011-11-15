import Nat._
import Vector._

/**
 * alpha is the learning  rate of the gradient descent.
 * iterations is the number of steps in the descent.
 */
class GradientDescent(val alpha : BigDecimal, val iterations : Int) extends Serializable {
	def apply[N  <: Nat](gradient : (Vector[N] => Vector[N]), theta0 : Vector[N]) : Vector[N] = {
		var result = theta0;
		for (iteration <- 0 until iterations) {
			result = result - alpha *: gradient(result)
		}
		result
	}
}

object GradientDescent {
	def apply(alpha : BigDecimal, iterations : Int) : GradientDescent = new GradientDescent(alpha, iterations)
}