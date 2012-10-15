package com.digitalbrikes.machine_learning

import org.specs2.mutable._
import com.digitalbrikes.linear_algebra.Nat
import com.digitalbrikes.linear_algebra.Nat._
import com.digitalbrikes.linear_algebra.Vector
import com.digitalbrikes.linear_algebra.Vector._
import com.digitalbrikes.linear_algebra.Matrix
import com.digitalbrikes.linear_algebra.Matrix._

final class TestRegression(minimize: GradientDescent) extends BaseRegression(minimize) {
  def cost[M <: Nat, N <: Nat](X: Matrix[M, N], y: Vector[M], lambda: BigDecimal): Vector[N] => BigDecimal =
    theta => {
      val scale = 2 * X.rowCount
      val difference = X * theta - y
      (difference.norm()(0, 0) + lambda * theta.norm()(0, 0)) / scale
    }

  def gradient[M <: Nat, N <: Nat](X: Matrix[M, N], y: Vector[M], lambda: BigDecimal): Vector[N] => Vector[N] =
    theta => {
      val scale = X.rowCount
      val difference = X * theta - y
      (X.transpose() * difference + lambda *: theta) / scale
    }
}

final class BaseRegressionSpec extends Specification {
  val m = Nat(4)
  val n = Nat(2)
  val X = Matrix.ones((m, n))
  val y = Vector(m, Array[BigDecimal](1, 0, 1, 0))
  val minimize = GradientDescent(1., 1)

  "the training result" should {
    val lambda = 1.
    val result = new TestRegression(minimize).train(X, y, lambda)

    "use zeros as the intial parameters" in {
      result.trainingCurve.head._1 must be_==(zeros(n, one))
    }

    "contains the minimization result" in {
      result.theta must be_==(minimize(new TestRegression(minimize).gradient(X, y, lambda), zeros(X.columns, one)).minimum)
    }

    "uses the model's cost function with no regularization" in {
      result.error must be_==(new TestRegression(minimize).cost(X, y, 0.)(result.theta))
    }
  }

}