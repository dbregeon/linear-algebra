package steps

import cucumber.runtime.{EN, ScalaDsl}
import org.scalatest.matchers.ShouldMatchers
import com.digitalbrikes.linear_algebra.Matrix
import com.digitalbrikes.linear_algebra.Matrix._
import com.digitalbrikes.linear_algebra.Nat
import com.digitalbrikes.linear_algebra.Nat._
import com.digitalbrikes.machine_learning.LinearRegression
import com.digitalbrikes.machine_learning.GradientDescent
import com.digitalbrikes.machine_learning.TrainingResult
import com.digitalbrikes.linear_algebra.Vector
import com.digitalbrikes.linear_algebra.Vector._

object LinearRegressionsteps extends ScalaDsl with EN with ShouldMatchers {
  var trainingResult : Option[TrainingResult[_ <: Nat]] = None
    
  When("""^I apply the linear regression$""") {
      val matrices = MachineLearningDataSteps.matrices.get.values
	  trainingResult = Some(LinearRegression(GradientDescent(0.01, 10000)).train(matrices._1, matrices._2, 1.))
  }
  
  Then("""^I should get the right theta for the simple housing data result$""") {
    trainingResult.get.theta should be(Vector(Nat(2), Array[BigDecimal](BigDecimal("-3.889014334580242475827532682811039"), BigDecimal("1.192204391831954466856300264880725"))))
  }
  
  Then("""^I should get the right theta for the complex housing data result$""") {
    trainingResult.get.theta should be(Vector(Nat(3), Array[BigDecimal](BigDecimal("340412.659574"), BigDecimal("110631.050279"), BigDecimal("-6649.474271"))))
  }
}
