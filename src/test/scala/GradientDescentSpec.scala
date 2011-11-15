import org.specs2.mutable._
import Nat._
import Vector._

class GradientDescentSpec extends Specification {
	def constantGradient[N <: Nat](n : N) : (Vector[N] => Vector[N]) = (any => Vector(n, Array[BigDecimal](1, 1, 1)))
	
	"gradientDescent" should {
		"return the theta minus alpha times the gradient" in {
	     	val n = Nat(3)
	     	val theta = Vector(n, Array[BigDecimal](1, 1, 1))
			
			GradientDescent(0.1, 1)(constantGradient(n), theta) must be_==(0.9 *: theta)
		}
		
		"return applies the learning rate for the number of iterations" in {
	     	val n = Nat(3)
	     	val theta = Vector(n, Array[BigDecimal](1, 1, 1))
	     	val expected = Vector(n, Array[BigDecimal](0.9, 0.9, 0.9))
			
			GradientDescent(0.01, 10)(constantGradient(n), theta) must be_==((1 - 10 * 0.01) *: theta)
		}
	}
}