import org.scalacheck._
import org.scalacheck.Prop._

object BigDecimalSpecification extends Properties("BigDecimal") {
	import Arbitrary.arbitrary

	val myGen = for {
		  n <- arbitrary[Double]
	} yield (BigDecimal(n, java.math.MathContext.UNLIMITED))

	property("addition is commutative") = forAll(myGen, myGen) {(a: BigDecimal, b: BigDecimal) => 
		a + b == b + a
	}
	
	property("subtraction is the inverse of addition") = forAll(myGen, myGen) {(a: BigDecimal, b: BigDecimal) => 
    	b == a + b -a
	}
	
	property("multiplication is commutative") = forAll(myGen, myGen) {(a: BigDecimal, b: BigDecimal) => 
		a * b == b * a
	}
	
	property("division is the inverse of multiplication") = forAll(myGen, myGen) {(a: BigDecimal, b: BigDecimal) => 
		(a != BigDecimal(0)) ==> (b == a * b / a)
	}
}