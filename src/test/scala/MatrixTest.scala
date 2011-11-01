import org.scalacheck._
import org.scalacheck.Prop._
import Nat._

object MatrixSpecification extends Properties("Matrix") {
	import Arbitrary.arbitrary
	
	val unlimitedBigDecimalGen = for {
		  n <- arbitrary[Double]
	} yield (BigDecimal(n, java.math.MathContext.UNLIMITED))

	val sameDimensionMatrixGenerator = for {
		  n <- Gen.choose(1, 100)
		  m <- Gen.choose(1, 100)
		  values1 <- Gen.containerOfN[Array, BigDecimal](n * m, unlimitedBigDecimalGen)
		  values2 <- Gen.containerOfN[Array, BigDecimal](n * m, unlimitedBigDecimalGen)
	} yield (Matrix(Nat(n), Nat(m), values1), Matrix(Nat(n), Nat(m), values2))
	
	val matrixGenerator = for {
		  n <- Gen.choose(1, 100)
		  m <- Gen.choose(1, 100)
		  values1 <- Gen.containerOfN[Array, BigDecimal](n * m, unlimitedBigDecimalGen)
	} yield (Matrix(Nat(n), Nat(m), values1))
	
	def addMatrices[M <: Nat, N <: Nat](tuple: (Matrix[M, N], Matrix[M, N])) = {
		val (a, b) =  tuple
		(a + b) == (b + a)
	}
	
	def subtractMatrices[M <: Nat, N <: Nat](tuple: (Matrix[M, N], Matrix[M, N])) = {
		val (a, b) =  tuple
		b == (a + b - a)
	}

	property("addition is commutative") = forAll(sameDimensionMatrixGenerator) {tuple  => 
		addMatrices(tuple)
	}
	
	property("subtraction is the inverse of the addition") = forAll(sameDimensionMatrixGenerator)  {tuple =>
	  	subtractMatrices(tuple)
	}
	
	property("scalar multiplication is associative") = forAll(matrixGenerator, unlimitedBigDecimalGen, unlimitedBigDecimalGen) {(m : Matrix[_, _], s1 : BigDecimal, s2 : BigDecimal) => 
		((s1 * s2) *: m) == (s1 *: (s2 *: m))
	}
	
	property("scalar multiplication is associative") = forAll(matrixGenerator, unlimitedBigDecimalGen, unlimitedBigDecimalGen) {(m : Matrix[_, _], s1 : BigDecimal, s2 : BigDecimal) => 
		((s1 * s2) *: m) == (s1 *: (s2 *: m))
	}
	
	property("one is the neutral element of the scalar multiplication") = forAll(matrixGenerator) {(m : Matrix[_, _]) => 
		(1 *: m) == m
	}
}