import org.specs2.mutable._

class MatrixSpec extends Specification {
	"Matrix " should {
		"have 2 rows" in {
			var m = Matrix(Nat(2), Nat(3), Array[BigDecimal](1, 2, 3, 4, 5, 6))
			
			m.rows must be_==(2)
		}
		
		"have 3 columns" in {
			var m = Matrix(Nat(2), Nat(3), Array[BigDecimal](1, 2, 3, 4, 5, 6))
			
			m.columns must be_==(3)
		}
	}
	
	"Vector " should {
		"have 2 rows" in {
			var v = Vector(Nat(2), Array[BigDecimal](1, 2))
			
			v.rows must be_==(2)
		}
		
		"have 1 column" in {
			var v = Vector(Nat(2), Array[BigDecimal](1, 2))
			
			v.columns must be_==(1)
		}
	}

	"Matrix addition" should {
		"add matching cells" in {
			var first = Matrix(Nat(2), Nat(2), Array[BigDecimal](1, 2, 3, 4))
			var second = Matrix(Nat(2), Nat(2), Array[BigDecimal](1, 2, 3, 4))
			
			Matrix(Nat(2), Nat(2), Array[BigDecimal](2, 4, 6, 8)) == first + second
		}
	}
	
	"Matrix subtraction" should {
		"subtract matching cells" in {
			var first = Matrix(Nat(2), Nat(2), Array[BigDecimal](1, 2, 3, 4))
			var second = Matrix(Nat(2), Nat(2), Array[BigDecimal](1, 2, 3, 4))
			
			(first - second) must be_==(Matrix(Nat(2), Nat(2), Array[BigDecimal](0, 0, 0, 0)))
		}
	}
	
	"Matrix multiplication" should {
		"multiply rows into columns" in {
			var first = Matrix(Nat(2), Nat(3), Array[BigDecimal](1, 2, 3, 4, 5 , 6))
			var second = Matrix(Nat(3), Nat(2), Array[BigDecimal](1, 2, 3, 4, 5, 6))
			
			(first * second) must be_==(Matrix(Nat(2), Nat(2), Array[BigDecimal](22, 28, 49, 64)))
		}
	}
	
	"Matrix transposition" should {
		"transpose column and rows" in {
			var m = Matrix(Nat(2), Nat(3), Array[BigDecimal](1, 2, 3, 4, 5 , 6))
			
			m.transpose must be_==(Matrix(Nat(3), Nat(2), Array[BigDecimal](1, 4, 2, 5, 3, 6)))
		}
	}
	
	"Matrix apply" should {
		"return value of cell 0 0" in {
			var m = Matrix(Nat(2), Nat(2), Array[BigDecimal](1, 2, 3, 4))
			
			1 == m(0,0)
		}
		
		"return value of cell 0 1" in {
			var m = Matrix(Nat(2), Nat(2), Array[BigDecimal](1, 2, 3, 4))
			
			2 == m(0,1)
		}
		
		"return value of cell 1 0" in {
			var m = Matrix(Nat(2), Nat(2), Array[BigDecimal](1, 2, 3, 4))
			
			3 == m(1,0)
		}
		
		"return value of cell 1 1" in {
			var m = Matrix(Nat(2), Nat(2), Array[BigDecimal](1, 2, 3, 4))
			
			4 == m(1,1)
		}
	}
}
