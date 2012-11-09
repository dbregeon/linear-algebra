package com.digitalbrikes.linear_algebra

import org.specs2.mutable._
import com.digitalbrikes.linear_algebra.Nat._

class MatrixSpec extends Specification {
	"Matrix " should {
		"have 2 rows" in {
			val m = Matrix(Nat(2), Nat(3), Array[BigDecimal](1, 2, 3, 4, 5, 6))
			
			m.rowCount must be_==(2)
		}
		
		"have 3 columns" in {
			val m = Matrix(Nat(2), Nat(3), Array[BigDecimal](1, 2, 3, 4, 5, 6))
			
			m.columnCount must be_==(3)
		}
	}
	
	"Vector " should {
		"have 2 rows" in {
			val v = Vector(Nat(2), Array[BigDecimal](1, 2))
			
			v.rowCount must be_==(2)
		}
		
		"have 1 column" in {
			val v = Vector(Nat(2), Array[BigDecimal](1, 2))
			
			v.columnCount must be_==(1)
		}
	}

	"Matrix addition" should {
		"add matching cells" in {
			val first = Matrix(Nat(2), Nat(2), Array[BigDecimal](1, 2, 3, 4))
			val second = Matrix(Nat(2), Nat(2), Array[BigDecimal](1, 2, 3, 4))
			
			Matrix(Nat(2), Nat(2), Array[BigDecimal](2, 4, 6, 8)) == first + second
		}
	}
	
	"Matrix subtraction" should {
		"subtract matching cells" in {
			val first = Matrix(Nat(2), Nat(2), Array[BigDecimal](1, 2, 3, 4))
			val second = Matrix(Nat(2), Nat(2), Array[BigDecimal](1, 2, 3, 4))
			
			(first - second) must be_==(Matrix(Nat(2), Nat(2), Array[BigDecimal](0, 0, 0, 0)))
		}
	}
	
	"Matrix multiplication" should {
		"multiply rows into columns" in {
			val first = Matrix(Nat(2), Nat(3), Array[BigDecimal](1, 2, 3, 4, 5 , 6))
			val second = Matrix(Nat(3), Nat(2), Array[BigDecimal](1, 2, 3, 4, 5, 6))
			
			(first * second) must be_==(Matrix(Nat(2), Nat(2), Array[BigDecimal](22, 28, 49, 64)))
		}
	}
	
	"Matrix transposition" should {
		"transpose column and rows" in {
			val m = Matrix(Nat(2), Nat(3), Array[BigDecimal](1, 2, 3, 4, 5 , 6))
			
			m.transpose must be_==(Matrix(Nat(3), Nat(2), Array[BigDecimal](1, 4, 2, 5, 3, 6)))
		}
	}
	
	"Matrix apply" should {
		"return value of cell 0 0" in {
			val m = Matrix(Nat(2), Nat(2), Array[BigDecimal](1, 2, 3, 4))
			
			1 == m(0,0)
		}
		
		"return value of cell 0 1" in {
			val m = Matrix(Nat(2), Nat(2), Array[BigDecimal](1, 2, 3, 4))
			
			2 == m(0,1)
		}
		
		"return value of cell 1 0" in {
			val m = Matrix(Nat(2), Nat(2), Array[BigDecimal](1, 2, 3, 4))
			
			3 == m(1,0)
		}
		
		"return value of cell 1 1" in {
			val m = Matrix(Nat(2), Nat(2), Array[BigDecimal](1, 2, 3, 4))
			
			4 == m(1,1)
		}
	}
	
	"Matrix map" should {
		"substitute the result of the function for its parameter" in {
			val m = Matrix(Nat(2), Nat(3), Array[BigDecimal](1, 2, 3, 4, 5, 6))
			val function = ((value : BigDecimal) => value + 1)
			val expectedResult = Matrix(Nat(2), Nat(3), Array[BigDecimal](2, 3, 4, 5, 6, 7))
			
			m.map(function) must be_==(expectedResult)
		}
	}
	
	"Matrix mapByRow" should {
		"substitute the result of the function for its parameter value" in {
			val m = Matrix(Nat(2), Nat(3), Array[BigDecimal](1, 2, 3, 4, 5, 6))
			val function = ((index : Int, value : BigDecimal) => value + index)
			val expectedResult = Matrix(Nat(2), Nat(3), Array[BigDecimal](1, 3, 5, 4, 6, 8))
			
			m.mapByRow(function) must be_==(expectedResult)
		}
	}
	
	"Matrix mapByColumn" should {
		"substitute the result of the function for its parameter value" in {
			val m = Matrix(Nat(2), Nat(3), Array[BigDecimal](1, 2, 3, 4, 5, 6))
			val function = ((index : Int, value : BigDecimal) => value + index)
			val expectedResult = Matrix(Nat(2), Nat(3), Array[BigDecimal](1, 2, 3, 5, 6, 7))
			
			m.mapByColumn(function) must be_==(expectedResult)
		}
	}
	
	"Matrix foreach" should {
		"substitute the result of the function for its parameter value" in {
			val m = Matrix(Nat(2), Nat(3), Array[BigDecimal](1, 2, 3, 4, 5, 6))
			val function = ((row : Int, column : Int, value : BigDecimal) => value + row + column)
			val expectedResult = Matrix(Nat(2), Nat(3), Array[BigDecimal](1, 3, 5, 5, 7, 9))
			
			m.foreach(function) must be_==(expectedResult)
		}
	}
	
	"Matrix foldByColumn" should {
		"return a single row matrix with the function accumulations" in {
			val m = Matrix(Nat(2), Nat(3), Array[BigDecimal](1, 2, 3, 4, 5, 6))
			val function = ((accu : BigDecimal, value : BigDecimal) => value + accu)
			val initialAccu = 2
			val expectedResult = Matrix(one, Nat(3), Array[BigDecimal](7, 9, 11))
			
			m.foldByColumn(initialAccu)(function) must be_==(expectedResult)
		}
	}
	
	"Matrix fold" should {
		"return the accumulation of function applications" in {
			val m = Matrix(Nat(2), Nat(3), Array[BigDecimal](1, 2, 3, 4, 5, 6))
			val function = ((accu : BigDecimal, value : BigDecimal) => value + accu)
			val initialAccu = 2
			val expectedResult = 23
			
			m.fold(initialAccu)(function) must be_==(expectedResult)
		}
	}
	
	"Matrix column" should {
	  "return the first column of the matrix" in {
	    val m = Matrix(Nat(2), Nat(3), Array[BigDecimal](1, 2, 3, 4, 5, 6))
	    m.column(0) must be_==(Vector(Nat(2), Array[BigDecimal](1, 4)))
	  }
	}
	
	"Matrix row" should {
	  "return the first row of the matrix" in {
	    val m = Matrix(Nat(2), Nat(3), Array[BigDecimal](1, 2, 3, 4, 5, 6))
	    m.row(0) must be_==(Matrix(one, Nat(3), Array[BigDecimal](1, 2, 3)))
	  }
	}
}
