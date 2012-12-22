package com.digitalbrikes.linear_algebra

import scala.collection.immutable.{Vector => _Vector}
import scala.math.BigDecimal.int2bigDecimal
import org.specs2.mutable.Specification
import com.digitalbrikes.linear_algebra.Nat.one
import com.digitalbrikes.linear_algebra.Nat.Succ
import com.digitalbrikes.linear_algebra.Nat.Nat

class SVDSpec extends Specification {
	"svd " should {
		"compute the singular values of [1 2; 3 4]" in {
			val m = Matrix(Nat*(2), Nat*(2), _Vector(1, 2, 3, 4))
			val expectedSvd = diag(Vector(Nat*(2), _Vector(BigDecimal("5.464985704219042650451188493284222"), BigDecimal("0.3659661906262578204229643842613920"
))))
			svd(m) must be_==(expectedSvd)
		}
		
		"compute the singular values of the 1..10 diagonal and 11..19 super diagonal matrix" in {
		  val m = Matrix(Nat*(10), Nat*(10), _Vector(1, 11, 0, 0, 0, 0, 0, 0, 0, 0,
				  									 0, 2, 12, 0, 0, 0, 0, 0, 0, 0,
				  									 0, 0, 3, 13, 0, 0, 0, 0, 0, 0,
				  									 0, 0, 0, 4, 14, 0, 0, 0, 0, 0,
				  									 0, 0, 0, 0, 5, 15, 0, 0, 0 ,0,
				  									 0, 0, 0, 0, 0, 6, 16, 0, 0, 0,
				  									 0, 0, 0, 0, 0, 0, 7, 17, 0, 0,
				  									 0, 0, 0, 0, 0, 0, 0, 8, 18, 0,
				  									 0, 0, 0, 0, 0, 0, 0, 0, 9, 19,
				  									 0, 0, 0, 0, 0, 0, 0, 0, 0, 10))
		  val expectedSvd = diag(Vector(Nat*(10), _Vector(BigDecimal("25.42179965736982193713903314233158"), BigDecimal("21.67454376320876571190625160392626"), BigDecimal("18.83560304332330109549299983198791"), BigDecimal("16.52457526199022754650577451538148"), BigDecimal("14.60213510606496529212917915091193"), BigDecimal("13.00147009753728844199517434790693"), BigDecimal("11.67129853621059329717334280128261"), BigDecimal("10.57443854776729406572680590767882"), BigDecimal("9.788063687939952235928212210495226"), BigDecimal("0.00009226029434232605864697773963289320"))))
		  svd(m) must be_==(expectedSvd)
		}
		
		"compute the singular values of a 2x3 matrix" in {
		    val rows = Nat*(2)
		    val columns = Nat.successor(rows)
		  	val m = Matrix(rows, columns, _Vector(BigDecimal("2.5180e-01"), BigDecimal("7.0645e-01"), BigDecimal("6.9026e-03"), BigDecimal("3.5878e-01"), BigDecimal("5.6886e-04"), BigDecimal("8.9756e-01")))
			val expectedSvd = Matrix(rows, columns, _Vector(BigDecimal("0.9788222282088936221822422477087632"), 0, 0, 0, BigDecimal("0.7340074247258239925313874131887494"), 0))
			
			svd(m.transpose).transpose must be_==(expectedSvd)
		}
		
		"compute the singular values of a 3x2 matrix" in {
			val columns = Nat*(2)
			val rows = Nat.successor(columns)
		  	val m = Matrix(rows, columns, _Vector(BigDecimal("2.5180e-01"), BigDecimal("3.5878e-01"), BigDecimal("7.0645e-01"), BigDecimal("5.6886e-04"), BigDecimal("6.9026e-03"), BigDecimal("8.9756e-01")))
			val expectedSvd = Matrix(rows, columns, _Vector(BigDecimal("0.97882"), 0, 0, BigDecimal("0.73401"), 0, 0)) 
//			println("SVD for 3x2 = " + svd(m))
			svd(m) must be_==(expectedSvd)
		}
	}
}