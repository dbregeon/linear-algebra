package com.digitalbrikes.linear_algebra

import com.digitalbrikes.linear_algebra.Nat._
import com.digitalbrikes.linear_algebra.Matrix._
import com.digitalbrikes.linear_algebra.Vector._

object svd {	
	def apply[M <: N, N <: Nat](X : Matrix[M,N]) : Matrix[M, N] = {
	  (0 to X.columnCount).foldLeft((X, identity(X.rows), identity(X.columns)))((matrices, columnIndex) => bidiagonalise(matrices, columnIndex))._1
	}
	
	def bidiagonalise[M <: Nat, N <: Nat](matrices : (Matrix[M, N], Matrix[M, M], Matrix[N, N]), columnIndex : Int) : (Matrix[M, N], Matrix[M, M], Matrix[N, N]) = matrices match {
	  case (b, u, v) => {
		    val Qk = householder(b.column(columnIndex), columnIndex)
		    val Pk = householder(b.transpose.column(columnIndex), columnIndex + 1)
		    (Qk * b * Pk.transpose, u * Qk.transpose , Pk * v)
		  }
	}
	
//	def scale [M <: Nat, N <: Nat](X : Matrix[M,N], columnIndex : Int, scale : BigDecimal) : (Matrix[M, N], BigDecimal) = {
//	  var squares : BigDecimal = 0
//	  (X.foreach((row, column, value) => if (column == columnIndex && row < columnIndex) {
//	   val scaledValue = value / scale
//	   squares += scaledValue * scaledValue
//	   scaledValue
//	  } else value), squares)
//	}
//	
//	def calcScale [M <: N, N <: Nat](X : Matrix[M,N], columnIndex : Int) : BigDecimal =
//	  (columnIndex to X.rowCount).foldLeft[BigDecimal](0)((scale, rowIndex) => scale + X(rowIndex, columnIndex).abs)
}