package com.digitalbrikes.linear_algebra

import com.digitalbrikes.linear_algebra.Nat._
import com.digitalbrikes.linear_algebra.Matrix._

/** 
 * Algorithm 6.5-1 in Golub & Van Loan, Matrix Computations
 * Johns Hopkins University Press
 * Finds an upper bidiagonal matrix B so that A=U*B*V'
 * with U,V orthogonal.
*/
object upperbidiagonal {
	/**
	 * Golub-Kahan bidiagonalisation.
	 */
	def apply[M <: N, N <: Nat](X : Matrix[M,N]) :(Matrix[M, N], Matrix[M, M], Matrix[N, N]) = {
	  (0 until X.columnCount).foldLeft((X, identity(X.rows), identity(X.columns)))((matrices, columnIndex) => bidiagonaliseStep(matrices, columnIndex))
	}
	
	/**
	 * Computes Householder reflectors for a given column and applies them to the previous step results. 
	 */
	private def bidiagonaliseStep[M <: N, N <: Nat](matrices : (Matrix[M, N], Matrix[M, M], Matrix[N, N]), columnIndex : Int) : (Matrix[M, N], Matrix[M, M], Matrix[N, N]) = matrices match {
	  case (b, u, v) => {
		    val Qk = householder(b.column(columnIndex), columnIndex)
		    val newB = Qk * b 
		    val Pk = if (columnIndex < b.rowCount - 2) householder(newB.row(columnIndex).transpose, columnIndex + 1) else identity(newB.columns)
		    (newB * Pk.transpose, u * Qk.transpose,  Pk * v)
		  }
	}
}