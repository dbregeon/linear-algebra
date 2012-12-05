package com.digitalbrikes.linear_algebra

import com.digitalbrikes.linear_algebra.Nat._
import com.digitalbrikes.linear_algebra.Matrix._
import com.digitalbrikes.linear_algebra.Vector._
import scala.collection.immutable.{Vector => _Vector}

object svd {	
	def apply[M <: N, N <: Nat](X : Matrix[M,Succ[N]]) : Matrix[M, Succ[N]] = diagonalise(bidiagnoliase(X))
	
	/**
	 * Demmel Kahan diagonalisation of a bidiagonal matrix using givens rotations.
	 */
	private def diagonalise[M <: N, N <: Nat](X : Matrix[M,Succ[N]]) : Matrix[M, Succ[N]] = {
	  val initialValue = diagonalAndSuperDiagonal(X)
	  def isGoodEnough(guess : (Vector[Succ[N]], Vector[N])) : Boolean = guess._2.norm <= guess._1.norm * BigDecimal(10).pow(-32)
	  lazy val values : Stream[(Vector[Succ[N]], Vector[N])] = initialValue #:: (values map diagonaliseStep)
	  val (svd, _) = values.filter(isGoodEnough(_)).head
	  Matrix(X.rows, X.columns, _Vector.tabulate(X.columnCount * X.columnCount)(index => if (index % X.columnCount == index / X.columnCount && index < svd.rowCount) svd(0, index / X.columnCount) else 0))
	}
	
	private def diagonaliseStep[N <: Nat](vectors : (Vector[Succ[N]], Vector[N])) : (Vector[Succ[N]], Vector[N]) = {
	  val (d, e) = vectors
	  // c_old = 1
	  // c = 1
	  // for i in 1.. n-1
	  //	(c,s,r) = rot(cdi, ei)
	  //	if (i != 1) then e(i-1) = r * s_old
	  //	(c_old, s_old, di) = rot(c_old * r, d(i+1) * s)
	  // h = c * d(n)
	  // e(n-1) = h * s_old
	  // d(n) = h * c_old
	  
	  (d, e)
	}
	
	/**
	 * Extracts the diagonal and super diagonal of a matrix.
	 */
	private def diagonalAndSuperDiagonal[M <: Nat, N <: Nat](X : Matrix[M,Succ[N]]) : (Vector[Succ[N]], Vector[N]) = {
	  val d = Vector(X.columns, Array.tabulate(X.rowCount) (index => X(index, index)))
	  val e = Vector(X.columns.predecessor, Array.tabulate(X.rowCount - 1) (index => X(index, index + 1)))
	  (d, e)
	}
	
	/**
	 * Golub-Kahan bidiagonalisation.
	 */
	private def bidiagnoliase[M <: N, N <: Nat](X : Matrix[M,Succ[N]]) : Matrix[M, Succ[N]] = {
	  val initialState = (X, identity(X.rows), identity(X.columns))
	  (0 to X.columnCount).foldLeft(initialState)((matrices, columnIndex) => bidiagonaliseStep(matrices, columnIndex))._1
	}
	
	/**
	 * Computes Householder reflectors for a given column and applies them to the previous step results. 
	 */
	private def bidiagonaliseStep[M <: Nat, N <: Nat](matrices : (Matrix[M, N], Matrix[M, M], Matrix[N, N]), columnIndex : Int) : (Matrix[M, N], Matrix[M, M], Matrix[N, N]) = matrices match {
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