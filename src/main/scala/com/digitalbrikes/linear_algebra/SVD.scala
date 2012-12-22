package com.digitalbrikes.linear_algebra

import scala.collection.immutable.Stream.consWrapper
import scala.collection.immutable.{ Vector => _Vector }
import scala.math.BigDecimal.int2bigDecimal

import com.digitalbrikes.linear_algebra.Nat.Nat
import com.digitalbrikes.linear_algebra.Nat.Succ
import com.digitalbrikes.linear_algebra.Vector.Vector
import scala.math.min

object svd {
  def apply[M <: N, N <: Succ[Nat]](X: Matrix[M, N]): Matrix[M, N] = diagonalise(upperbidiagonal(X)._1)

  /**
   * Demmel Kahan diagonalisation of a bidiagonal matrix using givens rotations.
   */
  private def diagonalise[M <: N, N <: Succ[Nat]](X: Matrix[M, N]): Matrix[M, N] = {
    val initialValue = diagonalAndSuperDiagonal(X)
    def isGoodEnough[P <: Nat](guess: (Vector[Succ[P]], Vector[P])): Boolean = {
      guess._2.norm <= guess._1.norm * BigDecimal(10).pow(-32)
    }
    def valueStream[P <: Nat](initial : (Vector[Succ[P]], Vector[P])) = {
    	lazy val values: Stream[(Vector[Succ[Nat]], Vector[Nat])] = initialValue #:: (values map diagonaliseStep)
    	values
    }
    val (svd, _) = valueStream(initialValue).filter(isGoodEnough(_)).head
    Matrix(X.rows, X.columns, _Vector.tabulate(X.columnCount * X.rowCount)(index => if (index % X.columnCount == index / X.columnCount && index / X.columnCount < svd.rowCount) svd(index / X.columnCount, 0).abs else 0))
  }

  private def diagonaliseStep[N <: Nat](vectors: (Vector[Succ[N]], Vector[N])): (Vector[Succ[N]], Vector[N]) = {
    val (d, e) = vectors
    val initialState: (BigDecimal, BigDecimal, BigDecimal, _Vector[BigDecimal], _Vector[BigDecimal]) = (BigDecimal(1), BigDecimal(1), BigDecimal(0), _Vector.empty, _Vector.empty)
    val (currentC, previousC, previousS, newD, newE) = (0 to e.rowCount - 1).foldLeft(initialState)((state, row) => {
      val (currentC, previousC, previousS, inProgressD, inProgressE) = state
      val (c, s, r) = planerot(currentC * d(row, 0), e(row, 0))
      val toBeReturnedE = if (0 == row) inProgressE else inProgressE :+ (r * previousS)
      val (toBeReturnedPreviousC, toBeReturnedPreviousS, dForRow) = planerot(previousC * r, d(row + 1, 0) * s)
      (c, toBeReturnedPreviousC, toBeReturnedPreviousS, inProgressD :+ dForRow, toBeReturnedE)
    })
    val h = currentC * d(0, d.rowCount - 1)
    (Vector(d.rows, newD :+ (h * previousC)), Vector(e.rows, newE :+ (h * previousS)))
  }

  /**
   * Extracts the diagonal and super diagonal of a matrix.
   */
  private def diagonalAndSuperDiagonal[M <: N, N <: Succ[Nat]](X: Matrix[M, N]) = {
    val superdiagonal = Nat(scala.math.min(X.columnCount, X.rowCount) - 1);
    val diagonal = Nat.successor(superdiagonal);
    val d = Vector(diagonal, _Vector.tabulate(diagonal.rank)(index => X(index, index)))
    val e = Vector(superdiagonal, _Vector.tabulate(superdiagonal.rank) (index => X(index, index + 1)))
    (d, e)
  }
}