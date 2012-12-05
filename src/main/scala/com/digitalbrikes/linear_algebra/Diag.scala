package com.digitalbrikes.linear_algebra

import scala.collection.immutable.{Vector => _Vector}
import scala.math.BigDecimal.int2bigDecimal

import com.digitalbrikes.linear_algebra.Nat.Nat
import com.digitalbrikes.linear_algebra.Nat.Succ
import com.digitalbrikes.linear_algebra.Nat.Zero

object diag {	
	def apply[N <: Nat](X : Matrix[Succ[Zero], N]) : Matrix[N, N] = {
	  Matrix(X.columns, X.columns, _Vector.tabulate(X.columnCount * X.columnCount)(index => if (index % X.columnCount == index / X.columnCount) X(0, index / X.columnCount) else 0))
	}
}