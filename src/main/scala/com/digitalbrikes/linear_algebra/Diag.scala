package com.digitalbrikes.linear_algebra

import com.digitalbrikes.linear_algebra.Nat._

object diag {	
	def apply[N <: Nat](X : Matrix[Succ[Zero], N]) : Matrix[N, N] = {
	  Matrix(X.columns, X.columns, Array.tabulate(X.columnCount * X.columnCount)(index => if (index % X.columnCount == index / X.columnCount) X(0, index / X.columnCount) else 0))
	}
}