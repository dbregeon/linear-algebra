package com.digitalbrikes.linear_algebra

import com.digitalbrikes.linear_algebra.Nat._

object std {	
	def apply[M <: Nat, N <: Nat](X : Matrix[M,N]) : Matrix[Succ[Zero], N] = {
	  val averages = mean(X)
	  sqrt(X.mapByRow((columnIndex, value) => {
		  val diff = (value - averages(0, columnIndex))
		  diff * diff
	  }).foldByColumn(0)((accu, value) => accu + value)).map(value => value / (X.rowCount - 1))
	}
}