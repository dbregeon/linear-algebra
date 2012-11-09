package com.digitalbrikes.linear_algebra

import com.digitalbrikes.linear_algebra.Nat._

object mean {	
	def apply[M <: Nat, N <: Nat](X : Matrix[M,N]) : Matrix[Succ[Zero], N] =
	  X.foldByColumn(0)((accu, value) => accu + value).map(value => value / X.rowCount)
}