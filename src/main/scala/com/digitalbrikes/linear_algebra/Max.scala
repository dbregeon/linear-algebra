package com.digitalbrikes.linear_algebra

import com.digitalbrikes.linear_algebra.Nat._

object max {	
	def apply[M <: Nat, N <: Nat](X : Matrix[M,N]) : Matrix[Succ[Zero], N] =
	  X.foldByColumn(0)((accu, value) => accu.max(value))
}