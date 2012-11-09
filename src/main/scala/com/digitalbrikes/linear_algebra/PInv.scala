package com.digitalbrikes.linear_algebra

import com.digitalbrikes.linear_algebra.Nat._

object pinv {	
	def apply[M <: N, N <: Nat](X : Matrix[M,N]) : Matrix[M, N] = {
	  svd(X)
	}
}