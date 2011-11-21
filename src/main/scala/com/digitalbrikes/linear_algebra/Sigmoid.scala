package com.digitalbrikes.linear_algebra

import com.digitalbrikes.linear_algebra.Nat._

object sigmoid {
	def apply(x : BigDecimal) : BigDecimal = 1 / (1 + scala.math.exp(x.doubleValue))
	
	def apply[M <: Nat, N <: Nat](X : Matrix[M,N]) : Matrix[M,N] = X.map(this.apply)
}