package com.digitalbrikes.linear_algebra

import com.digitalbrikes.linear_algebra.Nat._

object abs {
	def apply(x : BigDecimal) : BigDecimal = x.abs
	
	def apply[M <: Nat, N <: Nat](X : Matrix[M,N]) : Matrix[M,N] = X.map(this.apply)
}