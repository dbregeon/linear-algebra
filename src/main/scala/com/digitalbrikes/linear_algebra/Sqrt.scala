package com.digitalbrikes.linear_algebra

import com.digitalbrikes.linear_algebra.Nat._

object sqrt {
	def apply(x : BigDecimal) : BigDecimal = scala.math.sqrt(x.doubleValue)
	
	def apply[M <: Nat, N <: Nat](X : Matrix[M,N]) : Matrix[M,N] = X.map(this.apply)
}