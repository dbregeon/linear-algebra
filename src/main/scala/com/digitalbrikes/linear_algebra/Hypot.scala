package com.digitalbrikes.linear_algebra

import com.digitalbrikes.linear_algebra.Nat._

object hypot {
	def apply(x : BigDecimal, y : BigDecimal) : BigDecimal = {
	  def actual(x : BigDecimal, y : BigDecimal) : BigDecimal = if (x < y) actual(y, x) else x * sqrt(BigDecimal(1) + (y * y)  / (x * x))
	  if (x == BigDecimal(0) && y == BigDecimal(0))
	    BigDecimal(0)
	  else
		actual(abs(x), abs(y))
	}
}