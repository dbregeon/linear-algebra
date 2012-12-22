package com.digitalbrikes.linear_algebra

object planerot {	
	def apply(x : BigDecimal, y : BigDecimal) : (BigDecimal, BigDecimal, BigDecimal) = {
	  val (a, b) = (abs(x), abs(y))
	  val r = hypot(x, y)
	  if (r == BigDecimal(0))
	    (0, 0, 0)
	  else
		(x / r, y / r, r)
	}
}