package com.digitalbrikes.linear_algebra

import com.digitalbrikes.linear_algebra.Nat._

object sqrt {
	def apply(x : BigDecimal) : BigDecimal = {
	  lazy val half = BigDecimal(0.5)
	  def improve(guess : BigDecimal) : BigDecimal = if (guess.compare(BigDecimal(0)) == 0) guess + BigDecimal(10).pow(-32) else half * (guess + x / guess)
	  def isGoodEnough(guess : BigDecimal) : Boolean = abs(guess * guess - x) <= x * BigDecimal(10).pow(-32)
	  def initialValue() : BigDecimal = try { scala.math.sqrt(x.doubleValue) } catch { case _ => half * x }
	  lazy val values : Stream[BigDecimal] = initialValue #:: (values map improve)
	  values.filter(isGoodEnough(_)).head
	}
	
	def apply[M <: Nat, N <: Nat](X : Matrix[M,N]) : Matrix[M,N] = X.map(this.apply)
}