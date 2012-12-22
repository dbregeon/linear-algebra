package com.digitalbrikes.linear_algebra

import scala.collection.immutable.{Vector => _Vector}
import scala.math.BigDecimal.int2bigDecimal

import com.digitalbrikes.linear_algebra.Nat.Nat
import com.digitalbrikes.linear_algebra.Vector.Vector
import com.digitalbrikes.linear_algebra.Nat.Succ
import com.digitalbrikes.linear_algebra.Nat.Zero

object diag {		
	def apply[N <: Nat](v : Vector[N]) : Matrix[N, N] = {
	  Matrix(v.rows, v.rows, _Vector.tabulate(v.rowCount * v.rowCount)(index => if (index % v.rowCount == index / v.rowCount) v(index % v.rowCount, 0) else 0))
	}
}