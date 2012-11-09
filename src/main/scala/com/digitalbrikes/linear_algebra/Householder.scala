package com.digitalbrikes.linear_algebra

import com.digitalbrikes.linear_algebra.Nat._
import com.digitalbrikes.linear_algebra.Matrix._
import com.digitalbrikes.linear_algebra.Vector._
import scala.math.BigDecimal._

object householder {	
	def apply[N <: Nat](x : Vector[N], columnIndex : Int) : Matrix[N, N] = {
	  val u = if (columnIndex <= x.rowCount) {
		normalize(transform(scale(x.foreach((row, _, value) => if (row < columnIndex) 0 else value)), columnIndex), columnIndex)
	  } else
	    zeros((x.rows, one))
	  identity((x.rows)) - 2 *: (u * u.transpose)
	}
	
	def sign(value : BigDecimal) : BigDecimal = if (value < 0) -1 else 1
	
	def scale[N <: Nat](x : Vector[N]) : Vector[N] = {
	  val s = max(abs(x))
	  if (s.compare(BigDecimal(0)) == 0)
	    x
	  else {
		  x / s
	  }
	}
	
	def normalize[N <: Nat](x : Vector[N], columnIndex : Int) : Vector[N] = {
	  val norm = x.norm
	  if (norm.compare(BigDecimal(0)) == 0)
	    x
	  else {
		  x / norm
	  }
	}
	
	def transform[N <: Nat](x : Vector[N], columnIndex : Int) : Vector[N] =
	  x.mapByColumn((rowIndex, value) => if (rowIndex == columnIndex) value + sign(x(columnIndex,0)) * x.norm else if (rowIndex > columnIndex) value else 0)
}