package com.digitalbrikes.linear_algebra

import scala.collection.Seq
import scala.collection.immutable.{Vector => _Vector}
import scala.io.Source
import scala.math.BigDecimal.double2bigDecimal
import scala.math.BigDecimal.int2bigDecimal

import com.digitalbrikes.linear_algebra.Nat.Succ
import com.digitalbrikes.linear_algebra.Nat.natToInt

import Nat.Nat
import Nat.Succ
import Nat.Zero
import Nat.one

class Matrix[Rows <: Nat, Columns <: Nat](val rows : Rows, val columns : Columns, private val values : Seq[BigDecimal]) extends Serializable {
	def rowCount : Int = rows.rank
	def columnCount : Int = columns.rank
	
	def size : (Rows, Columns) = (rows, columns)

	def +(m : Matrix[Rows, Columns]) : Matrix[Rows, Columns] = new Matrix[Rows, Columns](rows, columns, _Vector.tabulate(values.length) (index => (values(index) + m.values(index))))
	def -(m : Matrix[Rows, Columns]) : Matrix[Rows, Columns] = new Matrix[Rows, Columns](rows, columns, _Vector.tabulate(values.length) (index => (values(index) - m.values(index))))
	def *:(lambda : BigDecimal) : Matrix[Rows, Columns] = new Matrix[Rows, Columns](rows, columns, _Vector.tabulate(values.length) (index => lambda * values(index)))
	def /(lambda : BigDecimal) : Matrix[Rows, Columns] = new Matrix[Rows, Columns](rows, columns, _Vector.tabulate(values.length) (index => values(index) / lambda))
	def *[OtherColumns <: Nat](m : Matrix[Columns, OtherColumns]) : Matrix[Rows, OtherColumns] = {
		val multiplicationValues : _Vector[BigDecimal] = _Vector.tabulate(rowCount * m.columnCount) (index => {
			val row = index / m.columnCount
			val column = index % m.columnCount
			(0 to columns -1).foldLeft[BigDecimal](0)((accu : BigDecimal, counter : Int) => accu + (apply(row, counter) * m(counter, column)))
		})
		new Matrix[Rows, OtherColumns](rows, m.columns, multiplicationValues)
	}

	def norm() : BigDecimal = sqrt(fold(0)((accu, value) => accu + value * value))
	
	def map(function : (BigDecimal => BigDecimal)) : Matrix[Rows, Columns] = new Matrix[Rows, Columns](rows, columns, _Vector.tabulate(values.length) (index => function(values(index))))
	
	def mapByRow(function : (Int, BigDecimal) => BigDecimal) : Matrix[Rows, Columns] = new Matrix[Rows, Columns](rows, columns, _Vector.tabulate(values.length) (index => function(index % columnCount, values(index))))
	
	def mapByColumn(function : (Int, BigDecimal) => BigDecimal) : Matrix[Rows, Columns] = new Matrix[Rows, Columns](rows, columns, _Vector.tabulate(values.length) (index => function(index / columnCount, values(index))))
	
	def foldByColumn(z : BigDecimal)(function : (BigDecimal, BigDecimal) => BigDecimal) : Matrix[Succ[Zero], Columns] =
	  new Matrix[Succ[Zero], Columns](one, columns, _Vector.tabulate(columnCount)(index => (0 to rowCount -1).foldLeft(z)((accu, count) => function(accu, apply(count, index)))))
	  
	def fold(z : BigDecimal)(function : (BigDecimal, BigDecimal) => BigDecimal) : BigDecimal = values.fold(z)(function)
	  
	def foreach(function : (Int, Int, BigDecimal) => BigDecimal) : Matrix[Rows, Columns] = new Matrix[Rows, Columns](rows, columns, _Vector.tabulate(values.length) (index => function(index / columnCount, index % columnCount, values(index)))) 
	
	def columns(range : IndexedSeq[Int]) : Matrix[Rows, Nat] = {
		val subValues = new Array[BigDecimal](range.size * rowCount)
		val subColumns = Nat(range.size)
		(0 to rowCount -1).foreach(currentRow => {
			range.foreach(column => {
				subValues(currentRow * subColumns + column)= values(currentRow*columnCount + column)
			})
		})
		new Matrix(rows, subColumns, subValues)
	}
	
	def column(column : Int) : Matrix[Rows, Succ[Zero]] = {
		val subValues = new Array[BigDecimal](rowCount)
		
		(0 to rowCount -1).foreach(currentRow => subValues(currentRow)= values(currentRow*columnCount + column))
		
		Vector(rows, subValues) 
	}
	
	def rows(range : IndexedSeq[Int]) : Matrix[Nat, Columns] = {
		val subValues = new Array[BigDecimal](columnCount * range.size)
		val subRows = Nat(range.size)
		range.foreach(currentRow => {
			(0 until columnCount).foreach(column => {
				subValues((currentRow - range.head) * columnCount + column)= values(currentRow*columnCount + column)
			})
		})
		new Matrix(subRows, columns, subValues)
	}
	
	def row(row : Int) : Matrix[Succ[Zero], Columns] = {
		val subValues = new Array[BigDecimal](columnCount)
		
		(0 to columnCount -1).foreach(currentColumn => subValues(currentColumn)= values(columnCount*row + currentColumn))
		
		Matrix(one, columns, subValues) 
	}
	
	def normalize : (Matrix[Rows, Columns], Matrix[Succ[Zero], Columns]) = {
	  val columnMeans = mean(this) 
	  (this - (Matrix.ones(rows, one) * columnMeans), columnMeans)
	}
	
	def transpose() : Matrix[Columns, Rows] = new Matrix[Columns, Rows](columns, rows, _Vector.tabulate(values.length) (index => (values((index % rows) * columns + index / rows))))
	
	def apply(row : Int, column : Int) : BigDecimal = values(row*columnCount + column)
	
	override def equals(that : Any): Boolean = that match {
		case that : Matrix[_,_] => rowCount == that.rowCount && columnCount == that.columnCount && valuesEquals(that.values)
		case _             		=> false
	}
		
	override def toString(): String =
		(0 to rowCount -1).foldLeft("\n")((accu : String, rowCounter : Int) =>
			(0 to columnCount -1).foldLeft(accu + "\t")((innerAccu : String, columnCounter : Int) =>
				innerAccu + "\t" + values(rowCounter * columnCount + columnCounter)) + "\t\n")
		
	private
	def valuesEquals(otherValues: Seq[BigDecimal]): Boolean = {
		(0 to values.length -1).forall(counter => values(counter) == otherValues(counter))
	}
}

object Matrix {	
	def apply[Rows <: Nat, Columns <: Nat](rows : Rows, columns : Columns, values : Seq[BigDecimal]) : Matrix[Rows, Columns] = {
		new Matrix[Rows, Columns](rows, columns, values)
	}
	
	def apply(source : Source) : Matrix[Nat, Nat] = buildFromTuple(loadData(source))
	
	def ones[Rows <: Nat, Columns <: Nat](tuple : (Rows,Columns)) : Matrix[Rows, Columns] = {
		val (rows, columns) = tuple
		apply(rows, columns, _Vector.tabulate(rows.rank * columns.rank) (index => 1))
	}
	
	def zeros[Rows <: Nat, Columns <: Nat](tuple : (Rows,Columns)) : Matrix[Rows, Columns] = {
		val (rows, columns) = tuple
		apply(rows, columns, _Vector.tabulate(rows.rank * columns.rank) (index => 0))
	}
	
	def identity[Dimension <: Nat](dimension : Dimension) : Matrix[Dimension, Dimension] = {
	  apply(dimension, dimension, _Vector.tabulate(dimension.rank * dimension.rank) (index => if (index % dimension.rank == index / dimension.rank) 1 else 0))
	}
	
	private
	
	def buildFromTuple[Rows <: Nat, Columns <: Nat](tuple : (Rows, Columns, Seq[BigDecimal])) : Matrix[Rows, Columns] = {
		val (rows, columns, values) = tuple
		apply(rows, columns, values)
	}
	
	def loadData(source : Source) : (Nat, Nat, Seq[BigDecimal]) = {
		val lines = source.getLines.toIterable
		val rows = lines.size
		val values = lines.flatMap((line) => line.split(",")).map((s) => BigDecimal(s))
		(Nat(rows), Nat(values.size / rows), values.toSeq)
	}

	implicit def doubleToMatrix(value : Double) : Matrix[Succ[Zero], Succ[Zero]] = Matrix(one, one, _Vector.tabulate(1) ((index) => value))	
	implicit def intToMatrix(value : Int) : Matrix[Succ[Zero], Succ[Zero]] = Matrix(one, one, _Vector.tabulate(1) ((index) => value))
	implicit def matrixToBigDecimal(M : Matrix[Succ[Zero], Succ[Zero]]) : BigDecimal = M(0,0)
}

object Vector {
	type Vector[M <: Nat]=  Matrix[M, Succ[Zero]]
	
	def apply[Rows <: Nat](rows : Rows, values : Seq[BigDecimal]) : Vector[Rows] = {
		new Vector[Rows](rows, one, values)
	}
}