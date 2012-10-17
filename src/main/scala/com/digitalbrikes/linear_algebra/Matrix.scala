package com.digitalbrikes.linear_algebra

import Nat._
import scala.io.Source

class Matrix[Rows <: Nat, Columns <: Nat](val rows : Rows, val columns : Columns, private val values : Array[BigDecimal]) extends Serializable {
	def rowCount : Int = rows.rank
	def columnCount : Int = columns.rank
	
	def size : (Rows, Columns) = (rows, columns)

	def +(m : Matrix[Rows, Columns]) : Matrix[Rows, Columns] = new Matrix[Rows, Columns](rows, columns, Array.tabulate(values.length) (index => (values(index) + m.values(index))))
	def -(m : Matrix[Rows, Columns]) : Matrix[Rows, Columns] = new Matrix[Rows, Columns](rows, columns, Array.tabulate(values.length) (index => (values(index) - m.values(index))))
	def *:(lambda : BigDecimal) : Matrix[Rows, Columns] = new Matrix[Rows, Columns](rows, columns, Array.tabulate(values.length) (index => lambda * values(index)))
	def /(lambda : BigDecimal) : Matrix[Rows, Columns] = new Matrix[Rows, Columns](rows, columns, Array.tabulate(values.length) (index => values(index) / lambda))
	def *[OtherColumns <: Nat](m : Matrix[Columns, OtherColumns]) : Matrix[Rows, OtherColumns] = {
		val multiplicationValues : Array[BigDecimal] = Array.tabulate(rowCount * m.columnCount) (index => {
			val row = index / m.columnCount
			val column = index % m.columnCount
			(0 to columns -1).foldLeft[BigDecimal](0)((accu : BigDecimal, counter : Int) => accu + (apply(row, counter) * m(counter, column)))
		})
		new Matrix[Rows, OtherColumns](rows, m.columns, multiplicationValues)
	}

	def norm() : Matrix[Columns,Columns] = {
		transpose * this
	}
	
	def map(function : (BigDecimal => BigDecimal)) : Matrix[Rows, Columns] = new Matrix[Rows, Columns](rows, columns, Array.tabulate(values.length) (index => function(values(index))))
	
	def columns(range : IndexedSeq[Int]) : Matrix[Rows, Nat] = {
		val subValues = new Array[BigDecimal](range.size * rowCount)
		val subColumns = Nat(range.size)
		(0 to rowCount -1).foreach(currentRow => {
			range.foreach(column => {
				subValues(currentRow * subColumns)= values(currentRow*columnCount + column)
			})
		})
		new Matrix(rows, subColumns, subValues)
	}
	
	def column(column : Int) : Matrix[Rows, Succ[Zero]] = {
		val subValues = new Array[BigDecimal](rowCount)
		
		(0 to rowCount -1).foreach(currentRow => subValues(currentRow)= values(currentRow*columnCount + column))
		
		Vector(rows, subValues) 
	}
	
	def transpose() : Matrix[Columns, Rows] = new Matrix[Columns, Rows](columns, rows, Array.tabulate(values.length) (index => (values((index % rows) * columns + index / rows))))
	
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
	def valuesEquals(otherValues: Array[BigDecimal]): Boolean = {
		(0 to values.length -1).forall(counter => values(counter) == otherValues(counter))
	}
}

object Matrix {	
	def apply[Rows <: Nat, Columns <: Nat](rows : Rows, columns : Columns, values : Array[BigDecimal]) : Matrix[Rows, Columns] = {
		new Matrix[Rows, Columns](rows, columns, values)
	}
	
	def apply(source : Source) : Matrix[Nat, Nat] = buildFromTuple(loadData(source))
	
	def ones[Rows <: Nat, Columns <: Nat](tuple : (Rows,Columns)) : Matrix[Rows, Columns] = {
		val (rows, columns) = tuple
		apply(rows, columns, Array.tabulate(rows.rank * columns.rank) (index => 1))
	}
	
	def zeros[Rows <: Nat, Columns <: Nat](tuple : (Rows,Columns)) : Matrix[Rows, Columns] = {
		val (rows, columns) = tuple
		apply(rows, columns, Array.tabulate(rows.rank * columns.rank) (index => 0))
	}
	
	private
	
	def buildFromTuple[Rows <: Nat, Columns <: Nat](tuple : (Rows, Columns, Array[BigDecimal])) : Matrix[Rows, Columns] = {
		val (rows, columns, values) = tuple
		apply(rows, columns, values)
	}
	
	def loadData(source : Source) : (Nat, Nat, Array[BigDecimal]) = {
		val lines = source.getLines.toIterable
		val rows = lines.size
		val values = lines.flatMap((line) => line.split(",")).map((s) => BigDecimal(s)).toArray
		(Nat(rows), Nat(values.size / rows), values)
	}

	implicit def doubleToMatrix(value : Double) : Matrix[Succ[Zero], Succ[Zero]] = Matrix(one, one, Array.tabulate(1) ((index) => value))	
	implicit def intToMatrix(value : Int) : Matrix[Succ[Zero], Succ[Zero]] = Matrix(one, one, Array.tabulate(1) ((index) => value))
}

object Vector {
	type Vector[M <: Nat]=  Matrix[M, Succ[Zero]]
	
	def apply[Rows <: Nat](rows : Rows, values : Array[BigDecimal]) : Vector[Rows] = {
		new Vector[Rows](rows, one, values)
	}
}