package com.digitalbrikes.linear_algebra

import Nat._

class Matrix[Rows <: Nat, Columns <: Nat](val rows : Rows, val columns : Columns, val values : Array[BigDecimal]) extends Serializable {
	def rowCount : Int = rows.rank
	def columnCount : Int = columns.rank
	
	def size : (Rows, Columns) = (rows, columns)

	def +(m : Matrix[Rows, Columns]) : Matrix[Rows, Columns] = new Matrix[Rows, Columns](rows, columns, Array.tabulate(values.length) (index => (values(index) + m.values(index))))
	def -(m : Matrix[Rows, Columns]) : Matrix[Rows, Columns] = new Matrix[Rows, Columns](rows, columns, Array.tabulate(values.length) (index => (values(index) - m.values(index))))
	def *:(lambda : BigDecimal) : Matrix[Rows, Columns] = new Matrix[Rows, Columns](rows, columns, Array.tabulate(values.length) (index => lambda * values(index)))
	def /(lambda : BigDecimal) : Matrix[Rows, Columns] = new Matrix[Rows, Columns](rows, columns, Array.tabulate(values.length) (index => values(index) / lambda))
	def *[OtherColumns <: Nat](m : Matrix[Columns, OtherColumns]) : Matrix[Rows, OtherColumns] = {
		var multiplicationValues : Array[BigDecimal] = Array.tabulate(rowCount * m.columnCount) (index => {
			var row = index / m.columnCount
			var column = index % m.columnCount
			var result : BigDecimal = 0
			for (i <- 0 until columns) {
				result = result + (apply(row, i) * m(i, column))
			}
			result
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
		for (currentRow <- 0 until rowCount) {
			var currentColumn = 0
			range.foreach((column) => {
				subValues(currentRow * subColumns + currentColumn)= values(currentRow*columnCount + column)
			})
		}
		new Matrix(rows, subColumns, subValues)
	}
	
	def column(column : Int) : Matrix[Rows, Succ[Zero]] = {
		val subValues = new Array[BigDecimal](rowCount)
		
		for (currentRow <- 0 until rowCount) {
			subValues(currentRow)= values(currentRow*columnCount + column)
		}
		new Matrix(rows, one, subValues) 
	}
	
	def transpose() : Matrix[Columns, Rows] = new Matrix[Columns, Rows](columns, rows, Array.tabulate(values.length) (index => (values((index % rows) * columns + index / rows))))
	
	def apply(row : Int, column : Int) : BigDecimal = values(row*columnCount + column)
	
	override def equals(that : Any): Boolean = that match {
		case that : Matrix[_,_] => rowCount == that.rowCount && columnCount == that.columnCount && valuesEquals(that.values)
		case _             		=> false
	}
	
	override def toString(): String = {
		var result = "\n"
		for (i <- 0 until rowCount) {
			result += "\t"
			for (j <- 0 until columnCount) {
				result += "\t" + values(i * columnCount + j)
			}
			result += "\t\n"
		}
		result
	}
		
	private
	def valuesEquals(otherValues: Array[BigDecimal]): Boolean = {
		var result = true
		var i = 0
		while ((i < values.length) && result) {
			result = result && (values(i) == otherValues(i))
			i += 1
		}
		result
	}
}

object Matrix {	
	def apply[Rows <: Nat, Columns <: Nat](rows : Rows, columns : Columns, values : Array[BigDecimal]) : Matrix[Rows, Columns] = {
		new Matrix[Rows, Columns](rows, columns, values)
	}
	
	def apply(file : String) : Matrix[Nat, Nat] = buildFromTuple(loadData(file))
	
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
	
	def loadData(file : String) : (Nat, Nat, Array[BigDecimal]) = {
		val data = scala.io.Source.fromFile(file)
		val lines = data.getLines.toIterable
		val rows = lines.size
		val values = lines.flatMap((line) => line.split(",")).map((s) => BigDecimal(s)).toArray
		data.close()
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