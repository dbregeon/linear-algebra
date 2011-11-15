import Nat._

class Matrix[Rows <: Nat, Columns <: Nat](val rows : Int, val columns : Int, val values : Array[BigDecimal]) extends Serializable {
	def +(m : Matrix[Rows, Columns]) : Matrix[Rows, Columns] = new Matrix[Rows, Columns](rows, columns, Array.tabulate(values.length) (index => (values(index) + m.values(index))))
	def -(m : Matrix[Rows, Columns]) : Matrix[Rows, Columns] = new Matrix[Rows, Columns](rows, columns, Array.tabulate(values.length) (index => (values(index) - m.values(index))))
	def *:(lambda : BigDecimal) : Matrix[Rows, Columns] = new Matrix[Rows, Columns](rows, columns, Array.tabulate(values.length) (index => lambda * values(index)))
	def /(lambda : BigDecimal) : Matrix[Rows, Columns] = new Matrix[Rows, Columns](rows, columns, Array.tabulate(values.length) (index => values(index) / lambda))
	def *[OtherColumns <: Nat](m : Matrix[Columns, OtherColumns]) : Matrix[Rows, OtherColumns] = {
		var multiplicationValues : Array[BigDecimal] = Array.tabulate(rows * m.columns) (index => {
			var row = index / m.columns
			var column = index % m.columns
			var result : BigDecimal = 0
			for (i <- 0 until columns) {
				result = result + (apply(row, i) * m(i, column))
			}
			result
		})
		new Matrix[Rows, OtherColumns](rows, m.columns, multiplicationValues)
	}
	
	def transpose() : Matrix[Columns, Rows] = new Matrix[Columns, Rows](columns, rows, Array.tabulate(values.length) (index => (values((index % rows) * columns + index / rows))))
	
	def apply(row : Int, column : Int) : BigDecimal = values(row*columns + column)
	
	override def equals(that : Any): Boolean = that match {
		case that : Matrix[_,_] => rows == that.rows && columns == that.columns && valuesEquals(that.values)
		case _             		=> false
	}
	
	override def toString(): String = {
		var result = "\n"
		for (i <- 0 until rows) {
			result += "\t"
			for (j <- 0 until columns) {
				result += "\t" + values(i * columns + j)
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
	implicit def apply[Rows <: Nat, Columns <: Nat](rows : Rows, columns : Columns, values : Array[BigDecimal]) : Matrix[Rows, Columns] = {
		new Matrix[Rows, Columns](rows.value, columns.value, values)
	}
}

object Vector {
	type Vector[M <: Nat]=  Matrix[M, Succ[Zero]]
	
	implicit def apply[Rows <: Nat](rows : Rows, values : Array[BigDecimal]) : Vector[Rows] = {
		new Vector[Rows](rows.value, 1, values)
	}
}