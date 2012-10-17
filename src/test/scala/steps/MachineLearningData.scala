package steps

import cucumber.runtime.{EN, ScalaDsl}
import org.scalatest.matchers.ShouldMatchers
import com.digitalbrikes.linear_algebra.Matrix
import com.digitalbrikes.linear_algebra.Matrix._
import com.digitalbrikes.linear_algebra.Nat
import com.digitalbrikes.linear_algebra.Nat._
import scala.runtime.Nothing$
import java.net.URL
import scala.io.Source
import com.digitalbrikes.linear_algebra.Vector
import com.digitalbrikes.linear_algebra.Vector._
  
object MachineLearningDataSteps extends ScalaDsl with EN with ShouldMatchers {
  var matrices : Option[Matrices] = None
  
  Given("""^the simple housing data$""") {
	  val source = scala.io.Source.fromURL(getClass().getClassLoader().getResource("ex1data1.txt"))
	  matrices = Some(new Matrices(source))
  }
  
  
}

class Matrices(source : Source) {
  def values = {
	  val data = loadData(source)
	  source.close()
	  (appendToZeros(data.column(0)), data.column(1))
  } 
  
  def appendToZeros[M <: Nat, N <: Nat](matrix : Matrix[M,N]) : Matrix[M, Succ[N]] = {
    val columns = matrix.columns + 1
    matrix.columns.successor match {
      case c : Succ[N] => Matrix(matrix.rows, c, Array.tabulate(matrix.rows.rank * columns) (index => if (0 == index % columns) 1 else matrix(index / columns, (index - index % columns) % matrix.columns)))
    }
  }
  
  def loadData[M <: Nat, N <: Nat](source : Source) = Matrix(source)
}

