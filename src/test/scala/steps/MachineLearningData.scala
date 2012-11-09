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
  
  Given("""^the complex housing data$""") {
	  val source = scala.io.Source.fromURL(getClass().getClassLoader().getResource("ex1data2.txt"))
	  matrices = Some(new Matrices(source))
  }
}

class Matrices(source : Source) {
  def values = {
	  val data = loadData(source)
	  source.close()
	  (appendToOnes(data.columns(0 to data.columns - 2)), data.column(data.columns - 1))
  } 
  
  def appendToOnes[M <: Nat, N <: Nat](matrix : Matrix[M,N]) : Matrix[M, Succ[N]] = {
    val columns = matrix.columns + 1
    Matrix(matrix.rows, successor(matrix.columns), Array.tabulate(matrix.rows.rank * columns) (index => if (0 == index % columns) 1 else matrix(index / columns, (index - index % columns) % matrix.columns)))
  }
  
  def loadData[M <: Nat, N <: Nat](source : Source) = Matrix(source)
}

