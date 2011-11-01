object Nat {
  abstract class Nat {
  	def eval: Int
  	
  	def successor = new Succ[this.type](this.eval)
  }
  class Zero extends Nat {
  	def eval = 0
  }
  class Succ[T <: Nat](value : Int) extends Nat {
  	def eval = value
  }
  
 
  def apply(number:Int) : Nat = {
  	var result : Nat = new Zero()
  	for (i <- 1 until number) {
  		result = result.successor
  	}
  	result
  }
}
