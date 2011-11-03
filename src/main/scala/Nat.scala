object Nat {
  abstract class Nat(val value :Int) {
  	def successor = new Succ[this.type](value)
  }
  class Zero extends Nat(0) {
  }
  class Succ[T <: Nat](value : Int) extends Nat(value + 1) {
  }
  
 
  def apply(number:Int) : Nat = {
  	var result : Nat = new Zero()
  	for (i <- 0 until number) {
  		result = result.successor
  	}
  	result
  }
}
