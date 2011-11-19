object Nat {
  abstract class Nat(val rank :Int) {
	def successor : Nat
  }
  
  class Zero extends Nat(0) {
    def successor = new Succ[Zero](0)
  }
  
  class Succ[T <: Nat](r : Int) extends Nat(r + 1) {
    def successor = new Succ[Succ[T]](rank)
  }
  
  def zero : Zero = new Zero()
  def one : Succ[Zero] = zero.successor
  
  implicit def natToInt(nat : Nat) : Int = nat.rank
   
  def apply(number:Int) : Nat = {
  	var result : Nat = zero
  	for (i <- 0 until number) {
  		result = result.successor
  	}
  	result
  }
}
