package com.digitalbrikes.linear_algebra

object Nat {
  sealed class Nat(val rank :Int)
  case class Zero() extends Nat(0)
  case class Succ[T <: Nat](val predecessor : T,  r : Int) extends Nat(r + 1)
  
  def zero : Zero = new Zero()
  def one : Succ[Zero] = successor(zero)
  
  def successor[T <: Nat](nat : T) = new Succ[T](nat, nat.rank)
  
  implicit def natToInt(nat : Nat) : Int = nat.rank
   
  def apply(number:Int) : Nat =
    (0 to number - 1).foldLeft[Nat](zero)((accu : Nat, _ : Int) => successor(accu))
}
