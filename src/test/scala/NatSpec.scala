import org.specs2.mutable._

class NatSpec extends Specification {
	"Nat(0)" should {
		"have a value of 0" in {
			Nat(0).value must be_==(0)
		}
	}
	
	"Nat(1)" should {		
		"have a value of 1" in {
			Nat(1).value must be_==(1)
		}
	}
	
	"Nat(2)" should {
		"have a value of 2" in {
			Nat(2).value must be_==(2)
		}
	}
}