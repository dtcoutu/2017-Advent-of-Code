import org.scalatest._

class Day13Spec extends FlatSpec with Matchers {
	val sampleInput = """0: 3
1: 2
4: 4
6: 4"""
	
	"Parse input" should "return a sequence of Scanners" in {
		assert(Day13.parseInput(sampleInput).size == 4)
	}
	
	"Scanner" should "capture packet if it is at the top" in {
		assert(Day13.Scanner(6,4).packetCapture == true)
	}
	
	it should "fail to capture the package for depth 0, range 3" in {
		assert(Day13.Scanner(0,3).packetCapture == false)
	}
	it should "fail to capture the package for depth 1, range 2" in {
		assert(Day13.Scanner(1,2).packetCapture == false)
	}
	it should "fail to capture the package for depth 4, range 4" in {
		assert(Day13.Scanner(4,4).packetCapture == false)
	}
	
	"Severity of trip" should "be calculated from captured packets" in {
		assert(Day13.calculateTripSeverity(sampleInput) == 24)
	}
}