import org.scalatest._

class Day11Spec extends FlatSpec with Matchers {
	"Walk" should "determine ending location" in {
		assert(Day11.process("ne,ne,ne") == Day11.Location(3,0,-3))
		assert(Day11.process("ne,ne,sw,sw") == Day11.Location(0,0,0))
		assert(Day11.process("ne,ne,s,s") == Day11.Location(2,-2,0))
	}
	
	"Process" should "determine number of direct steps away" in {
		assert(Day11.stepsAway("ne,ne,ne") == 3)
		assert(Day11.stepsAway("se,sw,se,sw,sw") == 3)
	}
}