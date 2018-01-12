import org.scalatest._

class Day12Spec extends FlatSpec with Matchers {
	"Parse line" should "create programs and connect them" in {
		val program = Day12.parseLine("0 <-> 2")
		
		assert(program._2 == List(2))
	}
	
	it should "handle multiple connections in a single line" in {
		val program = Day12.parseLine("2 <-> 0, 3, 4")

		assert(program._2.size == 3)
	}

		val sampleInput = """0 <-> 2
1 <-> 1
2 <-> 0, 3, 4
3 <-> 2, 4
4 <-> 2, 3, 6
5 <-> 6
6 <-> 4, 5"""
	
	"Parse input" should "create a list of programs with connections" in {
		val programs = Day12.parseInput(sampleInput)
		
		assert(programs.size == 7)
	}
	
	"Programs connected to 0" should "return the number of programs that are in the group" in {
		assert(Day12.countConnectedToZero(sampleInput) == 6)
	}
	
	val infiniteLoopInput = """0 <-> 1, 2
1 <-> 0, 2
2 <-> 0"""
	
	it should "avoid infinite loops"  in {
		assert(Day12.countConnectedToZero(infiniteLoopInput) == 3)
	}
	
	"Count groups" should "count the number of different groups that exist" in {
		assert(Day12.countGroups(sampleInput) == 2)
	}
}