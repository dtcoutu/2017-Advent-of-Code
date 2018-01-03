import org.scalatest._

class Day10Spec extends FlatSpec with Matchers {
	"Convert input" should "take input and make it a sequence of ascii codes with appended defaults" in {
		assert(Day10.convertInput("1,2,3") == List(49,44,50,44,51,17,31,73,47,23))
	}
	
	"Reverse sublist" should "work with basic case" in {
		val initialKnot = Day10.KnotHash(0, 0, List(0,1,2,3,4))
		assert(initialKnot.process(3).list == List(2,1,0,3,4))
	}
	
	it should "work with wrapping around the end of the list" in {
		val initialKnot = Day10.KnotHash(3, 0, List(0,1,2,3,4))
		assert(initialKnot.process(4).list == List(4,3,2,1,0))
	}
	
	it should "work with in middle of list" in {
		val initialKnot = Day10.KnotHash(1, 0, List(0,1,2,3,4))
		assert(initialKnot.process(2).list == List(0,2,1,3,4))
	}
	
	it should "work with entire list selected from non-zero index" in {
		val initialKnot = Day10.KnotHash(1,3,List(4, 3, 0, 1, 2))
		assert(initialKnot.process(5).list == List(3,4,2,1,0))
	}
	
	"Skip size" should "increase after every request" in {
		val initialKnot = Day10.KnotHash(0, 0, List(0,1,2,3,4))
		val secondKnot = initialKnot.process(3)
		assert(secondKnot.skipSize == 1)
		val thirdKnot = secondKnot.process(2)
		assert(thirdKnot.skipSize == 2)
	}
	
	"Current position" should "increase by length and skip size" in {
		val initialKnot = Day10.KnotHash(0, 0, List(0,1,2,3,4))
		val secondKnot = initialKnot.process(2)
		assert(secondKnot.currentPosition == 2)
		val thirdKnot = secondKnot.process(1)
		assert(thirdKnot.currentPosition == 4)
	}
	
	it should "wrap around the end of the list" in {
		val initialKnot = Day10.KnotHash(4, 2, List(0,1,2,3,4))
		assert(initialKnot.process(2).currentPosition == 3)
	}
	
	"Process" should "succeed for sample data" in {
		val knot = Day10.process(List(0, 1, 2, 3, 4), List(3, 4, 1, 5))
		assert(knot.currentPosition == 4)
		assert(knot.skipSize == 4)
		assert(knot.list == List(3,4,2,1,0))
	}
}