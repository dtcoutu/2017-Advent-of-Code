import org.scalatest._

class Day9Spec extends FlatSpec with Matchers {

  "Garbage" should "handle single blocks of garbage" in {
    assert(Day9.input("<>") == List(Day9.Garbage()))
		assert(Day9.input("<random characters>") == List(Day9.Garbage(17)))
		assert(Day9.input("<<<<>") == List(Day9.Garbage(3)))
		assert(Day9.input("""<{o"i!a,<{i<a>""") == List(Day9.Garbage(10)))
  }
	
	"Ignore" should "avoid closing garbage too soon" in {
		assert(Day9.input("<{!>}>") == List(Day9.Garbage(2)))
		assert(Day9.input("<!!>") == List(Day9.Garbage(0)))
		assert(Day9.input("<!!!>>") == List(Day9.Garbage(0)))
	}
	
	"Group" should "keep track of nested groups" in {
		assert(Day9.input("{}") == List(Day9.Group()))
		
		assert(Day9.input("{{{}}}") == List(Day9.Group(List(Day9.Group(List(Day9.Group()))))))
		
		assert(Day9.input("{{},{}}") == List(Day9.Group(List(Day9.Group(), Day9.Group()))))
		
		assert(Day9.input("{{{},{},{{}}}}") ==
			List(Day9.Group(
				List(Day9.Group(
					List(Day9.Group(List(Day9.Group())), Day9.Group(), Day9.Group())
				))
			))
		)
		
		assert(Day9.input("{<{},{},{{}}>}") == List(Day9.Group(List(Day9.Garbage(10)))))

		assert(Day9.input("{<a>,<a>,<a>,<a>}") == List(Day9.Group(List(Day9.Garbage(1), Day9.Garbage(1), Day9.Garbage(1), Day9.Garbage(1)))))
		
		assert(Day9.input("{{<a>},{<a>},{<a>},{<a>}}") == List(Day9.Group(List(Day9.Group(List(Day9.Garbage(1))),
																																					Day9.Group(List(Day9.Garbage(1))),
																																					Day9.Group(List(Day9.Garbage(1))),
																																					Day9.Group(List(Day9.Garbage(1)))))))
																																					
		assert(Day9.input("{{<!>},{<!>},{<!>},{<a>}}") == List(Day9.Group(List(Day9.Group(List(Day9.Garbage(13)))))))
	}
	
	"Ignore" should "avoid closing group too soon" in {
		assert(Day9.input("{!}{}}") == List(Day9.Group(List(Day9.Group()))))
	}
	
	"Group" should "keep track of score" in {
		assert(Day9.scoreInput("{}") == 1)
		assert(Day9.scoreInput("{{{}}}") == 6)
		assert(Day9.scoreInput("{{},{}}") == 5)
		assert(Day9.scoreInput("{{{},{},{{}}}}") == 16)
		assert(Day9.scoreInput("{{<a!>},{<a!>},{<a!>},{<ab>}}") == 3)
	}
	
	"Garbage" should "count characters contained" in {
		assert(Day9.garbageCountInput("<>") == 0)
		assert(Day9.garbageCountInput("<random characters>") == 17)
	}
}