object Day9 {
	def readInput: String = {
		scala.io.Source.fromFile("src/main/resources/day9.txt").mkString
	}
	
	def input(data: String): List[StreamProcessor] = {
		process(data.toList)
	}
	
	def scoreInput(data: String): Int = {
		val groups = input(data)
		
		def loop(groups: List[StreamProcessor], depth: Int): Int = {
			groups match {
				case List() => 0
				case head :: tail => head match {
					case Group(children) => {
						depth + loop(children, depth+1) + loop(tail, depth)
					}
					case _ => loop(tail, depth)
				}
			}
		}
		
		loop(groups, 1)
	}
	
	def garbageCountInput(data: String): Int = {
		val groups = input(data)
		
		def loop(groups: List[StreamProcessor], total: Int): Int = {
			groups match {
				case List() => total
				case head :: tail => head match {
					case Garbage(characters) => loop(tail, total + characters)
					case Group(children) => loop(tail, loop(children, total))
					case _ => loop(tail, total)
				}
			}
		}
		
		loop(groups, 0)
	}
	
	def process(input: List[Char]): List[StreamProcessor] = {
		def loop(input: List[Char], roots: List[StreamProcessor] = List()): List[StreamProcessor] = {
			input match {
				case List() => roots
				case head :: tail => head match {
					case '<' => {
						val (remainingInput, processedGarbage) = Garbage().process(tail)
						loop(remainingInput, processedGarbage :: roots)
					}
					case '{' => {
						val (remainingInput, processedGroup) = Group().process(tail)
						loop(remainingInput, processedGroup :: roots)
					}
					case '!' => {
						val (remainingInput, processedIgnore) = Ignore().process(tail)
						loop(remainingInput, roots)
					}
					case _ => loop(tail, roots)
				}
			}
		}
		loop(input, List())
	}
	
	sealed trait StreamProcessor
	case class Group(children: List[StreamProcessor] = List()) extends StreamProcessor {
		def process(input: List[Char], children: List[StreamProcessor] = List()): (List[Char], StreamProcessor) = {
			input match {
				case List() => (List(), this)
				case head :: tail => head match {
					case '!' => {
						val (remainingInput, processedIgnore) = Ignore().process(tail)
						process(remainingInput, children)
					}
					case '{' => {
						val (remainingInput, processedGroup) = Group().process(tail)
						process(remainingInput, processedGroup :: children)
					}
					case '}' => {
						(tail, Group(children))
					}
					case '<' => {
						val (remainingInput, processedGarbage) = Garbage().process(tail)
						process(remainingInput, processedGarbage :: children)
					}
					case _ => process(tail, children)
				}
			}
		}
	}
	case class Garbage(characters: Int = 0) extends StreamProcessor {
		def process(input: List[Char], characters: Int = 0): (List[Char], StreamProcessor) = {
			input match {
				case List() => (List(), this)
				case head :: tail => head match {
					case '>' => (tail, Garbage(characters))
					case '!' => {
						val (remainingInput, processedIgnore) = Ignore().process(tail)
						process(remainingInput, characters)
					}
					case _ => process(tail, characters+1)
				}
			}
		}
	}

	case class Ignore() extends StreamProcessor {
		def process(input: List[Char]): (List[Char], StreamProcessor) = {
			input match {
				case List() => (List(), this)
				case head :: tail => (tail, this)
			}
		}
	}
	
	def main(args: Array[String]): Unit = {
		println("score = " + scoreInput(readInput))
		println("garbage count = " + garbageCountInput(readInput))
	}
}