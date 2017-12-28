object Day9 {
	def readInput: List[Char] = {
		scala.io.Source.fromFile("input/day9.txt").mkString.toList
	}
	
	sealed trait StreamProcessor {
		def process(input: List[Char]): (List[Char], StreamProcessor)
	}
	case class Group(children: List[StreamProcessor] = List()) extends StreamProcessor {
		def process(input: List[Char]): (List[Char], StreamProcessor) = {
			input match {
				case List() => (List(), this)
				case head :: tail => head match {
					case '!' => {
						val initialIgnore = Ignore()
						val (remainingInput, processedIgnore) = initialIgnore.process(tail)
						process(remainingInput)
						//process(remainingInput, processedGarbage :: processed)
					}
					case '{' => {
						val initialGroup = Group()
						val (remainingInput, processedGroup) = initialGroup.process(tail)
						process(remainingInput)
					}

					case '}' => (tail, Group())
					case '<' => {
						val initialGarbage = Garbage()
						val (remainingInput, processedGarbage) = initialGarbage.process(tail)
						process(remainingInput)
					}
				}
			}
		}
	}
	case class Garbage() extends StreamProcessor {
		def process(input: List[Char]): (List[Char], StreamProcessor) = {
			input match {
				case List() => (List(), this)
				case head :: tail => head match {
					case '!' => {
						val initialIgnore = Ignore()
						val (remainingInput, processedIgnore) = initialIgnore.process(tail)
						process(remainingInput)
						//process(remainingInput, processedIgnore :: processed)
					}
					case '>' => (tail, this)
					case _ => process(tail)
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
	
}