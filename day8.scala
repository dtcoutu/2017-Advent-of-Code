object Day8 {
	def readInput: Seq[String] = {
		scala.io.Source.fromFile("input/day8.txt").mkString.split("\n")
	}
	
	object AvailableRegisters {
		var registers = scala.collection.mutable.Map[String, Int]()
		var largestValueEver = 0
		
		def get(register: String): (String, Int) = {
			if (registers.contains(register)) (register, registers(register))
			else {
				registers(register) = 0
				(register, 0)
			}
		}
		
		def update(register: String, newValue: Int) = {
			registers(register) = newValue
			if (newValue > largestValueEver) largestValueEver = newValue
		}
		
		def largestValue: Int = {
			registers.valuesIterator.max
		}
		
		def reset = {
			registers = scala.collection.mutable.Map[String, Int]()
		}
	}
	
	case class Instruction(register: String, modifier: Modifier, condition: Condition) {
		def process = {
			if (condition.eval) modifier.perform(register)
		}
	}
	case class Modifier(operation: Operation, value: Int) {
		def perform(register: String) = {
			val registerWithValue = AvailableRegisters.get(register)
			val modifier = operation.value * value
			AvailableRegisters.update(register, registerWithValue._2 + modifier)
		}
	}
	case class Condition(register: String, comparator: String, value: Int) {
		def eval: Boolean = {
			val registerWithValue = AvailableRegisters.get(register)
			comparator match {
				case "<" => registerWithValue._2 < value
				case "<=" => registerWithValue._2 <= value
				case "==" => registerWithValue._2 == value
				case ">=" => registerWithValue._2 >= value
				case ">" => registerWithValue._2 > value
				case "!=" => registerWithValue._2 != value
			}
		}
	}

	sealed trait Operation {
		def value: Int
	}
	case object inc extends Operation {
		def value: Int = 1
	}
	case object dec extends Operation {
		def value: Int = -1
	}
	
	object Operation {
		def fromString(value: String): Operation = {
			Vector(inc, dec).find(_.toString == value).get
	  }
	}
	
	val CommandRegEx = """(\w+) (inc|dec) (\-?\d+) if (\w+) (<|<=|==|>=|>|!=) (\-?\d+)""".r
	
	def parseInstructions(input: Seq[String]): Seq[Instruction] = {
		def parseLine(line: String): Instruction = {
			line match {
				case CommandRegEx(register, operation, modifierValue, checkRegister, comparator, checkValue) => {
					Instruction(register, Modifier(Operation.fromString(operation), modifierValue.toInt), Condition(checkRegister, comparator, checkValue.toInt))
				}
			}
		}
		
		input.map(parseLine)		
	}
	
	def process(instructions: Seq[Instruction]) = {
		instructions.map(_.process)
	}
	
	def largestRegisterValue(input: Seq[String]): Int = {
		AvailableRegisters.reset
		val lines = parseInstructions(input)
		process(lines)
		AvailableRegisters.largestValue
	}
	
	def largestValueEver: Int = {
		AvailableRegisters.largestValueEver
	}
}

// register operation modifier if register comparator value

def testInput = """b inc 5 if a > 1
a inc 1 if b < 5
c dec -10 if a >= 1
c inc -20 if c == 10"""

assert (Day8.largestRegisterValue(testInput.split("\n")) == 1)

println(Day8.largestRegisterValue(Day8.readInput))
println("largest value ever = " + Day8.largestValueEver)
