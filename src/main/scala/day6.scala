class Day6 {
	case class Bank(blocks: Int) {
		def addOne: Bank = Bank(blocks+1)
	}

	case class Memory(banks: List[Bank]) {
		def bankWithMostBlocks(): Int = {
			return banks.zipWithIndex.maxBy(_._1.blocks)._2
		}
	
		def redistribute: Memory = {
			val bankIndex = bankWithMostBlocks()
			val blocks = banks(bankIndex).blocks
			Memory(distributeBanks(banks.updated(bankIndex, Bank(0)), nextBankIndex(bankIndex), blocks))
		}
	
		def distributeBanks(banks: List[Bank], bankIndex: Int, blocksRemaining: Int): List[Bank] = {
			if (blocksRemaining == 0) return banks
			else distributeBanks(banks.updated(bankIndex, banks(bankIndex).addOne), nextBankIndex(bankIndex), blocksRemaining-1)
		}
	
		def nextBankIndex(bankIndex: Int): Int = {
			return (bankIndex + 1) % banks.size
		}
	}

	object Debugger {
		def stepsBeforeDuplicateState(memory: Memory): (Int, Int) = {
			return debug(List(memory), 0)
		}
	
		def debug(memoryStates: List[Memory], steps: Int): (Int, Int) = {
			val nextMemoryState = memoryStates.head.redistribute
			if (memoryStates.exists(_.equals(nextMemoryState))) return (steps+1, memoryStates.indexOf(nextMemoryState)+1)
			else debug(nextMemoryState :: memoryStates, steps+1)
		}
	}

	def stringToMemoryBanks(input: String): Memory = {
		return Memory(input.split("\\t").map(a => Bank(a.toInt)).toList)
	}

	val test = stringToMemoryBanks("0	2	7	0")

	assert (Debugger.stepsBeforeDuplicateState(test) == (5, 4))

	val actual = stringToMemoryBanks("10	3	15	10	5	15	5	15	9	2	5	8	5	2	3	6")

	println(Debugger.stepsBeforeDuplicateState(actual))
}