import scala.annotation.tailrec

object Day10 {
	def inputData = "129,154,49,198,200,133,97,254,41,6,2,1,255,0,191,108"
	def numbers = (0 to 255).toList
	
	def currentPosition = 0
	def skipSize = 0

	case class KnotHash(currentPosition: Int, skipSize: Int, list: List[Int]) {
		def process(length: Int): KnotHash = {
			val wrapping = ((currentPosition + length) >= list.size)
			val endingIndex = if (wrapping) (currentPosition + length) % list.size else (currentPosition + length)
			def sublist = {
				if (wrapping) list.slice(currentPosition, list.size) ++ list.take(endingIndex)
				else list.slice(currentPosition, endingIndex)
			}
			
			val middleRemainder = if (wrapping) list.slice(endingIndex, currentPosition) else List()
			val preRemainder = if (wrapping) List() else list.slice(0, currentPosition)
			val postRemainder = if (wrapping) List() else list.slice(endingIndex, list.length)
			
			def buildReversedList = {
				val reversedSublist = sublist.reverse
				if (wrapping) reversedSublist.takeRight(endingIndex) ++ middleRemainder ++ reversedSublist.dropRight(endingIndex)
				else preRemainder ++ reversedSublist ++ postRemainder
			}
			
			val nextPosition = (currentPosition + length + skipSize) % list.size
			
			KnotHash(nextPosition, skipSize + 1, buildReversedList)
		}
		
		val denseHash: List[Int] = list.grouped(16).toList.map(l => l.foldLeft(0)(_ ^ _))
		def hashString: String = denseHash.map { v =>
			val hex = v.toHexString
			if (hex.length < 2) "0" + hex else hex
		}.mkString("")
	}
	
	@tailrec
	def processKnot(inputData: List[Int], knotHash: KnotHash): KnotHash = inputData match {
		case List() => knotHash
		case head :: tail => {
			val next = knotHash.process(head)
			processKnot(tail, next)
		}
	}

	def process(numbers: List[Int], input: List[Int]): KnotHash = {
		val startingKnotHash = KnotHash(0, 0, numbers)
		
		processKnot(input, startingKnotHash)
	}
	
	def firstTwoProduct(numbers: List[Int], input: List[Int]): Int = {
		val endingKnot = process(numbers, input)
		endingKnot.list(0) * endingKnot.list(1)
	}
	
	def convertInput(input: String): List[Int] = {
		input.toList.map(_.toInt) ::: List(17,31,73,47,23)
	}
	
	def manyRounds(input: List[Int]): KnotHash = {
		val startingKnotHash = KnotHash(0, 0, numbers)
		
		def loop(remainingRounds: Int, knotHash: KnotHash): KnotHash = 
			if (remainingRounds == 0) knotHash
			else loop(remainingRounds-1, processKnot(input, knotHash))
			
		loop(64, startingKnotHash)
	}
	
	def secondPart(input: String): String = {
		val secondKnotHash = manyRounds(convertInput(input))
		secondKnotHash.hashString
	}
	
	def main(args: Array[String]): Unit = {
		val input = inputData.split(",").map(_.toInt).toList
		println("product = " + firstTwoProduct(numbers, input))
		println("hash string = " + secondPart(inputData))
	}
}