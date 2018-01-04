import scala.annotation.tailrec

object Day12 {
	def readInput: String = {
		scala.io.Source.fromFile("src/main/resources/day12.txt").mkString
	}
	
	def parseInput(input: String): Map[Int, List[Int]] = {
		input.split("\n").map(parseLine).toMap
	}
	
	def parseLine(input: String): (Int, List[Int]) = {
		val inputParts = input.split(" <-> ")

		(inputParts(0).toInt, inputParts(1).split(", ").map(_.toInt).toList)
	}
	
	def countConnectedToZero(input: String): Int = {
		val programs = parseInput(input)
		
		def isConnected(programId: Int): Boolean = {
			@tailrec
			def checkChildren(programsToCheck: List[Int], checked: List[Int] = List()): Boolean = {
				programsToCheck match {
					case List() => false
					case head :: tail if checked.contains(head) => checkChildren(tail, checked)
					case head :: tail => if (head == 0) true else checkChildren(programs(head) ::: tail, head :: checked)
				}
			}
			
			if (programId == 0) true
			else checkChildren(programs(programId))
		}
		
		@tailrec
		def loop(remainingProgramKeys: List[Int], confirmedConnectedToZero: Set[Int], count: Int): Int = {
			remainingProgramKeys match {
				case List() => count
				case head :: tail => {
					if (confirmedConnectedToZero.contains(head) || isConnected(head)) loop(tail, confirmedConnectedToZero + head, count+1)
					else loop(tail, confirmedConnectedToZero, count)
				}
			}
		}
		
		loop(programs.keySet.toList, Set(), 0)
	}
	
	def main(args: Array[String]): Unit = {
		println("Programs in the group that contains program ID 0: " + countConnectedToZero(readInput))
	}
}