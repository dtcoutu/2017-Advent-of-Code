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
		val connectedToZero = allProgramIdsConnectedTo(0, programs)
		
		connectedToZero.size
	}

	def allProgramIdsConnectedTo(targetProgramId: Int, programs: Map[Int,List[Int]]): Set[Int] = {
		def isConnectedTo(targetProgramId: Int, programIdToCheck: Int): Boolean = {
			@tailrec
			def checkChildren(programsToCheck: List[Int], checked: List[Int] = List()): Boolean = {
				programsToCheck match {
					case List() => false
					case head :: tail if checked.contains(head) => checkChildren(tail, checked)
					case head :: tail => if (head == targetProgramId) true else checkChildren(programs(head) ::: tail, head :: checked)
				}
			}
			
			if (programIdToCheck == targetProgramId) true
			else checkChildren(programs(programIdToCheck), List())
		}
		
		@tailrec
		def loop(remainingProgramKeys: List[Int], connectedPrograms: Set[Int]): Set[Int] = remainingProgramKeys match {
			case List() => connectedPrograms
			case head :: tail => {
				if (connectedPrograms.contains(head) || isConnectedTo(targetProgramId, head)) loop(tail, connectedPrograms + head)
				else loop(tail, connectedPrograms)
			}
		}
		
		loop(programs.keySet.toList, Set())
	}
	
	def countGroups(input: String): Int = {
		val programs = parseInput(input)
		
		def loop(remainingProgramKeys: List[Int], groups: List[Set[Int]]): List[Set[Int]] = remainingProgramKeys match {
			case List() => groups
			case head :: tail => {
				if (groups.exists(g => g.contains(head))) loop(tail, groups)
				// This got the job done, but I could have made the list of programs smaller as they were processed...
				else loop(tail, allProgramIdsConnectedTo(head, programs) :: groups)
			}
		}
		
		val groups = loop(programs.keySet.toList, List())
		
		groups.size
	}
	
	def main(args: Array[String]): Unit = {
		println("Programs in the group that contains program ID 0: " + countConnectedToZero(readInput))
		println("Number of groups: " + countGroups(readInput))
	}
}