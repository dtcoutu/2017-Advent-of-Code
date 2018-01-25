import scala.annotation.tailrec

object Day13 {
	def readInput: String = {
		scala.io.Source.fromFile("src/main/resources/day13.txt").mkString
	}
	
	case class Scanner(depth: Int, range: Int) {
		val severity = depth * range
		
		def packetCapture: Boolean = {
			if (depth == 0) false
			else depth % (range*2-2) == 0
		}
			
		val actualSeverity = if (packetCapture) severity else 0
	}
	
	val scannerRegex = """([0-9]+): ([0-9]+)""".r

	def parseInput(input: String): Seq[Scanner] = {
		input.split("\\n").map { line =>
			line match {
				case scannerRegex(depth, range) => Scanner(depth.toInt, range.toInt)
			}
		} 
	}
	
	def calculateTripSeverity(input: String): Int = {
		parseInput(input).map(_.actualSeverity).sum
	}
	
	def calculateDelayRequired(input: String): Int = {
	}

	def main(args: Array[String]): Unit = {
		println("Trip Severity: " + calculateTripSeverity(readInput))
	}
}