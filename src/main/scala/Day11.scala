object Day11 {
	def readInput: String = {
		scala.io.Source.fromFile("src/main/resources/day11.txt").mkString
	}
	
	case class Location(x: Int, y: Int, z: Int) {
		def update(direction: Direction) = Location(x+direction.xMod, y+direction.yMod, z+direction.zMod)
		
		val distanceFromOrigin = (Math.abs(x) + Math.abs(y) + Math.abs(z)) / 2
		
		def furtherThan(otherLocation: Location) = (distanceFromOrigin > otherLocation.distanceFromOrigin)
	}
	
	sealed trait Direction {
		val xMod: Int
		val yMod: Int
		val zMod: Int
	}
	object Direction extends Enumeration {
		def fromString(value: String): Direction = {
			Vector(N,NE,SE,S,SW,NW).find(a => a.toString == value.toUpperCase).get()
		}
	}
	case class N() extends Direction {
		val xMod = 0
		val yMod = 1
		val zMod = -1
	}
	case class NE() extends Direction {
		val xMod = 1
		val yMod = 0
		val zMod = -1
	}
	case class SE() extends Direction {
		val xMod = 1
		val yMod = -1
		val zMod = 0
	}
	case class S() extends Direction {
		val xMod = 0
		val yMod = -1
		val zMod = 1
	}
	case class SW() extends Direction {
		val xMod = -1
		val yMod = 0
		val zMod = 1
	}
	case class NW() extends Direction {
		val xMod = -1
		val yMod = 1
		val zMod = 0
	}
	
	def walk(steps: List[Direction]): (Location, Location) = {
		def loop(remainingSteps: List[Direction], location: Location, furthestLocation: Location): (Location, Location) = {
			def updateFurthest = if (location.furtherThan(furthestLocation)) location else furthestLocation
			
			remainingSteps match {
				case List() => (location, furthestLocation)
				case head :: tail => loop(tail, location.update(head), updateFurthest)
			}
		}
		
		loop(steps, Location(0,0,0), Location(0,0,0))
	}
	
	def process(path: String): (Location, Location) = {
		val steps = path.split(",").map(Direction.fromString).toList
		walk(steps)
	}
	
	def main(args: Array[String]): Unit = {
		val results = process(readInput)
		println("steps = " + results._1.distanceFromOrigin)
		println("furthest = " + results._2.distanceFromOrigin)
	}
}