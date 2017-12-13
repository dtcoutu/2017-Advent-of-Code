import scala.annotation.tailrec

case class Square(location: Location, value: Int)

case class Location(x: Int, y: Int, depth: Int) {
	def next(): Location = {
		if (x == depth && y == -depth) return Location(x+1, y, depth+1)
		else if (x == depth && y < depth) return Location(x, y+1, depth)
		else if (x > -depth && y == depth) return Location(x-1, y, depth)
		else if (x == -depth && y > -depth) return Location(x, y-1, depth)
		else return Location(x+1, y, depth)
	}
}

assert (Location(0,0,0).next() == Location(1,0,1))
assert (Location(1,-1,1).next() == Location(2,-1,2))
assert (Location(2,0,2).next() == Location(2,1,2))
assert (Location(2,-2,2).next() == Location(3,-2,3))



object Grid {
	@tailrec
	def addToLimit(grid: List[Square], limit: Int): Int = {
		val newGrid = add(grid)
		if (newGrid.head.value <= limit) addToLimit(newGrid,limit)
		else return newGrid.head.value
	}
	
	def add(grid: List[Square]): List[Square] = {
		if (grid == Nil) return List(Square(Location(0,0,0),1))
		val lastSquare = grid.head
		val nextLocation = lastSquare.location.next()
		val nextValue = sumNeighbors(nextLocation, grid)
		return Square(nextLocation, nextValue) :: grid
	}
	
	def sumNeighbors(location: Location, grid: List[Square]): Int = {
		val neighbors = grid.filter(square =>
			square.location.x >= location.x-1 &&
			square.location.x <= location.x+1 &&
			square.location.y >= location.y-1 &&
			square.location.y <= location.y+1
		)
		
		return neighbors.foldLeft(0)((sum, square) => sum + square.value)
	}
}

val testGrid1 = List(Square(Location(0,1,1),4), Square(Location(1,1,1),2),Square(Location(1,0,1),1),Square(Location(0,0,0),1))
val testGrid1Added = Grid.add(testGrid1)
assert (testGrid1Added.head == Square(Location(-1,1,1),5))


println(Grid.addToLimit(Nil, 325489))
