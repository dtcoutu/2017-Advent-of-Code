// Snagged from https://math.stackexchange.com/a/163101
def spiral(int n) {
	def k = Math.ceil((Math.sqrt(n)-1)/2)
	def t = 2*k+1
	def m = Math.pow(t,2) 
	t = t-1
	
	if (n>=m-t)
		return [k-(m-n),-k]
	else m=m-t
	
	if (n>=m-t)
		return [-k,-k+(m-n)]
	else m=m-t
	
	if (n>=m-t)
		return [-k+(m-n),k]
	else return [k,k-(m-n-t)]
}

def manhattanDistance(List startingPoint) {
	Math.abs(startingPoint[0]) + Math.abs(startingPoint[1])
}

def requiredSteps(int value) {
	manhattanDistance(spiral(value))
}

assert requiredSteps(1) == 0
assert requiredSteps(12) == 3
assert requiredSteps(23) == 2
assert requiredSteps(1024) == 31

println requiredSteps(325489)