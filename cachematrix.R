## Put comments here that give an overall description of what your
## functions do

## this function is used to created a list that contains functions to create/return/assign inversed/get inversed version of a matrix
## in order to use the function, one can type m <- makeCacheMatrix()

makeCacheMatrix <- function(x = matrix()) {
	inverse <- NULL
	set <- function(matrix)
	{
		x <<- matrix
		inverse <<- NULL
	}
	get <- function() x	
	setInverse <- function(inv) inverse <<- inv
	getInverse <- function() inverse
	
	## note the returned value is a list, the first element is called "set", 
	## whose value is the result of calling function "set" defined in the function
	list(set = set, get = get, setInverse = setInverse, getInverse = getInverse)
}


## this function takes use of the list created by the above function, if the inversed matrix already exists, it will directly use it and skip the repeated computation process

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
	inverse <- x$getInverse()
	if(!is.null(inverse))
	{
		message("getting cached data")
		return(inverse)
	}
	data <- x$get()
	inverse <- solve(data)
	x$setInverse(inverse)
	inverse
}
