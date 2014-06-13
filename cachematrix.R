## The functions makeCacheMatrix and cacheSolve, cache and compute
## the inverse of a matrix.   

## makeCacheMatrix(x = matrix(), returns a special matrix-object 
## which is a list containing a function:
##	set(value)		to set the value of the matrix
##	get()			to get the value of the matrix
##	setinverse(value)	to set the value of the inverse
##	getinverse()		to get the value of the inverse
makeCacheMatrix <- function(x = matrix()) {
	i <- NULL
	set <- function(y) {
		x <<- y
		i <<- NULL
	}
	get <- function() x
	setinverse <- function(inverse) i <<- inverse
	getinverse <- function() i
	list(set = set, get = get,
		setinverse = setinverse,
		getinverse = getinverse)
}


## cacheSolve, computes the inverse of a makeCacheMatrix-object.
## It first checks to see if the inverse has already been computed.
## If so, it gets the inverse from the cache and skips the computation.
## Otherwise, it calculates the inverse of the matrix and sets the 
## value of the inverse in the cache via the setinverse function.
cacheSolve <- function(x, ...) {
	i <- x$getinverse()
	if(!is.null(i)) {
		message("getting cached data")
		return(i)
	}
	data <- x$get()
	i <- solve(data, ...)
	x$setinverse(i)
	i
}
