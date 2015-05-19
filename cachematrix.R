## "Matrix inversion is usually a costly computation and there may be some benefit to caching 
## the inverse of a matrix rather than compute it repeatedly."

## "makeCacheMatrix" is a function which takes an input (matrix) and creates a "special matrix" 
## object. The object is cached so that it can be retrieved as a matrix (via the functions set 
## and get), or applied within the "cacheSolve" function to produce the inverse version of the 
## matrix (via the functions setinverse and getinverse).

makeCacheMatrix <- function(x = matrix()) {
        m = NULL
	set <- function(y) {
		x <<- y
		m <<- NULL
	}
	get <- function() x
	setinverse <- function(inverse) m <<- inverse
	getinverse <- function() m
	list(set=set, get=get, setinverse=setinverse, getinverse=getinverse)
}

## "cacheSolve" is a function which produces the inverse version of the matrix object. The
## function first checks to see if the inverse matrix has already been cached and, if so, 
## returns the matrix. If the inverse matrix is not available in the cache, the inverse 
## matrix is computed via the solve function and passed to the "setinverse" function before 
## being returned.

cacheSolve <- function(x, ...) {
	m <- x$getinverse()
	if(!is.null(m)) {
		message("Retrieving cached data")
		return(m)
	}
	data <- x$get()
	m <- solve(data)
	x$setinverse(m)
	return(m)
}
