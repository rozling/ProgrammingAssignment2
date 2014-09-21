## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function

# This function takes a matrix as an argument, sets up a variable to cache the inverse, then establishes get + set methods to get/set the original Matrix.
# setinverse function is used by an external function to set the inverse of this 'object'
# getinverse returns the cached inverse matrix, whose environment is the parent of this function

makeCacheMatrix <- function(originalMatrix = matrix()) {

	    cachedinverseMatrix <- NULL

	    set <- function(y) {
		        originalMatrix <<- y
		        cachedinverseMatrix <<- NULL
	    }

	    get <- function() originalMatrix
	    setinverse <- function(asdf) cachedinverseMatrix <<- asdf
	    getinverse <- function() cachedinverseMatrix

	    list(set = set, get = get,
	    	setinverse = setinverse,
	    	getinverse = getinverse)

}


## Write a short comment describing this function

# takes an input 'x', and checks whether it already has a cached inverse.
# if so it returns this value and ends; if not it makes a copy of the input matrix, calculates its inverse, sets the inverse on this object and returns its value

cacheSolve <- function(x, ...) {
    m <- x$getinverse()
    if(!is.null(m)) {
            message("getting cached data")
            return(m)
    }
    data <- x$get()
    m <- solve(data, ...)
    x$setinverse(m)
    m

}
