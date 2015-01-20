## A pair of functions to process the caching and retrieval
## of an inverse of a square invertible matrix.                 

#
# The makeCacheMatrix function creates a special list which contains
# functions to store and retrieve a matrix and the inverse of the matrix
# the internal functions are set and get for the value of the matrix
# setinverse and getinverse for the inverse of the matrix
#
makeCacheMatrix <- function(mtx = matrix()) {
    inv <- NULL
    set <- function(y) {
        mtx <<- y
        inv <<- NULL
    }
    get <- function() mtx
    setinverse <- function(inverse) inv <<- inverse
    getinverse <- function() inv
    list(set = set, get = get,
        setinverse = setinverse,
        getinverse = getinverse)
}


#
# The cacheSolve function returns the inverse of a matrix using a special
# list created with the makeCacheMatrix function. Before creating
# the inverse matrix to return, it checks the cache to see if
# the inverse was already calculated and returns the cached inverse 
# matrix if avaialble. If not available, it calculates the inverse
# with the solve function, saves it to the cache and returns it.
#
cacheSolve <- function(mtx, ...) {
## Return a matrix that is the inverse of 'mtx'
    inv <- mtx$getinverse()
    if(!is.null(inv)) {
        message("getting cached data")
        return(inv)
    }
    data <- mtx$get()
    inv <- solve(data, ...)
    mtx$setinverse(inv)
    inv
}
